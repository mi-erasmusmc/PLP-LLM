library(ggplot2)
library(fuzzyjoin)

studyPlotPalette <- c(
  "#0072B2", "#E69F00", "#009E73", "#D55E00",
  "#CC79A7", "#56B4E9", "#F0E442", "#000000"
)

studyOutput <- function(outputFolder, regeneratePlpPlots = TRUE) {
  runFolders <- list.dirs(outputFolder, recursive = FALSE, full.names = TRUE)
  runFolders <- runFolders[file.exists(file.path(runFolders, "plpResult", "runPlp.rds"))]
  
  runFolders <- runFolders[-grep("XGBoost", runFolders)] # Fix to remove XGBoost results for MIE abstract.

  if (length(runFolders) == 0) {
    stop("studyOutput: no PLP results found in ", outputFolder)
  }
  
  # Collect output
  summaryResults <- summarizeResults(runFolders, regeneratePlpPlots)
  saveRDS(summaryResults, file.path(outputFolder, "summaryResults.rds"))
  utils::write.csv(summaryResults$featureImportanceAll,
                  file.path(outputFolder, "feature_importance_by_run.csv"), row.names = FALSE)
  
  # Create plots
  # summaryResults <- readRDS(file.path(paste0(root,"/",outputFolder), "summaryResults.rds"))
  plotResults(summaryResults, paste0(root,"/",outputFolder))
}

# Collect output helpers
summarizeResults <- function(runFolders, regeneratePlpPlots = TRUE) {
  metricsAll <- list()
  curvesAll <- list()
  featureImportanceAll <- list()
  localFeatureImportanceAll <- list()
  explanationPatientsAll <- list()
  
  for (runFolder in runFolders) {
    runResult <- summariseRun(runFolder, regeneratePlpPlots)
    if (is.null(runResult)) {
      next
    }
    
    metricsAll[[length(metricsAll) + 1]] <- runResult$metrics
    curvesAll[[length(curvesAll) + 1]] <- runResult$curves
    featureImportanceAll[[length(featureImportanceAll) + 1]] <- runResult$featureImportance
    localFeatureImportanceAll[[length(localFeatureImportanceAll) + 1]] <- runResult$localFeatureImportance
    explanationPatientsAll[[length(explanationPatientsAll) + 1]] <- runResult$explanationPatients
  }
  
  metricsAll <- do.call(rbind, metricsAll)
  curvesAll <- do.call(rbind, curvesAll)
  featureImportanceAll <- do.call(rbind, featureImportanceAll)
  
  return(list(
    metricsAll = metricsAll,
    curvesAll = curvesAll,
    featureImportanceAll = featureImportanceAll,
    localFeatureImportanceAll = dplyr::bind_rows(localFeatureImportanceAll),
    explanationPatientsAll = dplyr::bind_rows(explanationPatientsAll)
  ))
}

summariseRun <- function(runFolder, regeneratePlpPlots = TRUE) {
  if (!file.exists(runFolder)) return(NULL)
  
  plpResult <- PatientLevelPrediction::loadPlpResult(file.path(runFolder, "plpResult"))
  if (regeneratePlpPlots) PatientLevelPrediction::plotPlp(plpResult = plpResult, saveLocation = runFolder)
  
  analysisName <- plpResult$analysisRef$analysisName
  
  analysisId <- plpResult$analysisRef$analysisId
  analysisId <- sub("^LLM_qwen_", "LLMqwen_", analysisId) # Fix to get output from LLM.
  parts <- strsplit(analysisId, "_", fixed = TRUE)[[1]]
  splitInfo <- list(model = parts[1], plp = parts[2], covariates = parts[3], iteration = as.integer(parts[4]))
  
  evaluationStats <- plpResult$performanceEvaluation[["evaluationStatistics"]]
  if (is.null(evaluationStats) || nrow(evaluationStats) == 0) {
    warning("studyOutput: missing evaluation statistics for ", analysisId)
    return(NULL)
  }
  
  testStats <- evaluationStats[evaluationStats$evaluation == "Test", , drop = FALSE]
  if (nrow(testStats) == 0) {
    warning("studyOutput: skipping ", analysisId, " because no test evaluation statistics are available")
    return(NULL)
  }
  metricsVector <- setNames(testStats$value, testStats$metric)
  
  predictions <- plpResult$prediction
  predictions <- predictions[predictions$evaluationType == "Test", , drop = FALSE]
  if (nrow(predictions) == 0) {
    warning("studyOutput: no test predictions for ", analysisId)
    return(NULL)
  }
  predictions$outcome <- as.integer(predictions$outcomeCount)
  
  rocObj <- pROC::roc(predictions$outcome, predictions$value, quiet = TRUE, direction = "<")
  youden <- rocObj$sensitivities + rocObj$specificities - 1
  bestIndex <- which.max(youden)
  selectedThreshold <- rocObj$thresholds[bestIndex]
  
  maleRates <- confusionRates(predictions[predictions$gender == 8507, , drop = FALSE], selectedThreshold)
  femaleRates <- confusionRates(predictions[predictions$gender == 8532, , drop = FALSE],selectedThreshold)
  fairnessData <- list(fpr_male = maleRates$fpr, tpr_male = maleRates$tpr, fpr_female = femaleRates$fpr, tpr_female = femaleRates$tpr)
  
  curves <- data.frame(
    rowId = predictions$rowId,
    gender = predictions$gender,
    ageYear = predictions$ageYear,
    model = splitInfo$model,
    plp = splitInfo$plp,
    covariates = splitInfo$covariates,
    iteration = splitInfo$iteration,
    outcome = predictions$outcome,
    score = predictions$value,
    stringsAsFactors = FALSE
  )
  curves <- curves[!is.na(curves$outcome) & is.finite(curves$score), , drop = FALSE]
  
  metrics <- data.frame(
    analysisId = analysisId,
    analysisName = analysisName,
    model = splitInfo$model,
    plp = splitInfo$plp,
    covariates = splitInfo$covariates,
    iteration = splitInfo$iteration,
    populationSize = as.integer(metricsVector[["populationSize"]]),
    outcomeCount = as.integer(metricsVector[["outcomeCount"]]),
    auc = metricsVector[["AUROC"]],
    auc_lower = metricsVector[["95% lower AUROC"]],
    auc_upper = metricsVector[["95% upper AUROC"]],
    auprc = as.numeric(metricsVector[["AUPRC"]]),
    brier = as.numeric(metricsVector[["brier score"]]),
    threshold = selectedThreshold,
    threshold_youden = youden[bestIndex],
    fpr_male = fairnessData$fpr_male,
    tpr_male = fairnessData$tpr_male,
    fpr_female = fairnessData$fpr_female,
    tpr_female = fairnessData$tpr_female,
    stringsAsFactors = FALSE
  )
  
  covariateImportance <- plpResult$model$covariateImportance
  localFeatureImportance <- NULL
  explanationPatients <- NULL
  
  if(str_detect(runFolder, "LLM_")){
    pred<-predictions %>%
      select(rowId, top_5_features) %>%
      unnest(cols = c(top_5_features)) %>%
      mutate(feature_name=trimws(feature_name))
    
    covariates<-plpResult$covariateSummary %>%
      rowwise() %>%
      mutate(covariateName=ifelse(str_detect(covariateName,"index: "),str_split(covariateName,"index: ")[[1]][2],covariateName)) %>%
      select(covariateId, covariateName)
    
    library(fuzzyjoin)
    ### match the LLM feature to the orginal list
    merged_df <- stringdist_join(
      pred, 
      covariates, 
      by = c("feature_name" = "covariateName"), # Columns to compare
      mode = "left",                     # Type of join: left, inner, full, right
      method = "jw",                     # Distance metric (e.g., "jw" = Jaro-Winkler)
      max_dist = 0.25,                   # Maximum distance threshold for a match
      distance_col = "dist_score"        # Optional: creates a column showing the distance
    )
    merged_df_n <- merged_df %>%
      group_by(rowId, feature_name, positive) %>%
      slice_min(order_by = dist_score) %>%
      mutate(covariateName=ifelse(
        is.na(covariateName), 
        covariates$covariateName[which(stringr::str_detect(covariates$covariateName, dplyr::first(feature_name)))][1], 
        covariateName)) %>%
      filter(!is.na(covariateName))
    
    # Carry the existing name matching forward, resolving tied matches to one ID.
    matched <- merged_df_n |>
      dplyr::ungroup() |>
      dplyr::select(-covariateId) |>
      dplyr::left_join(covariates, by = "covariateName") |>
      dplyr::arrange(dist_score, covariateId) |>
      dplyr::distinct(rowId, feature_name, positive, .keep_all = TRUE)
    validIds <- unique(pred$rowId[!is.na(pred$feature_name) & nzchar(pred$feature_name)])
    explanationPatients <- data.frame(rowId = validIds) |>
      dplyr::mutate(model = splitInfo$model, plp = splitInfo$plp,
                    covariates = splitInfo$covariates, iteration = splitInfo$iteration)
    localFeatureImportance <- matched |>
      dplyr::select(rowId, covariateId, covariateName, positive, feature_name, dist_score) |>
      dplyr::mutate(model = splitInfo$model, plp = splitInfo$plp,
                    covariates = splitInfo$covariates, iteration = splitInfo$iteration)
    # llmPredict builds its prompts from every stored covariate row, including
    # demographics. Do not substitute covariateValue > 0 for actual exposure.
    dataFolder <- file.path(dirname(runFolder), paste0("plpData_", splitInfo$plp, "_", splitInfo$covariates))
    plpData <- PatientLevelPrediction::loadPlpData(dataFolder)
    exposure <- as.data.frame(plpData$covariateData$covariates) |>
      dplyr::filter(rowId %in% validIds) |>
      dplyr::distinct(rowId, covariateId)
    covariateImportance <- conditionalMentionRates(matched, exposure, covariates)
    localFeatureImportance <- localFeatureImportance |>
      dplyr::left_join(dplyr::mutate(exposure, supplied = TRUE), by = c("rowId", "covariateId")) |>
      dplyr::mutate(supplied = dplyr::coalesce(supplied, FALSE))
    explanationPatients$matchedFeatures <- vapply(validIds, function(id)
      length(unique(matched$covariateId[matched$rowId == id])), integer(1))
  }
  
  nFeatures <- if (is.null(covariateImportance)) 0 else nrow(covariateImportance)
  
  featureImportance <- data.frame(
    model = rep(splitInfo$model, nFeatures),
    plp = rep(splitInfo$plp, nFeatures),
    covariates = rep(splitInfo$covariates, nFeatures),
    iteration = rep(splitInfo$iteration, nFeatures),
    covariateId = covariateImportance$covariateId,
    covariateName = covariateImportance$covariateName,
    covariateValue = as.numeric(covariateImportance$covariateValue),
    stringsAsFactors = FALSE
  )
  featureImportance$importanceType <- rep(if (is.null(explanationPatients)) "coefficient" else "mention_frequency", nrow(featureImportance))
  for (column in c("mentionCount", "exposureCount", "unsupportedMentionCount")) {
    featureImportance[[column]] <- if (is.null(explanationPatients)) rep(NA_integer_, nFeatures) else covariateImportance[[column]]
  }
  
  list(metrics = metrics, curves = curves, featureImportance = featureImportance,
       localFeatureImportance = localFeatureImportance, explanationPatients = explanationPatients)
}

# Conditional on feature exposure among test patients with a valid explanation.
# Unsupported mentions remain auditable but do not enter the numerator.
conditionalMentionRates <- function(matched, exposure, covariates) {
  mentions <- dplyr::distinct(matched, rowId, covariateId)
  exposure <- dplyr::distinct(exposure, rowId, covariateId)
  counts <- mentions |>
    dplyr::semi_join(exposure, by = c("rowId", "covariateId")) |>
    dplyr::count(covariateId, name = "mentionCount")
  unsupported <- mentions |>
    dplyr::anti_join(exposure, by = c("rowId", "covariateId")) |>
    dplyr::count(covariateId, name = "unsupportedMentionCount")
  covariates |>
    dplyr::ungroup() |>
    dplyr::distinct(covariateId, .keep_all = TRUE) |>
    dplyr::left_join(dplyr::count(exposure, covariateId, name = "exposureCount"), by = "covariateId") |>
    dplyr::left_join(counts, by = "covariateId") |>
    dplyr::left_join(unsupported, by = "covariateId") |>
    dplyr::mutate(dplyr::across(c(mentionCount, exposureCount, unsupportedMentionCount), ~dplyr::coalesce(.x, 0L)),
      covariateValue = dplyr::if_else(exposureCount > 0, mentionCount / exposureCount, NA_real_))
}

confusionRates <- function(data, threshold) {
  if (nrow(data) == 0 || is.na(threshold)) {
    return(list(fpr = NA_real_, tpr = NA_real_))
  }
  
  predictedPositive <- data$value >= threshold
  actualPositive <- data$outcome == 1
  actualNegative <- data$outcome == 0
  
  tp <- sum(predictedPositive & actualPositive, na.rm = TRUE)
  fn <- sum(!predictedPositive & actualPositive, na.rm = TRUE)
  fp <- sum(predictedPositive & actualNegative, na.rm = TRUE)
  tn <- sum(!predictedPositive & actualNegative, na.rm = TRUE)
  
  tpr <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
  fpr <- if ((fp + tn) > 0) fp / (fp + tn) else NA_real_
  
  list(fpr = fpr, tpr = tpr)
}

# Create plot helpers
plotResults <- function(summaryResults, outputFolder) {
  summaryData <- summaryResults
  
  # Performance 
  performancePlot <- createPerformancePlot(summaryData, outputFolder)
  aucCurvePlot <- createAucCurvePlot(summaryData, outputFolder)
  auprcCurvePlot <- createAuprcCurvePlot(summaryData, outputFolder)
  calibrationCurvePlot <- createCalibrationCurvePlot(summaryData, outputFolder)
  
  # Consistency
  consistencyPlot <- createConsistencyPlot(summaryData, outputFolder)
  risk95Plot <- create95RiskPlot(summaryData, outputFolder)
  decisionUncertaintyPlot <- createDecisionUncertaintyPlot(summaryData, outputFolder)
  
  # Fairness
  fairnessPlot <- createFairnessPlot(summaryData, outputFolder)
  
  # Interpretability
  stabilityPlot <- createStabilityPlot(summaryData, outputFolder)
  featureHeatmap <- createFeatureHeatmap(summaryData, outputFolder)
  createExplanationConsistencyPlot(summaryData, outputFolder)
  
  # Create overview of selected plots
  plots <- list(performancePlot, consistencyPlot, fairnessPlot, stabilityPlot)
  dashboard <- cowplot::plot_grid(plotlist = plots, ncol = 2, labels = "AUTO")
  ggplot2::ggsave(file.path(outputFolder, "summary_dashboard.png"), dashboard, width = 14, height = 10, dpi = 300)
  
}

createPerformancePlot <- function(summaryResults, outputFolder) {
  
  # Figure summarize performance metrics across models
  metrics <- summaryResults$metricsAll
  metrics$modelLabel <- paste(metrics$model)
  
  plotData <- rbind(
    aggregateMetric(metrics, "auc", "AUC"),
    aggregateMetric(metrics, "auprc", "AUPRC"),
    aggregateMetric(metrics, "brier", "Brier")
  )
  
  plotData <- plotData[!is.na(plotData$value), , drop = FALSE]
  plotData$metric <- factor(plotData$metric, levels = c("AUC", "AUPRC", "Brier"))
  
  plot <- ggplot2::ggplot(plotData, ggplot2::aes(metric, value, fill = modelLabel)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(0.8), width = 0.7) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = pmax(0, value - error), ymax = pmin(1, value + error)),
      position = ggplot2::position_dodge(0.8), width = 0.2
    ) +
    ggplot2::facet_wrap(~plp) +
    ggplot2::scale_fill_manual(values = studyPlotPalette, drop = FALSE) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(title = "Predictive performance", x = "Metric", y = "Score", fill = "Model") +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
  
  ggplot2::ggsave(filename = file.path(outputFolder, "performance_metrics.png"), plot = plot, width = 8, height = 6, dpi = 300)
  plot
}

createAucCurvePlot <- function(summaryResults, outputFolder) {
  curveData <- summaryResults$curvesAll
  curveData$modelLabel <- paste(curveData$model, curveData$plp, curveData$covariates, sep = " | ")
  
  rocGrid <- buildRocGrid(curveData)
  summaryRoc <- stats::aggregate(
    rocGrid$tpr,
    by = list(plp = rocGrid$plp, modelLabel = rocGrid$modelLabel, fpr = rocGrid$fpr),
    FUN = function(x) c(
      mean = mean(x, na.rm = TRUE),
      lower = as.numeric(stats::quantile(x, 0.10, na.rm = TRUE)),
      upper = as.numeric(stats::quantile(x, 0.90, na.rm = TRUE))
    )
  )
  summaryRoc <- data.frame(
    plp = summaryRoc$plp,
    modelLabel = summaryRoc$modelLabel,
    fpr = summaryRoc$fpr,
    tprMean = summaryRoc$x[, "mean"],
    tprLower = pmax(0, summaryRoc$x[, "lower"]),
    tprUpper = pmin(1, summaryRoc$x[, "upper"]),
    stringsAsFactors = FALSE
  )
  
  # TODO: select iteration related to median score instead of first?
  rocGrid <- rocGrid[rocGrid$iteration == 1L, , drop = FALSE]
  
  plot <- ggplot2::ggplot(rocGrid, ggplot2::aes(x = fpr, y = tpr, color = modelLabel, group = modelLabel)) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey70") +
    ggplot2::geom_ribbon(data = summaryRoc, ggplot2::aes(x = fpr, ymin = tprLower, ymax = tprUpper, fill = modelLabel, group = modelLabel), alpha = 0.2, color = NA, inherit.aes = FALSE) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::facet_wrap(~plp) +
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    ggplot2::scale_color_manual(values = studyPlotPalette, drop = FALSE) +
    ggplot2::scale_fill_manual(values = studyPlotPalette, drop = FALSE, guide = "none") +
    ggplot2::labs(title = "AUC curve", x = "1 - Specificity (False Positive Rate)", y = "Sensitivity (True Positive Rate)", color = "Model") +
    ggplot2::theme_minimal()
  ggplot2::ggsave(filename = file.path(outputFolder, "performance_auc_curve.png"), plot = plot, width = 8, height = 6, dpi = 300)
  plot
}

createAuprcCurvePlot <- function(summaryResults, outputFolder) {
  curveData <- summaryResults$curvesAll
  curveData$modelLabel <- paste(curveData$model, curveData$plp, curveData$covariates, sep = " | ")
  prGrid <- buildPrGrid(curveData)
  if (is.null(prGrid) || nrow(prGrid) == 0) return(NULL)
  summaryPr <- stats::aggregate(
    prGrid$precision,
    by = list(plp = prGrid$plp, modelLabel = prGrid$modelLabel, recall = prGrid$recall),
    FUN = function(x) c(
      mean = mean(x, na.rm = TRUE),
      lower = as.numeric(stats::quantile(x, 0.10, na.rm = TRUE)),
      upper = as.numeric(stats::quantile(x, 0.90, na.rm = TRUE))
    )
  )
  summaryPr <- data.frame(
    plp = summaryPr$plp,
    modelLabel = summaryPr$modelLabel,
    recall = summaryPr$recall,
    precisionMean = summaryPr$x[, "mean"],
    precisionLower = pmax(0, summaryPr$x[, "lower"]),
    precisionUpper = pmin(1, summaryPr$x[, "upper"]),
    stringsAsFactors = FALSE
  )
  prGrid <- prGrid[prGrid$iteration == 1L, , drop = FALSE]
  plot <- ggplot2::ggplot(prGrid, ggplot2::aes(x = recall, y = precision, color = modelLabel, group = modelLabel)) +
    ggplot2::geom_ribbon(data = summaryPr, ggplot2::aes(x = recall, ymin = precisionLower, ymax = precisionUpper, fill = modelLabel, group = modelLabel), alpha = 0.2, color = NA, inherit.aes = FALSE) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::facet_wrap(~plp) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    ggplot2::scale_color_manual(values = studyPlotPalette, drop = FALSE) +
    ggplot2::scale_fill_manual(values = studyPlotPalette, drop = FALSE, guide = "none") +
    ggplot2::labs(title = "AUPRC curve", x = "Recall (Sensitivity)", y = "Precision (PPV)", color = "Model") +
    ggplot2::theme_minimal()
  ggplot2::ggsave(filename = file.path(outputFolder, "performance_auprc_curve.png"), plot = plot, width = 8, height = 6, dpi = 300)
  plot
}

createCalibrationCurvePlot <- function(summaryResults, outputFolder) {
  curveData <- summaryResults$curvesAll
  curveData <- curveData[!is.na(curveData$outcome) & is.finite(curveData$score) & curveData$outcome %in% c(0L, 1L), , drop = FALSE]
  curveData$modelLabel <- paste(curveData$model, curveData$plp, curveData$covariates, sep = " | ")
  calibrationGrid <- buildCalibrationGrid(curveData)
  if (is.null(calibrationGrid) || nrow(calibrationGrid) == 0) return(NULL)
  summaryCalibration <- stats::aggregate(
    calibrationGrid$observed,
    by = list(plp = calibrationGrid$plp, modelLabel = calibrationGrid$modelLabel, predicted = calibrationGrid$predicted),
    FUN = function(x) c(
      mean = mean(x, na.rm = TRUE),
      lower = as.numeric(stats::quantile(x, 0.10, na.rm = TRUE)),
      upper = as.numeric(stats::quantile(x, 0.90, na.rm = TRUE))
    )
  )
  summaryCalibration <- data.frame(
    plp = summaryCalibration$plp,
    modelLabel = summaryCalibration$modelLabel,
    predicted = summaryCalibration$predicted,
    observedMean = summaryCalibration$x[, "mean"],
    observedLower = pmax(0, summaryCalibration$x[, "lower"]),
    observedUpper = pmin(1, summaryCalibration$x[, "upper"]),
    stringsAsFactors = FALSE
  )
  representativeCalibration <- calibrationGrid[calibrationGrid$iteration == 1L, , drop = FALSE]
  calibrationPlot <- ggplot2::ggplot(representativeCalibration, ggplot2::aes(x = predicted, y = observed, color = modelLabel, group = modelLabel)) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey70") +
    ggplot2::geom_ribbon(data = summaryCalibration, ggplot2::aes(x = predicted, ymin = observedLower, ymax = observedUpper, fill = modelLabel, group = modelLabel), alpha = 0.2, color = NA, inherit.aes = FALSE) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::facet_wrap(~plp) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    ggplot2::scale_color_manual(values = studyPlotPalette, drop = FALSE) +
    ggplot2::scale_fill_manual(values = studyPlotPalette, drop = FALSE, guide = "none") +
    ggplot2::labs(title = "Calibration curve", x = "Predicted probability", y = "Observed probability", color = "Model") +
    ggplot2::theme_minimal()
  distributionPlot <- ggplot2::ggplot(curveData, ggplot2::aes(x = score, fill = modelLabel)) +
    ggplot2::geom_histogram(position = "identity", bins = 25, alpha = 0.35) +
    ggplot2::facet_grid(outcome ~ plp) +
    ggplot2::coord_cartesian(xlim = c(0, 1), expand = FALSE) +
    ggplot2::scale_fill_manual(values = studyPlotPalette, drop = FALSE) +
    ggplot2::labs(x = "Predicted probability", y = "Count", fill = "Model") +
    ggplot2::theme_minimal()
  plot <- patchwork::wrap_plots(calibrationPlot, distributionPlot, ncol = 1, heights = c(3, 1.6))
  ggplot2::ggsave(filename = file.path(outputFolder, "performance_calibration_curve.png"), plot = plot, width = 9, height = 8, dpi = 300)
  plot
}

createConsistencyPlot <- function(summaryResults, outputFolder, deviationThreshold = 0.1) {
  curveData <- summaryResults$curvesAll
  
  spreadData <- curveData |>
    dplyr::group_by(plp, model, rowId) |>
    dplyr::summarise(
      nIterations = dplyr::n(),
      predRange = max(score, na.rm = TRUE) - min(score, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(nIterations >= 2) # iterations do not necessarily have same individuals in test set
  
  # Fix to include existing score.
  spreadData <- rbind(spreadData, list("CHADS2", "existing", 1, 2, 0.0), list("CHADS2", "existing", 2, 3, 0.0), list("CHADS2", "existing", 3, 4, 0.0))
 
  modelSummary <- spreadData |>
    dplyr::group_by(plp, model) |>
    dplyr::summarise(
      nRows = dplyr::n(),
      nHighDeviation = sum(predRange > deviationThreshold, na.rm = TRUE),
      pctHighDeviation = ifelse(nRows > 0, 100 * nHighDeviation / nRows, NA_real_),
      .groups = "drop"
    ) |>
    dplyr::mutate(label = sprintf("%.1f%%", pctHighDeviation))
    
  # Fix to include existing score.
  # modelSummary <- rbind(modelSummary, list("CHADS2", "existing", 14465, 0, 0.0, "0.0%"))
  
  plot <- ggplot2::ggplot(spreadData, ggplot2::aes(x = model, y = predRange, fill = model)) +
    ggplot2::geom_violin(alpha = 0.25, color = NA, trim = TRUE) +
    ggplot2::geom_boxplot(width = 0.15, outlier.alpha = 0.2, color = "black") +
    ggplot2::geom_text(data = modelSummary, ggplot2::aes(x = model, y = 0.98, label = label), inherit.aes = FALSE, size = 3, vjust = 1, color = "black") +
    ggplot2::facet_wrap(~plp) +
    ggplot2::scale_fill_manual(values = studyPlotPalette, drop = FALSE) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(title = "Consistency", x = "Model", y = "Prediction range") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1), legend.position = "none",
      panel.grid = ggplot2::element_blank())
  ggplot2::ggsave(filename = file.path(outputFolder, "consistency_plot.png"), plot = plot, width = 8, height = 6, dpi = 300)
  plot
}

create95RiskPlot <- function(summaryResults, outputFolder) {
  curveData <- summaryResults$curvesAll
  
  summary95R <- curveData |>
    dplyr::group_by(plp, model, rowId) |>
    dplyr::summarise(
      nIterations = dplyr::n(),
      risk_center = stats::median(score, na.rm = TRUE),
      risk_lower95 = as.numeric(stats::quantile(score, probs = 0.025, na.rm = TRUE)),
      risk_upper95 = as.numeric(stats::quantile(score, probs = 0.975, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::filter(nIterations >= 2) |>
    dplyr::mutate(risk_width95 = risk_upper95 - risk_lower95)
  if (nrow(summary95R) == 0) return(NULL)
  plot <- ggplot2::ggplot(summary95R, ggplot2::aes(x = model, y = risk_width95, fill = model)) +
    ggplot2::geom_violin(alpha = 0.25, color = NA, trim = TRUE) +
    ggplot2::geom_boxplot(width = 0.15, outlier.alpha = 0.2, color = "black") +
    ggplot2::facet_wrap(~plp) +
    ggplot2::scale_fill_manual(values = studyPlotPalette, drop = FALSE) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(title = "95% Risk Range", x = "Model", y = "95% Risk Range") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1), legend.position = "none")
  ggplot2::ggsave(filename = file.path(outputFolder, "consistency_95R.png"), plot = plot, width = 8, height = 6, dpi = 300)
  plot
}

createDecisionUncertaintyPlot <- function(summaryResults, outputFolder, mode = c("fixed")) {
  mode <- match.arg(mode)
  curveData <- summaryResults$curvesAll
  
  if (mode == "fixed") {
    tau <- 0.2 # TODO: use threshold selected by youden index instead?
    summaryDU <- curveData |>
      dplyr::group_by(plp, model, rowId) |>
      dplyr::summarise(
        nIterations = dplyr::n(),
        DU = mean(score >= tau, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::filter(nIterations >= 2)
    
    plot <- ggplot2::ggplot(summaryDU, ggplot2::aes(x = model, y = DU, fill = model)) +
      ggplot2::geom_violin(alpha = 0.25, color = NA, trim = TRUE) +
      ggplot2::geom_boxplot(width = 0.15, outlier.alpha = 0.2, color = "black") +
      ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50") +
      ggplot2::facet_wrap(~plp) +
      ggplot2::scale_fill_manual(values = studyPlotPalette, drop = FALSE) +
      ggplot2::coord_cartesian(ylim = c(0, 1)) +
      ggplot2::labs(title = "Decision Uncertainty", x = "Model", y = "P(Score >= Threshold)", subtitle = sprintf("Threshold = %.2f", tau)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1), legend.position = "none")
    
    ggplot2::ggsave(filename = file.path(outputFolder, "consistency_DU.png"), plot = plot, width = 8, height = 6, dpi = 300)
  } else {
    tauGridData <- expand.grid(tau = seq(0, 1, by = 0.01), stringsAsFactors = FALSE)
    summaryDU <- merge(curveData, tauGridData, by = NULL)
    
    summaryDU <- summaryDU |>
      dplyr::group_by(plp, model, rowId, tau) |>
      dplyr::summarise(
        nIterations = dplyr::n(),
        DU = mean(score >= tau, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::filter(nIterations >= 2)
    
    summaryDUCurve <- summaryDU |>
      dplyr::group_by(plp, model, tau) |>
      dplyr::summarise(
        DU_center = stats::median(DU, na.rm = TRUE),
        DU_lower = as.numeric(stats::quantile(DU, 0.10, na.rm = TRUE)),
        DU_upper = as.numeric(stats::quantile(DU, 0.90, na.rm = TRUE)),
        .groups = "drop"
      )
    
    plot <- ggplot2::ggplot(summaryDUCurve, ggplot2::aes(x = tau, y = DU_center, color = model, fill = model)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = DU_lower, ymax = DU_upper), alpha = 0.2, color = NA) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50") +
      ggplot2::facet_wrap(~plp) +
      ggplot2::scale_color_manual(values = studyPlotPalette, drop = FALSE) +
      ggplot2::scale_fill_manual(values = studyPlotPalette, drop = FALSE) +
      ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
      ggplot2::labs(title = "Decision Uncertainty", x = "Decision threshold", y = "P(Score >= Threshold)", subtitle = "Line = Median across runs, Ribbon = 10th-90th percentile") +
      ggplot2::theme_minimal()
    ggplot2::ggsave(filename = file.path(outputFolder, "consistency_DU_grid.png"), plot = plot, width = 8, height = 6, dpi = 300)
  }
  
  plot
}

createFairnessPlot <- function(summaryResults, outputFolder) {
  metrics <- summaryResults$metricsAll
  fairnessData <- metrics |>
    dplyr::group_by(plp, model) |>
    dplyr::summarise(
      x_mean = mean(fpr_female - fpr_male, na.rm = TRUE),
      y_mean = mean(tpr_female - tpr_male, na.rm = TRUE),
      x_min = min(fpr_female - fpr_male, na.rm = TRUE),
      x_max = max(fpr_female - fpr_male, na.rm = TRUE),
      y_min = min(tpr_female - tpr_male, na.rm = TRUE),
      y_max = max(tpr_female - tpr_male, na.rm = TRUE),
      .groups = "drop"
    )
  limitCandidates <- c(abs(fairnessData$x_min), abs(fairnessData$x_max), abs(fairnessData$y_min), abs(fairnessData$y_max))
  axisLimit <- max(limitCandidates, na.rm = TRUE)
  axisLimit <- max(axisLimit, 0.05)
  axisLimit <- min(0.5, axisLimit * 1.1)
  labelOffset <- axisLimit * 0.75
  plot <- ggplot2::ggplot(fairnessData, ggplot2::aes(x = x_mean, y = y_mean, color = model, shape = model)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
    ggplot2::geom_segment(ggplot2::aes(x = x_min, xend = x_max, y = y_mean, yend = y_mean), linewidth = 0.7, alpha = 0.9) +
    ggplot2::geom_segment(ggplot2::aes(x = x_mean, xend = x_mean, y = y_min, yend = y_max), linewidth = 0.7, alpha = 0.9) +
    ggplot2::geom_point(size = 3) +
    ggplot2::annotate("text", x = 0, y = 0, label = "Equal\nerror rates", size = 3, fontface = "bold", vjust = 1.8) +
    ggplot2::annotate("text", x = -labelOffset, y = 0, label = "Lower FPR\nfor females", size = 3, hjust = 1) +
    ggplot2::annotate("text", x = labelOffset, y = 0, label = "Lower FPR\nfor males", size = 3, hjust = 0) +
    ggplot2::annotate("text", x = 0, y = labelOffset, label = "Higher TPR\nfor females", size = 3, vjust = -0.2) +
    ggplot2::annotate("text", x = 0, y = -labelOffset, label = "Higher TPR\nfor males", size = 3, vjust = 1.2) +
    ggplot2::facet_wrap(~plp) +
    ggplot2::scale_color_manual(values = studyPlotPalette, drop = FALSE) +
    ggplot2::coord_equal(xlim = c(-axisLimit, axisLimit), ylim = c(-axisLimit, axisLimit), expand = TRUE) +
    ggplot2::labs(title = "Fairness", x = "FPR female - FPR male", y = "TPR female - TPR male", color = "Model", shape = "Model") +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
  ggplot2::ggsave(filename = file.path(outputFolder, "fairness_metrics.png"), plot = plot, width = 8, height = 6, dpi = 300)
  plot
}


# Select a common, prespecified number of leading features per run.
# Zero-frequency features are never promoted merely to fill K.
selectLeadingFeatures <- function(features, topK = 20) {
  if (!"importanceType" %in% names(features)) features$importanceType <- "coefficient"
  features |>
    dplyr::group_by(plp, covariates, model, iteration) |>
    dplyr::arrange(dplyr::desc(abs(covariateValue)), covariateId, .by_group = TRUE) |>
    dplyr::mutate(selected = is.finite(covariateValue) & abs(covariateValue) > 0 &
                    dplyr::row_number() <= topK) |>
    dplyr::ungroup()
}

selectThresholdFeatures <- function(features, mentionThreshold = 0.2, minExposure = 20) {
  stopifnot(mentionThreshold >= 0, mentionThreshold <= 1, minExposure >= 1)
  if (!"importanceType" %in% names(features)) features$importanceType <- "coefficient"
  if (!"exposureCount" %in% names(features)) stop("Regenerate summaries with exposure counts first")
  features |>
    dplyr::group_by(plp, covariates, model) |>
    dplyr::mutate(nRuns = dplyr::n_distinct(iteration)) |>
    dplyr::group_by(plp, covariates, model, covariateId) |>
    dplyr::mutate(eligible = if (dplyr::first(importanceType) == "mention_frequency")
      dplyr::n_distinct(iteration) == dplyr::first(nRuns) &&
        all(is.finite(exposureCount) & exposureCount >= minExposure & is.finite(covariateValue))
      else TRUE) |>
    dplyr::ungroup() |>
    dplyr::mutate(selected = eligible & is.finite(covariateValue) &
      ifelse(importanceType == "mention_frequency", covariateValue >= mentionThreshold,
             abs(covariateValue) > 0))
}

createStabilityPlot <- function(summaryResults, outputFolder, mentionThreshold = 0.2, minExposure = 20,
                               fileStem = "interpretability_stability") {
  features <- selectThresholdFeatures(summaryResults$featureImportanceAll, mentionThreshold, minExposure)
  utils::write.csv(features, file.path(outputFolder, paste0(fileStem, "_selection_by_run.csv")), row.names = FALSE)
  stability <- features |>
    dplyr::group_by(plp, covariates, model) |>
    dplyr::group_modify(~{
      # Retain zero columns and runs with no selected features.
      iterations <- sort(unique(.x$iteration))
      .x <- .x[.x$eligible, , drop = FALSE]
      ids <- sort(unique(as.character(.x$covariateId)))
      m <- matrix(0, length(iterations), length(ids))
      usable <- is.finite(.x$covariateValue)
      if (any(!iterations %in% .x$iteration[usable])) return(data.frame(stability = NA_real_, medianSelected = NA_real_, eligibleFeatures = length(ids)))
      m[cbind(match(.x$iteration, iterations), match(as.character(.x$covariateId), ids))] <- as.integer(.x$selected)
      data.frame(stability = nogueiraStability(m),
                 medianSelected = stats::median(rowSums(m)), eligibleFeatures = length(ids))
    }) |> dplyr::ungroup()
  stability$mentionThreshold <- mentionThreshold
  stability$minExposure <- minExposure
  
  existingRows <- summaryResults$metricsAll |>
    dplyr::filter(model == "existing") |>
    dplyr::distinct(plp, covariates) |>
    dplyr::mutate(model = "existing", stability = 1, medianSelected = 5,
      eligibleFeatures = 5)
  stability <- stability |>
    dplyr::filter(model != "existing") |>
    dplyr::bind_rows(existingRows)
  
  utils::write.csv(stability, file.path(outputFolder, paste0(fileStem, ".csv")), row.names = FALSE)
  plot <- ggplot2::ggplot(stability, ggplot2::aes(model, stability, fill = model)) +
    ggplot2::geom_col(na.rm = TRUE) +
    ggplot2::geom_text(ggplot2::aes(y = dplyr::coalesce(stability, 0),
      label = ifelse(is.na(stability), "Unavailable",
                     paste0("Median # features: ", medianSelected))), vjust = -0.4, size = 3) +
    ggplot2::facet_wrap(~plp) +
    ggplot2::scale_fill_manual(values = studyPlotPalette) +
    ggplot2::labs(title = "Interpretability", x = "Model", y = "Nogueira stability") +
    # ggplot2::labs(title = "Population feature stability", x = "Model", y = "Nogueira stability",
    #   subtitle = paste0("Lasso: nonzero coefficients; LLM: mention rate ≥ ", 100 * mentionThreshold,
    #                     "%\nLLM eligibility: ≥ ", minExposure, " exposed patients in every run")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1), legend.position = "none",
      panel.grid = ggplot2::element_blank())
  ggplot2::ggsave(file.path(outputFolder, paste0(fileStem, ".png")), plot, width = 10, height = 6, dpi = 300)
  plot
}

createFeatureHeatmap <- function(summaryResults, outputFolder, topN = 20) {
  features <- summaryResults$featureImportanceAll
  if (!"importanceType" %in% names(features)) features$importanceType <- "coefficient"
  # Complete absent run-feature combinations, including sparse Lasso tables.
  heatmapData <- features |>
    dplyr::group_by(plp, covariates, model, importanceType) |>
    dplyr::group_modify(~{
      ref <- dplyr::distinct(.x, covariateId, covariateName)
      grid <- merge(data.frame(iteration = unique(.x$iteration)), ref, by = NULL)
      grid <- dplyr::left_join(grid, .x, by = c("iteration", "covariateId", "covariateName"))
      grid$covariateValue[is.na(grid$covariateValue) &
        !grid$iteration %in% .x$iteration[is.na(.x$covariateValue)]] <- 0
      grid |>
        dplyr::group_by(covariateId, covariateName) |>
        dplyr::summarise(frequency = if (all(is.na(covariateValue))) NA_real_ else
          mean(if (.y$importanceType == "mention_frequency") covariateValue
               else as.numeric(abs(covariateValue) > 0), na.rm = TRUE), .groups = "drop")
    }) |> dplyr::ungroup()
  # Union of each model/configuration top N; incompatible scales are not averaged.
  top <- heatmapData |>
    dplyr::group_by(plp, covariates, model) |>
    dplyr::slice_max(frequency, n = topN, with_ties = FALSE, na_rm = TRUE) |>
    dplyr::ungroup() |> dplyr::distinct(plp, covariates, covariateId)
  heatmapData <- heatmapData |>
    dplyr::semi_join(top, by = c("plp", "covariates", "covariateId")) |>
    dplyr::mutate(measure = ifelse(importanceType == "mention_frequency",
      "LLM: mention rate when feature supplied", "Model: proportion of runs selecting feature"))
  utils::write.csv(heatmapData, file.path(outputFolder, "interpretability_feature_heatmap.csv"), row.names = FALSE)
  plot <- ggplot2::ggplot(heatmapData, ggplot2::aes(model, covariateName, fill = frequency)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = ifelse(is.na(frequency), "NA", sprintf("%.2f", frequency))), size = 2.8) +
    ggplot2::facet_wrap(~plp + covariates + measure, scales = "free") +
    ggplot2::scale_fill_gradient(low = "white", high = studyPlotPalette[1], limits = c(0, 1), na.value = "grey80") +
    ggplot2::labs(title = "Feature selection and explanation frequency", x = "Model", y = "Feature", fill = "Frequency") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
  ggplot2::ggsave(file.path(outputFolder, "interpretability_feature_heatmap.png"), plot, width = 12, height = 10, dpi = 300)
  plot
}

createExplanationConsistencyPlot <- function(summaryResults, outputFolder) {
  patients <- summaryResults$explanationPatientsAll
  local <- summaryResults$localFeatureImportanceAll
  if (is.null(patients) || nrow(patients) == 0 || is.null(local) || nrow(local) == 0) return(NULL)
  # Exclude explanations with no matched features; do not count them as empty sets.
  patients <- dplyr::filter(patients, matchedFeatures > 0)
  sets <- local |>
    dplyr::semi_join(patients, by = c("plp", "covariates", "model", "iteration", "rowId")) |>
    dplyr::group_by(plp, covariates, model, rowId, iteration) |>
    dplyr::summarise(features = list(unique(as.character(covariateId))), .groups = "drop")
  consistency <- sets |>
    dplyr::group_by(plp, covariates, model, rowId) |>
    dplyr::filter(dplyr::n() >= 2) |>
    dplyr::summarise(nIterations = dplyr::n(), jaccard = {
      pairs <- utils::combn(seq_along(features), 2)
      mean(apply(pairs, 2, function(pair) {
        a <- features[[pair[1]]]; b <- features[[pair[2]]]
        length(intersect(a, b)) / length(union(a, b))
      }))
    }, .groups = "drop")
  utils::write.csv(consistency, file.path(outputFolder, "interpretability_patient_consistency.csv"), row.names = FALSE)
  if (nrow(consistency) == 0) return(NULL)
  plot <- ggplot2::ggplot(consistency, ggplot2::aes(model, jaccard, fill = model)) +
    ggplot2::geom_boxplot() + ggplot2::facet_wrap(~plp + covariates) +
    ggplot2::scale_fill_manual(values = studyPlotPalette) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(title = "Same-patient explanation consistency", x = "LLM",
      y = "Mean pairwise Jaccard overlap", subtitle = "Both risk-increasing and protective mentions; at least two matched explanations") +
    ggplot2::theme_minimal() + ggplot2::theme(legend.position = "none")
  ggplot2::ggsave(file.path(outputFolder, "interpretability_patient_consistency.png"), plot, width = 10, height = 6, dpi = 300)
  plot
}

nogueiraStability <- function(selectedMatrix) {
  if (is.null(selectedMatrix) || nrow(selectedMatrix) < 5 || ncol(selectedMatrix) == 0) return(NA_real_)
  d <- ncol(selectedMatrix)
  kbar <- mean(rowSums(selectedMatrix), na.rm = TRUE)
  denominator <- (kbar / d) * (1 - kbar / d)
  if (!is.finite(denominator) || denominator <= 0) return(NA_real_)
  columnVariances <- apply(selectedMatrix, 2, stats::var)
  1 - (mean(columnVariances, na.rm = TRUE) / denominator)
}

buildRocGrid <- function(curveData, fprGrid=seq(0, 1, by = 0.01)) {
  groupKey <- interaction(curveData$plp, curveData$modelLabel, curveData$iteration, drop = TRUE)
  groups <- split(curveData, groupKey)
  rows <- lapply(groups, function(group) {
    if (nrow(group) < 2 || length(unique(group$outcome)) < 2) return(NULL)
    rocObj <- pROC::roc(group$outcome, group$score, quiet = TRUE, direction = "<")
    fpr <- 1 - rocObj$specificities
    tpr <- rocObj$sensitivities
    ord <- order(fpr, tpr)
    fpr <- fpr[ord]
    tpr <- tpr[ord]
    keep <- !duplicated(fpr)
    interp <- stats::approx(x = fpr[keep], y = tpr[keep], xout = fprGrid, rule = 2)$y
    data.frame(
      plp = group$plp[1],
      modelLabel = group$modelLabel[1],
      iteration = group$iteration[1],
      fpr = fprGrid,
      tpr = interp,
      stringsAsFactors = FALSE
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) return(NULL)
  do.call(rbind, rows)
}

buildPrGrid <- function(curveData, recallGrid = seq(0, 1, by = 0.01)) {
  groupKey <- interaction(curveData$plp, curveData$modelLabel, curveData$iteration, drop = TRUE)
  groups <- split(curveData, groupKey)
  rows <- lapply(groups, function(group) {
    group <- group[!is.na(group$outcome) & is.finite(group$score) & group$outcome %in% c(0L, 1L), , drop = FALSE]
    if (nrow(group) < 2 || length(unique(group$outcome)) < 2) return(NULL)
    ord <- order(group$score, decreasing = TRUE)
    outcome <- as.integer(group$outcome[ord])
    positives <- sum(outcome == 1L)
    if (positives == 0) return(NULL)
    tp <- cumsum(outcome == 1L)
    fp <- cumsum(outcome == 0L)
    recall <- tp / positives
    precision <- tp / (tp + fp)
    curve <- rbind(c(0, 1), cbind(recall, precision))
    curve <- stats::aggregate(curve[, 2], by = list(recall = curve[, 1]), FUN = max)
    interp <- stats::approx(x = curve$recall, y = curve$x, xout = recallGrid, rule = 2)$y
    data.frame(
      plp = group$plp[1],
      modelLabel = group$modelLabel[1],
      iteration = group$iteration[1],
      recall = recallGrid,
      precision = interp,
      stringsAsFactors = FALSE
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) return(NULL)
  do.call(rbind, rows)
}

buildCalibrationGrid <- function(curveData, predictionGrid = seq(0, 1, by = 0.01), span = 0.75) {
  groupKey <- interaction(curveData$plp, curveData$modelLabel, curveData$iteration, drop = TRUE)
  groups <- split(curveData, groupKey)
  rows <- lapply(groups, function(group) {
    group <- group[!is.na(group$outcome) & is.finite(group$score) & group$outcome %in% c(0L, 1L), , drop = FALSE]
    if (nrow(group) < 20 || length(unique(group$outcome)) < 2 || length(unique(group$score)) < 5) return(NULL)
    modelFit <- tryCatch(stats::loess(outcome ~ score, data = group, span = span), error = function(e) NULL)
    if (is.null(modelFit)) return(NULL)
    predRange <- range(group$score, na.rm = TRUE)
    grid <- predictionGrid[predictionGrid >= predRange[1] & predictionGrid <= predRange[2]]
    if (length(grid) < 2) return(NULL)
    observed <- tryCatch(stats::predict(modelFit, newdata = data.frame(score = grid)), error = function(e) NULL)
    if (is.null(observed)) return(NULL)
    observed <- pmin(1, pmax(0, observed))
    keep <- is.finite(observed)
    if (!any(keep)) return(NULL)
    data.frame(
      plp = group$plp[1],
      modelLabel = group$modelLabel[1],
      iteration = group$iteration[1],
      predicted = grid[keep],
      observed = observed[keep],
      stringsAsFactors = FALSE
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) return(NULL)
  do.call(rbind, rows)
}

aggregateMetric <- function(metrics, metricName, metricLabel) {
  agg <- stats::aggregate(
    metrics[[metricName]],
    by = list(modelLabel = metrics$modelLabel, plp = metrics$plp),
    FUN = function(x) c(value = mean(x, na.rm = TRUE), error = stats::sd(x, na.rm = TRUE))
  )
  data.frame(
    modelLabel = agg$modelLabel,
    plp = agg$plp,
    metric = metricLabel,
    value = agg$x[, "value"],
    error = agg$x[, "error"],
    stringsAsFactors = FALSE
  )
}
