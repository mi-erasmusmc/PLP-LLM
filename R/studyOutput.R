library(ggplot2)

studyPlotPalette <- c(
  "#0072B2", "#E69F00", "#009E73", "#D55E00",
  "#CC79A7", "#56B4E9", "#F0E442", "#000000"
)

studyOutput <- function(outputFolder) {

  runFolders <- list.dirs(outputFolder, recursive = FALSE, full.names = TRUE)
  runFolders <- runFolders[file.exists(file.path(runFolders, "plpResult", "runPlp.rds"))]

  if (length(runFolders) == 0) {
    stop("studyOutput: no PLP results found in ", outputFolder)
  }

  runSummaries <- list()
  featureSets <- list()
  featureUniverses <- list()
  curveSets <- list()

  for (runFolder in runFolders) {
    runSummary <- summariseRun(file.path(runFolder, "plpResult", "runPlp.rds"))
    if (is.null(runSummary)) {
      next
    }

    runSummaries[[length(runSummaries) + 1]] <- runSummary$metrics
    featureSets[[length(featureSets) + 1]] <- runSummary$selectedFeatures
    featureUniverses[[length(featureUniverses) + 1]] <- runSummary$featureUniverse
    curveSets[[length(curveSets) + 1]] <- runSummary$curveData
  }

  runSummaryData <- do.call(rbind, runSummaries)
  summaryData <- aggregateModelRuns(runSummaryData, featureSets, featureUniverses)

  # Output metrics
  summaryPath <- file.path(outputFolder, "studySummary.csv")
  write.csv(summaryData, summaryPath, row.names = FALSE)

  # Output figures
  saveSummaryPlots(summaryData, outputFolder)

  curveData <- do.call(rbind, curveSets)
  saveCurvePlots(curveData, outputFolder)
}

summariseRun <- function(rdsFile) {
  if (!file.exists(rdsFile)) {
    return(NULL)
  }

  plpResult <- readRDS(rdsFile)
  analysisId <- plpResult$analysisRef$analysisId
  analysisName <- plpResult$analysisRef$analysisName
  if (is.null(analysisName) || identical(analysisName, "")) {
    analysisName <- analysisId
  }

  splitInfo <- splitAnalysisId(analysisId)
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

  predictions$outcome <- as.integer(predictions$outcomeCount > 0)
  thresholdInfo <- bestThreshold(predictions$outcome, predictions$value)
  fairnessData <- fairnessBySex(predictions, thresholdInfo$threshold)
  curveData <- data.frame(
    model = splitInfo$model,
    plp = splitInfo$plp,
    covariates = splitInfo$covariates,
    iteration = splitInfo$iteration,
    outcome = predictions$outcome,
    score = predictions$value,
    stringsAsFactors = FALSE
  )
  curveData <- curveData[!is.na(curveData$outcome) & is.finite(curveData$score), , drop = FALSE]

  covariateImportance <- readCovariateImportance(dirname(rdsFile))
  if (is.null(covariateImportance)) {
    selectedFeatures <- integer()
    featureUniverse <- integer()
    interpretability <- NA_real_
  } else {
    featureUniverse <- unique(covariateImportance$covariateId)
    selectedFeatures <- covariateImportance$covariateId[abs(covariateImportance$covariateValue) > 0]
    interpretability <- NA_real_
  }

  list(
    metrics = data.frame(
      analysisId = analysisId,
      analysisName = analysisName,
      model = splitInfo$model,
      plp = splitInfo$plp,
      covariates = splitInfo$covariates,
      iteration = splitInfo$iteration,
      populationSize = metricValue(metricsVector, "populationSize"),
      outcomeCount = metricValue(metricsVector, "outcomeCount"),
      auc = metricValue(metricsVector, "AUROC"),
      auc_lower = metricValue(metricsVector, "95% lower AUROC"),
      auc_upper = metricValue(metricsVector, "95% upper AUROC"),
      auprc = metricValue(metricsVector, "AUPRC"),
      brier = metricValue(metricsVector, "brier score"),
      threshold = thresholdInfo$threshold,
      threshold_youden = thresholdInfo$youden,
      fpr_male = fairnessData$fpr_male,
      tpr_male = fairnessData$tpr_male,
      fpr_female = fairnessData$fpr_female,
      tpr_female = fairnessData$tpr_female,
      stringsAsFactors = FALSE
    ),
    selectedFeatures = selectedFeatures,
    featureUniverse = featureUniverse,
    curveData = curveData
  )
}

aggregateModelRuns <- function(runSummaryData, featureSets, featureUniverses) {
  summaryKey <- interaction(runSummaryData$model, runSummaryData$plp, runSummaryData$covariates, drop = TRUE, sep = " | ")
  splitIndices <- split(seq_len(nrow(runSummaryData)), summaryKey)

  rows <- lapply(names(splitIndices), function(key) {
    indices <- splitIndices[[key]]
    group <- runSummaryData[indices, , drop = FALSE]
    groupFeatureSets <- featureSets[indices]

    modelFeatureUniverse <- unique(unlist(featureUniverses[indices], use.names = FALSE))
    if (length(modelFeatureUniverse) == 0) {
      modelFeatureUniverse <- unique(unlist(lapply(groupFeatureSets, identity), use.names = FALSE))
    }

    selectedCount <- vapply(groupFeatureSets, length, integer(1))
    nogueiraStability <- if (length(groupFeatureSets) >= 2) {
      nogueiraStabilityEstimator(groupFeatureSets, p = length(modelFeatureUniverse))
    } else {
      NA_real_
    }

    data.frame(
      model = group$model[1],
      plp = group$plp[1],
      covariates = group$covariates[1],
      n_iterations = nrow(group),
      auc_mean = mean(group$auc, na.rm = TRUE),
      auc_sd = stats::sd(group$auc, na.rm = TRUE),
      auprc_mean = mean(group$auprc, na.rm = TRUE),
      auprc_sd = stats::sd(group$auprc, na.rm = TRUE),
      brier_mean = mean(group$brier, na.rm = TRUE),
      brier_sd = stats::sd(group$brier, na.rm = TRUE),
      threshold_mean = mean(group$threshold, na.rm = TRUE),
      threshold_sd = stats::sd(group$threshold, na.rm = TRUE),
      fpr_male_mean = mean(group$fpr_male, na.rm = TRUE),
      fpr_male_sd = stats::sd(group$fpr_male, na.rm = TRUE),
      tpr_male_mean = mean(group$tpr_male, na.rm = TRUE),
      tpr_male_sd = stats::sd(group$tpr_male, na.rm = TRUE),
      fpr_female_mean = mean(group$fpr_female, na.rm = TRUE),
      fpr_female_sd = stats::sd(group$fpr_female, na.rm = TRUE),
      tpr_female_mean = mean(group$tpr_female, na.rm = TRUE),
      tpr_female_sd = stats::sd(group$tpr_female, na.rm = TRUE),
      fpr_gap_mean = mean(group$fpr_female - group$fpr_male, na.rm = TRUE),
      tpr_gap_mean = mean(group$tpr_female - group$tpr_male, na.rm = TRUE),
      selected_features_mean = mean(selectedCount, na.rm = TRUE),
      selected_features_sd = stats::sd(selectedCount, na.rm = TRUE),
      interpretability_nogueira = nogueiraStability,
      auc_cv = coefficientOfVariation(group$auc),
      auprc_cv = coefficientOfVariation(group$auprc),
      brier_cv = coefficientOfVariation(group$brier),
      stringsAsFactors = FALSE
    )
  })

  summaryData <- do.call(rbind, rows)
  rownames(summaryData) <- NULL
  summaryData
}

saveSummaryPlots <- function(summaryData, outputFolder) {
  if (nrow(summaryData) == 0) {
    return(invisible(NULL))
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(invisible(NULL))
  }

  cowplotAvailable <- requireNamespace("cowplot", quietly = TRUE)
  modelLabel <- ifelse(
    is.na(summaryData$model) | summaryData$model == "",
    "Unknown",
    summaryData$model
  )
  summaryData$modelLabel <- factor(modelLabel, levels = unique(modelLabel))

  comparisonLabel <- paste(summaryData$plp, summaryData$covariates, sep = " | ")
  summaryData$comparisonLabel <- factor(comparisonLabel, levels = unique(comparisonLabel))

  performanceData <- stackMetricsWithError(
    summaryData,
    c("auc_mean", "auprc_mean", "brier_mean"),
    c("auc_sd", "auprc_sd", "brier_sd"),
    c("AUC", "AUPRC", "Brier")
  )
  if (nrow(performanceData) > 0) {
    performancePlot <- plotMetricPanel(
      performanceData,
      title = "Predictive performance",
      ylab = "Score",
      lowerBound = 0,
      upperBound = 1
    )
    ggplot2::ggsave(file.path(outputFolder, "studyPerformanceMetrics.png"), performancePlot, width = 8, height = 5, dpi = 300)
  } else {
    performancePlot <- NULL
  }
  


  fairnessData <- stackMetricsWithError(
    summaryData,
    c("fpr_male_mean", "tpr_male_mean", "fpr_female_mean", "tpr_female_mean", "fpr_gap_mean", "tpr_gap_mean"),
    c("fpr_male_sd", "tpr_male_sd", "fpr_female_sd", "tpr_female_sd", NA, NA),
    c("FPR male", "TPR male", "FPR female", "TPR female", "FPR gap", "TPR gap")
  )
  if (nrow(fairnessData) > 0) {
    fairnessPlot <- plotMetricPanel(
      fairnessData,
      title = "Fairness",
      ylab = "Rate",
      lowerBound = -1,
      upperBound = 1
    )
    ggplot2::ggsave(file.path(outputFolder, "studyFairnessMetrics.png"), fairnessPlot, width = 8, height = 5, dpi = 300)
  } else {
    fairnessPlot <- NULL
  }

  consistencyData <- stackMetricsWithError(
    summaryData,
    c("auc_cv", "auprc_cv", "brier_cv", "selected_features_sd", "threshold_sd"),
    c(NA, NA, NA, NA, NA),
    c("AUC CV", "AUPRC CV", "Brier CV", "Selected features SD", "Threshold SD")
  )
  if (nrow(consistencyData) > 0) {
    consistencyPlot <- plotMetricPanel(
      consistencyData,
      title = "Consistency",
      ylab = "Variation",
      lowerBound = 0,
      upperBound = NULL
    )
    ggplot2::ggsave(file.path(outputFolder, "studyConsistencyMetrics.png"), consistencyPlot, width = 8, height = 5, dpi = 300)
  } else {
    consistencyPlot <- NULL
  }

  interpretabilityData <- stackMetricsWithError(
    summaryData,
    c("interpretability_nogueira", "selected_features_mean", "selected_features_sd"),
    c(NA, "selected_features_sd", NA),
    c("Nogueira stability", "Selected features mean", "Selected features SD")
  )
  if (nrow(interpretabilityData) > 0) {
    interpretabilityPlot <- plotMetricPanel(
      interpretabilityData,
      title = "Interpretability",
      ylab = "Value",
      lowerBound = -1,
      upperBound = NULL
    )
    ggplot2::ggsave(file.path(outputFolder, "studyInterpretabilityMetrics.png"), interpretabilityPlot, width = 8, height = 5, dpi = 300)
  } else {
    interpretabilityPlot <- NULL
  }

  if (cowplotAvailable && any(vapply(list(performancePlot, fairnessPlot, consistencyPlot, interpretabilityPlot), Negate(is.null), logical(1)))) {
    plots <- Filter(Negate(is.null), list(performancePlot, fairnessPlot, consistencyPlot, interpretabilityPlot))
    dashboard <- cowplot::plot_grid(plotlist = plots, ncol = 2, labels = "AUTO")
    ggplot2::ggsave(file.path(outputFolder, "studySummaryDashboard.png"), dashboard, width = 14, height = 10, dpi = 300)
  }

  invisible(NULL)
}

saveCurvePlots <- function(curveData, outputFolder) {
  if (is.null(curveData) || nrow(curveData) == 0) {
    return(invisible(NULL))
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(invisible(NULL))
  }

  modelLabel <- ifelse(
    is.na(curveData$model) | curveData$model == "",
    "Unknown",
    curveData$model
  )
  curveData$modelLabel <- factor(modelLabel, levels = unique(modelLabel))

  comparisonLabel <- paste(curveData$plp, curveData$covariates, sep = " | ")
  curveData$comparisonLabel <- factor(comparisonLabel, levels = unique(comparisonLabel))

  paletteValues <- resolveModelPalette(levels(curveData$modelLabel))

  rocData <- buildRocCurveData(curveData)
  if (nrow(rocData) > 0) {
    rocPlot <- ggplot2::ggplot(rocData, ggplot2::aes(x = fpr, y = tpr, color = modelLabel, group = modelLabel)) +
      ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey70") +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
      ggplot2::scale_color_manual(values = paletteValues, drop = FALSE) +
      ggplot2::labs(title = "AUC curve", x = "False positive rate", y = "True positive rate", color = "Model") +
      ggplot2::theme_minimal()

    if (length(unique(rocData$comparisonLabel)) > 1) {
      rocPlot <- rocPlot + ggplot2::facet_wrap(~comparisonLabel)
    }

    ggplot2::ggsave(file.path(outputFolder, "studyAucCurve.png"), rocPlot, width = 8, height = 5, dpi = 300)
  }

  prData <- buildPrCurveData(curveData)
  if (nrow(prData) > 0) {
    prPlot <- ggplot2::ggplot(prData, ggplot2::aes(x = recall, y = precision, color = modelLabel, group = modelLabel)) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
      ggplot2::scale_color_manual(values = paletteValues, drop = FALSE) +
      ggplot2::labs(title = "AUPRC curve", x = "Recall", y = "Precision", color = "Model") +
      ggplot2::theme_minimal()

    if (length(unique(prData$comparisonLabel)) > 1) {
      prPlot <- prPlot + ggplot2::facet_wrap(~comparisonLabel)
    }

    ggplot2::ggsave(file.path(outputFolder, "studyAuprcCurve.png"), prPlot, width = 8, height = 5, dpi = 300)
  }

  calibrationData <- buildCalibrationData(curveData)
  if (nrow(calibrationData) > 0) {
    calibrationPlot <- ggplot2::ggplot(calibrationData, ggplot2::aes(x = predicted, y = observed, color = modelLabel, group = modelLabel)) +
      ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey70") +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::geom_point(size = 1.8) +
      ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
      ggplot2::scale_color_manual(values = paletteValues, drop = FALSE) +
      ggplot2::labs(title = "Calibration curve", x = "Mean predicted risk", y = "Observed event rate", color = "Model") +
      ggplot2::theme_minimal()

    if (length(unique(calibrationData$comparisonLabel)) > 1) {
      calibrationPlot <- calibrationPlot + ggplot2::facet_wrap(~comparisonLabel)
    }

    ggplot2::ggsave(file.path(outputFolder, "studyCalibrationCurve.png"), calibrationPlot, width = 8, height = 5, dpi = 300)
  }

  invisible(NULL)
}

buildRocCurveData <- function(curveData) {
  splitData <- split(curveData, interaction(curveData$modelLabel, curveData$comparisonLabel, drop = TRUE))
  rows <- lapply(splitData, function(groupData) {
    if (nrow(groupData) == 0 || length(unique(groupData$outcome)) < 2) {
      return(NULL)
    }

    curve <- binaryCurvePoints(groupData$outcome, groupData$score)
    if (nrow(curve) == 0) {
      return(NULL)
    }

    data.frame(
      modelLabel = groupData$modelLabel[1],
      comparisonLabel = groupData$comparisonLabel[1],
      fpr = curve$fpr,
      tpr = curve$tpr,
      stringsAsFactors = FALSE
    )
  })

  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(data.frame())
  }
  do.call(rbind, rows)
}

buildPrCurveData <- function(curveData) {
  splitData <- split(curveData, interaction(curveData$modelLabel, curveData$comparisonLabel, drop = TRUE))
  rows <- lapply(splitData, function(groupData) {
    if (nrow(groupData) == 0 || sum(groupData$outcome == 1) == 0) {
      return(NULL)
    }

    curve <- binaryCurvePoints(groupData$outcome, groupData$score)
    if (nrow(curve) == 0) {
      return(NULL)
    }

    precision <- ifelse(curve$tp + curve$fp > 0, curve$tp / (curve$tp + curve$fp), 1)
    data.frame(
      modelLabel = groupData$modelLabel[1],
      comparisonLabel = groupData$comparisonLabel[1],
      recall = curve$tpr,
      precision = precision,
      stringsAsFactors = FALSE
    )
  })

  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(data.frame())
  }
  do.call(rbind, rows)
}

buildCalibrationData <- function(curveData, bins = 10) {
  splitData <- split(curveData, interaction(curveData$modelLabel, curveData$comparisonLabel, drop = TRUE))
  rows <- lapply(splitData, function(groupData) {
    if (nrow(groupData) == 0) {
      return(NULL)
    }

    clippedScore <- pmin(pmax(groupData$score, 0), 1)
    binIndex <- cut(clippedScore, breaks = seq(0, 1, length.out = bins + 1), include.lowest = TRUE)
    stats <- stats::aggregate(
      cbind(predicted = clippedScore, observed = groupData$outcome),
      by = list(bin = binIndex),
      FUN = mean
    )

    if (nrow(stats) == 0) {
      return(NULL)
    }

    data.frame(
      modelLabel = groupData$modelLabel[1],
      comparisonLabel = groupData$comparisonLabel[1],
      predicted = stats$predicted,
      observed = stats$observed,
      stringsAsFactors = FALSE
    )
  })

  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(data.frame())
  }
  do.call(rbind, rows)
}

binaryCurvePoints <- function(outcome, score) {
  outcome <- as.integer(outcome)
  valid <- !is.na(outcome) & !is.na(score)
  outcome <- outcome[valid]
  score <- score[valid]
  if (length(outcome) == 0) {
    return(data.frame())
  }

  positives <- sum(outcome == 1)
  negatives <- sum(outcome == 0)
  if (positives == 0 || negatives == 0) {
    return(data.frame())
  }

  orderIndex <- order(score, decreasing = TRUE)
  sortedOutcome <- outcome[orderIndex]
  sortedScore <- score[orderIndex]
  cumulativeTp <- cumsum(sortedOutcome == 1)
  cumulativeFp <- cumsum(sortedOutcome == 0)
  changePoints <- c(which(diff(sortedScore) != 0), length(sortedScore))

  tp <- c(0, cumulativeTp[changePoints])
  fp <- c(0, cumulativeFp[changePoints])

  data.frame(
    tp = tp,
    fp = fp,
    tpr = tp / positives,
    fpr = fp / negatives,
    stringsAsFactors = FALSE
  )
}

resolveModelPalette <- function(modelLevels) {
  if (length(modelLevels) <= length(studyPlotPalette)) {
    paletteValues <- studyPlotPalette[seq_along(modelLevels)]
  } else {
    paletteValues <- grDevices::colorRampPalette(studyPlotPalette)(length(modelLevels))
  }
  names(paletteValues) <- modelLevels
  paletteValues
}

plotMetricPanel <- function(plotData, title, ylab, lowerBound = NULL, upperBound = NULL) {
  panelLimits <- panelLimits(plotData$value, plotData$error, lowerBound, upperBound)
  position <- ggplot2::position_dodge(width = 0.8)
  modelLevels <- levels(plotData$modelLabel)
  if (is.null(modelLevels)) {
    modelLevels <- unique(as.character(plotData$modelLabel))
  }
  paletteValues <- resolveModelPalette(modelLevels)

  plot <- ggplot2::ggplot(plotData, ggplot2::aes(x = metric, y = value, fill = modelLabel)) +
    ggplot2::geom_col(position = position, width = 0.7) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = pmax(panelLimits[1], value - error),
        ymax = pmin(panelLimits[2], value + error),
        group = modelLabel
      ),
      position = position,
      width = 0.2,
      na.rm = TRUE
    ) +
    ggplot2::scale_fill_manual(values = paletteValues, drop = FALSE) +
    ggplot2::scale_y_continuous(limits = panelLimits, expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(title = title, x = "Metric", y = ylab, fill = "Model") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))

  if (length(unique(plotData$comparisonLabel)) > 1) {
    plot <- plot + ggplot2::facet_wrap(~comparisonLabel)
  }

  plot
}

stackMetricsWithError <- function(summaryData, columnNames, errorNames, labels) {
  availableColumns <- columnNames[columnNames %in% names(summaryData)]
  if (length(availableColumns) == 0) {
    return(data.frame())
  }

  rows <- lapply(seq_along(availableColumns), function(index) {
    valueColumn <- availableColumns[index]
    errorColumn <- errorNames[match(valueColumn, columnNames)]
    metricLabel <- labels[match(valueColumn, columnNames)]
    errorValues <- if (!is.na(errorColumn) && errorColumn %in% names(summaryData)) {
      summaryData[[errorColumn]]
    } else {
      rep(0, nrow(summaryData))
    }

    data.frame(
      modelLabel = summaryData$modelLabel,
      comparisonLabel = summaryData$comparisonLabel,
      metric = metricLabel,
      value = summaryData[[valueColumn]],
      error = ifelse(is.na(errorValues), 0, errorValues),
      stringsAsFactors = FALSE
    )
  })

  stacked <- do.call(rbind, rows)
  metricOrder <- labels[match(availableColumns, columnNames)]
  stacked$metric <- factor(stacked$metric, levels = metricOrder)
  stacked[!is.na(stacked$value), , drop = FALSE]
}

panelLimits <- function(values, errors, lowerBound = NULL, upperBound = NULL) {
  ymin <- suppressWarnings(min(values - errors, na.rm = TRUE))
  ymax <- suppressWarnings(max(values + errors, na.rm = TRUE))
  if (!is.finite(ymin)) {
    ymin <- 0
  }
  if (!is.finite(ymax)) {
    ymax <- 1
  }
  if (!is.null(lowerBound)) {
    ymin <- min(ymin, lowerBound)
    ymin <- lowerBound
  }
  if (!is.null(upperBound)) {
    ymax <- upperBound
  }
  if (ymin == ymax) {
    ymax <- ymax + 1
  }
  c(ymin, ymax)
}

bestThreshold <- function(outcome, probability) {
  if (!requireNamespace("pROC", quietly = TRUE)) {
    return(list(threshold = NA_real_, youden = NA_real_))
  }

  if (length(unique(outcome)) < 2) {
    return(list(threshold = NA_real_, youden = NA_real_))
  }

  rocObj <- tryCatch(
    pROC::roc(outcome, probability, quiet = TRUE, direction = "<"),
    error = function(e) NULL
  )
  if (is.null(rocObj)) {
    return(list(threshold = NA_real_, youden = NA_real_))
  }

  youden <- rocObj$sensitivities + rocObj$specificities - 1
  finiteThresholds <- is.finite(rocObj$thresholds)
  if (!any(finiteThresholds)) {
    return(list(threshold = 0.5, youden = NA_real_))
  }
  youden[!finiteThresholds] <- NA_real_
  bestIndex <- which.max(youden)
  if (length(bestIndex) == 0 || is.na(youden[bestIndex])) {
    return(list(threshold = 0.5, youden = NA_real_))
  }
  list(
    threshold = rocObj$thresholds[bestIndex],
    youden = youden[bestIndex]
  )
}

fairnessBySex <- function(predictions, threshold) {
  maleCode <- 8507
  femaleCode <- 8532

  rates <- list(
    male = confusionRates(predictions[predictions$gender == maleCode, , drop = FALSE], threshold),
    female = confusionRates(predictions[predictions$gender == femaleCode, , drop = FALSE], threshold)
  )

  list(
    fpr_male = rates$male$fpr,
    tpr_male = rates$male$tpr,
    fpr_female = rates$female$fpr,
    tpr_female = rates$female$tpr
  )
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

readCovariateImportance <- function(plpResultFolder) {
  importanceFile <- file.path(plpResultFolder, "model", "covariateImportance.csv")
  if (!file.exists(importanceFile)) {
    return(NULL)
  }

  fileInfo <- file.info(importanceFile)
  if (is.na(fileInfo$size) || fileInfo$size == 0) {
    return(NULL)
  }

  covariateImportance <- tryCatch(
    utils::read.csv(importanceFile),
    error = function(e) NULL
  )
  if (is.null(covariateImportance)) {
    return(NULL)
  }
  if (!all(c("covariateId", "covariateValue") %in% names(covariateImportance))) {
    return(NULL)
  }

  covariateImportance
}

nogueiraStabilityEstimator <- function(features, p) {
  if (length(features) < 2 || p <= 0) {
    return(NA_real_)
  }

  featureUniverse <- sort(unique(as.character(unlist(lapply(features, as.character), use.names = FALSE))))
  if (length(featureUniverse) == 0) {
    return(NA_real_)
  }

  m <- length(features)
  selectedMatrix <- sapply(features, function(selected) {
    as.integer(featureUniverse %in% as.character(selected))
  })
  if (is.vector(selectedMatrix)) {
    selectedMatrix <- matrix(selectedMatrix, ncol = 1)
  }

  h_j <- rowSums(selectedMatrix)
  q <- sum(h_j)
  if (q == 0) {
    return(NA_real_)
  }

  if (q == m * p && length(featureUniverse) == p) {
    return(1)
  }

  numerator <- (1 / p) * sum((m / (m - 1)) * (h_j / m) * (1 - h_j / m))
  denominator <- (q / (m * p)) * (1 - q / (m * p))
  if (denominator == 0) {
    return(NA_real_)
  }

  1 - numerator / denominator
}

coefficientOfVariation <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 2) {
    return(NA_real_)
  }
  xMean <- mean(x)
  if (xMean == 0) {
    return(NA_real_)
  }
  stats::sd(x) / abs(xMean)
}

splitAnalysisId <- function(analysisId) {
  parts <- strsplit(analysisId, "_", fixed = TRUE)[[1]]
  if (length(parts) < 4) {
    return(list(model = analysisId, plp = NA, covariates = NA, iteration = NA))
  }

  iteration <- parts[length(parts)]
  covariates <- parts[length(parts) - 1]
  plpTask <- parts[length(parts) - 2]
  if (length(parts) > 3) {
    model <- paste(parts[1:(length(parts) - 3)], collapse = "_")
  } else {
    model <- parts[1]
  }

  if (identical(model, "")) {
    model <- NA
  }

  iterationNum <- suppressWarnings(as.integer(iteration))
  if (is.na(iterationNum)) {
    iterationNum <- iteration
  }

  list(model = model, plp = plpTask, covariates = covariates, iteration = iterationNum)
}

metricValue <- function(metrics, name) {
  if (is.null(metrics) || !name %in% names(metrics)) {
    return(NA_real_)
  }

  value <- metrics[[name]]
  if (is.numeric(value)) {
    return(as.numeric(value))
  }

  numericValue <- suppressWarnings(as.numeric(value))
  if (is.na(numericValue)) {
    return(NA_real_)
  }
  numericValue
}
