# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Options for reproducible research
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Use a fixed random seed? (if not, set to NULL)
fixedSeed <- 0xbeef;


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Additional configurations
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
evalSignal <- "abs(fracB-1/2)";

addLegend <- TRUE;
addSdEst <- FALSE;

plotAllRocCurves <- c(TRUE, FALSE)[1];
plotTracks <- c(TRUE, FALSE)[1];

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Color settings
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
palette <- brewer.pal(n=9, name="Set1");

# Colors for different data sets/methods
colorMap <- c(
  "*"="#000000",
  "NA"="#999999",
  "1"=palette[1],
  "2"=palette[5],
  "3"=palette[2],
  "4"=palette[4]
);

# Colors for heterozygous and homozygous SNPs
hetCol <- "#000000";
homCol <- "#999999";

# Colors for full-resolution and smoothed signals
fullResColorMap <- c("*" = "#000000", "NA" = "#999999");
smoothedColorMap <- c("*" = "#6666FF", "NA" = "#999999");


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Options for (x, signal) plots
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
trackAspect <- 0.22;
trackWidth <- 0.9;


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Options for (betaN, betaT) plots
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## options for for plotsByState
addLinearRegressionLines <- TRUE;
addDiagHorizLines <- TRUE;

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
fixedNbrOfPoints <- 100;


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Options for smoothing
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Bin by counts of genomic length?
byCount <- c(TRUE, FALSE)[1];

# "Width" of each bin
binCounts <- c(1, 2, 4)[1];

# Add smoothed track?
addBinTrack <- TRUE;


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Options for ROC analysis
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
fpLim <- c(0, 0.6);
# Number of different ROC colors
rocCols <- 2;



robust <- c(FALSE, TRUE)[1];
robustStr <- ifelse(robust, "median", "mean");
binFFracB <- ifelse(robust, "median", "mean");

# Infer document tags
if (regexpr("ismpolish", dataSet) != -1) {
  docTags <- "ismpolish";
  genTags <- c("Birdseed", "NGC");
  rocCurvesPattern <- "^(raw|TCN),Birdseed$|^TBN|^CMTN";
} else if (regexpr("(ACC,ra,-XY,BPN,-XY,AVG,FLN,-XY|CRMAv2)", dataSet) != -1) {
  docTags <- gsub(".*,((ACC,ra,-XY,BPN,-XY,AVG,FLN,-XY|CRMAv2)(|-CalMaTe)(,[^,]+)*).*", "\\1", dataSet);
#  docTags <- "CRMAv2";
  genTags <- c("Birdseed", "NGC")[2];
  rocCurvesPattern <- "^(raw|TCN),Birdseed$|^TBN|^CMTN";
} else if (regexpr("BeadStudio", dataSet) != -1) {
  docTags <- gsub(".*,(BeadStudio,[^,]+).*", "\\1", dataSet);
  genTags <- c("BeadStudio", "NGC")[2];
  if (confQuantile < 1) {
    rocCurvesPattern <- "^(raw|TCN),NGC$|^TBN|^CMTN";
  } else {
    # The BeadStudio genotype calls contains 4% NC:s, which need
    # to be excluded from the evaluation.  However, if done, the
    # comparison to naive genotype calls will no longer be objective.
    # rocCurvesPattern <- "^(raw|TCN),BeadStudio$|^TBN";
    rocCurvesPattern <- "^(raw|TCN),NGC$|^TBN|^CMTN";
  }
} else {
  throw("Cannot infer ('docTags', 'genTags') from 'dataSet': ", dataSet);
}

## if (regexpr("CalMaTe", dataSet) != -1) {
##   docTags <- c(docTags, "CalMaTe");
## }

docTags <- c(docTags, tumorType);

genTag <- genTags[length(genTags)];
