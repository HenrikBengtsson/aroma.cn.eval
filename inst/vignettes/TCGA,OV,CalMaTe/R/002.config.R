# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Options for reproducible research
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Use a fixed random seed? (if not, set to NULL)
fixedSeed <- 0xbeef;


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Additional configurations
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
addLegend <- TRUE;
addSdEst <- TRUE;
addCounts <- TRUE;

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



whatColorMap <- c("*" = "#6666FF", "NA" = "#999999");
methodColorMap <- c("*" = "#6666FF", "NA" = "#999999");



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Options for (x, signal) plots
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
trackWidth <- 0.9;


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Options for (betaN, betaT) plots
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
addDiagHorizLines <- TRUE;

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
fixedNbrOfPoints <- 100;


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Options for smoothing
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Bin by counts of genomic length?
byCount <- FALSE;

# Width of each bin
if (byCount) {
  binWidths <- c(1, 2, 4);
  binWidthS <- binWidths[length(binWidths)];
} else {
  binWidths <- c(5, 10, 25, 50)*1e3;
  binWidthS <- binWidths[length(binWidths)];
}
binWidths <- binWidths;

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
binFUN <- ifelse(robust, "median", "mean");

# Infer document tags
if (regexpr("(CRMAv2)", dataSetTags) != -1) {
  docTags <- gsub(".*,(CRMAv2)(|-CalMaTe)(,[^,]+)*).*", "\\1", dataSet);
} else if (regexpr("(BeadStudio,XY)", dataSetTags) != -1) {
  docTags <- gsub(".*,(BeadStudio,XY)(|-CalMaTe)(,[^,]+)*).*", "\\1", dataSet);
} else {
  throw("Cannot infer ('docTags') from 'dataSet': ", dataSet);
}

