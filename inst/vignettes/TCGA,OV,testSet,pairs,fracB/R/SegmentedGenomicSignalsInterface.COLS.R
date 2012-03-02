setMethodS3("getStateColorMap", "SegmentedGenomicSignalsInterface", function(this, ...) {
  colorMap <- getBasicField(this, ".stateColorMap");
  if (is.null(colorMap)) {
    colorMap <- c(
      "*"  = "#000000",
      "NA" = "#999999",
      "0" = "purple",
      "1" = "red",
      "2" = "orange",
      "3" = "blue"
    );
  }
  colorMap;
}, conflict="quiet")

############################################################################
# HISTORY:
# 2009-06-30
# o Moved most functions to aroma.core.
# 2009-06-29
# o Created.
############################################################################
