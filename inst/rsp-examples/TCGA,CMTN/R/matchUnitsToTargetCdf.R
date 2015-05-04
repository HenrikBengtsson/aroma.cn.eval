############################################################################
# Find up annotation data
############################################################################
matchUnitsToTargetCdf <- function(chipType, targetChipType, ..., force=FALSE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Check for cached results
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  key <- list("matchUnitsToTargetCdf", 
             chipType=chipType, targetChipType=targetChipType); 
  dirs <- c("aroma.affymetrix", chipType);
  units <- loadCache(key=key, dirs=dirs);
  if (!force && !is.null(units)) {
    return(units);
  }

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Identify the units that also exist in the target CDF
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  require("aroma.affymetrix") || throw("Package not loaded: aroma.affymetrix");
  cdf <- AffymetrixCdfFile$byChipType(chipType);

  if (targetChipType == chipType) {
    units <- seq(length=nbrOfUnits(cdf));
  } else {
    targetCdf <- AffymetrixCdfFile$byChipType(targetChipType);
    unitNames <- getUnitNames(cdf);
    targetUnitNames <- getUnitNames(targetCdf);
    commonNames <- intersect(unitNames, targetUnitNames);
    units <- match(commonNames, unitNames);
  }

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Memoize
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  saveCache(units, key=key, dirs=dirs);

  units;
} # matchUnitsToTargetCdf()


############################################################################
# HISTORY:
# 2009-02-23
# o Created.
############################################################################
