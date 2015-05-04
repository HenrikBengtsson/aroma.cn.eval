############################################################################
#
############################################################################
setMethodS3("extractListOfFracB", "list", function(this, name, chromosome, region=NULL, targetChipType=NULL, truth=NULL, what=NULL, ..., force=TRUE, verbose=FALSE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'name':
  if (is.list(name)) {
    chromosome <- name$chromosome;
    region <- name$region;
    name <- name$name;
  }
 
  # Argument 'targetChipType':
  if (!is.null(targetChipType)) {
    targetChipType <- Arguments$getCharacter(targetChipType);
  }

  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose);
  if (verbose) {
    pushState(verbose);
    on.exit(popState(verbose));
  }


  verbose && enter(verbose, "extractListOfFracB()");

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Extract FracBs
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Extract list of files
  print(name);
  dfList <- lapply(this, FUN=function(ds) {
    idx <- indexOf(ds, name);
    getFile(ds, idx);
  });

  fracBList <- list();
  for (kk in seq(dfList)) {
    df <- dfList[[kk]];
    verbose && enter(verbose, sprintf("File #%d ('%s') of %d", kk, getFullName(df), length(dfList)));

    # Extract only units that exist in target chip type?
    units <- NULL;
    if (!is.null(targetChipType)) {
      chipType <- getChipType(df);
      if (chipType != targetChipType) {
        units <- matchUnitsToTargetCdf(chipType, targetChipType);
        verbose && printf(verbose, "Identified %s units available in %s.\n", chipType, targetChipType);
      }
      verbose && str(verbose, units);
    }

    # Extract copy numbers
    verbose && enter(verbose, "Extracting FracBs");
    fracB <- extractRawAlleleBFractions(df, chromosome=chromosome, 
                           region=region, units=units, keepUnits=TRUE);

    # Remap the unit indices to the target chip type?
    if (!is.null(units) && !is.null(targetChipType) && (chipType != targetChipType)) {
      verbose && enter(verbose, "Remapping unit indices to target chip type");
      library("aroma.affymetrix");
      unitsS <- fracB$unit;
      verbose && printf(verbose, "%s units:\n", chipType);
      verbose && str(verbose, unitsS);
      cdf <- AffymetrixCdfFile$byChipType(chipType);
      targetCdf <- AffymetrixCdfFile$byChipType(targetChipType);
      unitNames <- getUnitNames(cdf)[unitsS];
      verbose && str(verbose, unitNames);
      unitsT <- indexOf(targetCdf, names=unitNames);
      verbose && printf(verbose, "%s units:\n", targetChipType);
      verbose && str(verbose, unitsT);
      fracB$unit <- unitsT;
      verbose && exit(verbose);
    }

    # Add true FracB functions?
    if (!is.null(truth)) {
##      fracB <- SegmentedAlleleBFractions(fracB, states=truth);
      fracB$state <- truth;
    }

    verbose && print(verbose, fracB);
    verbose && cat(verbose, "Units:");
    verbose && str(verbose, fracB$unit);

    verbose && exit(verbose);

    fracBList[[kk]] <- fracB;

    verbose && exit(verbose);
  } # for (kk ...)
  names(fracBList) <- names(dfList);

  # Sanity check
  if (!is.null(targetChipType)) {
    nbrOfUnits <- sapply(fracBList, FUN=nbrOfLoci);
    stopifnot(length(unique(nbrOfUnits)) == 1);
  }

  verbose && exit(verbose);

  fracBList;
});

############################################################################
# HISTORY:
# 2009-06-13
# o CLEAN UP: Now making use of extractRawAlleleBFractions() in aroma.core.
# 2009-06-10
# o Updated to make use of AlleleBFractions classes.
# 2009-02-23
# o Created.
############################################################################
