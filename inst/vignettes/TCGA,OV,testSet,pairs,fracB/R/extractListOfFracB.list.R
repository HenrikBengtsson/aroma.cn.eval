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
  # Check for cached results
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  key <- list(dataSets=list(method="extractListOfFracB.list()", fullnames=sapply(this, getFullName), chipTypes=sapply(this, getChipType), samples=lapply(this, getFullNames)), name=name, chromosome=chromosome, region=region, targetChipType=targetChipType, truth=truth, what=what, ...);
  dirs <- c("aroma.cn.eval");
  if (!force) {
    fracBList <- loadCache(key, dirs=dirs);
    if (!is.null(fracBList)) {
      verbose && cat(verbose, "Found cached results");
      verbose && exit(verbose);
      stop("SANITY CHECK; WE SHOULD HAVE TURNED OFF ALL MEMOIZATION.");
      return(fracBList);
    }
  }


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

    # Extract only units that exist in target chip type?
    units <- NULL;
    if (!is.null(targetChipType)) {
      verbose && enter(verbose, "Identifying target units");
      chipType <- getChipType(df);
      if (chipType != targetChipType) {
        units <- matchUnitsToTargetCdf(chipType, targetChipType);
      }
      verbose && str(verbose, units);
      verbose && exit(verbose);
    }

    # Extract copy numbers
    verbose && enter(verbose, "Extracting FracBs");
    fracB <- extractRawAlleleBFractions(df, chromosome=chromosome, 
                           region=region, units=units, keepUnits=TRUE);
    verbose && print(verbose, fracB);
    verbose && print(verbose, fracB);

    # Add true FracB functions?
    if (!is.null(truth)) {
      fracB <- SegmentedAlleleBFractions(fracB, states=truth);
    }
    verbose && print(verbose, fracB);
    verbose && exit(verbose);

    fracBList[[kk]] <- fracB;
  } # for (kk ...)
  names(fracBList) <- names(dfList);

  # Sanity check
  if (!is.null(targetChipType)) {
    nbrOfUnits <- sapply(fracBList, FUN=nbrOfLoci);
    stopifnot(length(unique(nbrOfUnits)) == 1);
  }

  # Save cache
  saveCache(fracBList, key=key, dirs=dirs);

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
