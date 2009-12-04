############################################################################
#
############################################################################
setMethodS3("extractListOfCopyNumbers", "list", function(this, name, chromosome, region=NULL, targetChipType=NULL, truth=NULL, what=what, ..., force=TRUE, cache=FALSE, verbose=FALSE) {
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


  verbose && enter(verbose, "extractListOfCopyNumbers()");


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Check for cached results
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  key <- list(dataSets=list(fullnames=sapply(this, getFullName), chipTypes=sapply(this, getChipType), samples=lapply(this, getFullNames)), name=name, chromosome=chromosome, region=region, targetChipType=targetChipType, truth=truth, what=what, ...);
  dirs <- c("aroma.cn.eval");
  if (!force) {
    cnList <- loadCache(key, dirs=dirs);
    if (!is.null(cnList)) {
      verbose && cat(verbose, "Found cached results");
      verbose && exit(verbose);
      stop("SANITY CHECK; WE SHOULD HAVE TURNED OFF ALL MEMOIZATION.");
      return(cnList);
    }
  }


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Extract CNs
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Extract list of files
  dfList <- lapply(this, FUN=function(ds) {
    idx <- indexOf(ds, name);
    getFile(ds, idx);
  });

  cnList <- list();
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
    verbose && enter(verbose, "Extracting CNs");
    cn <- extractRawCopyNumbers(df, chromosome=chromosome, 
                                            region=region, units=units);
    verbose && print(verbose, cn);
    verbose && exit(verbose);

    # Add true CN functions?
    if (!is.null(truth)) {
      cn <- SegmentedCopyNumbers(cn, states=truth);
    }

    cnList[[kk]] <- cn;
  } # for (kk ...)
  names(cnList) <- names(dfList);

  # Sanity check
  if (!is.null(targetChipType)) {
    nbrOfUnits <- sapply(cnList, FUN=nbrOfLoci);
    stopifnot(length(unique(nbrOfUnits)) == 1);
  }

  # Save cache?
  if (cache) {
    saveCache(cnList, key=key, dirs=dirs);
  }

  verbose && exit(verbose);

  cnList;
});

############################################################################
# HISTORY:
# 2009-02-23
# o Created.
############################################################################
