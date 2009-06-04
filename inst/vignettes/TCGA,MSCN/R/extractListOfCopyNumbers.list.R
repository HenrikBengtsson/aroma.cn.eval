############################################################################
#
############################################################################
setMethodS3("extractListOfCopyNumbers", "list", function(this, name, chromosome, region=NULL, targetChipType=NULL, truth=NULL, ...) {
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



  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Extract CNs
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Extract list of files
  dfList <- lapply(dsList, FUN=function(ds) {
    idx <- indexOf(ds, name);
    getFile(ds, idx);
  });

  cnList <- list();
  for (kk in seq(dfList)) {
    df <- dfList[[kk]];

    # Extract only units that exist in target chip type?
    units <- NULL;
    if (!is.null(targetChipType)) {
      chipType <- getChipType(df);
      if (chipType != targetChipType) {
        units <- matchUnitsToTargetCdf(chipType, targetChipType);
      }
    }

    # Extract copy numbers
    cn <- extractRawCopyNumbers(df, chromosome=chromosome, 
                                            region=region, units=units);

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

  cnList;
});

############################################################################
# HISTORY:
# 2009-02-23
# o Created.
############################################################################
