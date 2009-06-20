getFracBList <- function(dsList, fracBList=NULL, ..., what=c("fracB", "abs(fracB-1/2)"), verbose=TRUE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose);
  if (verbose) {
    pushState(verbose);
    on.exit(popState(verbose));
  }

  # Nothing todo?
  if (!is.null(fracBList)) {
    return(fracBList);
  }

  # Arguments 'what':
  what <- match.arg(what);

  verbose && enter(verbose, "getFracBList()");

  truth <- makeTruth(region, verbose=verbose);
  fracBList <- extractListOfFracB(dsList, region, truth=truth,
                       targetChipType=targetChipType, what=what, ..., verbose=verbose);
  verbose && print(verbose, fracBList);

  ## Rho
  if (what == "abs(fracB-1/2)") {
    fracBList <- lapply(fracBList, FUN=function(fracB) {
      fracB$y <- abs(fracB$y-1/2);
      fracB;
    });
    verbose && print(verbose, fracBList);
  }

  verbose && exit(verbose);

  fracBList;
} # getFracBList()




extractHeterozygous <- function(fracBList, genotypeCalls, confidenceScores=NULL, ..., verbose=TRUE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose);
  if (verbose) {
    pushState(verbose);
    on.exit(popState(verbose));
  }

  verbose && enter(verbose, "Extracting heterozygous subset");

  fracB <- fracBList[[1]];
  name <- gsub(",.*", "", fracB$name);
  idx <- indexOf(genotypeCalls, name);
  gf <- getFile(genotypeCalls, idx);
  verbose && print(verbose, gf);

  if (!is.null(confidenceScores)) {
    idx <- indexOf(confidenceScores, name);
    csf <- getFile(confidenceScores, idx);
    verbose && print(verbose, csf);
  }

  fracBList <- lapply(fracBList, FUN=function(fracB) {
    verbose && enter(verbose, "Extracting heterozygous subset");

    # Identify heterozygous loci
    isHet <- isHeterozygous(gf, units=fracB$unit, drop=TRUE);
    verbose && cat(verbose, "Heterozygous loci:");
    verbose && summary(verbose, isHet);

    fracB <- extractSubset(fracB, whichVector(isHet));
    verbose && exit(verbose);
    fracB;
  });

  verbose && print(verbose, fracBList);

  verbose && exit(verbose);

  fracBList;
} # extractHeterozygous()


############################################################################
# HISTORY:
# 2009-04-30
# o Created from getCnList.R.
############################################################################
