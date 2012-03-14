getFracBList <- function(dsList, fracBList=NULL, ..., what=c("fracB", "abs(fracB-1/2)"), pattern=NULL, verbose=TRUE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'dsList':
  if (length(dsList) == 0) {
    throw("Argument 'dsList' is empty.");
  }

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

  if (!is.null(pattern)) {
    verbose && cat(verbose, "Pattern: ", pattern);
    verbose && print(verbose, names(dsList));
    keep <- (regexpr(pattern, names(dsList)) != -1);
    verbose && print(verbose, keep);
    dsList <- dsList[keep];
    # Sanity check
    if (length(dsList) == 0) {
      throw("No data sets remaining after name pattern filtering.");
    }
  }


  truth <- makeTruth(region, verbose=verbose);
  fracBList <- extractListOfFracB(dsList, region, truth=truth,
            targetChipType=targetChipType, what=what, ..., verbose=verbose);
  verbose && print(verbose, fracBList);

  ## Rho
  if (what == "abs(fracB-1/2)") {
    fracBList <- lapply(fracBList, FUN=function(fracB) {
      y <- getSignals(fracB);
      dh <- 2*abs(y-1/2);
      fracB <- setSignals(fracB, dh);
      fracB;
    });
    verbose && print(verbose, fracBList);
  }

  verbose && exit(verbose);

  fracBList;
} # getFracBList()


############################################################################
# HISTORY:
# 2009-07-04
# o Added argument 'pattern'.
# 2009-04-30
# o Created from getCnList.R.
############################################################################
