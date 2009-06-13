getFracBList <- function(fracBList, ..., what=c("fracB", "abs(fracB-1/2)"), verbose=TRUE) {
  # Nothing todo?
  if (!is.null(fracBList)) {
    return(fracBList);
  }

  # Arguments 'what':
  what <- match.arg(what);

  truth <- makeTruth(region, verbose=verbose);
  fracBList <- extractListOfFracB(dsList, region, truth=truth,
                            targetChipType=targetChipType, ..., verbose=verbose);

  ## Rho
  if (what == "abs(fracB-1/2)") {
    fracBList <- lapply(fracBList, FUN=function(fracB) {
      fracB$y <- abs(fracB$y-1/2);
      fracB;
    });
  }

  fracBList;
} # getFracBList()


############################################################################
# HISTORY:
# 2009-04-30
# o Created from getCnList.R.
############################################################################
