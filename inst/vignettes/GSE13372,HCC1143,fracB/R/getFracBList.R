getFracBList <- function(cnList, ..., what=c("fracB"), verbose=TRUE) {
  # Nothing todo?
  if (!is.null(cnList)) {
    return(cnList);
  }

  # Arguments 'what':
  what <- match.arg(what);

  cnList <- extractListOfFracB(dsList, region, truth=truth,
                            targetChipType=targetChipType, verbose=verbose);

  cnList;
} # getFracBList()


############################################################################
# HISTORY:
# 2009-04-30
# o Created from getCnList.R.
############################################################################
