getFracBList <- function(cnList, ..., what=c("abs(fracB-1/2)", "fracB"), verbose=TRUE) {
  # Nothing todo?
  if (!is.null(cnList)) {
    return(cnList);
  }

  # Arguments 'what':
  what <- match.arg(what);

  cnList <- extractListOfCopyNumbers(dsList, region, truth=truth,
                            targetChipType=targetChipType, verbose=verbose);

  # CN ratios
  if (what == "abs(fracB-1/2)") {
    cnList <- lapply(cnList, FUN=function(cn) {
      cn$y <- abs(cn$y-1/2);
      cn;
    });
  }

  cnList;
}

getCnList <- getFracBList;

############################################################################
# HISTORY:
# 2009-04-09
# o Added argument 'what'.
# 2009-02-23
# o Created.
############################################################################
