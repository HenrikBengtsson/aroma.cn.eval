getCnList <- function(dsList, cnList=NULL, ..., what=c("log2ratios", "ratios"), pattern=NULL, verbose=TRUE) {
  # Nothing todo?
  if (!is.null(cnList)) {
    return(cnList);
  }

  # Arguments 'what':
  what <- match.arg(what);

  if (!is.null(pattern)) {
    keep <- grep(pattern, names(dsList));
    dsList <- dsList[keep];
  }

  truth <- makeTruth(region, verbose=verbose);
  cnList <- extractListOfCopyNumbers(dsList, region, truth=truth,
                            targetChipType=targetChipType, what=what, ..., verbose=verbose);

  # CN ratios
  if (what == "ratios") {
    cnList <- lapply(cnList, FUN=function(cn) {
      y <- getSignals(cn);
      y <- 2*2^y;
      cn <- setSignals(cn, y);
      cn;
    });
  }

  cnList;
} # getCnList()


############################################################################
# HISTORY:
# 2009-07-04
# o Added argument 'pattern'.
# 2009-04-09
# o Added argument 'what'.
# 2009-02-23
# o Created.
############################################################################
