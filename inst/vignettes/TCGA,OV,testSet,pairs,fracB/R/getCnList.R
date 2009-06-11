getCnList <- function(cnList, ..., what=c("log2ratios", "ratios"), verbose=TRUE) {
  # Nothing todo?
  if (!is.null(cnList)) {
    return(cnList);
  }

  # Arguments 'what':
  what <- match.arg(what);

  cnList <- extractListOfCopyNumbers(list(acs), region, truth=truth,
                            targetChipType=targetChipType, verbose=verbose);

  # CN ratios
  if (what == "ratios") {
    cnList <- lapply(cnList, FUN=function(cn) {
      cn$y <- 2*2^cn$y;
      cn;
    });
  }

  cnList;
} # getCnList()


############################################################################
# HISTORY:
# 2009-04-09
# o Added argument 'what'.
# 2009-02-23
# o Created.
############################################################################
