getCnList <- function(cnList, ...) {
  if (!is.null(cnList)) {
    return(cnList);
  }
  cnList <- extractListOfCopyNumbers(dsList, region, truth=truth,
                                    targetChipType=targetChipType);
  cnList;
} # getCnList()


############################################################################
# HISTORY:
# 2009-02-23
# o Created.
############################################################################
