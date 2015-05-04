getCnList <- function(cnList, ..., verbose=TRUE) {
  if (!is.null(cnList)) {
    return(cnList);
  }
  cnList <- extractListOfCopyNumbers(dsList, region, truth=truth,
                            targetChipType=targetChipType, verbose=verbose);
  cnList;
} # getCnList()


############################################################################
# HISTORY:
# 2009-02-23
# o Created.
############################################################################
