################################################################################
# sample data (w/o replacement) to get the same number of points in each state
################################################################################
setMethodS3("getBalancedRegions", "SegmentedGenomicSignalsInterface", function(this, minNbP=NULL, ..., verbose=FALSE) {
  
  if (is.null(minNbP)) {
    minNbP <- table(getStates(this))
  }

  res <- NULL
  states <- getUniqueStates(this)
  for (ss in seq(states)) {
    state <- states[ss]
    thisByState <- extractSubsetByState(this, state)
    nbUnits <- length(thisByState$y)
    if (!is.na(state)) {
      thisByState <- extractSubset(thisByState, sample(nbUnits, minNbP))
    }
    if (is.null(res)) {
      res <- thisByState
    }
    else {
      append(res, thisByState)
    }
  }
  res
})


############################################################################
# HISTORY:
# 2009-06-30
# o Created.
############################################################################
