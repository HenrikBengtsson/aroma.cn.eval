##############################################################
# sample data to get the same number of points in each state
##############################################################
setMethodS3("getBalancedRegions", "SegmentedGenomicSignalsInterface", function(this, minNbP=NULL, ..., verbose=FALSE) {
  if (is.null(minNbP)) {
    states <- getStates(this);
    minNbP <- table(states);
  }

  res <- NULL;
  states <- getUniqueStates(this);
  # Sanity check
  for (ss in seq_along(states)) {
    state <- states[ss];
    thisByState <- extractSubsetByState(this, state);
    nbUnits <- length(getSignals(thisByState));
    if (!is.na(state)) {
      subset <- sample(nbUnits, minNbP, ...);
      thisByState <- extractSubset(thisByState, subset);
    }
    if (is.null(res)) {
      res <- thisByState;
    } else {
      res <- append(res, thisByState);
    }
  } # for (ss ...)

  res;
})


############################################################################
# HISTORY:
# 2012-03-01
# o No longer assuming reference variables.
# 2009-10-26
# o Resampling with replacement now allowed (implicitly through '...').
# 2009-06-30
# o Created.
############################################################################
