##############################################################
# sample data to get the same number of points in each state
##############################################################
## setMethodS3("getBalancedRegions", "SegmentedGenomicSignalsInterface", function(this, minNbP=NULL, ..., verbose=FALSE) {

setMethodS3("getBalancedRegions", "RawGenomicSignals", function(this, minNbP=NULL, ..., verbose=FALSE) {
  if (is.null(minNbP)) {
    states <- this$state;
    minNbP <- table(states);
    rm(states);
  }

  res <- NULL;
  states <- this$state;
  states <- sort(unique(states), na.last=TRUE);
  for (ss in seq_along(states)) {
    thisByState <- subset(this, is.element(state, states[ss]));

    nbUnits <- length(getSignals(thisByState));
    if (!is.na(states[ss])) {
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
# 2012-03-14
# o getBalancedRegions() is now defined for RawGenomicSignals.
# 2012-03-01
# o No longer assuming reference variables.
# 2009-10-26
# o Resampling with replacement now allowed (implicitly through '...').
# 2009-06-30
# o Created.
############################################################################
