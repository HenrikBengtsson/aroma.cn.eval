##############################################################
# sample data to get the same number of points in each state
##############################################################
setMethodS3("getBalancedRegions", "RawGenomicSignals", function(this, minNbP=NULL, ..., verbose=FALSE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose);
  if (verbose) {
    pushState(verbose);
    on.exit(popState(verbose));
  }

  verbose && enter(verbose, "Balancing segments to have equal number of loci"); 
  if (is.null(minNbP)) {
    states <- this$state;
    minNbP <- table(states);
    rm(states);
  }
  verbose && cat(verbose, "Target number of loci per segment: ", minNbP); 

  res <- NULL;
  states <- this$state;
  states <- sort(unique(states), na.last=TRUE);
  verbose && printf(verbose, "Unique states: (%s) [%d]\n", hpaste(states), length(states));

  for (ss in seq_along(states)) {
    verbose && enter(verbose, sprintf("State #%d ('%s') of %d", ss, states[ss], length(states)));

    verbose && enter(verbose, "Extract subset of loci with this state");
    thisByState <- subset(this, is.element(state, states[ss]));
    verbose && print(verbose, thisByState);
    verbose && cat(verbose, "States:");
    verbose && print(verbose, table(thisByState$state, useNA="ifany"));
    # Sanity check
    stopifnot(all(is.element(thisByState$state, states[ss])));
    verbose && exit(verbose);

    if (!is.na(states[ss])) {
      verbose && enter(verbose, "Sample");

      nbUnits <- nbrOfLoci(thisByState);
      subset <- sample(seq_len(nbUnits), size=minNbP, ...);
      verbose && cat(verbose, "Subset:");
      verbose && str(verbose, subset);
      # Sanity check
      stopifnot(all(is.finite(subset)));
      stopifnot(all(1 <= subset & subset <= nbUnits));

      thisByState <- extractSubset(thisByState, subset);
      verbose && print(verbose, thisByState);
      verbose && cat(verbose, "States:");
      verbose && print(verbose, table(thisByState$state, useNA="ifany"));

      # Sanity check
      stopifnot(nbrOfLoci(thisByState) == minNbP);
      stopifnot(all(is.element(thisByState$state, states[ss])));

      verbose && exit(verbose);
    }

    if (is.null(res)) {
      res <- thisByState;
    } else {
      res <- append(res, thisByState);
    }

    verbose && exit(verbose);
  } # for (ss ...)

  verbose && exit(verbose);

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
