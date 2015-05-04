setMethodS3("testSeparation", "SegmentedGenomicSignalsInterface", function(this, test=c("t.test", "ks.test"), stateIdxs=1:2, ...) {
  # Argument 'test':
  if (is.function(test)) {
    testFcn <- test;
  } else {
    test <- match.arg(test);
  }

  # Argument 'stateIdxs':
  stateIdxs <- Arguments$getIndices(stateIdxs, length=c(2,2));


  # Test function
  if (test == "t.test") {
    testFcn <- function(x, y, ...) {
      stats::t.test(x, y, alternative=c("two.sided"));
    }
  } else if (test == "ks.test") {
    testFcn <- function(x, y, ...) {
      stats::ks.test(x, y, alternative=c("two.sided"));
    }
  }

  # Identify states to use
  states <- getStates(this);
  uStates <- na.omit(unique(states));
  nStates <- length(uStates);
  if (nStates == 0) {
    throw("Cannot do test: None of the signals have a finite state.");
  } else if (nStates == 1) {
    throw("Cannot do test: There is only one state: ", uStates[1]);
  }

  # The two states to test for.
  stateA <- uStates[stateIdxs[1]];
  stateB <- uStates[stateIdxs[2]];

  # Get the signals for the two states
  y <- getSignals(this);

  idxs <- whichVector(states == stateA);
  yA <- y[idxs];

  idxs <- whichVector(states == stateB);
  yB <- y[idxs];

  rm(states, idxs, y);

  testFcn(yA, yB);
}) # testSeparation()

############################################################################
# HISTORY:
# 2009-06-25
# o Added testSeparation().
# o Created by extending PN's code in the TumorBoost vignette.
############################################################################  
