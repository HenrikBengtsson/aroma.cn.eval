###########################################################################/**
# @set "class=SegmentedGenomicSignalsInterface"
# @RdocMethod testSeparation
#
# @title "Tests statistically the separation between two states"
#
# \description{
#  @get "title".
# }
#
# @synopsis
#
# \arguments{
#   \item{test}{A @character string specifying the statistical test to use.}
#   \item{stateIdxs}{An @integer @vector specifying the indicies of the
#     two states to be used.}
#   \item{...}{Not used.}
# }
#
# \value{
#  Returns what test function returns.
# }
#
# @author "HB,PN"
#
# \seealso{
#   @see "aroma.core::SegmentedGenomicSignalsInterface"
# }
#*/###########################################################################
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

  idxs <- which(states == stateA);
  yA <- y[idxs];

  idxs <- which(states == stateB);
  yB <- y[idxs];

  states <- idxs <- y <- NULL; # Not needed anymore

  testFcn(yA, yB);
}) # testSeparation()

############################################################################
# HISTORY:
# 2013-09-23
# o SPEEDUP/CLEANUP: normalizeTumorBoost() now uses which() instead of
#   whichVector() of 'R.utils'.  Before R (< 2.11.0), which() used to be
#   10x slower than whichVector(), but now it's 3x faster.
# 2012-02-25
# o Added Rdoc comments.
# o Added to the aroma.cn.eval package.  Used to be part of a vignette.
# 2009-06-25
# o Added testSeparation().
# o Created by extending PN's code in the TumorBoost vignette.
############################################################################
