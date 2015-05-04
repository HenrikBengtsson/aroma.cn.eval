setMethodS3("fitRoc", "RawGenomicSignals", function(this, states=NULL, recall=states[1], ...) {
  # Extract by state?
  if (!is.null(states)) {
    this <- subset(this, is.element(state, states));
  }

  # Extract state and signals
  y <- getSignals(this);
  state <- this$state;

  stopifnot(!is.null(y));
  stopifnot(!is.null(state));

  # Recall?
  if (!is.null(recall)) {
    state <- as.integer(is.element(state, recall));
  }

  fitRoc(truth=state, data=y, ...);
})


############################################################################
# HISTORY:
# 2012-03-14
# o Created fitRoc() from ditto for SegmentedGenomicSignalsInterface
#   in aroma.cn.eval v0.3.1.
############################################################################
