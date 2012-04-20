setMethodS3("fitRoc", "RawGenomicSignals", function(this, field, states=NULL, recall=states[1], ...) {
  # Argument 'field':
  field <- Arguments$getCharacter(field);

  # Extract by state?
  if (!is.null(states)) {
    this <- subset(this, is.element(state, states));
  }

  # Extract state and signals
  y <- this[[field]];
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
# o Added argument 'field' to fitRoc().
# o Created fitRoc() from ditto for SegmentedGenomicSignalsInterface
#   in aroma.cn.eval v0.3.1.
############################################################################
