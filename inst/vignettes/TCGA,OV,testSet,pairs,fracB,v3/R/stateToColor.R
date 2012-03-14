setMethodS3("stateToColor", "default", function(states, colorMap, na.rm=FALSE, ...) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'states':
  states <- Arguments$getVector(states);

  # Argument 'colorMap':
  colorMap <- Arguments$getVector(colorMap);
  if (is.null(names(colorMap))) {
    throw("Argument 'colorMap' must be named.");
  }

  # Argument 'na.rm':
  na.rm <- Arguments$getLogical(na.rm);



  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Make missing values invisible?
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (na.rm) {
    colorMap["NA"] <- as.character(NA);
  }


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Default colors?
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  hasDefColor <- is.element("*", names(colorMap));
  if (hasDefColor) {
    for (type in c("0", "-", "+")) {
      if (!is.element(type, names(colorMap))) {
        colorMap[type] <- colorMap["*"];
      }
    }
  }


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Colorize states
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Default missing-value colors
  naColor <- as.character(colorMap["NA"]);
  cols <- rep(naColor, times=length(states));

  # Neutral states
  if (is.element("0", names(colorMap))) {
    idxs <- whichVector(states == 0);
    cols[idxs] <- colorMap["0"];
  }

  # Negative states
  if (is.element("-", names(colorMap))) {
    idxs <- whichVector(states < 0);
    cols[idxs] <- colorMap["-"];
  }

  # Positive states
  if (is.element("+", names(colorMap))) {
    idxs <- whichVector(states > 0);
    cols[idxs] <- colorMap["+"];
  }

#  print(table(states, exclude=NULL));
  uStates <- sort(unique(states), na.last=TRUE);
  uStates <- na.omit(uStates);
  for (kk in seq(along=uStates)) {
    state <- uStates[kk];
    key <- sprintf("%s", state);

    if (is.element(key, names(colorMap))) {
      idxs <- whichVector(states == state);
      cols[idxs] <- colorMap[key];
    }
  } # for (kk ...)

  cols;
}) # stateToColor()


############################################################################
# HISTORY:
# 2012-03-14
# o Added stateToColor().
# o Created from getStateColors() for SegmentedGenomicSignalsInterface in
#   aroma.core v2.4.13, which was first created 2009-06-29.
# o Created.
############################################################################
