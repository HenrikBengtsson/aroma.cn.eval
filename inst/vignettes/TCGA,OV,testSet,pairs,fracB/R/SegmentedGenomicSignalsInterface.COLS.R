setMethodS3("getStateColorMap", "SegmentedGenomicSignalsInterface", function(this, ...) {
  colorMap <- this$.stateColorMap;
  if (is.null(colorMap)) {
    colorMap <- c(
      "*"  = "#000000",
      "NA" = "#999999",
      "1" = "red",
      "2" = "orange",
      "3" = "blue",
      "4" = "purple"
    );
  }
  colorMap;
})

setMethodS3("setStateColorMap", "SegmentedGenomicSignalsInterface", function(this, colorMap, ...) {
  # Argument 'colorMap':
  colorMap <- Arguments$getCharacters(colorMap);
  if (is.null(names(colorMap))) {
    throw("Argument 'colorMap' must be a named vector.");
  }
  this$.stateColorMap <- colorMap;
})


setMethodS3("getStateColors", "SegmentedGenomicSignalsInterface", function(this, na.rm=FALSE, ...) {
  colorMap <- getStateColorMap(this);
  if (na.rm) {
    colorMap["NA"] <- as.character(NA);
  }
  hasDefColor <- is.element("*", names(colorMap));

  states <- getStates(this);
  print(table(states, exclude=NULL));
  uStates <- sort(unique(states), na.last=TRUE);
  uStates <- na.omit(uStates);

  naColor <- as.character(colorMap["NA"]);
  cols <- rep(naColor, times=length(states));
  for (kk in seq(along=uStates)) {
    state <- uStates[kk];
    key <- sprintf("%s", state);

    if (!is.element(key, names(colorMap))) {
      if (!hasDefColor) {
        throw("State does not exist in color map: ", state);
      }
      key <- "*";
    }
    col <- colorMap[key];
    idxs <- whichVector(states == state);
    cols[idxs] <- col;
  } # for (kk ...)

  cols;
})


############################################################################
# HISTORY:
# 2009-06-29
# o Created.
############################################################################
