# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Local functions
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
getRegionLabels <- function(state) {
  lab <- character(length=length(state));
  lab[state == 0] <- "normal (1,1)";
  lab[state == 1] <- "gain (1,2)";
  lab[state == 2] <- "deletion (0,1)";
  lab[state == 3] <- "copy neutral LOH (0,2)";
  lab[state == 4] <- "gain (1,3)";
  lab;
} # getRegionLabels()

getRegionPcns <- function(state) {
  switch(state+1, c(1,1), c(1,2), c(0,1), c(0,2), c(1,3))
} # getRegionPcns()

getQuantStates <- function(states, evalSignal=c("TCN", "abs(fracB-1/2)", "minorCn", "majorCn")) {
  match.arg(evalSignal);

  pcns <- sapply(states, FUN=getRegionPcns);
  qs <- switch(evalSignal, 
    TCN = apply(pcns, MARGIN=2, FUN=sum),
    "abs(fracB-1/2)" = states,
    minorCn = pcns[1,],
    majorCn = pcns[2,]
  );

  ## if ties then order by TCN
  switch(length(unique(qs)), apply(pcns, MARGIN=2, FUN=sum), qs);
} # getQuantStates()

# Count the number of loci with each state
getNbLociPerState <- function(object) {
  states <- getStates(object);
  tbl <- table(states, exclude=NULL);
  uniqueStates <- as.integer(names(tbl));
  o <- order(abs(uniqueStates));
  tbl <- tbl[o];
  names <- names(tbl);
  sign <- as.integer(names);
  names(tbl) <- getRegionLabels(sign);
  tbl;
} # getNbLociPerState()

test <- function(signal, ...) {
  testSeparation(signal, test="ks.test")$statistic;
} # test()

test <- function(signal, testFUN, output=c("statistic")) {
  states <- getStates(signal);
  us <- na.omit(getUniqueStates(signal));
  # Sanity check
  stopifnot(length(us) == 2);
  signals <- getSignals(signal);
  res <- testFUN(signals[states == us[1]], signals[states == us[2]]);
  res[[output]];
} # test()
