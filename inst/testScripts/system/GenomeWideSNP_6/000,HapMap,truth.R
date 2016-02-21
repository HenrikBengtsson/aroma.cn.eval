# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Known CN states
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# A function give the true CN state for a given set of positions
truth <- function(x, chromosome=NULL, name=NULL, ...) {
  t <- integer(length(x));

  stopifnot(!is.null(chromosome));
  stopifnot(!is.null(name));
  parts <- strsplit(name, split=",", fixed=TRUE)[[1]];
  sampleName <- parts[1];
  if (sampleName == "NA06985") {
    if (chromosome == 2) {
      # Location of change points
      xCps <- c(83.09, 83.72)*1e6;
      # Neutral
      t[x <= xCps[1] | x >= xCps[2]] <- 0L;
      # Loss
      t[xCps[1] <= x & x < xCps[2]] <- -1L;
      dx <- 50e3;
    } else if (chromosome == 14) {
      # Location of change points
      xCps <- c(19.20, 19.49)*1e6;
      # Neutral
      t[x <= xCps[1] | x >= xCps[2]] <- 0L;
      # Loss
      t[xCps[1] <= x & x < xCps[2]] <- +1L;
      dx <- 10e3;
    } else {
      stop("Cannot infer CN state. Chromosome not in DB: ", chromosome);
    }
  } else if (sampleName == "NA06991") {
    if (chromosome == 22) {
      # Location of change points
      xCps <- c(24.00, 24.25)*1e6;
      # Neutral
      t[x <= xCps[1] | x >= xCps[2]] <- 0L;
      # Loss
      t[xCps[1] <= x & x < xCps[2]] <- -1L;
      dx <- 10e3;
    } else {
      stop("Cannot infer CN state. Chromosome not in DB: ", chromosome);
    }
  } else if (sampleName == "NA06993") {
    if (chromosome == 1) {
      # Location of change points
      xCps <- c(147.30, 147.51)*1e6;
      # Neutral
      t[x <= xCps[1] | x >= xCps[2]] <- 0L;
      # Loss
      t[xCps[1] <= x & x < xCps[2]] <- -1L;
      dx <- 10e3;
    } else {
      stop("Cannot infer CN state. Chromosome not in DB: ", chromosome);
    }
  } else if (sampleName == "NA07022") {
    if (chromosome == 4) {
      # Location of change points
      xCps <- c(118.75, 119.77)*1e6;
      # Neutral
      t[x <= xCps[1] | x >= xCps[2]] <- 0L;
      # Loss
      t[xCps[1] <= x & x < xCps[2]] <- +1L;
      dx <- 50e3;
    } else {
      stop("Cannot infer CN state. Chromosome not in DB: ", chromosome);
    }
  } else if (sampleName == "NA11830") {
    if (chromosome == 15) {
      # Location of change points
      xCps <- c(29.77, 30.24)*1e6;
      # Neutral
      t[x <= xCps[1] | x >= xCps[2]] <- 0L;
      # Loss
      t[xCps[1] <= x & x < xCps[2]] <- +1L;
      # Uncertain
      t[xCps[2] <= x & x <= 30.7e6] <- NA;
      dx <- 50e3;
    } else {
      stop("Cannot infer CN state. Chromosome not in DB: ", chromosome);
    }
  } else if (sampleName == "NA11839") {
    if (chromosome == 22) {
      # Location of change points
      xCps <- c(24.00, 24.25)*1e6;
      # Neutral
      t[x <= xCps[1] | x >= xCps[2]] <- 0L;
      # Loss
      t[xCps[1] <= x & x < xCps[2]] <- +1L;
      dx <- 30e3;
    } else {
      stop("Cannot infer CN state. Chromosome not in DB: ", chromosome);
    }
  } else {
    stop("Cannot infer CN state. Sample not in DB: ", name);
  }

  # Safety region of 50kb each side
  for (kk in seq_along(xCps)) {
    xCp <- xCps[kk];
    t[xCp-dx < x & x < xCp+dx] <- NA;
  }

  t;
} # truth()
