############################################################################
# Known CN aberrant regions
############################################################################
regions <- c(
  "GSM337641:Chr1@100.1-107.5,cp=103.8+/-0.25,s=0/-1",
  "GSM337641:Chr3@80-90.9,cp=85.3+/-0.25,s=-1/0",
  "GSM337641:Chr4@60.5-65.75,cp=63.40+/-0.25,s=0/+1",
  "GSM337641:Chr10@61-69,cp=65.30+/-0.25,s=+1/0",
  "GSM337641:Chr11@78.2-83,cp=80.2+/-0.25,s=0/-1",
  "GSM337641:Chr12@57-63,cp=59.8+/-0.25,s=+1/0"
);


truth <- function(x, chromosome, name, ...) {
  name <- gsub(",.*", "", name);

  theRegion <- NULL;
  for (kk in seq_along(regions)) {
    region <- regions[kk];
    region <- parseRegion(region);
    if (region$name != name)
      next;
    if (region$chromosome != chromosome)
      next;
    theRegion <- region;
  } # for (kk ...)

  # Default state is state 0.

  if (!is.null(theRegion)) {
    cp <- theRegion$params$cp["position"];
    delta <- theRegion$params$cp["delta"];
    states <- theRegion$params$s;
    cp <- cp*1e6;
    delta <- delta*1e6;

    res <- rep(as.integer(NA), times=length(x));
    res[x <= cp-delta] <- states[1];
    res[x > cp+delta] <- states[2];
  } else {
    throw("Unknown truth: ", name, " Chr", chromosome);
    res <- integer(length(x));
    state <- +1L;
    cps <- NULL;
    if (chromosome == 1) {
      cps <- c(103.8,107.7)*1e6;
      dx <- 200e3;
      state <- -1L;
    } else if (chromosome == 3) {
      cps <- c(85.3,91)*1e6;
      dx <- 100e3;
      state <- +1L;
    } else if (chromosome == 4) {
      cps <- c(63.4,65.8)*1e6;
      dx <- 50e3;
      state <- +1L;
    } else if (chromosome == 10) {
      cps <- c(60,65.3)*1e6;
      dx <- 100e3;
      state <- +1L;
    } else if (chromosome == 11) {
      cps <- c(78.1,80.2)*1e6;
      dx <- 100e3;
      state <- +1L;
    } else if (chromosome == 12) {
      cps <- c(56.0,59.8)*1e6;
      dx <- 100e3;
      state <- +1L;
    }

    if (length(cps) > 0) {
      res[cps[1] <= x & x < cps[2]] <- state;
      dx <- rep(dx, length.out=2);
      for (kk in seq_along(cps)) {
        res[cps[kk]-dx[kk] <= x & x < cps[kk]+dx[kk]] <- NA;
      }
    }
#   params <- list(changepoints=cps, delta=dx);
#   attr(res, "params") <- params;
  }

  res;
} # truth()



############################################################################
# HISTORY:
# 2009-04-05
# o Now changepoint locations and the safety margin(s) are also returned.
# 2009-02-23
# o Created.
############################################################################
