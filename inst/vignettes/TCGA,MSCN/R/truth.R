############################################################################
# Known CN aberrant regions
############################################################################
regions <- c(
  "TCGA-06-0178-01Avs10B:Chr10@93.18-136.38"
);


truth <- function(x, chromosome, name, ...) {
  name <- gsub(",.*", "", name);
  res <- integer(length(x));
  state <- -1L;
  cps <- NULL;

  ## region <- c(93.18,136.38)*1e6; 
  if (chromosome == 10) {
    cps <- c(114.77,Inf)*1e6;
    dx <- 500e3;
    state <- +1L;
  }

  if (length(cps) > 0) {
    res[cps[1] <= x & x < cps[2]] <- state;
    dx <- rep(dx, length.out=2);
    for (kk in seq(along=cps)) {
      res[cps[kk]-dx[kk] <= x & x < cps[kk]+dx[kk]] <- NA;
    }
  }

  res;
} # truth()



############################################################################
# HISTORY:
# 2009-02-23
# o Created.
############################################################################
