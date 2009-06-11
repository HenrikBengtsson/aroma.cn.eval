############################################################################
# Known CN aberrant regions
############################################################################
regions <- c(
##              "TCGA-12-0620:Chr17@0.0-55.0,cp=23.5+/-2.0,s=0/+1"
##              "TCGA-23-1027:Chr2@112.0-138.0,cp=124.0+/-2.0,s=0/+1"
             "TCGA-23-1027:Chr2@110.0-140.0,cp=125.0+/-2.0,s=0/+1"
##              ,
##              "TCGA-23-1027:Chr2@126.0-160.0,cp=141.0+/-2.0,s=+1/0"
);


truth <- function(x, chromosome, name, ...) {
  name <- gsub(",.*", "", name);

  theRegion <- NULL;
  for (kk in seq(along=regions)) {
    region <- regions[kk];
    region <- parseRegion(region);
    if (region$name != name)
      next;
    if (region$chromosome != chromosome)
      next;
    theRegion <- region;
  } # for (kk ...)

  # Default state is state 0.

  if (is.null(theRegion)) {
    throw("Unknown truth: ", name, " Chr", chromosome);
  }

  cp <- theRegion$params$cp["position"];
  delta <- theRegion$params$cp["delta"];
  states <- theRegion$params$s;
  cp <- cp*1e6;
  delta <- delta*1e6;

  res <- rep(as.integer(NA), times=length(x));
  res[x <= cp-delta] <- states[1];
  res[x > cp+delta] <- states[2];

  res;
} # truth()



############################################################################
## HISTORY:
## 2009-04-05
## o Now changepoint locations and the safety margin(s) are also returned.
## 2009-02-23
## o Created.
############################################################################
