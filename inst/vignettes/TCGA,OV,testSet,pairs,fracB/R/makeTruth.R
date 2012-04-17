# "TCGA-23-1027:Chr2@126.0-156.0,cp=141.0+/-2.0,s=0/+1"
makeTruth <- function(region, ..., verbose=FALSE) {
  # Arguments 'verbose':
  verbose <- Arguments$getVerbose(verbose);

  verbose && enter(verbose, "Creating truth function");  
  if (is.character(region)) {
    verbose && cat(verbose, "Region: ", region);
    region <- parseRegion(region);
  }
  verbose && str(verbose, region);
  stopifnot(is.list(region));

  cp <- region$params$cp["position"];
  cp <- cp*1e6;
  verbose && cat(verbose, "Change point location: ", cp);
  stopifnot(is.finite(cp));

  delta <- region$params$cp["delta"];
  delta <- delta*1e6;
  verbose && cat(verbose, "Width of safety region: ", delta);
  stopifnot(is.finite(delta));

  cpRegion <- cp + delta*c(-1,1);
  verbose && printf(verbose, "Change-point region: [%.0f,%.0f]", cpRegion[1], cpRegion[2]);
  
  states <- region$params$s;
  verbose && cat(verbose, "States:");
  verbose && str(verbose, states);


  res <- function(data, ...) {
    if (is.data.frame(data)) {
      x <- data$x;
    } else {
      x <- data;
    }
    res <- rep(as.integer(NA), times=length(x));
    res[x <= cpRegion[1]] <- states[1];
    res[x >  cpRegion[2]] <- states[2];
    res;
  }

  verbose && str(verbose, res);
  verbose && exit(verbose);

  res;
} # makeTruth()


############################################################################
# HISTORY:
# 2012-03-12
# o Now makeTruth() returns a function that also accepts a data.frame
#   as input.  This is part of the migration to making use of the
#   virtual fields of the new RawGenomicSignals (aroma.core >=2.4.11).
# 2009-06-13
# o Created.
############################################################################
