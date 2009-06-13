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


  res <- function(x, ...) {
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
# 2009-06-13
# o Created.
############################################################################
