###########################################################################/**
# @RdocFunction makeTruth
#
# @title "Creates a state function for a given ROC change-point region"
#
# \description{
#  @get "title"
# }
#
# @synopsis
#
# \arguments{
#   \item{region}{A @character string.}
#   \item{...}{Not used.}
#   \item{verbose}{See @see "R.utils::Verbose".}
# }
#
# \value{
#  Returns a @function that takes a @data.frame of (chromosome, x) loci
#  and returns the genomic states for those loci.
# }
#
# \seealso{
#  @see "parseRegion".
# }
#
# @author
#
# @keyword internal
# @keyword utilities
#*/###########################################################################
makeTruth <- function(region, ..., verbose=FALSE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Arguments 'region':
  if (is.character(region)) {
    region <- parseRegion(region);
  }
  stopifnot(is.list(region));

  # Arguments 'verbose':
  verbose <- Arguments$getVerbose(verbose);

  verbose && enter(verbose, "Creating truth function");
  verbose && cat(verbose, "Region:");
  verbose && str(verbose, region);

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


  expr <- substitute(function(data, ...) {
    if (is.data.frame(data)) {
      chr <- data$chromosome;
      x <- data$x;
    } else {
      chr <- NULL;
      x <- data;
    }

    state <- rep(NA_integer_, times=length(x));
    state[x <= START] <- STATE_R;
    state[x >  END  ] <- STATE_L;

    # Subset by chromosome?
    if (!is.null(chr)) {
      state[chr != CHROMOSOME] <- NA_integer_;
    }

    state;
  }, list(CHROMOSOME=region$chromosome, START=cpRegion[1L], END=cpRegion[2L], STATE_R=states[1L], STATE_L=states[2L]));
  # Drop 'srcref'
  expr[[4L]] <- NULL;
  stateFcn <- eval(expr);
  attr(stateFcn, "source") <- region;

  verbose && str(verbose, stateFcn);
  verbose && exit(verbose);

  stateFcn;
} # makeTruth()


############################################################################
# HISTORY:
# 2012-12-12
# o DOCUMENTATION: Added help for makeTruth().
# o Added to package.
# 2012-03-14
# o Now makeTruth() looks at the chromosome too.
# 2012-03-12
# o Now makeTruth() returns a function that also accepts a data.frame
#   as input.  This is part of the migration to making use of the
#   virtual fields of the new RawGenomicSignals (aroma.core >=2.4.11).
# 2009-06-13
# o Created.
############################################################################
