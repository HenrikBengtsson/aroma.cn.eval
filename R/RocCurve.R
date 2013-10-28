###########################################################################/**
# @RdocClass RocCurve
#
# @title "The RocCurve class"
#
# \description{
#  @classhierarchy
# }
#
# @synopsis
#
# \arguments{
#   \item{roc}{A @list.}
#   \item{cuts}{A @numerical @vector of N cuts.}
#   \item{...}{Not used.}
# }
#
# \section{Fields and Methods}{
#  @allmethods "public"
# }
#
# @author
#*/###########################################################################
setConstructorS3("RocCurve", function(roc=NULL, cuts=NULL, ...) {
  extend(BasicObject(), "RocCurve",
    roc=roc,
    cuts=cuts
  )
})

setMethodS3("as.character", "RocCurve", function(x, ...) {
  # To please R CMD check
  object <- x;

  s <- sprintf("%s: ", class(object)[1]);
  s <- c(s, sprintf("AUC: %.4g", auc(object)));
  s <- c(s, sprintf("Number of cuts: %d", length(object$cuts)));
  class(s) <- "GenericSummary";
  s;
})


setMethodS3("getFpRate", "RocCurve", function(object, ...) {
  object$roc[,"fpRate"];
})

setMethodS3("getTpRate", "RocCurve", function(object, ...) {
  object$roc[,"tpRate"];
})


setMethodS3("auc", "RocCurve", function(object, ...) {
  x <- getFpRate(object);
  y <- getTpRate(object);
  ROC::trapezint(x, y, a=0, b=1);
})


setMethodS3("plot", "RocCurve", function(x, xlim=c(0,1), ylim=c(1-diff(xlim),1), lwd=2, ...) {
  # To please R CMD check
  object <- x;

  plot(object$roc, type="l", xlim=xlim, ylim=ylim, lwd=lwd, ...);
})

setMethodS3("points", "RocCurve", function(x, ...) {
  # To please R CMD check
  object <- x;

  points(object$roc, ...);
})

setMethodS3("lines", "RocCurve", function(x, lwd=2, ...) {
  # To please R CMD check
  object <- x;

  lines(object$roc, lwd=lwd, ...);
})



############################################################################
# HISTORY:
# 2007-08-20
# o Added file caching to fitRoc2().
# 2007-08-19
# o Renamed argument 'call' to 'toCall' in fitRoc().
# 2007-04-15
# o Added scanTpAtFp().
# 2007-04-14
# o Added interpolation in findTpAtFp().
# o Removed gc() from fitRoc().
# o Added findTpAtFp() to locate TP rate for given FP rate.
# 2007-03-2x
# o Added fitRoc().
# o Created.
############################################################################
