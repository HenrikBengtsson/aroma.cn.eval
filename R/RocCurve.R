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
#   \item{roc}{A @matrix with N (FP,TP) rate data points.}
#   \item{cuts}{A @numerical @vector of N+1 cuts.}
#   \item{counts}{A @numerical @vector of N+1 cuts.}
#   \item{...}{Not used.}
# }
#
# \section{Fields and Methods}{
#  @allmethods "public"
# }
#
# @author
#*/###########################################################################
setConstructorS3("RocCurve", function(roc=NULL, cuts=NULL, counts=NULL, ...) {
  # Argument 'roc':
  if (is.list(roc)) {
    rocList <- roc;
    if (!is.element("roc", names(rocList))) {
      throw("When argument 'roc' is a list, it must contain element 'roc'.");
    }
    roc <- rocList$roc;
    if (is.element("cuts", names(rocList))) cuts <- rocList$cuts;
    if (is.element("counts", names(rocList))) counts <- rocList$counts;
  }

  if (!is.null(roc)) {
    if (!is.matrix(roc)) {
      stop("Argument 'roc' must be a matrix: ", class(roc)[1L]);
    }
  }

  if (!is.null(cuts)) {
    if (!is.numeric(cuts)) {
      stop("Argument 'cuts' must be a numeric vector: ", class(cuts)[1L]);
    }
    if (length(cuts) != nrow(roc) + 1L) {
      stop("Length argument 'cuts' is not one more than the number of rows in argument 'roc': ", length(cuts), " != ", nrow(roc) + 1L);
    }
  }


  extend(BasicObject(), "RocCurve",
    roc=roc,
    cuts=cuts,
    counts=counts
  )
})

setMethodS3("as.character", "RocCurve", function(x, ...) {
  # To please R CMD check
  object <- x;

  s <- sprintf("%s: ", class(object)[1]);
  s <- c(s, sprintf("AUC: %.4g", auc(object)));
  s <- c(s, sprintf("Number of cuts: %d", length(object$cuts)));
  counts <- getCounts(object);
  if (is.null(counts)) counts <- c(NA_integer_, NA_integer_);
  s <- c(s, sprintf("Number of positives: %d (%.2f%%) of %d", counts[1L], 100*counts[1L]/sum(counts), sum(counts)));

  GenericSummary(s);
})


setMethodS3("getCounts", "RocCurve", function(object, ...) {
  object$counts;
})

setMethodS3("getFpRate", "RocCurve", function(object, ...) {
  object$roc[,"fpRate"];
})

setMethodS3("getTpRate", "RocCurve", function(object, ...) {
  object$roc[,"tpRate"];
})


setMethodS3("auc", "RocCurve", function(object, ...) {
  roc <- object$roc[,c("fpRate","tpRate")];

  # Add (0,0)?
  roc0 <- roc[1L,]
  if (roc0[1] != 0 || roc0[2] != 0) roc <- rbind(c(0,0), roc);

  # Add (1,1)?
  roc1 <- roc[nrow(roc),]
  if (roc1[1] != 1 || roc1[2] != 1) roc <- rbind(roc, c(1,1));

  trapezint(roc[,"fpRate"], roc[,"tpRate"], a=0, b=1);
})


setMethodS3("plot", "RocCurve", function(x, lwd=2, xlim=c(0,1), ylim=c(1-diff(xlim),1), xlab="FP rate", ylab="TP rate", ...) {
  # To please R CMD check
  object <- x;

  plot(object$roc, type="l", lwd=lwd, xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim, ...);
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
# 2014-03-24
# o Now RocCurve also stored total counts.
# o ROBUSTNESS: Added validation of arguments to RocCurve().
# o Now RocCurve() also accepts a list with element 'roc' and 'cuts'.
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
