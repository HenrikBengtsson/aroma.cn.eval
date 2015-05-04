%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  RocCurve.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{RocCurve}
\docType{class}
\alias{RocCurve}


\title{The RocCurve class}

\description{
 Package:  aroma.cn.eval \cr
\bold{Class RocCurve}\cr

\code{logical}\cr
\code{~~|}\cr
\code{~~+--}\code{\link[R.oo]{BasicObject}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{RocCurve}\cr

\bold{Directly known subclasses:}\cr
\cr

public class \bold{RocCurve}\cr
extends \link[R.oo]{BasicObject}\cr


}

\usage{
RocCurve(roc=NULL, cuts=NULL, counts=NULL, ...)
}

\arguments{
  \item{roc}{A \code{\link[base]{matrix}} with N (FP,TP) rate data points.}
  \item{cuts}{A \code{\link[base]{numeric}}al \code{\link[base]{vector}} of N+1 cuts.}
  \item{counts}{A \code{\link[base]{numeric}}al \code{\link[base]{vector}} of N+1 cuts.}
  \item{...}{Not used.}
}

\section{Fields and Methods}{
 \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{as.character} \tab  -\cr
 \tab \code{auc} \tab  -\cr
 \tab \code{getCounts} \tab  -\cr
 \tab \code{getFpRate} \tab  -\cr
 \tab \code{getTpRate} \tab  -\cr
 \tab \code{lines} \tab  -\cr
 \tab \code{plot} \tab  -\cr
 \tab \code{points} \tab  -\cr
}


 \bold{Methods inherited from BasicObject}:\cr
$, $<-, [[, [[<-, as.character, attach, detach, equals, extend, getFields, getInstantiationTime, hasField, hashCode, isReferable, objectSize, print, asThis

 \bold{Methods inherited from logical}:\cr
Ops,nonStructure,vector-method, Ops,structure,vector-method, Ops,vector,nonStructure-method, Ops,vector,structure-method, as.data.frame, as.raster, coerce,ANY,logical-method, whichVector


}

\author{Henrik Bengtsson}
\keyword{classes}
