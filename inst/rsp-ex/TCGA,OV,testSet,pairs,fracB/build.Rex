######################################################################
# Author: Henrik Bengtsson
#
# Interactive usage:
# rm(list=ls())
# source("build.Rex")
######################################################################
if (interactive()) savehistory();
library("R.rsp");

fullnameRsp <- "BengtssonH_2009c-SupplNote";

filenameRsp <- sprintf("%s.tex.rsp", fullnameRsp);
filenamePDF <- sprintf("%s.pdf", fullnameRsp);
filenameTeX <- sprintf("%s.tex", fullnameRsp);
rsp(filenameRsp, force=TRUE, verbose=-5);

# Copy *.tex and *.pdf files to each final locations
texPathname <- gsub(".pdf", ".tex", pdfPathname, fixed=TRUE);
file.copy(filenameTeX, texPathname, overwrite=TRUE);
file.copy(filenamePDF, pdfPathname, overwrite=TRUE);

if (isFile("build.log")) {
  logPathname <- gsub(".pdf", ".log", pdfPathname, fixed=TRUE);
  file.copy("build.log", logPathname, overwrite=TRUE);
}


######################################################################
# HISTORY:
# 2012-02-08
# o Updated to make use rsp() (since R.rsp v0.5.0 [2011-04-14]).
# 2011-03-18
# o Added header and history comments.
# 2010-09-28
# o ...
# 2010-??-??
# o Created.
######################################################################
