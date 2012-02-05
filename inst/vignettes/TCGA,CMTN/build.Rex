######################################################################
# Author: Henrik Bengtsson
#
# Interactive usage:
# source("build.Rex")
######################################################################
if (interactive()) try(savehistory());
library("R.rsp");

fullnameRsp <- "CalMaTe-SupplementaryNotes";
filenameRsp <- sprintf("%s.tex.rsp", fullnameRsp);
filenameTeX <- sprintf("%s.tex", fullnameRsp);
filenamePDF <- sprintf("%s.pdf", fullnameRsp);

filenameAux <- sprintf("%s.aux", fullnameRsp);
if (file.exists(filenameAux)) {
   file.remove(filenameAux);
}
			      
filenameBbl <- sprintf("%s.bbl", fullnameRsp);
if (file.exists(filenameBbl)) {
   file.remove(filenameBbl);
}

# => *.tex.rsp => ... => *.pdf
rsptex(filenameRsp, envir=globalenv(), pdf=TRUE, force=TRUE, verbose=-5);

pathnameTeX <- gsub(".pdf", ".tex", pdfPathname, fixed=TRUE);
file.copy(filenameTeX, pathnameTeX, overwrite=TRUE);
file.copy(filenamePDF, pdfPathname, overwrite=TRUE);

filenameLOG <- "build.log";
pathnameLOG <- gsub(".pdf", ".log", pdfPathname, fixed=TRUE);
if (isFile(filenameLOG)) {
  file.copy(filenameLOG, pathnameLOG, overwrite=TRUE);
}


######################################################################
# HISTORY:
# 2012-02-06
# o Created from corresponding file in the TumorBoost vignette
######################################################################
