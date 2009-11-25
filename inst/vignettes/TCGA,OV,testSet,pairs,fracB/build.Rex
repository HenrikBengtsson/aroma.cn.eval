if (interactive()) savehistory();
library("R.rsp");

fullnameRsp <- "BengtssonH_2009c-SupplementaryNotes";
filenameRsp <- sprintf("%s.tex.rsp", fullnameRsp);
filenameTeX <- sprintf("%s.tex", fullnameRsp);
filenamePDF <- sprintf("%s.pdf", fullnameRsp);

# => *.tex.rsp => ... => *.pdf
rsptex(filenameRsp, envir=globalenv(), pdf=TRUE, force=TRUE, verbose=-5);

pathnameTeX <- gsub(".pdf", ".tex", pdfPathname, fixed=TRUE);
file.copy(filenameTeX, pathnameTeX, overwrite=TRUE);
file.copy(filenamePDF, pdfPathname, overwrite=TRUE);
