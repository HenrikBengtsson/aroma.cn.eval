pdfDev <- function(..., width=840, device=grDevices::pdf) {
  figDevice(..., width=width, device=device, scale=1/100, extension="pdf");
} # pdfDev()

############################################################################
# HISTORY:
# 2009-12-03
# o Added pdfDev.
############################################################################
