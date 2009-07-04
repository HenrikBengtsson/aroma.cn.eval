setMethodS3("figDevice", "default", function(label, width=6, height=aspect*width, aspect=1, scale=1, device, extension, ..., path="figures", safe=TRUE, force=FALSE) {
  # Argument 'label':
  label <- Arguments$getCharacter(label);

  # Argument 'device':
  if (!is.function(device)) {
    throw("Argument 'device' is not a function: ", mode(device));
  }

  # Argument 'extension':
  extension <- Arguments$getCharacter(extension);

  # Argument 'path':
  path <- Arguments$getWritablePath(path);


  fullname <- label;
  # Encode?
  if (safe) {
    fullname <- gsub(".", "_", label, fixed=TRUE);
  }

  filename <- sprintf("%s.%s", fullname, extension);
  pathname <- Arguments$getWritablePathname(filename, path=path);
  res <- FALSE;
  if (force || !isFile(pathname)) {
    devNew(device, pathname, width=scale*width, height=scale*height, ..., label=label);
    res <- TRUE;
  }

  res <- Object(res);
  res$label <- label;
  res$fullname <- fullname;
  res$pathname <- pathname;

  invisible(res);
}, protected=TRUE) # figDevice()


pngDev <- function(..., scale=140, device=findPngDevice()) {
  figDevice(..., width=width, scale=scale, device=device, extension="png");
} # pngDev()



############################################################################
# HISTORY:
# 2009-07-03
# o Added argument 'scale' to figDevice() to make argument 'width' be
#   device independent.
# 2009-07-02
# o Added generic figDevice().
# o Created.
############################################################################
