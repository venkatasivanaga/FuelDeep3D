.onLoad <- function(libname, pkgname) {
  # Locate the installed inst/python directory
  py_root <- system.file("python", package = pkgname)
  
  # If it doesn't exist, do nothing
  if (!nzchar(py_root) || !dir.exists(py_root)) {
    return(invisible(NULL))
  }
  
  # Get current PYTHONPATH and split into components
  current <- Sys.getenv("PYTHONPATH", "")
  if (nzchar(current)) {
    paths <- strsplit(current, .Platform$path.sep, fixed = TRUE)[[1]]
  } else {
    paths <- character(0)
  }
  
  # Only prepend if not already present
  if (!py_root %in% paths) {
    new_path <- if (nzchar(current)) {
      paste(py_root, current, sep = .Platform$path.sep)
    } else {
      py_root
    }
    Sys.setenv(PYTHONPATH = new_path)
  }
  
  invisible(NULL)
}
