#' Create/use a venv and install Python deps listed in inst/python/requirements.txt
#' @export
vegseg_py_setup <- function(envdir = NULL) {
  # Locate the installed python directory from inst/python
  py_root <- system.file("python", package = "vegseg")
  if (py_root == "" || !dir.exists(py_root)) {
    stop("Could not find 'inst/python' directory in the installed 'vegseg' package.")
  }
  
  # Default venv location: <package>/python/.venv unless user overrides
  if (is.null(envdir)) {
    envdir <- file.path(py_root, ".venv")
  }
  
  # Create venv only if it does not already exist
  if (!reticulate::virtualenv_exists(envdir)) {
    message(">> Creating virtualenv at: ", envdir)
    reticulate::virtualenv_create(envdir, python = NULL)
    
    # Install requirements if present
    req <- file.path(py_root, "requirements.txt")
    if (file.exists(req)) {
      message(">> Installing Python dependencies from: ", req)
      reticulate::virtualenv_install(
        envdir,
        packages    = character(),     # no direct packages
        requirements = req,            # <-- use this argument
        ignore_installed = TRUE
      )
    } else {
      message(">> No requirements.txt found at: ", req)
    }
  } else {
    message(">> Using existing virtualenv at: ", envdir)
  }
  
  # Activate this venv for reticulate
  reticulate::use_virtualenv(envdir, required = TRUE)
  invisible(envdir)
}
