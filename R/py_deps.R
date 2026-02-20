# ============================================================
# FuelDeep3D: Conda + Python deps setup (CRAN-safe, opt-in)
# ============================================================

# Helper: detect R CMD check / CRAN check environments
.is_r_cmd_check <- function() {
  nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")) ||
    identical(Sys.getenv("NOT_CRAN"), "false") ||
    identical(Sys.getenv("CI"), "true")
}

#' Ensure a Conda environment and Python dependencies for FuelDeep3D
#'
#' @description
#' Creates (if needed) and activates a Conda environment for FuelDeep3D, then
#' installs the required Python dependencies into that environment using pip via
#' \code{reticulate::conda_install(..., pip = TRUE)}.
#'
#' @details
#' This function is intentionally opt-in. It requires:
#' \itemize{
#'   \item An existing Conda installation (Miniconda/Anaconda), discoverable by reticulate.
#'   \item Internet connection to install Python packages.
#' }
#'
#' For CRAN safety, this function will not run during \code{R CMD check}.
#'
#' @param envname Character. Name of the Conda environment to use/create.
#' @param python_version Character. Python version used if a new env is created (e.g. "3.10").
#' @param reinstall Logical. If TRUE, forces dependency installation even if key modules are present.
#' @param cpu_only Logical. If TRUE, installs CPU-only PyTorch wheels. If FALSE, installs CUDA wheels (cu121).
#' @param conda Path to conda binary. Default uses \code{reticulate::conda_binary()}.
#'
#' @return Invisibly TRUE if environment was ensured + activated, FALSE if skipped during check.
#'
#' @examples
#' \dontrun{
#' # Requires Conda + internet
#' ensure_py_env(envname = "pointnext", python_version = "3.10", cpu_only = TRUE)
#'
#' # CUDA wheels (requires compatible NVIDIA drivers)
#' ensure_py_env(envname = "pointnext", python_version = "3.10", cpu_only = FALSE)
#'
#' # Inspect reticulate Python
#' reticulate::py_config()
#' }
#'
#' @export
ensure_py_env <- function(envname = "pointnext",
                          python_version = "3.10",
                          reinstall = FALSE,
                          cpu_only = TRUE,
                          conda = NULL) {
  if (.is_r_cmd_check()) {
    message("Skipping ensure_py_env() during R CMD check.")
    return(invisible(FALSE))
  }
  
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required. Install it with install.packages('reticulate').",
         call. = FALSE)
  }
  
  # Find conda
  conda_bin <- conda
  if (is.null(conda_bin) || !nzchar(conda_bin)) {
    conda_bin <- reticulate::conda_binary()
  }
  if (is.null(conda_bin) || !nzchar(conda_bin)) {
    stop(
      "No Conda installation detected by reticulate.\n",
      "Install Miniconda/Anaconda, restart R, then try again.\n",
      "Tip: reticulate can install Miniconda via reticulate::install_miniconda().",
      call. = FALSE
    )
  }
  
  # List envs
  envs <- tryCatch(reticulate::conda_list(conda = conda_bin),
                   error = function(e) NULL)
  env_exists <- !is.null(envs) && envname %in% envs$name
  
  if (!env_exists) {
    message(">> Conda env '", envname, "' not found; creating with Python ", python_version, " ...")
    reticulate::conda_create(
      envname = envname,
      packages = paste0("python=", python_version),
      conda = conda_bin
    )
    message(">> Created Conda env '", envname, "'.")
  } else {
    message(">> Reusing existing Conda env '", envname, "'.")
  }
  
  # Install deps
  install_py_deps(envname = envname, only_if_missing = !reinstall, cpu_only = cpu_only, conda = conda_bin)
  
  # Activate env for this session
  reticulate::use_condaenv(envname, conda = conda_bin, required = TRUE)
  message(">> Activated Conda env '", envname, "' for this R session.")
  invisible(TRUE)
}

#' Install Python dependencies into a Conda environment (FuelDeep3D)
#'
#' @description
#' Installs required Python packages into an existing Conda environment using pip.
#'
#' @details
#' If \code{only_if_missing = TRUE}, checks for key importable modules and skips
#' installation when they already exist.
#'
#' @param envname Character. Name of the Conda environment.
#' @param only_if_missing Logical. Skip install if key modules are already present.
#' @param cpu_only Logical. If TRUE, installs CPU-only PyTorch; otherwise installs cu121 wheels.
#' @param conda Path to conda binary. Default uses \code{reticulate::conda_binary()}.
#'
#' @return Invisibly TRUE if installation ran, FALSE if skipped.
#'
#' @examples
#' \dontrun{
#' install_py_deps(envname = "pointnext", cpu_only = TRUE)
#' }
#'
#' @export
install_py_deps <- function(envname = "pointnext",
                            only_if_missing = TRUE,
                            cpu_only = TRUE,
                            conda = NULL) {
  if (.is_r_cmd_check()) {
    message("Skipping install_py_deps() during R CMD check.")
    return(invisible(FALSE))
  }
  
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Package 'reticulate' is required. Install it with install.packages('reticulate').",
         call. = FALSE)
  }
  
  conda_bin <- conda
  if (is.null(conda_bin) || !nzchar(conda_bin)) {
    conda_bin <- reticulate::conda_binary()
  }
  if (is.null(conda_bin) || !nzchar(conda_bin)) {
    stop("No Conda installation detected by reticulate.", call. = FALSE)
  }
  
  envs <- tryCatch(reticulate::conda_list(conda = conda_bin),
                   error = function(e) NULL)
  if (is.null(envs) || !(envname %in% envs$name)) {
    stop("Conda env '", envname, "' not found. Create it first with ensure_py_env().",
         call. = FALSE)
  }
  
  # Activate env so py_module_available checks the right python
  reticulate::use_condaenv(envname, conda = conda_bin, required = TRUE)
  
  if (isTRUE(only_if_missing)) {
    message(">> Checking key Python modules in env '", envname, "' ...")
    key_modules <- c("torch", "numpy", "sklearn", "laspy", "tqdm")
    missing <- key_modules[
      !vapply(key_modules, reticulate::py_module_available, logical(1))
    ]
    
    if (length(missing) == 0) {
      message(">> Key modules already present in '", envname, "'. Skipping install.")
      return(invisible(FALSE))
    }
    
    message(">> Missing modules: ", paste(missing, collapse = ", "), " -> installing full set ...")
  } else {
    message(">> Installing/updating Python deps in env '", envname, "' ...")
  }
  
  # IMPORTANT:
  # - Some CSS color names like "lime" are not valid in base R.
  # - But that's unrelated here; just noting.
  #
  # Torch wheels:
  torch_args <- if (isTRUE(cpu_only)) {
    c("torch==2.5.1", "torchvision==0.20.1", "torchaudio==2.5.1")
  } else {
    c(
      "--extra-index-url", "https://download.pytorch.org/whl/cu121",
      "torch==2.5.1+cu121",
      "torchvision==0.20.1+cu121",
      "torchaudio==2.5.1+cu121"
    )
  }
  
  pip_args <- c(
    torch_args,
    "numpy~=2.2",
    "scipy~=1.15",
    "scikit-learn~=1.7",
    "tqdm>=4.66",
    "laspy~=2.6",
    "lazrs~=0.7",
    "matplotlib~=3.10",
    "seaborn~=0.13"
  )
  
  message(">> Installing Python deps into '", envname, "' using pip via reticulate::conda_install()")
  message(">> Mode: ", if (cpu_only) "CPU-only torch" else "CUDA cu121 torch (requires compatible NVIDIA drivers)")
  
  reticulate::conda_install(
    envname = envname,
    packages = pip_args,
    pip = TRUE,
    conda = conda_bin
  )
  
  message(">> Finished installing Python deps into '", envname, "'.")
  invisible(TRUE)
}
