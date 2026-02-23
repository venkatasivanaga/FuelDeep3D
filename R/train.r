#' Train the FuelDeep3D model (build NPZ tiles if missing)
#'
#' @description
#' Trains the FuelDeep3D point-cloud model using the Python training pipeline shipped with
#' the package (under \code{inst/extdata/python}) and executed via \pkg{reticulate}.
#' If preprocessed NPZ tiles are not found in \code{file.path(cfg$out_dir, "train")},
#' the function automatically runs the dataset builder to generate \code{train/val/test}
#' NPZ tiles from the input LAS/LAZ before starting training.
#'
#' @details
#' Training proceeds in two stages:
#' \enumerate{
#'   \item \strong{Dataset preparation (optional):} If no \code{.npz} files are found in
#'   \code{cfg$out_dir/train}, the function calls the Python function
#'   \code{dataset.build_dataset_from_las()} to create NPZ tiles for training, validation,
#'   and testing. Dataset tiling behavior is controlled by fields in \code{cfg}
#'   (e.g., \code{block_size}, \code{stride}, \code{sample_n}, \code{repeat_per_tile},
#'   \code{min_pts_tile}, \code{cell_size}, \code{quantile}, splits, and \code{seed}).
#'
#'   \item \strong{Model training:} The function calls \code{train.train_model(cfg)} in the
#'   shipped Python code. The returned object (e.g., best metrics and checkpoint path)
#'   is converted back to R with \code{reticulate::py_to_r()}.
#' }
#'
#' \strong{Environment:} This function requires a working Python environment with the required
#' dependencies installed (e.g., PyTorch). If \code{setup_env = TRUE}, it calls
#' \code{\link{ensure_py_env}} before importing Python modules.
#'
#' \strong{Safety during checks:} \code{train()} is intentionally disabled during
#' \code{R CMD check} to avoid running long, non-deterministic computations on CRAN.
#'
#' \strong{Optional cleanup:} If \code{cfg$delete_tiles_after_train} is \code{TRUE},
#' the generated NPZ directories \code{train/}, \code{val/}, and \code{test/} under
#' \code{cfg$out_dir} are deleted after training completes.
#' 
#' @param cfg A list created by [config()].
#' @param setup_env Logical; if `TRUE`, calls [ensure_py_env()] to create/use a venv.
#'
#' @return A list with training outputs (e.g., best metrics and checkpoint path),
#'   returned from the Python trainer.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("reticulate", quietly = TRUE) && 
#'     reticulate::py_module_available("torch")) {
#'     
#' library(FuelDeep3D)
#' library(reticulate)
#' use_condaenv("pointnext", required = TRUE)
#'
#' cfg <- config(
#'   las_path     = system.file("extdata", "las", "trees.laz", package = "FuelDeep3D"),
#'   out_dir      = system.file("extdata", "npz_files", package = "FuelDeep3D"),
#'   out_pred_dir = system.file("extdata", "output_directory", package = "FuelDeep3D"),
#'   model_path   = system.file("extdata", "model", "best_model.pth", package = "FuelDeep3D"),
#'   epochs       = 2, batch_size = 16,
#'   learning_rate = 1e-5, weight_decay = 1e-4,
#'   block_size = 6, stride = 1, sample_n = 4096,
#'   repeat_per_tile = 4, min_pts_tile = 512,
#'   cell_size = 0.25, quantile = 0.05,
#'   delete_tiles_after_train = TRUE
#' )
#' 
#' res <- train(cfg, setup_env = FALSE)        # trains & saves best .pth
#' 
#' }
#' }
#' @export
train <- function(cfg, setup_env = FALSE) {
  stopifnot(is.list(cfg))

  # Safety: never run training during R CMD check
  if (nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_"))) {
    stop("train() is disabled during R CMD check. See ?train for \\\\dontrun{} examples.",
         call. = FALSE)
  }

  if (isTRUE(setup_env)) {
    ensure_py_env(envname = "pointnext", python_version = "3.10", cpu_only = TRUE)
  }

  # locate inst/extdata/python
  py_dir <- system.file("extdata", "python", package = "FuelDeep3D")
  if (!nzchar(py_dir) || !dir.exists(py_dir)) {
    stop("Could not find 'inst/extdata/python' in the installed FuelDeep3D package.",
         call. = FALSE)
  }

  # ---- 1) Ensure NPZ tiles exist ----
  train_dir <- file.path(cfg$out_dir, "train")
  has_npz <- dir.exists(train_dir) &&
    length(list.files(train_dir, pattern = "\\.npz$", full.names = TRUE)) > 0

  if (!has_npz) {
    message(">> No NPZ tiles found in ", train_dir,
            " -> running build_dataset_from_las() ...")

    py_dataset <- reticulate::import_from_path("dataset", path = py_dir, delay_load = FALSE)

    py_dataset$build_dataset_from_las(
      LAS_PATH        = cfg$las_path,
      OUT_DIR         = cfg$out_dir,
      SAMPLE_N        = cfg$sample_n,
      BLOCK_SIZE      = cfg$block_size,
      STRIDE          = cfg$stride,
      VAL_SPLIT       = cfg$val_split,
      TEST_SPLIT      = cfg$test_split,
      SEED            = cfg$seed,
      REPEAT_PER_TILE = cfg$repeat_per_tile,
      MIN_PTS_TILE    = cfg$min_pts_tile,
      CELL_SIZE       = cfg$cell_size,
      QUANTILE        = cfg$quantile
    )
  } else {
    message(">> Found existing NPZ tiles in ", train_dir, " -> skipping preprocessing.")
  }

  # ---- 2) Import Python trainer ----
  py_train <- reticulate::import_from_path("train", path = py_dir, delay_load = FALSE)

  # ---- 3) Make sure numeric fields are not strings ----
  cfg$batch_size       <- as.integer(cfg$batch_size)
  cfg$epochs           <- as.integer(cfg$epochs)
  cfg$learning_rate    <- as.numeric(cfg$learning_rate)
  cfg$weight_decay     <- as.numeric(cfg$weight_decay)
  cfg$block_size       <- as.numeric(cfg$block_size)
  cfg$stride           <- as.numeric(cfg$stride)
  cfg$sample_n         <- as.integer(cfg$sample_n)
  cfg$repeat_per_tile  <- as.integer(cfg$repeat_per_tile)
  cfg$min_pts_tile     <- as.integer(cfg$min_pts_tile)
  cfg$cell_size        <- as.numeric(cfg$cell_size)
  cfg$quantile         <- as.numeric(cfg$quantile)
  cfg$delete_tiles_after_train <- as.logical(cfg$delete_tiles_after_train)

  message(">> Calling Python train.train_model(config)")
  res <- py_train$train_model(cfg)

  if (is.null(res)) {
    stop("Python returned NULL. Check console for Python errors.", call. = FALSE)
  }

  res <- reticulate::py_to_r(res)

  # ---- optional cleanup: delete NPZ tiles if requested ----
  if (isTRUE(cfg$delete_tiles_after_train)) {
    train_dir <- file.path(cfg$out_dir, "train")
    val_dir   <- file.path(cfg$out_dir, "val")
    test_dir  <- file.path(cfg$out_dir, "test")

    message(">> Deleting NPZ tiles in ", cfg$out_dir, " (train/val/test) ...")

    for (d in c(train_dir, val_dir, test_dir)) {
      if (dir.exists(d)) {
        unlink(d, recursive = TRUE, force = TRUE)
        message("   - Deleted directory: ", d)
      }
    }
  }

  res
}
