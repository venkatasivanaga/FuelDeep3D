#' Predict fuel classes for a LAS/LAZ file using a pre-trained model
#'
#' @description
#' Runs inference on a LAS/LAZ file and writes a new LAS/LAZ with predicted fuel classes.
#' This function uses Python inference code shipped in \code{inst/extdata/python} and
#' loads a pre-trained PyTorch model via \code{reticulate}.
#'
#' @details
#' \itemize{
#'   \item \strong{Model classes:} The shipped model is always loaded as a 3-class classifier
#'   (\code{num_classes = 3} in the Python model). Predictions are produced for these 3 classes.
#'
#'   \item \strong{Output writing:} Predictions are written using the Python helper
#'   \code{write_predictions_to_las()}.
#'   \describe{
#'     \item{\code{mode = "overwrite"}}{Replaces the LAS \code{Classification} field with predictions.}
#'     \item{\code{mode = "extra"}}{Keeps the original \code{Classification} and writes predictions to an additional attribute (behavior is defined by the Python writer).}
#'   }
#'
#'   \item \strong{Optional 4th class (ground):} If \code{cfg$num_classes == 4}, the function
#'   post-processes the 3-class prediction LAS with \code{\link{add_ground_csf}} to detect ground
#'   points and assign them to class \code{3}, producing a final 4-class LAS/LAZ.
#'
#'   \item \strong{Device selection:} Uses \code{cfg$device} if provided; otherwise selects
#'   \code{"cuda"} when available and falls back to \code{"cpu"}.
#' }
#'
#' Note: \code{setup_env} is accepted for API compatibility; if you want it to actually run the
#' environment setup, add a call such as \code{if (isTRUE(setup_env)) ensure_py_env()} before importing.
#' 
#' @param cfg A configuration list created by [config()].
#' @param mode Character. `"overwrite"` replaces the LAS `Classification` values with
#'   model predictions; `"extra"` keeps original classification and adds a new attribute
#'   for predictions (behavior depends on the Python writer).
#' @param setup_env Logical. If `TRUE`, calls [ensure_py_env()] (or your env setup helper)
#'   before importing Python modules. Default: `FALSE`.
#' @param csf_args List of arguments passed to [add_ground_csf()] when `cfg$num_classes == 4`.
#'
#' @return A character string giving the path to the output LAS/LAZ file.
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
#'   las_path     = system.file("extdata", "las", "tree2.laz", package = "FuelDeep3D"),
#'   
#'   # Option 2: write to a custom folder (edit this path)
#'   # out_pred_dir = "C:/Users/yourusername/Downloads/FuelDeep3D_predictions",
#'   out_pred_dir = file.path(tempdir(), "FuelDeep3D_predictions"),
#'   model_path   = system.file("extdata", "model", "best_model.pth", package = "FuelDeep3D"),
#'   num_classes  = 3
#' )
#'
#' out_las <- predict(cfg, mode = "overwrite", setup_env = FALSE)
#' out_las
#' }
#' }
#' @export
predict <- function(cfg, mode = c("overwrite", "extra"), setup_env = FALSE,
                    csf_args = list()) {
  
  mode <- match.arg(mode)
  stopifnot(is.list(cfg))
  
  # --- 1) Python imports ---
  py_dir <- system.file("extdata", "python", package = "FuelDeep3D")
  if (py_dir == "" || !dir.exists(py_dir))
    stop("Could not find extdata/python in FuelDeep3D package.")
  
  py_model <- reticulate::import_from_path("model", path = py_dir, delay_load = FALSE)
  py_infer <- reticulate::import_from_path("infer", path = py_dir, delay_load = FALSE)
  torch    <- reticulate::import("torch", delay_load = FALSE)
  
  # --- 2) Select device ---
  device <- if (is.null(cfg$device)) {
    if (torch$cuda$is_available()) "cuda" else "cpu"
  } else cfg$device
  
  # --- 3) Build + load model (always 3-class model) ---
  message(">> Loading model weights: ", cfg$model_path)
  mdl <- py_model$HeightAwarePointNetTiny(
    in_ch            = 4L,
    num_classes      = 3L,        # <-- ALWAYS 3 MODEL CLASSES
    k                = 16L,
    z_idx            = 2L,
    hag_idx          = 3L,
    use_height_prior = TRUE
  )$to(device)
  
  state_dict <- torch$load(cfg$model_path, map_location = device)
  mdl$load_state_dict(state_dict)
  mdl$eval()
  
  # --- 4) Run Python inference ---
  message(">> Running inference on: ", cfg$las_path)
  y_pred <- py_infer$infer_on_las_path(
    model           = mdl,
    las_path        = cfg$las_path,
    BLOCK_SIZE      = as.numeric(cfg$block_size),
    STRIDE          = as.numeric(cfg$stride),
    SAMPLE_N        = as.integer(cfg$sample_n),
    REPEAT_PER_TILE = as.integer(cfg$repeat_per_tile),
    MIN_PTS_TILE    = as.integer(cfg$min_pts_tile),
    CELL_SIZE       = as.numeric(cfg$cell_size),
    QUANTILE        = as.numeric(cfg$quantile),
    DEVICE          = device
  )
  
  # --- 5) Output directory ---
  out_dir <- cfg$out_pred_dir
  if (is.null(out_dir) || !nzchar(out_dir)) out_dir <- "extdata/output_predictions"
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  # --- 6) Write the 3-class prediction LAS ---
  message(">> Writing FuelDeep3D 3-class prediction...")
  pred_las <- py_infer$write_predictions_to_las(
    in_las  = cfg$las_path,
    out_dir = out_dir,
    y_pred  = y_pred,
    mode    = mode
  )
  
  # --- 7) If num_classes == 3 - return immediately ---
  if (cfg$num_classes == 3) {
    message(">> num_classes = 3 - returning 3-class output")
    return(pred_las)
  }
  
  # --- 8) If num_classes == 4 - apply CSF ground extraction ---
  message(">> num_classes = 4 - adding ground class using CSF ...")
  
  # Output name for final LAS
  base <- tools::file_path_sans_ext(basename(pred_las))
  ext  <- tools::file_ext(pred_las)
  out_las <- file.path(out_dir, paste0(base, "_ground.", ext))
  
  add_ground_csf(
    in_las  = pred_las,
    out_las = out_las,
    csf_args = csf_args
  )
  
  message(">> Final 4-class LAS written to: ", out_las)
  return(out_las)
}
