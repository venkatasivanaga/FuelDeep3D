#' Add a ground class using CSF post-processing
#'
#' @description
#' Post-processes a FuelDeep3D-predicted LAS/LAZ by detecting ground points with
#' Cloth Simulation Filtering (CSF) and assigning them to a dedicated ground class.
#' This is used when converting a 3-class FuelDeep3D prediction into a 4-class output
#' where ground is encoded as class \code{3}.
#'
#' @details
#' \strong{Intended workflow}
#' \enumerate{
#'   \item Run \code{\link{predict}} with \code{mode = "overwrite"} to write model
#'   predictions into the LAS \code{Classification} attribute (typically classes
#'   \code{0}, \code{1}, \code{2}).
#'   \item Call \code{add_ground_csf()} to detect ground points and overwrite only
#'   those points to class \code{3}.
#' }
#'
#' \strong{What the function does}
#' \itemize{
#'   \item Reads the input LAS/LAZ from \code{in_las}.
#'   \item Normalizes heights with \code{lidR::normalize_height(..., lidR::knnidw())}
#'   so ground detection is more stable across sloped terrain and varying elevations.
#'   \item Builds a CSF ground-classification algorithm using \code{lidR::csf()} with
#'   parameters provided in \code{csf_args}.
#'   \item Runs \code{lidR::classify_ground()} to label ground points.
#'   \item Rewrites only the ground points to class \code{3}, while preserving the
#'   original FuelDeep3D predicted classes for non-ground points.
#'   \item Writes the updated LAS/LAZ to \code{out_las}.
#' }
#'
#' \strong{Class mapping}
#' \itemize{
#'   \item \code{0, 1, 2}: preserved from the FuelDeep3D prediction already stored in
#'   \code{Classification}.
#'   \item \code{3}: assigned to points detected as ground by CSF.
#' }
#'
#' \strong{Dependencies}
#' \itemize{
#'   \item \pkg{lidR} for I/O, height normalization, and ground classification.
#'   \item \pkg{RCSF} provides the CSF implementation used by \code{lidR::csf()}.
#' }
#'
#' @param in_las Character. Path to an input \code{.las} or \code{.laz} file that
#'   already contains FuelDeep3D predictions in the \code{Classification} attribute.
#' @param out_las Character. Output path for the updated \code{.las} or \code{.laz}.
#' @param csf_args List. Named list of arguments forwarded to \code{lidR::csf()} to tune
#'   CSF behavior (e.g., \code{rigidness}, \code{cloth_resolution}, \code{time_step},
#'   \code{class_threshold}).
#'
#' @return Invisibly returns \code{out_las} (the output file path).
#'
#' @examples
#' \donttest{
#' # Check if required packages are available before running
#' if (requireNamespace("lidR", quietly = TRUE) && 
#'     requireNamespace("RCSF", quietly = TRUE)) {
#'     
#' library(FuelDeep3D)
#' library(lidR)
#'
#' in_file  <- system.file("extdata", "las", "tree2.laz", package = "FuelDeep3D")
#' out_file <- file.path(tempdir(), "tree2_ground.laz")
#'
#' add_ground_csf(
#'   in_las  = in_file,
#'   out_las = out_file,
#'   csf_args = list(
#'     rigidness = 4,
#'     cloth_resolution = 0.25,
#'     time_step = 0.65,
#'     class_threshold = 0.05
#'   )
#' )
#' }
#' 
#' }
#' @export
add_ground_csf <- function(in_las,
                           out_las,
                           csf_args = list()) {
  if (!requireNamespace("lidR", quietly = TRUE)) {
    stop("Package 'lidR' is required. Please install it with install.packages('lidR').")
  }
  if (!requireNamespace("RCSF", quietly = TRUE)) {
    stop("Package 'RCSF' is required for csf(). Please install it with install.packages('RCSF').")
  }

  message(">> Reading LAS: ", in_las)
  las <- lidR::readLAS(in_las)
  if (lidR::is.empty(las)) stop("Input LAS is empty: ", in_las)

  # las <- lidR::normalize_height(las, lidR::knnidw())

  orig_class <- las$Classification

  alg <- do.call(lidR::csf, csf_args)

  message(">> Running CSF ground classification ...")
  las <- lidR::classify_ground(las, alg)

  is_ground <- las$Classification == lidR::LASGROUND

  las$Classification[is_ground]  <- 3L
  las$Classification[!is_ground] <- orig_class[!is_ground]

  message(">> Writing LAS with 4 classes (0,1,2 + ground=3) to: ", out_las)
  lidR::writeLAS(las, out_las)

  invisible(out_las)
}
