# ============================================================
# FuelDeep3D - Evaluation utilities (Confusion matrix + metrics)
# ============================================================

# ------------------------------
# Internal helper: class labels
# ------------------------------
format_class_labels <- function(classes, class_names = NULL, show_codes = TRUE) {
  classes_chr <- as.character(classes)
  
  if (is.null(class_names)) {
    return(classes_chr)
  }
  
  # Allow either:
  # 1) named vector: c("0"="Ground","1"="Branch","2"="Leaves")
  # 2) unnamed vector matching length(classes): c("Ground","Branch","Leaves")
  if (!is.null(names(class_names)) && any(nzchar(names(class_names)))) {
    nm <- as.character(class_names)
    names(nm) <- as.character(names(class_names))
    
    mapped <- nm[classes_chr]
    mapped[is.na(mapped)] <- classes_chr[is.na(mapped)]
    
    if (isTRUE(show_codes)) {
      return(paste0(classes_chr, " (", mapped, ")"))
    }
    return(mapped)
  }
  
  # Unnamed vector
  if (length(class_names) != length(classes_chr)) {
    stop(
      "class_names must be either a named vector keyed by class code, ",
      "or an unnamed vector with the same length as 'classes'.",
      call. = FALSE
    )
  }
  
  if (isTRUE(show_codes)) {
    return(paste0(classes_chr, " (", as.character(class_names), ")"))
  }
  as.character(class_names)
}

# ------------------------------
# Internal helper: metrics from cm
# ------------------------------
metrics_from_cm <- function(cm) {
  precision <- diag(cm) / colSums(cm)
  recall    <- diag(cm) / rowSums(cm)
  f1        <- 2 * precision * recall / (precision + recall)
  
  precision[!is.finite(precision)] <- NA_real_
  recall[!is.finite(recall)]       <- NA_real_
  f1[!is.finite(f1)]               <- NA_real_
  
  list(precision = precision, recall = recall, f1 = f1)
}

# ------------------------------
# Internal helper: build confusion matrix from LAS
# ------------------------------
.build_cm_from_las <- function(las,
                               truth_col = "label",
                               pred_col  = "Classification",
                               classes   = NULL,
                               drop_na   = TRUE) {
  if (!requireNamespace("lidR", quietly = TRUE)) {
    stop("Package 'lidR' is required. Install it with install.packages('lidR').", call. = FALSE)
  }
  stopifnot(inherits(las, "LAS"))
  
  df <- las@data
  if (!(truth_col %in% names(df))) stop("Truth column not found: ", truth_col, call. = FALSE)
  if (!(pred_col  %in% names(df))) stop("Prediction column not found: ", pred_col, call. = FALSE)
  
  truth <- suppressWarnings(as.integer(df[[truth_col]]))
  pred  <- suppressWarnings(as.integer(df[[pred_col]]))
  
  if (isTRUE(drop_na)) {
    ok <- is.finite(truth) & is.finite(pred)
    truth <- truth[ok]
    pred  <- pred[ok]
  }
  if (!length(truth)) stop("No valid truth/pred values to evaluate.", call. = FALSE)
  
  if (is.null(classes)) {
    classes <- sort(unique(c(truth, pred)))
    classes <- classes[is.finite(classes)]
  } else {
    classes <- as.integer(classes)
  }
  
  truth_f <- factor(truth, levels = classes)
  pred_f  <- factor(pred,  levels = classes)
  
  table(True = truth_f, Pred = pred_f)
}

# ------------------------------
# Internal helper: square/align cm
# ------------------------------
.as_square_cm <- function(cm, classes = NULL) {
  m <- as.matrix(cm)
  
  # Ensure row/col names exist (as codes)
  if (is.null(rownames(m))) rownames(m) <- as.character(seq_len(nrow(m)) - 1L)
  if (is.null(colnames(m))) colnames(m) <- as.character(seq_len(ncol(m)) - 1L)
  
  r <- rownames(m)
  c <- colnames(m)
  
  # Determine the class set to use (in a stable order)
  if (!is.null(classes)) {
    all_classes <- as.character(classes)
  } else {
    # Keep row order first, then add any new col classes not already present
    all_classes <- c(r, setdiff(c, r))
  }
  
  # Create square matrix filled with 0 and place existing cells
  out <- matrix(0, nrow = length(all_classes), ncol = length(all_classes),
                dimnames = list(True = all_classes, Pred = all_classes))
  out[r, c] <- m
  
  out
}

# ============================================================
# Evaluate: single LAS
# ============================================================

#' Evaluate predictions stored in a single LAS/LAZ
#'
#' @description
#' Computes a confusion matrix and standard multi-class metrics when both the
#' ground-truth labels and predictions are stored as columns in the same
#' \code{lidR::LAS} object.
#'
#' @details
#' The function returns:
#' \itemize{
#'   \item \strong{overall_accuracy}: \eqn{\sum diag(cm) / \sum cm}
#'   \item \strong{class_accuracy}: per-class accuracy computed as \eqn{TP / (TP+FN)}
#'         (same as per-class recall / producer's accuracy).
#'   \item \strong{precision}, \strong{recall}, \strong{f1}: per-class metrics.
#'   \item \strong{balanced_accuracy}: mean of \code{class_accuracy} across classes (macro average).
#' }
#'
#' To keep the confusion matrix shape stable across files (even when some classes
#' are missing), pass \code{classes = 0:2} or \code{classes = 0:3}.
#'
#' @param las A \code{lidR::LAS} object containing truth and prediction fields.
#' @param truth_col Character. Name of the ground-truth label column (default \code{"label"}).
#' @param pred_col Character. Name of the prediction column (default \code{"Classification"}).
#' @param classes Optional integer vector of expected class IDs (e.g. \code{0:2}, \code{0:3}).
#'   If \code{NULL}, inferred from observed truth and pred values.
#' @param drop_na Logical. If \code{TRUE} (default), rows with NA/Inf in truth or pred are dropped.
#' @param class_names Optional class name mapping. Either:
#'   \itemize{
#'     \item Named: \code{c("0"="Ground","1"="Branch","2"="Leaves")}
#'     \item Unnamed length-K: \code{c("Ground","Branch","Leaves")} (must match \code{classes})
#'   }
#' @param show_codes Logical. If \code{TRUE} (default), class labels display like \code{"0 (Ground)"}.
#'
#' @return A list containing the confusion matrix and metrics.
#'
#' @examples
#' if (requireNamespace("lidR", quietly = TRUE)){
#' 
#' library(lidR)
#' las <-  readLAS(system.file("extdata", "las", "tree2.laz", package = "FuelDeep3D"))
#'
#' res <- evaluate_single_las(
#'   las,
#'   truth_col = "label",
#'   pred_col  = "Classification",
#'   classes   = 0:2,
#'   class_names = c("0"="Ground","1"="Branch","2"="Leaves")
#' )
#'
#' res$class_accuracy
#' cat(sprintf("accuracy = %.2f%%\n", 100 * res$accuracy))
#' }
#' @export
evaluate_single_las <- function(las,
                                truth_col = "label",
                                pred_col  = "Classification",
                                classes   = NULL,
                                drop_na   = TRUE,
                                class_names = NULL,
                                show_codes = TRUE) {
  if (!requireNamespace("lidR", quietly = TRUE)) {
    stop("Package 'lidR' is required. Install it with install.packages('lidR').", call. = FALSE)
  }
  stopifnot(inherits(las, "LAS"))
  
  df <- las@data
  if (!(truth_col %in% names(df))) stop("Truth column not found: ", truth_col, call. = FALSE)
  if (!(pred_col  %in% names(df))) stop("Prediction column not found: ", pred_col, call. = FALSE)
  
  truth <- suppressWarnings(as.integer(df[[truth_col]]))
  pred  <- suppressWarnings(as.integer(df[[pred_col]]))
  
  if (isTRUE(drop_na)) {
    ok <- is.finite(truth) & is.finite(pred)
    truth <- truth[ok]
    pred  <- pred[ok]
  }
  if (!length(truth)) stop("No valid truth/pred values to evaluate.", call. = FALSE)
  
  if (is.null(classes)) {
    classes <- sort(unique(c(truth, pred)))
    classes <- classes[is.finite(classes)]
  } else {
    classes <- as.integer(classes)
  }
  
  truth_f <- factor(truth, levels = classes)
  pred_f  <- factor(pred,  levels = classes)
  
  cm <- table(True = truth_f, Pred = pred_f)
  
  overall_acc <- sum(diag(cm)) / sum(cm)
  class_acc   <- diag(cm) / rowSums(cm)
  class_acc[!is.finite(class_acc)] <- NA_real_
  
  m <- metrics_from_cm(cm)
  support <- rowSums(cm)
  balanced_acc <- mean(class_acc, na.rm = TRUE)
  
  class_labels <- format_class_labels(classes, class_names = class_names, show_codes = show_codes)
  dimnames(cm) <- list(True = class_labels, Pred = class_labels)
  
  names(class_acc) <- class_labels
  names(m$precision) <- class_labels
  names(m$recall) <- class_labels
  names(m$f1) <- class_labels
  
  list(
    confusion_matrix = cm,
    confusion = cm,
    accuracy = overall_acc,          
    overall_accuracy = overall_acc,
    class_accuracy = class_acc,
    balanced_accuracy = balanced_acc,
    precision = m$precision,
    recall = m$recall,
    f1 = m$f1,
    support = support,
    classes = classes,
    class_labels = class_labels,
    n = sum(cm)
  )
}

# ============================================================
# Evaluate: two LAS
# ============================================================

#' Evaluate predictions stored in two LAS/LAZ objects
#'
#' @description
#' Computes the confusion matrix and metrics when ground truth and predictions come from
#' two separate \code{lidR::LAS} objects.
#'
#' @details
#' \strong{Important:} This assumes the LAS objects are point-wise aligned (with the same points and order).
#' If they are not aligned, metrics will be meaningless.
#'
#' @inheritParams evaluate_single_las
#' @param truth_las A \code{lidR::LAS} containing ground-truth labels.
#' @param pred_las A \code{lidR::LAS} containing predicted labels.
#'
#' @return A list containing the confusion matrix and metrics.
#'
#' @examples
#' if (requireNamespace("lidR", quietly = TRUE)){
#' 
#' library(lidR)
#' truth <-  readLAS(system.file("extdata", "las", "tree2.laz", package = "FuelDeep3D"))
#' pred  <- readLAS(system.file("extdata", "las", "tree21.laz", package = "FuelDeep3D"))
#'
#' res <- evaluate_two_las(
#'   truth, pred,
#'   truth_col = "label",
#'   pred_col  = "Classification",
#'   classes   = 0:2,
#'   class_names = c("0"="Ground","1"="Branch","2"="Leaves")
#' )
#'
#' res$class_accuracy
#' cat(sprintf("accuracy = %.2f%%\n", 100 * res$accuracy))
#' }
#' @export
evaluate_two_las <- function(truth_las,
                             pred_las,
                             truth_col = "label",
                             pred_col  = "Classification",
                             classes   = NULL,
                             drop_na   = TRUE,
                             class_names = NULL,
                             show_codes = TRUE) {
  if (!requireNamespace("lidR", quietly = TRUE)) {
    stop("Package 'lidR' is required. Install it with install.packages('lidR').", call. = FALSE)
  }
  stopifnot(inherits(truth_las, "LAS"))
  stopifnot(inherits(pred_las, "LAS"))
  
  if (lidR::is.empty(truth_las) || lidR::is.empty(pred_las)) {
    stop("One of the LAS objects is empty.", call. = FALSE)
  }
  if (lidR::npoints(truth_las) != lidR::npoints(pred_las)) {
    stop("LAS objects are not aligned: different number of points.", call. = FALSE)
  }
  
  df_t <- truth_las@data
  df_p <- pred_las@data
  
  if (!(truth_col %in% names(df_t))) stop("Truth column not found in truth_las: ", truth_col, call. = FALSE)
  if (!(pred_col  %in% names(df_p))) stop("Prediction column not found in pred_las: ", pred_col, call. = FALSE)
  
  truth <- suppressWarnings(as.integer(df_t[[truth_col]]))
  pred  <- suppressWarnings(as.integer(df_p[[pred_col]]))
  
  if (isTRUE(drop_na)) {
    ok <- is.finite(truth) & is.finite(pred)
    truth <- truth[ok]
    pred  <- pred[ok]
  }
  if (!length(truth)) stop("No valid truth/pred values to evaluate.", call. = FALSE)
  
  if (is.null(classes)) {
    classes <- sort(unique(c(truth, pred)))
    classes <- classes[is.finite(classes)]
  } else {
    classes <- as.integer(classes)
  }
  
  truth_f <- factor(truth, levels = classes)
  pred_f  <- factor(pred,  levels = classes)
  
  cm <- table(True = truth_f, Pred = pred_f)
  
  overall_acc <- sum(diag(cm)) / sum(cm)
  class_acc   <- diag(cm) / rowSums(cm)
  class_acc[!is.finite(class_acc)] <- NA_real_
  
  m <- metrics_from_cm(cm)
  support <- rowSums(cm)
  balanced_acc <- mean(class_acc, na.rm = TRUE)
  
  class_labels <- format_class_labels(classes, class_names = class_names, show_codes = show_codes)
  dimnames(cm) <- list(True = class_labels, Pred = class_labels)
  
  names(class_acc) <- class_labels
  names(m$precision) <- class_labels
  names(m$recall) <- class_labels
  names(m$f1) <- class_labels
  
  list(
    confusion_matrix = cm,
    confusion = cm,
    accuracy = overall_acc,          
    overall_accuracy = overall_acc,
    class_accuracy = class_acc,
    balanced_accuracy = balanced_acc,
    precision = m$precision,
    recall = m$recall,
    f1 = m$f1,
    support = support,
    classes = classes,
    class_labels = class_labels,
    n = sum(cm)
  )
}

# ------------------------------------------------------------
# 3D LAS Visualization (height-colored, configurable background)
# ------------------------------------------------------------
#' Plot a 3D LAS point cloud colored by elevation
#'
#' @description
#' Visualize a \code{lidR::LAS} point cloud in 3D using \pkg{rgl}, with points colored by elevation (Z).
#' Supports subsampling for performance, custom height color ramps, optional coordinate centering,
#' an optional compact legend, and optional "thickness by height" (points become larger at higher Z).
#'
#' @details
#' **Color mapping**
#' \itemize{
#'   \item Elevations are optionally clamped to \code{zlim} and normalized to \eqn{[0,1]} for palette lookup.
#'   \item When \code{zlim} is provided, values outside the range are clamped so the color scale stays comparable.
#' }
#'
#' **Legend**
#' \itemize{
#'   \item The legend shows the same color scale as used for points.
#'   \item When \code{legend_label_mode = "norm_z"}, labels are shown as
#'   \code{norm=0 (z = ...)} and \code{norm=1 (z = ...)} to clarify that 0/1 refers to the normalized scale.
#'   \item Use \code{legend_height_frac} to shorten the legend visually (e.g., 0.5 = half height).
#' }
#'
#' **CRAN / non-interactive environments**
#' \itemize{
#'   \item During \code{R CMD check}, the function returns early (no \pkg{rgl} window is opened).
#'   \item If \pkg{rgl} is not installed, a warning is raised and the function returns invisibly.
#' }
#'
#' @param las A \code{lidR::LAS} object.
#' @param bg Background color for the \pkg{rgl} scene. (e.g., \code{"black"}, \code{"white"}, \code{"#111111"}).
#' @param zlim NULL or numeric length-2 vector giving the Z range used for coloring (values are clamped to this range).
#' @param height_palette Vector of colors to define the height color ramp
#' (e.g., \code{c("purple","blue","cyan","yellow","red")}).
#' @param size Numeric. Base point size (thickness). If \code{size_by_height = TRUE}, this acts as a multiplier on \code{size_range}.
#' @param max_points Integer. If LAS has more than this many points, a random subsample is plotted for speed.
#' Use NULL to disable subsampling.
#' @param title Character. Plot title (converted to ASCII-safe to avoid \pkg{rgl} text errors).
#' @param center Logical. If TRUE, shifts X/Y/Z so minima become 0 (helps visualization when coordinates are large).
#'
#' @param add_legend Logical. If TRUE, draws a vertical colorbar and min/max labels.
#' @param legend_height_frac Numeric in (0,1]. Visually compress legend height (e.g., 0.5 = half height).
#' @param legend_width_frac Numeric. Legend width as a fraction of X-range.
#' @param legend_xpad_frac Numeric. Legend x-offset as a fraction of X-range.
#' @param legend_side Character. Either \code{"right"} or \code{"left"}.
#' @param legend_pos Numeric length-2 vector \code{c(x, y)} to override legend base position (use NA to ignore a coordinate).
#' @param legend_label_mode Character. One of \code{"norm_z"}, \code{"z"}, or \code{"norm"}.
#' @param z_digits Integer. Number of digits used for legend Z labels.
#' @param z_unit Character. Unit label appended to legend Z values (e.g., \code{"m"}). Use \code{""} for none.
#'
#' @param size_by_height Logical. If TRUE, point thickness increases with height.
#' @param size_range Numeric length-2. Min/max point size (before multiplying by \code{size}) when \code{size_by_height = TRUE}.
#' @param size_power Numeric > 0. Controls how quickly thickness increases with height (larger = more emphasis at top).
#'
#' @param zoom,theta,phi Camera controls passed to \code{rgl::view3d()}.
#'
#' @return Invisibly returns NULL.
#' @export
#'
#' @examples
#' if (requireNamespace("lidR", quietly = TRUE) && interactive()) {
#'     # Your plot code here
#' library(lidR)
#'
#' las <-  readLAS(system.file("extdata", "las", "tree2.laz", package = "FuelDeep3D"))
#'
#' # 1) Default plot (black bg, legend on, thickness by height)
#' plot_3d(las)
#'
#' # 2) Custom palette + white background
#' plot_3d(
#'   las,
#'   bg = "white",
#'   height_palette = c("purple","blue","cyan","yellow","red"),
#'   title = "Custom palette"
#' )
#'
#' # 3) Fixed Z color scale for comparisons + no legend
#' plot_3d(
#'   las,
#'   zlim = c(0, 40),
#'   add_legend = FALSE,
#'   title = "Fixed Z (0-40), no legend"
#' )
#'
#' # 4) Turn OFF thickness-by-height; use a single point size
#' plot_3d(
#'   las,
#'   size_by_height = FALSE,
#'   size = 4,
#'   title = "Uniform thicker points"
#' )
#'
#' # 5) Legend on the LEFT and thicker legend bar
#' plot_3d(
#'   las,
#'   legend_side = "left",
#'   legend_width_frac = 0.05,
#'   title = "Legend left"
#' )
#'
#' # 6) Make everything thicker (multiplies size_range when size_by_height=TRUE)
#' plot_3d(
#'   las,
#'   size = 1.8,
#'   size_range = c(1, 7),
#'   size_power = 1.2,
#'   title = "Thicker points by height"
#' )
#' }
#'
#' @export
plot_3d <- function(las,
                        bg = "black",
                        zlim = NULL,
                        height_palette = NULL,
                        size = 0.6,
                        max_points = 400000L,
                        title = "LAS 3D View",
                        center = FALSE,
                        add_legend = TRUE,
                        legend_height_frac = 0.5,
                        legend_width_frac = 0.015,
                        legend_xpad_frac = 0.03,
                        legend_side = "right",
                        legend_pos  = c(NA_real_, NA_real_),
                        legend_label_mode = c("rel_z", "norm_z", "z", "norm"),
                        z_digits = 2L,
                        z_unit = "m",
                        size_by_height = TRUE,
                        size_range = c(1, 4),
                        size_power = 1.2,
                        zoom = 0.7,
                        theta = 0,
                        phi = -90) {
  
  if (!requireNamespace("lidR", quietly = TRUE)) {
    stop("Package 'lidR' is required. Install it with install.packages('lidR').", call. = FALSE)
  }
  stopifnot(inherits(las, "LAS"))
  
  # ---- HARD CRAN GUARD ----
  if (identical(Sys.getenv("_R_CHECK_PACKAGE_NAME_"), "FuelDeep3D")) {
    warning("Skipping rgl visualization during R CMD check.", call. = FALSE)
    return(invisible(NULL))
  }
  
  if (!requireNamespace("rgl", quietly = TRUE)) {
    warning("Package 'rgl' is not installed. Install it to enable 3D plotting.", call. = FALSE)
    return(invisible(NULL))
  }
  
  if (lidR::is.empty(las)) return(invisible(NULL))
  
  # -------- helpers --------
  safe_ascii <- function(s) {
    s <- gsub("\u2013|\u2014", "-", s)  # en/em dash -> hyphen
    iconv(s, from = "", to = "ASCII//TRANSLIT", sub = "")
  }
  
  pick_text_col <- function(bg_col) {
    rgb <- try(grDevices::col2rgb(bg_col), silent = TRUE)
    if (inherits(rgb, "try-error")) return("white")
    r <- rgb[1, 1] / 255; g <- rgb[2, 1] / 255; b <- rgb[3, 1] / 255
    lum <- 0.2126 * r + 0.7152 * g + 0.0722 * b
    if (lum < 0.5) "white" else "black"
  }
  
  legend_label_mode <- match.arg(legend_label_mode)
  
  # optional subsample for performance
  n <- lidR::npoints(las)
  if (!is.null(max_points) && is.finite(max_points) && max_points > 0 && n > max_points) {
    idx <- sample.int(n, size = as.integer(max_points))
    las_plot <- las[idx]
  } else {
    las_plot <- las
  }
  
  xyz <- las_plot@data[, c("X", "Y", "Z"), drop = FALSE]
  x <- xyz[["X"]]; y <- xyz[["Y"]]; z <- xyz[["Z"]]
  
  # optional centering (display coordinates)
  if (isTRUE(center)) {
    x <- x - min(x, na.rm = TRUE)
    y <- y - min(y, na.rm = TRUE)
    z_disp <- z - min(z, na.rm = TRUE)
  } else {
    z_disp <- z
  }
  
  # choose z-limits for coloring
  if (is.null(zlim)) {
    zmin <- min(z_disp, na.rm = TRUE)
    zmax <- max(z_disp, na.rm = TRUE)
  } else {
    if (!is.numeric(zlim) || length(zlim) != 2L || any(!is.finite(zlim))) {
      stop("zlim must be NULL or a numeric vector of length 2 with finite values.", call. = FALSE)
    }
    zmin <- min(zlim); zmax <- max(zlim)
  }
  if (!is.finite(zmin) || !is.finite(zmax) || zmin == zmax) {
    stop("Invalid z-range for coloring (zlim or data range).", call. = FALSE)
  }
  
  # clamp and normalize to [0,1]
  zc <- pmin(pmax(z_disp, zmin), zmax)
  t  <- (zc - zmin) / (zmax - zmin)
  
  # palette
  if (is.null(height_palette)) {
    height_palette <- c("blue", "cyan", "green", "yellow", "orange", "red")
  }
  ramp <- grDevices::colorRampPalette(height_palette)
  pal  <- ramp(256L)
  cols <- pal[pmax(1L, pmin(256L, as.integer(round(t * 255L)) + 1L))]
  
  # scene
  text_col <- pick_text_col(bg)
  rgl::open3d()
  rgl::bg3d(color = bg)
  rgl::title3d(safe_ascii(title), color = text_col)
  
  # ---------- points ----------
  if (!isTRUE(size_by_height)) {
    rgl::points3d(x, y, z_disp, col = cols, size = size)
  } else {
    if (!is.numeric(size_range) || length(size_range) != 2L) {
      stop("size_range must be numeric length-2, e.g., c(1, 7).", call. = FALSE)
    }
    # size is a multiplier so users always "see" point size
    smin <- min(size_range) * size
    smax <- max(size_range) * size
    
    if (!is.finite(smin) || !is.finite(smax) || smin <= 0 || smax <= 0) {
      stop("size_range * size must be finite and > 0.", call. = FALSE)
    }
    pwr <- ifelse(is.finite(size_power) && size_power > 0, size_power, 1)
    tt <- t ^ pwr
    
    # bin + draw in layers (portable; avoids per-point size vector issues)
    nb <- 7L
    bins <- cut(tt, breaks = nb, include.lowest = TRUE, labels = FALSE)
    
    for (b in seq_len(nb)) {
      idx <- which(bins == b)
      if (length(idx)) {
        frac <- (b - 1) / (nb - 1)
        sb <- smin + (smax - smin) * (frac ^ pwr)
        rgl::points3d(x[idx], y[idx], z_disp[idx], col = cols[idx], size = sb)
      }
    }
  }
  
  # ---------- legend ----------
  if (isTRUE(add_legend)) {
    frac_h <- max(0.05, min(1, legend_height_frac))
    xr <- diff(range(x, na.rm = TRUE)); if (!is.finite(xr) || xr <= 0) xr <- 1
    
    w    <- max(0.001, legend_width_frac) * xr
    xpad <- max(0.0,   legend_xpad_frac)  * xr
    
    # anchor by side
    if (tolower(legend_side) == "left") {
      xpos0 <- min(x, na.rm = TRUE) - xpad - w
    } else {
      xpos0 <- max(x, na.rm = TRUE) + xpad
    }
    ypos0 <- min(y, na.rm = TRUE)
    
    # optional override
    if (is.numeric(legend_pos) && length(legend_pos) == 2L) {
      if (is.finite(legend_pos[1])) xpos0 <- legend_pos[1]
      if (is.finite(legend_pos[2])) ypos0 <- legend_pos[2]
    }
    
    zseq <- seq(zmin, zmax, length.out = 200)
    tbar <- (zseq - zmin) / (zmax - zmin)
    cols_bar <- pal[pmax(1L, pmin(256L, as.integer(round(tbar * 255L)) + 1L))]
    
    # compressed bar coordinates (visual), but still represents full zmin..zmax
    zb0  <- zmin
    zb1  <- zmin + frac_h * (zmax - zmin)
    zpos <- zb0 + frac_h * (zseq - zmin)
    
    for (i in seq_len(length(zseq) - 1)) {
      rgl::quads3d(
        c(xpos0, xpos0 + w, xpos0 + w, xpos0),
        c(ypos0, ypos0, ypos0, ypos0),
        c(zpos[i], zpos[i], zpos[i + 1], zpos[i + 1]),
        col = cols_bar[i],
        lit = FALSE
      )
    }
    
    # labels outside bar depending on side
    xlab <- if (tolower(legend_side) == "left") xpos0 - 2*w else xpos0 + 2*w
    adjx <- if (tolower(legend_side) == "left") 1 else 0
    
    zfmt <- paste0("%.", as.integer(z_digits), "f")
    zmin_s <- sprintf(zfmt, zmin)
    zmax_s <- sprintf(zfmt, zmax)
    unit_s <- if (nzchar(z_unit)) paste0(" ", z_unit) else ""
    
    # Relative height range (top - bottom) in the same units as z
    zspan <- zmax - zmin
    zspan_s <- sprintf(zfmt, zspan)
    
    if (legend_label_mode == "rel_z") {
      lab_min <- paste0("0", unit_s, " (z = ", zmin_s, ")")
      lab_max <- paste0(zspan_s, unit_s, " (z = ", zmax_s, ")")
    }
    else if (legend_label_mode == "norm_z") {
      lab_min <- paste0("norm=0 (z = ", zmin_s, unit_s, ")")
      lab_max <- paste0("norm=1 (z = ", zmax_s, unit_s, ")")
      
    } else if (legend_label_mode == "norm") {
      lab_min <- "0"
      lab_max <- "1"
      
    } else { # "z"
      lab_min <- paste0(zmin_s, unit_s)
      lab_max <- paste0(zmax_s, unit_s)
    }
    
    
    rgl::text3d(xlab, ypos0, zb0, texts = lab_min, col = text_col, adj = c(adjx, 0))
    rgl::text3d(xlab, ypos0, zb1, texts = lab_max, col = text_col, adj = c(adjx, 1))
  }
  
  # camera
  rgl::view3d(theta = theta, phi = phi, zoom = zoom)
  invisible(NULL)
}


# ============================================================
# Print metrics table (includes macro/weighted; overall optional)
# ============================================================

#' Print per-class metrics and summary averages
#'
#' @description
#' Formats and prints a per-class metrics table from the output of
#' \code{\link{evaluate_single_las}} or \code{\link{evaluate_two_las}}.
#' The table includes per-class Support, Accuracy, Precision, Recall, and F1.
#' Optionally includes Macro and Weighted averages, and an Overall accuracy row.
#'
#' @details
#' \itemize{
#'   \item \strong{Support}: number of true points in each class (row sum of confusion matrix).
#'   \item \strong{Accuracy}: per-class accuracy here means \strong{Recall} (TP / (TP + FN)).
#'     This is sometimes called \emph{producer's accuracy}.
#'   \item \strong{Macro avg}: unweighted mean across classes (ignores class imbalance).
#'   \item \strong{Weighted avg}: mean across classes weighted by Support (reflects imbalance).
#'   \item \strong{Overall accuracy}: sum(diag(cm)) / sum(cm). This is optional because
#'     it often duplicates the weighted recall/accuracy in multi-class settings.
#' }
#'
#' @param results List returned by \code{evaluate_single_las()} or \code{evaluate_two_las()}.
#' @param digits Integer. Number of decimal places to round numeric metrics.
#' @param include_macro Logical. If TRUE, include a "Macro avg" row.
#' @param include_weighted Logical. If TRUE, include a "Weighted avg" row.
#' @param include_overall_accuracy Logical. If TRUE, include an "Overall accuracy" row.
#'
#' @return
#' Invisibly returns a \code{data.frame} with per-class metrics and optional summary rows.
#'
#' @examples
#' if (requireNamespace("lidR", quietly = TRUE)){
#' library(lidR)
#' las <-  readLAS(system.file("extdata", "las", "tree2.laz", package = "FuelDeep3D"))
#'
#' res <- evaluate_single_las(
#'   las,
#'   truth_col = "label",
#'   pred_col  = "Classification",
#'   classes   = 0:2,
#'   class_names = c("0"="Ground","1"="Branch","2"="Leaves")
#' )
#'
#' print_metrics_table(res, include_overall_accuracy = TRUE)
#' }
#'
#' @export
print_metrics_table <- function(results,
                                digits = 4,
                                include_macro = TRUE,
                                include_weighted = TRUE,
                                include_overall_accuracy = FALSE) {
  if (is.null(results$class_labels) || is.null(results$support)) {
    stop("results must come from evaluate_single_las() or evaluate_two_las().", call. = FALSE)
  }
  
  labs <- results$class_labels
  support <- as.numeric(results$support)
  names(support) <- labs
  
  acc <- as.numeric(results$class_accuracy); names(acc) <- labs
  pr  <- as.numeric(results$precision);      names(pr)  <- labs
  rc  <- as.numeric(results$recall);         names(rc)  <- labs
  f1  <- as.numeric(results$f1);             names(f1)  <- labs
  
  # per-class table
  df <- data.frame(
    Class     = labs,
    Support   = support,
    Accuracy  = round(acc, digits),
    Precision = round(pr,  digits),
    Recall    = round(rc,  digits),
    F1_Score  = round(f1,  digits),
    stringsAsFactors = FALSE
  )
  
  out_rows <- list(df)
  
  # weights for weighted averages
  w <- support / sum(support)
  
  if (isTRUE(include_macro)) {
    macro_row <- data.frame(
      Class     = "Macro avg",
      Support   = sum(support),
      Accuracy  = round(mean(acc, na.rm = TRUE), digits),
      Precision = round(mean(pr,  na.rm = TRUE), digits),
      Recall    = round(mean(rc,  na.rm = TRUE), digits),
      F1_Score  = round(mean(f1,  na.rm = TRUE), digits),
      stringsAsFactors = FALSE
    )
    out_rows <- c(out_rows, list(macro_row))
  }
  
  if (isTRUE(include_weighted)) {
    weighted_row <- data.frame(
      Class     = "Weighted avg",
      Support   = sum(support),
      Accuracy  = round(sum(w * acc, na.rm = TRUE), digits),
      Precision = round(sum(w * pr,  na.rm = TRUE), digits),
      Recall    = round(sum(w * rc,  na.rm = TRUE), digits),
      F1_Score  = round(sum(w * f1,  na.rm = TRUE), digits),
      stringsAsFactors = FALSE
    )
    out_rows <- c(out_rows, list(weighted_row))
  }
  
  if (isTRUE(include_overall_accuracy)) {
    overall_row <- data.frame(
      Class     = "Overall accuracy",
      Support   = sum(support),
      Accuracy  = round(as.numeric(results$overall_accuracy), digits),
      Precision = NA_real_,
      Recall    = NA_real_,
      F1_Score  = NA_real_,
      stringsAsFactors = FALSE
    )
    out_rows <- c(out_rows, list(overall_row))
  }
  
  df_out <- do.call(rbind, out_rows)
  rownames(df_out) <- NULL
  print(df_out)
  invisible(df_out)
}


# ============================================================
# Noise Filtering Utilities
# ============================================================

#' Remove sparse outlier points using Statistical Outlier Removal (SOR)
#'
#' @description
#' Applies a k-nearest-neighbor Statistical Outlier Removal (SOR) filter to points
#' above a user-defined height threshold. Points at or below the threshold are
#' preserved unchanged.
#'
#' @details
#' The filter is applied only to points with \code{Z} is greater than \code{height_thresh}.
#' For each of these points, the mean distance to its \code{k} nearest neighbors
#' is computed in 3D XYZ space. A point is kept if:
#' \deqn{d_i < mean(d) + zscore * sd(d)}
#' where \code{d_i} is the mean kNN distance for point i.
#'
#' To keep behavior stable and safe, \code{k} is automatically capped so that
#' \code{k < n}, where \code{n} is the number of points being filtered.
#'
#' This function preserves the original point order by computing a global keep
#' mask and subsetting once.
#'
#' @param las A \code{lidR::LAS} object.
#' @param height_thresh Numeric. Height (meters) above which filtering is applied.
#' @param k Integer. Number of nearest neighbors used by the SOR filter.
#' @param zscore Numeric. Standard deviation multiplier controlling outlier rejection.
#'
#' @return A filtered \code{lidR::LAS} object.
#'
#' @examples
#' # Check for both lidR and dbscan before running
#' if (requireNamespace("lidR", quietly = TRUE) && 
#'     requireNamespace("dbscan", quietly = TRUE)) {
#'   
#'   las <- lidR::readLAS(system.file("extdata", "las", "tree2.laz", package = "FuelDeep3D"))
#'   
#'   if (!lidR::is.empty(las)) {
#'     las_small <- las[seq_len(min(20000, lidR::npoints(las)))]
#'     las_clean <- remove_noise_sor(las_small, height_thresh = 1, k = 10, zscore = 2.5)
#'     
#'     # Optional: Print to console to show it worked
#'     print(lidR::npoints(las_small))
#'     print(lidR::npoints(las_clean))
#'   }
#' }
#'
#' @export
remove_noise_sor <- function(las,
                             height_thresh = 5,
                             k = 20,
                             zscore = 2.5) {
  if (!requireNamespace("lidR", quietly = TRUE)) {
    stop("Package 'lidR' is required. Install it with install.packages('lidR').",
         call. = FALSE)
  }
  if (!requireNamespace("dbscan", quietly = TRUE)) {
    stop("Package 'dbscan' is required. Install it with install.packages('dbscan').",
         call. = FALSE)
  }
  
  stopifnot(inherits(las, "LAS"))
  if (lidR::is.empty(las)) return(las)
  
  if (!is.numeric(height_thresh) || length(height_thresh) != 1 || height_thresh < 0) {
    stop("height_thresh must be a single non-negative numeric value.", call. = FALSE)
  }
  if (!is.numeric(k) || length(k) != 1 || k < 1) {
    stop("k must be a single positive integer.", call. = FALSE)
  }
  if (!is.numeric(zscore) || length(zscore) != 1 || zscore <= 0) {
    stop("zscore must be a single positive numeric value.", call. = FALSE)
  }
  
  k <- as.integer(k)
  
  # Global masks (preserve original order)
  is_high <- las$Z > height_thresh
  if (!any(is_high, na.rm = TRUE)) return(las)
  
  # Work only on high points
  high_idx <- which(is_high)
  high_pts <- las[high_idx]
  
  if (lidR::is.empty(high_pts)) return(las)
  
  # Build XYZ and drop non-finite rows (defensive, CRAN-safe)
  xyz <- cbind(high_pts$X, high_pts$Y, high_pts$Z)
  ok <- is.finite(xyz[, 1]) & is.finite(xyz[, 2]) & is.finite(xyz[, 3])
  
  if (!all(ok)) {
    xyz <- xyz[ok, , drop = FALSE]
    high_idx <- high_idx[ok]
    high_pts <- high_pts[ok]
  }
  
  n <- nrow(xyz)
  if (is.null(n) || n < 2L) return(las)
  
  # kNNdist requires k < n; cap safely
  k_use <- min(k, n - 1L)
  if (k_use < 1L) return(las)
  
  knn_dist <- dbscan::kNNdist(xyz, k = k_use)
  
  # kNNdist can return vector in edge cases -> handle both
  if (is.null(dim(knn_dist))) {
    d_mean <- as.numeric(knn_dist)
  } else {
    d_mean <- rowMeans(knn_dist)
  }
  
  if (length(d_mean) == 0L || any(!is.finite(d_mean))) return(las)
  
  mu <- mean(d_mean)
  sigma <- stats::sd(d_mean)
  
  if (!is.finite(sigma) || sigma <= 0) return(las)
  
  keep_high <- d_mean < (mu + zscore * sigma)
  if (length(keep_high) != length(high_idx)) return(las)
  
  # Build final keep mask for all points (preserve order)
  keep_all <- rep(TRUE, lidR::npoints(las))
  keep_all[high_idx] <- keep_high
  
  las[keep_all]
}


# ============================================================
# Print confusion matrix (LAS or cm)
# ============================================================

#' Print a confusion matrix (LAS/LAZ or precomputed cm)
#'
#' @description
#' Prints a confusion matrix as a readable table. You can pass either:
#' \itemize{
#'   \item a precomputed confusion matrix \code{table/matrix}, OR
#'   \item a \code{lidR::LAS} object (then \code{truth_col} and \code{pred_col} are used to build the matrix).
#' }
#'
#' @param x A confusion matrix (table/matrix) OR a \code{lidR::LAS} object.
#' @param truth_col Character. Truth label column name (used when \code{x} is LAS).
#' @param pred_col Character. Prediction column name (used when \code{x} is LAS).
#' @param classes Optional integer vector of expected class IDs (e.g., \code{0:2}).
#'   Keeps matrix shape stable even if some classes are missing.
#' @param class_names Optional mapping for nicer labels (named or unnamed).
#' @param show_codes Logical. If \code{TRUE} (default), labels display like \code{"0 (Ground)"}.
#' @param drop_na Logical. Drop rows where truth/pred is NA/Inf (used when \code{x} is LAS).
#' @param row_normalize Logical. If TRUE, each row is converted to proportions (rows sum to 1).
#' @param digits Integer. Number of decimal places to show when \code{row_normalize = TRUE}.
#'
#' @return Invisibly returns the printed data frame.
#'
#' @examples
#' if (requireNamespace("lidR", quietly = TRUE)){
#' 
#' library(lidR)
#' las <-  readLAS(system.file("extdata", "las", "tree2.laz", package = "FuelDeep3D"))
#'
#' print_confusion_matrix(
#'   las,
#'   truth_col = "label",
#'   pred_col  = "Classification",
#'   classes   = 0:2,
#'   class_names = c("0"="Ground","1"="Branch","2"="Leaves"),
#'     row_normalize = TRUE,
#'     digits = 3
#' )
#' }
#' @export
print_confusion_matrix <- function(x,
                                   truth_col = "label",
                                   pred_col  = "Classification",
                                   row_normalize = FALSE,
                                   digits = 3,
                                   classes   = NULL,
                                   class_names = NULL,
                                   show_codes = TRUE,
                                   drop_na = TRUE) {
  
  if (inherits(x, "LAS")) {
    cm <- .build_cm_from_las(
      x,
      truth_col = truth_col,
      pred_col  = pred_col,
      classes   = classes,
      drop_na   = drop_na
    )
    if (is.null(classes)) {
      classes <- rownames(as.matrix(cm))
    }
  } else {
    cm <- x
    if (is.null(classes)) {
      m0 <- as.matrix(cm)
      if (!is.null(rownames(m0))) {
        classes <- rownames(m0)
      } else {
        classes <- as.character(seq_len(nrow(m0)) - 1L)
      }
    }
  }
  
  m <- .as_square_cm(cm, classes = classes)
  if (isTRUE(row_normalize)) {
    rs <- rowSums(m)
    rs[rs == 0] <- NA_real_
    m <- m / rs
  }
  if (isTRUE(row_normalize)) {
    m <- round(m, digits)
  }
  
  
  labs <- format_class_labels(rownames(m), class_names = class_names, show_codes = show_codes)
  dimnames(m) <- list(True = labs, Pred = labs)
  
  df <- as.data.frame.matrix(m)
  print(df)
  invisible(df)
}

# ============================================================
# Confusion matrix heatmap (cm input)
# ============================================================

#' Plot a confusion matrix heatmap (ggplot2)
#'
#' @description
#' Creates a heatmap visualization of a confusion matrix. If \code{row_normalize = TRUE},
#' each row is converted to proportions so rows sum to 1.
#'
#' @importFrom rlang .data
#' 
#' @param palette_type Character. Color palette strategy for the heatmap.
#'   Common options include \code{"viridis"}, \code{"brewer"}, and \code{"gradient"}.
#' @param palette_name Character. Palette name used when \code{palette_type} supports
#'   named palettes (e.g., viridis option name or RColorBrewer palette name).
#' @param brewer_direction Integer. Direction for RColorBrewer palettes.
#'   Use \code{1} for default direction, \code{-1} to reverse.
#' @param gradient_low Character. Low-end color for \code{palette_type="gradient"}.
#'   Can be any valid R color (e.g., \code{"white"} or \code{"#FFFFFF"}).
#' @param gradient_mid Character. Mid-point color for \code{palette_type="gradient"}.
#'   Can be any valid R color.
#' @param gradient_high Character. High-end color for \code{palette_type="gradient"}.
#'   Can be any valid R color.
#' @param base_n Integer. Number of colors to generate for discrete palettes
#'   (used when generating a ramp for the heatmap).
#' @param na_fill Character. Fill color used for \code{NA} cells in the heatmap.
#' @param label_size Numeric. Text size for cell labels (counts/percentages) and/or axis labels,
#'   depending on your implementation.
#' @param auto_label_color Logical. If \code{TRUE}, automatically chooses a readable label
#'   text color based on the cell fill (improves contrast).
#' @param label_color_dark Character. Label text color to use on light cells when
#'   \code{auto_label_color=TRUE}.
#' @param label_color_light Character. Label text color to use on dark cells when
#'   \code{auto_label_color=TRUE}.
##' @param cm A confusion matrix (table or matrix). Rows = true class, columns = predicted class.
#' @param las_name Optional character. A short LAS/LAZ filename label appended to the title.
#' @param title Character. Plot title.
#' @param row_normalize Logical. If TRUE, normalize each row so it sums to 1 (proportions).
#' @param digits Integer. Number of decimal digits to show when row_normalize = TRUE.
#' @param show_values Logical. If TRUE, print values inside cells.
#' @param class_names Optional named character vector mapping class codes to readable names.
#'   Example: c("0"="Ground","1"="Leaves","2"="Branch").
#' @param show_codes Logical. If TRUE, show both code + name on axes (e.g., "1 - Stem").
#' @param flip_y Logical. If TRUE, reverses y-axis order (often looks more like a typical CM).
#'
#' @return A ggplot object (invisibly).
#'
#' @examples
#' if (requireNamespace("lidR", quietly = TRUE) && 
#'     requireNamespace("ggplot2", quietly = TRUE)){
#'     
#' library(lidR)
#'
#' # Read LAS/LAZ
#' las <-  readLAS(system.file("extdata", "las", "tree2.laz", package = "FuelDeep3D"))
#'
#' # Confusion matrix: True labels vs Predicted class (LAS Classification)
#' cm <- table(True = las@data$label, Pred = las@data$Classification)
#'
#' # ------------------------------------------------------------
#' # 1) Row-normalized confusion matrix (Proportions)
#' #    - Best to understand per-class recall behavior
#' #    - row_normalize = TRUE is important here
#' # ------------------------------------------------------------
#' plot_confusion_matrix(
#'   cm,
#'   row_normalize = TRUE,
#'   las_name = "trees.laz",
#'   title = "Confusion Matrix (Row-normalized)",
#'   class_names = c("0" = "Ground", "1" = "Leaves", "2" = "Branch"),
#'   palette_type = "viridis",
#'   palette_name = "cividis"
#' )
#'
#' # ------------------------------------------------------------
#' # 2) Counts confusion matrix (Raw counts)
#' #    - Shows absolute misclassification volume
#' #    - row_normalize = FALSE is important here
#' # ------------------------------------------------------------
#' plot_confusion_matrix(
#'   cm,
#'   row_normalize = FALSE,
#'   las_name = "trees.laz",
#'   title = "Confusion Matrix (Counts)",
#'   class_names = c("0" = "Ground", "1" = "Leaves", "2" = "Branch"),
#'   palette_type = "viridis",
#'   palette_name = "viridis"
#' )
#'
#' # ------------------------------------------------------------
#' # 3) Brewer palette example (soft + classic)
#' #    - Works great for both normalized and counts
#' # ------------------------------------------------------------
#' plot_confusion_matrix(
#'   cm,
#'   row_normalize = TRUE,
#'   las_name = "trees.laz",
#'   title = "Confusion Matrix (Brewer Blues, Row-normalized)",
#'   class_names = c("0" = "Ground", "1" = "Leaves", "2" = "Branch"),
#'   palette_type = "brewer",
#'   palette_name = "Blues"
#' )
#'
#' # ------------------------------------------------------------
#' # 4) Custom modern gradient (minimal + professional)
#' # ------------------------------------------------------------
#' plot_confusion_matrix(
#'   cm,
#'   row_normalize = TRUE,
#'   las_name = "trees.laz",
#'   title = "Confusion Matrix (Custom Gradient, Row-normalized)",
#'   class_names = c("0" = "Ground", "1" = "Leaves", "2" = "Branch"),
#'   palette_type = "gradient",
#'   gradient_low  = "white",
#'   gradient_high = "#2C3E50"
#' )
#'
#' # ------------------------------------------------------------
#' # 5) Base palette example (if you still want them)
#' #    - heat / terrain / topo / cm / rainbow
#' # ------------------------------------------------------------
#' plot_confusion_matrix(
#'   cm,
#'   row_normalize = TRUE,
#'   las_name = "trees.laz",
#'   title = "Confusion Matrix (Base heat, Row-normalized)",
#'   class_names = c("0" = "Ground", "1" = "Leaves", "2" = "Branch"),
#'   palette_type = "base",
#'   palette_name = "heat"
#' )
#' }
#' @export
plot_confusion_matrix <- function(cm,
                                  las_name = NULL,
                                  title = "Confusion Matrix",
                                  row_normalize = FALSE,
                                  digits = 3,
                                  show_values = TRUE,
                                  class_names = NULL,
                                  show_codes = FALSE,
                                  flip_y = TRUE,
                                  # ---- palette controls ----
                                  palette_type = c("default", "viridis", "brewer", "gradient", "base"),
                                  palette_name = NULL,
                                  brewer_direction = 1,
                                  gradient_low = "white",
                                  gradient_high = "#132B43",
                                  gradient_mid = NULL,
                                  base_n = 256,
                                  na_fill = "grey90",
                                  # ---- label controls ----
                                  label_size = 4,
                                  auto_label_color = TRUE,
                                  label_color_dark = "white",
                                  label_color_light = "black") {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required. Install it with install.packages('ggplot2').", call. = FALSE)
  }
  if (!requireNamespace("scales", quietly = TRUE)) {
    stop("Package 'scales' is required. Install it with install.packages('scales').", call. = FALSE)
  }
  
  # Make sure we plot a square matrix with aligned classes
  m <- .as_square_cm(cm, classes = NULL)
  true_classes <- rownames(m)
  pred_classes <- colnames(m)
  
  if (isTRUE(row_normalize)) {
    rs <- rowSums(m)
    rs[rs == 0] <- NA_real_
    m <- m / rs
  }
  
  df <- as.data.frame(as.table(m), stringsAsFactors = FALSE)
  colnames(df) <- c("True", "Pred", "Value")
  
  if (isTRUE(row_normalize)) {
    df$Label <- ifelse(is.na(df$Value), "", formatC(df$Value, format = "f", digits = digits))
    fill_name <- "Proportion"
  } else {
    df$Label <- ifelse(is.na(df$Value), "", as.character(as.integer(round(df$Value))))
    fill_name <- "Count"
  }
  
  # Axis labels (class codes -> names)
  true_labels <- format_class_labels(true_classes, class_names = class_names, show_codes = show_codes)
  pred_labels <- format_class_labels(pred_classes, class_names = class_names, show_codes = show_codes)
  
  df$True <- factor(df$True, levels = true_classes, labels = true_labels)
  df$Pred <- factor(df$Pred, levels = pred_classes, labels = pred_labels)
  df$True <- factor(df$True, levels = true_labels)  # lock for flip
  
  if (!is.null(las_name) && nzchar(las_name)) {
    title <- paste0(title, " - ", las_name)
  }
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = Pred, y = True, fill = Value)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.2) +
    ggplot2::coord_equal() +
    ggplot2::labs(title = title, x = "Predicted", y = "True", fill = fill_name) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
  
  # ---- apply palette scale ----
  palette_type <- match.arg(palette_type)
  
  if (palette_type == "default") {
    # ggplot default (do nothing)
  } else if (palette_type == "viridis") {
    if (is.null(palette_name)) palette_name <- "viridis"
    p <- p + ggplot2::scale_fill_viridis_c(option = palette_name, na.value = na_fill)
  } else if (palette_type == "brewer") {
    if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
      stop("Package 'RColorBrewer' is required for palette_type='brewer'. Install it with install.packages('RColorBrewer').",
           call. = FALSE)
    }
    if (is.null(palette_name)) palette_name <- "Blues"
    p <- p + ggplot2::scale_fill_distiller(
      palette = palette_name,
      direction = brewer_direction,
      na.value = na_fill
    )
  } else if (palette_type == "gradient") {
    if (!is.null(gradient_mid)) {
      p <- p + ggplot2::scale_fill_gradient2(
        low = gradient_low, mid = gradient_mid, high = gradient_high,
        na.value = na_fill
      )
    } else {
      p <- p + ggplot2::scale_fill_gradient(
        low = gradient_low, high = gradient_high,
        na.value = na_fill
      )
    }
  } else if (palette_type == "base") {
    # base palettes: rainbow, heat, terrain, topo, cm.colors
    if (is.null(palette_name)) palette_name <- "heat"
    pal_fun <- switch(
      tolower(palette_name),
      "rainbow" = grDevices::rainbow,
      "heat"    = grDevices::heat.colors,
      "terrain" = grDevices::terrain.colors,
      "topo"    = grDevices::topo.colors,
      "cm"      = grDevices::cm.colors,
      stop("Unknown base palette_name. Use one of: rainbow, heat, terrain, topo, cm", call. = FALSE)
    )
    cols <- pal_fun(base_n)
    p <- p + ggplot2::scale_fill_gradientn(colors = cols, na.value = na_fill)
  }
  
  # ---- adaptive label color (black/white) ----
  if (isTRUE(show_values)) {
    if (isTRUE(auto_label_color)) {
      # Build a function that maps Value -> hex fill color using the same palette choice
      rng <- range(df$Value, na.rm = TRUE)
      if (!all(is.finite(rng)) || diff(rng) == 0) {
        df$LabelColor <- label_color_light
      } else {
        t <- (df$Value - rng[1]) / (rng[2] - rng[1])  # 0..1
        
        # compute colors for each tile based on palette type
        get_hex <- function(tt) {
          tt <- pmax(0, pmin(1, tt))
          if (palette_type == "viridis") {
            if (is.null(palette_name)) palette_name <- "viridis"
            return(viridisLite::viridis(length(tt), option = palette_name)[pmax(1, ceiling(tt * length(tt)))])
          }
          if (palette_type == "brewer") {
            if (is.null(palette_name)) palette_name <- "Blues"
            cols <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, palette_name))(256)
            return(cols[pmax(1, floor(tt * 255) + 1)])
          }
          if (palette_type == "gradient") {
            if (!is.null(gradient_mid)) {
              cols <- grDevices::colorRampPalette(c(gradient_low, gradient_mid, gradient_high))(256)
            } else {
              cols <- grDevices::colorRampPalette(c(gradient_low, gradient_high))(256)
            }
            return(cols[pmax(1, floor(tt * 255) + 1)])
          }
          if (palette_type == "base") {
            if (is.null(palette_name)) palette_name <- "heat"
            pal_fun <- switch(
              tolower(palette_name),
              "rainbow" = grDevices::rainbow,
              "heat"    = grDevices::heat.colors,
              "terrain" = grDevices::terrain.colors,
              "topo"    = grDevices::topo.colors,
              "cm"      = grDevices::cm.colors
            )
            cols <- pal_fun(256)
            return(cols[pmax(1, floor(tt * 255) + 1)])
          }
          
          # default fallback similar to ggplot default
          cols <- grDevices::colorRampPalette(c("white", "#132B43"))(256)
          cols[pmax(1, floor(tt * 255) + 1)]
        }
        
        hex <- get_hex(t)
        
        # luminance: choose white text on dark fills, black text on light fills
        rgb <- grDevices::col2rgb(hex) / 255
        lum <- 0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ]
        df$LabelColor <- ifelse(lum < 0.5, label_color_dark, label_color_light)
        df$LabelColor[is.na(df$Value)] <- label_color_light
      }
      
      p <- p + ggplot2::geom_text(
        data = df,
        ggplot2::aes(label = .data$Label, color = .data$LabelColor),
        size = label_size,
        show.legend = FALSE
      ) + ggplot2::scale_color_identity()
    
    } else {
      p <- p + ggplot2::geom_text(ggplot2::aes(label = Label), size = label_size)
    }
  }
  
  if (isTRUE(flip_y)) {
    p <- p + ggplot2::scale_y_discrete(limits = rev(levels(df$True)))
  }
  
  print(p)
  invisible(p)
}

#' Class distribution summary for a LAS point cloud
#'
#' Computes how many points belong to each class value in a given LAS attribute
#' field (e.g., predicted classes stored in \code{Classification}, or original
#' labels stored in \code{label}). Returns a tidy \code{data.frame} with counts
#' and percentages.
#'
#' This is useful after prediction to quickly inspect class balance and verify
#' that classes look reasonable (e.g., not all points predicted as one class).
#'
#' @param las A \code{LAS} object from \code{lidR}.
#' @param field Character. Column name in \code{las@data} containing class values
#'   (e.g., \code{"Classification"}, \code{"label"}).
#' @param class_labels Optional. A named character vector mapping class values
#'   to human-readable names, e.g. \code{c("0"="Ground","1"="Stem","2"="Crown")}.
#'   Names must match the class values as characters.
#' @param include_na Logical. If \code{TRUE} (default), includes \code{NA} as a
#'   separate row (shown as class \code{"<NA>"}). If \code{FALSE}, drops NA values.
#' @param sort_by Character. Sort rows by \code{"n_points"} (default),
#'   \code{"class"}, or \code{"percent"}.
#' @param decreasing Logical. If \code{TRUE} (default), sorts descending.
#'
#' @return A \code{data.frame} with columns:
#' \describe{
#'   \item{class}{Class value as character (NA shown as \code{"<NA>"})}
#'   \item{name}{(Optional) human-readable name if \code{class_labels} provided}
#'   \item{n_points}{Number of points in that class}
#'   \item{percent}{Percent of total points in that class}
#' }
#'
#' @examples
#' if (requireNamespace("lidR", quietly = TRUE)){
#' library(lidR)
#' las <-  readLAS(system.file("extdata", "las", "tree2.laz", package = "FuelDeep3D"))
#'
#' # 1) Predicted distribution (common case)
#' las_class_distribution(las, field = "Classification")
#'
#' # 2) Raw label distribution
#' las_class_distribution(las, field = "label")
#'
#' # 3) With human-readable names
#' labs <- c("0"="Ground vegetation", "1"="Foliage", "2"="Branches")
#' las_class_distribution(las, field = "Classification", class_labels = labs)
#'
#' # 4) Drop NA rows if you don't want them
#' las_class_distribution(las, field = "Classification", include_na = FALSE)
#' }
#'
#' @export
las_class_distribution <- function(las,
                                   field = "Classification",
                                   class_labels = NULL,
                                   include_na = TRUE,
                                   sort_by = c("n_points", "class", "percent"),
                                   decreasing = TRUE) {
  if (!requireNamespace("lidR", quietly = TRUE)) {
    stop("Package 'lidR' is required.", call. = FALSE)
  }
  stopifnot(inherits(las, "LAS"))
  
  if (!field %in% names(las@data)) {
    stop(sprintf("Field '%s' not found in las@data.", field), call. = FALSE)
  }
  
  sort_by <- match.arg(sort_by)
  
  v <- las@data[[field]]
  
  if (isFALSE(include_na)) {
    v <- v[!is.na(v)]
  }
  
  tab <- table(v, useNA = if (isTRUE(include_na)) "ifany" else "no")
  n_total <- sum(tab)
  
  classes <- names(tab)
  # table() uses "<NA>" label for NA bucket on many setups; normalize explicitly
  classes[is.na(classes)] <- "<NA>"
  
  df <- data.frame(
    class = classes,
    n_points = as.integer(tab),
    percent = round(100 * as.integer(tab) / n_total, 2),
    stringsAsFactors = FALSE
  )
  
  if (!is.null(class_labels)) {
    if (is.null(names(class_labels)) || any(names(class_labels) == "")) {
      stop("class_labels must be a NAMED character vector like c('0'='Ground').", call. = FALSE)
    }
    df$name <- unname(class_labels[df$class])
  }
  
  o <- switch(
    sort_by,
    n_points = order(df$n_points, decreasing = decreasing),
    percent  = order(df$percent,  decreasing = decreasing),
    class    = order(df$class,    decreasing = decreasing)
  )
  df[o, , drop = FALSE]
}

