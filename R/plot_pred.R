# ============================================================
# FuelDeep3D: Unified 3D LAS viewer (NO LEGEND)
#   - Color by any class field (e.g., "label" or "Classification")
#   - Custom class colors + custom class names (optional)
#   - Optional downsampling (none by default)
#   - Optional point thickness by height
#
# Depends: lidR, rgl
# ============================================================

.safe_ascii <- function(s) {
  s <- gsub("\u2013|\u2014", "-", s) # en/em dash -> hyphen
  iconv(s, from = "", to = "ASCII//TRANSLIT", sub = "")
}

.pick_text_col <- function(bg_col) {
  rgb <- try(grDevices::col2rgb(bg_col), silent = TRUE)
  if (inherits(rgb, "try-error")) return("white")
  r <- rgb[1, 1] / 255; g <- rgb[2, 1] / 255; b <- rgb[3, 1] / 255
  lum <- 0.2126 * r + 0.7152 * g + 0.0722 * b
  if (lum < 0.5) "white" else "black"
}

.default_class_colors <- function() {
  c(
    "0" = "#1F77B4",  # blue
    "1" = "#8B4513",  # brown
    "2" = "#228B22",  # green
    "3" = "#9467BD"   # purple (optional)
  )
}


.default_class_labels <- function() {
  c(
    "0" = "Ground",
    "1" = "Stem",
    "2" = "Crown",
    "3" = "Other"
  )
}

.as_named_color_map <- function(classes, class_colors = NULL) {
  classes <- as.character(classes)
  
  # no colors provided -> auto palette
  if (is.null(class_colors) || identical(class_colors, "auto")) {
    auto <- grDevices::hcl.colors(length(classes), palette = "Zissou 1")
    names(auto) <- classes
    return(auto)
  }
  
  # "default" handled outside OR here if you prefer
  if (identical(class_colors, "default")) {
    class_colors <- .default_class_colors()
  }
  
  # must be character
  if (!is.character(class_colors)) {
    stop("class_colors must be a character vector (colors).", call. = FALSE)
  }
  
  # ---- CASE A: user gave NAMED vector ----
  if (!is.null(names(class_colors)) && any(nzchar(names(class_colors)))) {
    out <- rep(NA_character_, length(classes))
    names(out) <- classes
    
    hit <- intersect(classes, names(class_colors))
    out[hit] <- unname(class_colors[hit])
    
    miss <- which(is.na(out))
    if (length(miss)) {
      auto <- grDevices::hcl.colors(length(miss), palette = "Zissou 1")
      out[miss] <- auto
    }
    return(out)
  }
  
  # ---- CASE B: user gave UNNAMED vector ----
  if (length(class_colors) < length(classes)) {
    stop(sprintf(
      "You provided %d colors but there are %d classes. Provide one color per class (or use a named vector).",
      length(class_colors), length(classes)
    ), call. = FALSE)
  }
  
  out <- class_colors[seq_along(classes)]
  names(out) <- classes
  out
}


.as_display_labels <- function(classes, class_labels = c("default", "none")) {
  classes <- as.character(classes)
  disp <- classes
  
  if (is.null(class_labels) || (is.character(class_labels) && length(class_labels) == 1 && class_labels == "none")) {
    return(disp)
  }
  
  if (is.character(class_labels) && length(class_labels) == 1 && class_labels == "default") {
    base <- .default_class_labels()
    hit <- intersect(classes, names(base))
    if (length(hit)) disp[match(hit, classes)] <- unname(base[hit])
    return(disp)
  }
  
  if (is.null(names(class_labels)) || any(names(class_labels) == "")) {
    stop("class_labels must be a NAMED vector like c('0'='Ground','1'='Stem').", call. = FALSE)
  }
  
  hit <- intersect(classes, names(class_labels))
  if (length(hit)) disp[match(hit, classes)] <- unname(class_labels[hit])
  disp
}

.downsample_las <- function(las,
                            downsample = c("none", "voxel", "random"),
                            voxel_size = 0.10,
                            max_points = 200000L) {
  if (is.null(downsample)) downsample <- "none"
  downsample <- match.arg(downsample)
  
  if (downsample == "none") return(las)
  
  if (downsample == "random") {
    n <- lidR::npoints(las)
    if (!is.null(max_points) && is.finite(max_points) && max_points > 0 && n > max_points) {
      idx <- sample.int(n, size = as.integer(max_points))
      return(las[idx])
    }
    return(las)
  }
  
  # voxel
  if (!is.numeric(voxel_size) || length(voxel_size) != 1L || !is.finite(voxel_size) || voxel_size <= 0) {
    stop("For downsample='voxel', voxel_size must be a single positive number (LAS units).", call. = FALSE)
  }
  
  d <- las@data
  if (!all(c("X", "Y", "Z") %in% names(d))) stop("LAS missing X/Y/Z columns.", call. = FALSE)
  
  vx <- floor(d$X / voxel_size)
  vy <- floor(d$Y / voxel_size)
  vz <- floor(d$Z / voxel_size)
  
  key  <- paste(vx, vy, vz, sep = "_")
  keep <- !duplicated(key)
  las[keep]
}

#' Plot a LAS point cloud in 3D colored by a class field
#'
#' Visualize a \code{lidR} \code{LAS} object in an interactive \code{rgl} window and
#' color points by any discrete class field stored in \code{las@data}.
#'
#' This function is designed for both *raw* and *predicted* outputs:
#' \itemize{
#'   \item \code{field = "label"}: color by original labels stored in \code{las@data$label}
#'   \item \code{field = "Classification"}: color by predicted classes stored in \code{las@data$Classification}
#' }
#'
#' A fixed in-window legend overlay is intentionally **not** implemented.
#' Instead, when \code{verbose = TRUE}, a class-to-color (and optional class-to-name)
#' mapping is printed to the R console for clarity and reproducibility.
#'
#' @param las A \code{LAS} object from \code{lidR}.
#' @param field Character. Column name in \code{las@data} used to color points
#'   (e.g., \code{"label"}, \code{"Classification"}).
#' @param bg Background color of the 3D scene (passed to \code{rgl::bg3d()}).
#'   Use a dark background (e.g., \code{"black"}) for bright colors, or a light background
#'   (e.g., \code{"white"}) for darker colors.
#' @param title Character. Title shown in the rgl window. Non-ASCII characters may be
#'   transliterated for compatibility.
#'
#' @param class_colors Controls the class-to-color mapping. Supported forms:
#' \itemize{
#'   \item \code{"default"}: FuelDeep3D default palette for classes (recommended)
#'   \item \code{"auto"} or \code{NULL}: generate a distinct palette automatically
#'   \item Named character vector: explicit mapping, e.g.
#'         \code{c("0"="#1F77B4","1"="#8B4513","2"="#228B22")}
#'   \item Unnamed character vector: assigned in the order of legend classes
#'         (i.e., \code{show_all_classes} if provided, otherwise \code{sort(unique(field))}),
#'         e.g. \code{c("blue","brown","green")}.
#' }
#' Note: color names must be valid R colors (see \code{grDevices::colors()}).
#' Some CSS names (e.g. \code{"lime"}) are not valid in base R; use hex (e.g. \code{"#00FF00"})
#' or a valid R name like \code{"limegreen"}.
#'
#' @param class_labels Controls the class-to-name mapping (used only in console output).
#' Supported forms:
#' \itemize{
#'   \item \code{"default"}: default names for classes 0..3 (printed in the console)
#'   \item \code{"none"} or \code{NULL}: do not rename classes (print raw class values)
#'   \item Named character vector: e.g. \code{c("0"="Ground","1"="Stem","2"="Crown")}
#' }
#'
#' @param show_all_classes Optional. A vector of class values (e.g., \code{c(0,1,2,3)})
#'   to force a stable class order for color assignment and console printing, even if some
#'   classes are not present in the current LAS object.
#'
#' @param downsample Downsampling mode:
#' \itemize{
#'   \item \code{"none"} (default): plot all points
#'   \item \code{"random"}: randomly sample up to \code{max_points}
#'   \item \code{"voxel"}: keep one point per voxel of size \code{voxel_size}
#' }
#'
#' @param voxel_size Numeric. Voxel size (in LAS units) used when \code{downsample = "voxel"}.
#'   Smaller values retain more points; larger values reduce density more aggressively.
#' @param max_points Integer. Maximum number of points plotted when \code{downsample = "random"}.
#'
#' @param size Numeric. Base point size passed to \code{rgl::points3d()}.
#'   Smaller values show finer detail; larger values increase thickness.
#' @param size_by_height Logical. If \code{TRUE}, point thickness increases with height (Z),
#'   implemented by binning points into layers for compatibility across platforms.
#' @param size_range Numeric length-2. Multipliers applied to \code{size} when \code{size_by_height = TRUE}.
#'   Example \code{c(0.7, 2.5)} means low points use \code{0.7*size} and high points use \code{2.5*size}.
#' @param size_power Numeric. Growth curve for thickness when \code{size_by_height = TRUE}.
#'   Values > 1 emphasize thicker points near the top; values < 1 increase thickness earlier.
#'
#' @param verbose Logical. If \code{TRUE}, prints:
#' \itemize{
#'   \item total points vs plotted points
#'   \item the class-to-color mapping
#'   \item (optional) class display names
#' }
#'
#' @param zoom,theta,phi Camera controls passed to \code{rgl::view3d()}.
#'
#' @return Invisibly returns a list with:
#' \describe{
#'   \item{mapping}{data.frame of class, name, and color used}
#'   \item{n_total}{number of points in input LAS}
#'   \item{n_plotted}{number of points actually plotted}
#'   \item{downsample}{downsampling mode used}
#'   \item{field}{field used for coloring}
#' }
#'
#' @examples
#' if (requireNamespace("lidR", quietly = TRUE) && interactive()){
#' 
#' library(lidR)
#' las <-  readLAS(system.file("extdata", "las", "tree2.laz", package = "FuelDeep3D"))
#'
#' # 1) Predicted classes (default palette; no legend overlay)
#' predicted_plot3d(
#'   las,
#'   field = "Classification",
#'   bg = "white",
#'   title = "Predicted classes"
#' )
#'
#' # 2) Raw labels
#' predicted_plot3d(
#'   las,
#'   field = "label",
#'   bg = "black",
#'   title = "Original labels"
#' )
#'
#' # 3) Named custom colors (stable mapping)
#' my_cols <- c("0"="#1F77B4", "1"="#8B4513", "2"="#228B22")
#' my_labs <- c("0"="Ground vegetation", "1"="Leaves/Foliage", "2"="Branch/Stem")
#' predicted_plot3d(
#'   las,
#'   field = "Classification",
#'   class_colors = my_cols,
#'   class_labels = my_labs,
#'   bg = "white"
#' )
#'
#' # 4) Unnamed custom colors (assigned in class order)
#' # If classes are 0,1,2 this maps 0->black, 1->red, 2->green.
#' predicted_plot3d(
#'   las,
#'   field = "Classification",
#'   show_all_classes = c(0,1,2),
#'   class_colors = c("black", "red", "#00FF00"), # hex is safest for "lime"
#'   bg = "white"
#' )
#'
#' # 5) Downsample (voxel) for huge point clouds
#' predicted_plot3d(
#'   las,
#'   field = "Classification",
#'   downsample = "voxel",
#'   voxel_size = 0.10,
#'   size = 2,
#'   bg = "white"
#' )
#'
#' # 6) Thickness by height
#' predicted_plot3d(
#'   las,
#'   field = "Classification",
#'   size = 1.2,
#'   size_by_height = TRUE,
#'   size_range = c(0.8, 2.8),
#'   size_power = 1.2
#' )
#' }
#' @export
predicted_plot3d <- function(las,
                              field = "Classification",
                              bg = "white",
                              title = "LAS 3D View",
                              class_colors = "default",
                              class_labels = "default",
                              show_all_classes = NULL,
                              downsample = c("none", "voxel", "random"),
                              voxel_size = 0.10,
                              max_points = 200000L,
                              size = 2,
                              size_by_height = FALSE,
                              size_range = c(0.7, 2.5),
                              size_power = 1.2,
                              verbose = TRUE,
                              zoom = 0.7,
                              theta = 0,
                              phi = -90) {
  
  if (!requireNamespace("lidR", quietly = TRUE)) stop("Package 'lidR' is required.", call. = FALSE)
  if (!requireNamespace("rgl", quietly = TRUE))  stop("Package 'rgl' is required.", call. = FALSE)
  
  if (is.null(class_colors)) {
    class_colors <- .default_class_colors()
  }
  
  stopifnot(inherits(las, "LAS"))
  if (lidR::is.empty(las)) return(invisible(NULL))
  
  # ---- HARD CRAN GUARD ----
  if (identical(Sys.getenv("_R_CHECK_PACKAGE_NAME_"), "FuelDeep3D")) {
    warning("Skipping rgl visualization during R CMD check.", call. = FALSE)
    return(invisible(NULL))
  }
  
  if (!field %in% names(las@data)) {
    stop(sprintf("Field '%s' not found in las@data.", field), call. = FALSE)
  }
  
  n_total <- lidR::npoints(las)
  
  downsample <- match.arg(downsample)
  lasp <- .downsample_las(las, downsample = downsample, voxel_size = voxel_size, max_points = max_points)
  n_plot <- lidR::npoints(lasp)
  
  d <- lasp@data
  x <- d$X; y <- d$Y; z <- d$Z
  cls_chr <- as.character(d[[field]])
  
  # mapping order (for printing)
  if (!is.null(show_all_classes)) {
    classes <- as.character(show_all_classes)
  } else {
    classes <- sort(unique(cls_chr))
  }
  
  cmap <- .as_named_color_map(classes, class_colors = class_colors)
  
  # ensure unexpected values still get a color
  unknown <- setdiff(unique(cls_chr), names(cmap))
  if (length(unknown)) {
    extra <- .as_named_color_map(unknown, class_colors = "auto")
    cmap <- c(cmap, extra)
  }
  
  pt_cols <- unname(cmap[cls_chr])
  
  # ---- draw scene ----
  rgl::open3d()
  rgl::bg3d(color = bg)
  rgl::title3d(.safe_ascii(title), color = .pick_text_col(bg))
  
  # points
  if (!isTRUE(size_by_height)) {
    rgl::points3d(x, y, z, col = pt_cols, size = size)
  } else {
    # binned thickness (portable)
    if (!is.numeric(size_range) || length(size_range) != 2L) {
      stop("size_range must be numeric length-2, e.g., c(0.7, 2.5).", call. = FALSE)
    }
    
    zmin <- min(z, na.rm = TRUE); zmax <- max(z, na.rm = TRUE)
    if (!is.finite(zmin) || !is.finite(zmax) || zmin == zmax) {
      rgl::points3d(x, y, z, col = pt_cols, size = size)
    } else {
      t <- (z - zmin) / (zmax - zmin)
      smin <- min(size_range) * size
      smax <- max(size_range) * size
      pwr  <- ifelse(is.finite(size_power) && size_power > 0, size_power, 1)
      
      nb <- 7L
      bins <- cut((t^pwr), breaks = nb, include.lowest = TRUE, labels = FALSE)
      for (b in seq_len(nb)) {
        idx <- which(bins == b)
        if (length(idx)) {
          frac <- (b - 1) / (nb - 1)
          sb <- smin + (smax - smin) * frac
          rgl::points3d(x[idx], y[idx], z[idx], col = pt_cols[idx], size = sb)
        }
      }
    }
  }
  
  rgl::view3d(theta = theta, phi = phi, zoom = zoom)
  
  # ---- console mapping (reliable legend alternative) ----
  disp <- .as_display_labels(classes, class_labels = class_labels)
  
  map_df <- data.frame(
    class = classes,
    name  = disp,
    color = unname(cmap[classes]),
    stringsAsFactors = FALSE
  )
  
  if (isTRUE(verbose)) {
    message(sprintf("Plotted %d / %d points (downsample = %s).", n_plot, n_total, downsample))
    message(sprintf("Colored by field: %s", field))
    print(map_df, row.names = FALSE)
  }
  
  invisible(list(
    mapping = map_df,
    n_total = n_total,
    n_plotted = n_plot,
    downsample = downsample,
    field = field
  ))
}
