library("grid")

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

"%|W|%" <- function(a, b) {
  if (!is.waive(a)) a else b
}

is.waive <- function(x) inherits(x, "waiver")

# Compute central angle between two points.
# Multiple by radius of sphere to get great circle distance
# @arguments longitude
# @arguments latitude
dist_central_angle <- function(lon, lat) {
  # Convert to radians
  lat <- lat * pi / 180
  lon <- lon * pi / 180

  hav <- function(x) sin(x / 2) ^ 2
  ahav <- function(x) 2 * asin(x)

  n <- length(lat)
  ahav(sqrt(hav(diff(lat)) + cos(lat[-n]) * cos(lat[-1]) * hav(diff(lon))))
}


expand_default <- function(scale, discrete = c(0, 0.6), continuous = c(0.05, 0)) {
  scale$expand %|W|% if (scale$is_discrete()) discrete else continuous
}


# Col union
# Form the union of columns in a and b.  If there are columns of the same name in both a and b, take the column from a.
#
# @param data frame a
# @param data frame b
# @keyword internal
cunion <- function(a, b) {
  if (length(a) == 0) return(b)
  if (length(b) == 0) return(a)

  cbind(a, b[setdiff(names(b), names(a))])
}

# Given a theme object and element name, return a grob for the element
element_render <- function(theme, element, ..., name = NULL) {

  # Get the element from the theme, calculating inheritance
  el <- calc_element(element, theme)
  if (is.null(el)) {
    message("Theme element ", element, " missing")
    return(zeroGrob())
  }

  ggname(paste(element, name, sep = "."), element_grob(el, ...))
}


# Name ggplot grid object
# Convenience function to name grid objects
#
# @keyword internal
ggname <- function(prefix, grob) {
  grob$name <- grobName(grob, prefix)
  grob
}

# Convert a snake_case string to camelCase
camelize <- function(x, first = FALSE) {
  x <- gsub("_(.)", "\\U\\1", x, perl = TRUE)
  if (first) x <- firstUpper(x)
  x
}

snakeize <- function(x) {
  x <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
  x <- gsub(".", "_", x, fixed = TRUE)
  x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
  tolower(x)
}

firstUpper <- function(s) {
  paste(toupper(substring(s, 1,1)), substring(s, 2), sep = "")
}

snake_class <- function(x) {
  snakeize(class(x)[1])
}


# Look for object first in parent environment and if not found, then in
# ggplot2 namespace environment.  This makes it possible to override default
# scales by setting them in the parent environment.
find_global <- function(name, env, mode = "any") {
  if (exists(name, envir = env, mode = mode)) {
    return(get(name, envir = env, mode = mode))
  }

  nsenv <- asNamespace("ggalt")
  if (exists(name, envir = nsenv, mode = mode)) {
    return(get(name, envir = nsenv, mode = mode))
  }

  NULL
}

find_subclass <- function (super, class, env) {
  name <- paste0(super, camelize(class, first = TRUE))
  obj <- find_global(name, env = env)
  if (is.null(name)) {
    stop("No ", tolower(super), " called ", name, ".", call. = FALSE)
  }
  else if (!inherits(obj, super)) {
    stop("Found object is not a ", tolower(super), ".", call. = FALSE)
  }
  obj
}

alt_aesthetics <- function(type, name) {
  obj <- switch(type,
                geom = find_subclass("Geom", name, globalenv()),
                stat = find_subclass("Stat", name, globalenv())
  )
  aes <- alt_aesthetics_item(obj)

  paste("\\code{", type, "_", name, "} ",
        "understands the following aesthetics (required aesthetics are in bold):\n\n",
        "\\itemize{\n",
        paste("  \\item \\code{", aes, "}", collapse = "\n", sep = ""),
        "\n}\n", sep = "")
}

alt_aesthetics_item <- function(x) {
  req <- x$required_aes
  all <- union(req, sort(x$aesthetics()))

  ifelse(all %in% req,
         paste0("\\strong{", all, "}"),
         all
  )
}

#' Lollipop charts
#'
#' The lollipop geom is used to create lollipop charts.
#'
#' Lollipop charts are the creation of Andy Cotgreave going back to 2011. They
#' are a combination of a thin segment, starting at with a dot at the top and are a
#' suitable alternative to or replacement for bar charts.
#'
#' Use the \code{horizontal} parameter to abate the need for \code{coord_flip()}
#' (see the \code{Arguments} section for details).
#'
#' \if{html}{
#' A sample of the output from \code{geom_lollipop()}:
#'
#' \figure{geomlollipop01.png}{options: width="100\%" alt="Figure: geomlollipop01.png"}
#' }
#'
#' \if{latex}{
#' A sample of the output from \code{geom_lollipop()}:
#'
#' \figure{geomlollipop01.png}{options: width=10cm}
#' }
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "point")}
#' @inheritParams ggplot2::layer
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning.  If \code{TRUE} silently removes missing values.
#' @param ... other arguments passed on to \code{\link{layer}}. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   \code{color = "red"} or \code{size = 3}. They may also be parameters
#'   to the paired geom/stat.
#' @param horizontal \code{horizontal} is \code{FALSE} (the default), the function
#'   will draw the lollipops up from the X axis (i.e. it will set \code{xend}
#'   to \code{x} & \code{yend} to \code{0}). If \code{TRUE}, it wiill set
#'   \code{yend} to \code{y} & \code{xend} to \code{0}). Make sure you map the
#'   \code{x} & \code{y} aesthetics accordingly. This parameter helps avoid
#'   the need for \code{coord_flip()}.
#' @param point.size the size of the point
#' @param point.colour the colour of the point
#' @inheritParams ggplot2::layer
#' @export
#' @examples
#' df <- data.frame(trt=LETTERS[1:10],
#'                  value=seq(100, 10, by=-10))
#'
#' ggplot(df, aes(trt, value)) + geom_lollipop()
#'
#' ggplot(df, aes(value, trt)) + geom_lollipop(horizontal=TRUE)
geom_lollipop <- function(mapping = NULL, data = NULL, ...,
                          horizontal = FALSE,
                          point.colour = NULL, point.size = NULL,
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = "identity",
    geom = GeomLollipop,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      horizontal = horizontal,
      point.colour = point.colour,
      point.size = point.size,
      ...
    )
  )
}

#' @rdname ggalt-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomLollipop <- ggproto("GeomLollipop", Geom,
  required_aes = c("x", "y"),
  non_missing_aes = c("size", "shape", "point.colour", "point.size", "horizontal"),
  default_aes = aes(
    shape = 19, colour = "black", size = 0.5, fill = NA,
    alpha = NA, stroke = 0.5
  ),

  setup_data = function(data, params) {
    if (params$horizontal) {
      transform(data, yend = y, xend = 0)
    } else {
      transform(data, xend = x, yend = 0)
    }
  },

  draw_group = function(data, panel_scales, coord,
                        point.colour = NULL, point.size = NULL,
                        horizontal = FALSE) {

    points <- data
    points$colour <- point.colour %||% data$colour
    points$size <- point.size %||% (data$size * 2.5)

    gList(
      ggplot2::GeomSegment$draw_panel(data, panel_scales, coord),
      ggplot2::GeomPoint$draw_panel(points, panel_scales, coord)
    )

  },

  draw_key = draw_key_point
)