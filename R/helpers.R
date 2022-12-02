#' @import ggplot2
return_color_manual_geom = function(p, color=NULL) {

  #return empty geom if they don't provide a color or if the plot doesn't have color
  if (is.null(color) | !("colour" %in% names(p$mapping))) return(ggplot2::geom_blank())

  # make sure they have a vector the same length as data
  variable_name = quo_name(p$mapping$colour)
  levels_of_group = levels(p$data[[variable_name]])

  if (length(color) != length(levels_of_group)) {
    stop(paste0("Your grouping variable (", variable_name, ") contains ", length(levels_of_group), " levels.
                Your color vector needs to be the same length"))
  }

  return(scale_color_manual(values=color))
}


remove_geom <- function(ggplot2_object, geom_type) {
  # Delete layers that match the requested type.

  layers <- lapply(ggplot2_object$layers, function(x) {
    if (class(x$geom)[1] == geom_type) NULL else  x
  })

  # Delete the unwanted layers.
  layers <- layers[!sapply(layers, is.null)]
  ggplot2_object$layers <- layers
  ggplot2_object
}

#' @import ggplot2
smooth_method_check = function(method=NULL) {
  if (is.null(method)) return ('geom_smooth(method="loess", se=se, formula = y~x)')
  if (method=="rlm") return('geom_smooth(method = "rlm", se = se, formula = y~x)')
  if (method=="poisson") return('geom_smooth(method = "glm", method.args = list(family = "poisson"), se = se, formula = y~x)')
  if (method=="Gamma") return('geom_smooth(method = "glm", method.args = list(family = "Gamma"), se = se, formula = y~x)')
  if (method=="polynomial" | method == "quadratic") return('stat_smooth(method="lm", se=se, formula=y ~ poly(x, 2, raw=TRUE))')
  if (method=="cubic") return('stat_smooth(method="lm", se=se, formula=y ~ poly(x, 3, raw=TRUE))')
  if (method=="lm") return('stat_smooth(method="lm", se=se, formula = y~x)')
}
