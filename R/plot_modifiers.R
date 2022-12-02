#' Modify the points in a flexplot (or ggplot) graphic
#'
#' @param p The flexplot (or ggplot) object
#' @param shape The new shape desired. Defaults to 19 (circles).
#' @param colour The new colour desired. Defaults to "black".
#' @param size The new size of the points desired. Defaults to 1.5.
#'
#' @return A new plot with the modifications
#' @export
#' @importFrom graphics plot
modify_points = function(p, shape=19, colour="black", size=1.5) {
  c = ggplot2::ggplot_build(p)

  # make modifications
  c$data[[1]]$size = size
  c$data[[1]]$colour = colour
  c$data[[1]]$shape = shape
  c = ggplot2::ggplot_gtable(c)

  # return plot
  library(grid)
  grid.newpage()
  grid.draw(c)
}

#' Modify the labels in a flexplot (or ggplot) graphic
#'
#' @param p The flexplot (or ggplot) object
#' @param y The new label for the variable on the y axis
#' @param x The new label for the variable on the x axis
#' @param colour The new label for the variable in the legend.
#'  (This will also modify the underlying shape/line arguments)
#' @param row_panels The new labels for the variable in row panels
#' @param col_panels The new labels for the variable in column panels
#'
#' @return a flexplot graphic
#' @export
#' @import flexplot magrittr
#'
#' @examples
#' a = flexplot(ptsd~agility + superpower | speed, data=avengers)
#' a %>% modify_labels(y="PTSD", x="Agility", color="Superpower", col_panels = "Speed")
modify_labels = function(p, y=NULL, x=NULL, color=NULL, row_panels=NULL, col_panels=NULL) {

  if (!is.null(y)) p$labels$y = y
  if (!is.null(x)) p$labels$x = x
  if (!is.null(color)) {
    p$labels$colour = color
    p$labels$linetype = color
    p$labels$shape = color
  }

  ## if there's panels
  # check if there's actually row panels
  if (!is.null(row_panels)) {
    if (length(names(p$facet$params$rows))!=0) names(p$facet$params$rows) = row_panels
  }
  if (!is.null(col_panels)) {
    if (length(names(p$facet$params$cols))!=0)  names(p$facet$params$cols) = col_panels
  }
  return(p)
}

#' Modify the fitted line for a flexplot graphic
#'
#' @param p A flexplot graphic
#' @param method Type of smoothing (fitted) funciton used. Can be
#' rlm, poisson, loess, Gamma, polynomial, cubic, or lm
#' @param se Should standard errors (confidence bands) be displayed
#'
#' @return a flexplot graphic
#' @export
#' @import ggplot2
#'
#' @examples
#' a = flexplot(ptsd~agility + superpower , data=avengers)
#' a %>% modify_smooth(method="lm", color=c("blue", "brown"))
modify_smooth = function(p, method="lm", se=F, color=NULL) {

  # delete existing smoothing layers
  p = remove_geom(p, "GeomAbline")
  p = remove_geom(p, "GeomSmooth")

  # add geom for color
  added_geom = return_color_manual_geom(p, color)

  # convert method into something ggplot can understand
  method_call = smooth_method_check(method)

  # change the color
  if (!is.null(color) & length(color) == 1) method_call = gsub("),", ",", paste0(method_call, ", colour = '", color, "')"), fixed=T)
  p + suppressMessages(eval(parse(text=method_call))) + added_geom
}
