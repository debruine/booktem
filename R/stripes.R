#' Colour Stripes CSS
#'
#' Create the CSS for a striped border on each book page
#'
#' @param colors a named vector of the colours, the names are the alias and the values are the colour (in any CSS format, such as hex)
#' @param height the height of each stripe in pixels
#'
#' @return a character string of the CSS
#' @export
#'
#' @examples
#' # default PsyTeachR colours
#' stripes()
#'
#' # red, yellow and blue thick stripes
#' stripes(c(red = "red", yellow = "yellow", blue = "blue"), 10)
stripes <- function(colors = c(pink   = "#983E82",
                               orange = "#E2A458",
                               yellow = "#F5DC70",
                               green  = "#59935B",
                               blue   = "#467AAC",
                               purple = "#61589C"),
                    height = 3) {

  clist <- glue::glue("--{name}: {color};",
             name = names(colors),
             color = colors) |>
    paste(collapse = "\n  ")

  blist <- glue::glue("0 -{h}px 0 0px var(--{color})",
                      h = height * 1:length(colors),
                      color = rev(names(colors))) |>
    paste(collapse = ",\n    ")

  css <- glue::glue(.open = "[", .close = "]",
"/* (edit style.css for your own book styles) */

/* named colours */
:root {\n  [clist]\n}

/* rainbow borders */
body {
  margin-top: [length(colors) * height]px;
  box-shadow:\n    [blist];
}")

  return(css)
}
