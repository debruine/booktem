#' Copy Rmd to qmd
#'
#' Copy Rmd files and rename to .qmd
#'
#' @param from_path the source directory where your Rmd files are
#' @param to_path the destination directory where you want the qmd files
#'
#' @return the path to the destination directory
#' @export
#'
rmd2qmd <- function(from_path = "./", to_path = "./") {
  from_path <- normalizePath(from_path)
  to_path <- normalizePath(to_path)

  # find all rmd files at from_path
  files <- list.files(from_path, pattern = "\\.rmd",
                      ignore.case = TRUE,
                      full.names = TRUE)
  # copy to to_path as .qmd
  sink <- sapply(files, function(file) {
    new_name <- gsub("\\.rmd", "\\.qmd", file, ignore.case = TRUE)
    new_path <- gsub(from_path, to_path, new_name)
    file.copy(file, new_path)
  })

  invisible(to_path)
}
