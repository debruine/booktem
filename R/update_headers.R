#' Copy Rmd to qmd
#'
#' Copy Rmd files to the current and rename to .qmd
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

#' Update headers to quarto style
#'
#' @param path Path to the directory that contains your .qmd files
#'
#' @return NULL
#' @export
#'
update_headers <- function(path = "./") {
  # scan all qmd files at path
  qmd <- list.files(path, "\\.qmd$", full.names = TRUE)

  all_headers <- c()
  for (q in qmd) {
    # find all "^# title {#label}"
    txt <- suppressWarnings(readLines(q))
    header_indices <- grep("^#{1,6} .* \\{#(.*)\\}", txt)
    headers <- txt[header_indices]
    all_headers <- c(all_headers, headers)

    # replace with "^# title {#sec-label}"
    new_headers <- gsub("\\{#", "\\{#sec-", headers) |>
      gsub("\\{#(sec-){2,}", "\\{#sec-", x = _)
    txt[header_indices] <- new_headers

    # find all "](#label)" and replace with "](#sec-label)"
    txt <- gsub("](#", "](#sec-", txt, fixed = TRUE)

    writeLines(txt, q)
  }

  # find all "\@ref(label)" and replace with @sec-label
  labels <- all_headers |>
    gsub("^.* \\{#", "", x = _) |>
    gsub("\\}.*", "", x = _) |>
    unique()

  refs <- glue::glue("\\@ref({labels})")
  replace <- glue::glue("@sec-{labels}") |>
    gsub("^@(sec-){2,}", "@sec-", x = _)

  for (q in qmd) {
    # find all "^# title {#label}"
    txt <- suppressWarnings(readLines(q))
    for (i in seq_along(refs)) {
      txt <- gsub(refs[i], replace[i], txt, fixed = TRUE)
    }
    writeLines(txt, q)
  }
}

#' Update figures to quarto style
#'
#' @param path Path to the directory that contains your .qmd files
#'
#' @return NULL
#' @export
#'
# update_figs <- function(path = "./") {
#   # find all "^(ref:label) reference text"
#   # move reference text to matching fig.cap
#
#   # find all "\@ref(fig:label)
#   # replace with @fig-label
#   # find all "```{r, fig:label"
#   # replace with "```{r, fig-label"
# }
