#' Create a Template Book
#'
#' @param path path to the location for your book on your machine
#' @param title title of the book
#' @param author author of the book
#' @param description short description of the book
#' @param license what license to add (CC-BY only for now)
#' @param google_analytics the google analytics address
#' @param twitter the twitter adddress
#' @param repo_url the github repo URL (e.g., "myusername/mybook")
#' @param repo_branch the branch to use (usually "main" or "master")
#' @param repo_actions github repo links to add to the right sidebar
#' @param downloads download types to include
#' @param sharing "twitter, facebook, linkedin"
#' @param footer defaults to "license YEAR, author"
#' @param light_theme "flatly"
#' @param dark_theme "darkly"
#' @param stripe_css created by stripe() function
#'
#' @return sets up a project and renders the demo
#' @export
#'
create_book <- function(path = "book",
                        title = "Book Template",
                        author = "Me",
                        description = "",
                        license = "CC-BY",
                        google_analytics = "",
                        twitter = "",
                        repo_url = "",
                        repo_branch = "main",
                        repo_actions = "edit, issue, source",
                        downloads = "pdf, epub",
                        sharing = "twitter, facebook, linkedin",
                        footer = paste(license, format(Sys.Date(), "%Y"), ",", author),
                        light_theme = "flatly",
                        dark_theme = "darkly",
                        stripe_css = stripes()) {
  usethis::create_project(path, FALSE, FALSE)

  # create _quarto.yml
  file <- system.file("quarto", "_quarto.yml", package = "booktem")
  template <- paste(readLines(file), collapse = "\n")
  quarto_yml <- glue::glue(template)
  write(quarto_yml, file.path(path, "_quarto.yml"))

  # add license
  if (license == "CC-BY") {
    xfun::in_dir(path, usethis::use_ccby_license())
  }

  # copy files
  index <- system.file("quarto", "index.qmd", package = "booktem")
  file.copy(index, file.path(path))
  refs <- system.file("quarto", "references.qmd", package = "booktem")
  file.copy(refs, file.path(path))

  # includes
  include <- system.file("quarto/include", package = "booktem")
  file.copy(include, file.path(path), recursive = TRUE)
  write(stripe_css, file.path(path, "include", "style.css"))

  # render book
  xfun::in_dir(path, quarto::quarto_render(as_job = FALSE))
  browseURL(file.path(path, "docs", "index.html"))
}
