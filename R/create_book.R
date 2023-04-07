#' Create a Template Book
#'
#' Set up a book by creating a project at the specified path, rendering the demo book in Quarto, opening the book in a web browser, and opening the project in a new RStudio window.
#'
#' The argument `socials` adds linked icons to the right footer. See the available icons at <https://icons.getbootstrap.com/> E.g.:
#'
#' ```
#' list(mastodon = "https://tech.lgbt/@debruine",
#'      github = "https://github.com/debruine",
#'      twitter = "https://twitter.com/lisadebruine")
#' ````
#'
#' @param path path to the location for your book on your machine
#' @param title title of the book
#' @param subtitle subtitle of the book
#' @param author author of the book
#' @param description short description of the book
#' @param output_dir directory to render output to
#' @param license what license to add (CC-BY only for now)
#' @param google_analytics the google analytics address
#' @param socials a list of social media URLs to put in the footer, named as a relevant icon (see Details)
#' @param repo_url the github repo URL (e.g., "myusername/mybook")
#' @param repo_branch the branch to use (usually "main" or "master")
#' @param repo_actions github repo links to add to the right sidebar
#' @param downloads download types to include
#' @param sharing "twitter, facebook, linkedin"
#' @param margin_header defaults to ""
#' @param footer defaults to "license YEAR, author"
#' @param light_theme "flatly" (see https://quarto.org/docs/output-formats/html-themes.html)
#' @param dark_theme "darkly"
#' @param css custom styles (e.g., `stripes()` creates the signature PsyTeachR rainbow stripes)
#' @param df_print how to print tables (default, kable, tibble, paged)
#' @param webexercises whether to use webexercises for interactive exercises
#' @param open whether to activate the new project in RStudio
#' @param render whether to render the quarto book when opening
#'
#' @return sets up a project and renders the demo
#' @export
#' @importFrom rlang is_interactive
#'
create_book <- function(path = "book",
                        title = "Book Template",
                        subtitle = "",
                        author = "Me",
                        description = "Book Description",
                        output_dir = "docs",
                        license = "CC-BY",
                        google_analytics = "",
                        socials = list(),
                        repo_url = "",
                        repo_branch = "main",
                        repo_actions = "edit, issue, source",
                        downloads = "pdf, epub",
                        sharing = "twitter, facebook, linkedin",
                        margin_header = "",
                        footer = paste(license, format(Sys.Date(), "%Y"), ",", author),
                        light_theme = "flatly",
                        dark_theme = "darkly",
                        css = stripes(),
                        df_print = "kable",
                        webexercises = TRUE,
                        open = rlang::is_interactive(),
                        render = TRUE) {
  # checks ----
  requireNamespace("knitr")
  # prompt quarto install if not available
  if (!nzchar(Sys.which("quarto"))) {
    stop("Quarto isn't installed, see https://quarto.org/docs/get-started/")
  }
  requireNamespace("quarto")
  if (webexercises) requireNamespace("webexercises")

  # create project -----
  usethis::ui_todo("Setting up project...")
  usethis::create_project(path = path, open = FALSE)

  # add content ----
  usethis::ui_todo("Adding content...")

  ## create _quarto.yml ----
  social_links <- glue::glue("\n      - icon: {names(socials)}\n        href: {socials}", .trim = FALSE) |>
    paste(collapse = "")

  file <- system.file("quarto", "_quarto.yml", package = "booktem")
  template <- paste(readLines(file), collapse = "\n")
  quarto_yml <- glue::glue(template)
  write(quarto_yml, file.path(path, "_quarto.yml"))
  usethis::ui_done("Modified _quarto.yml")

  ## add license ----
  if (license == "CC-BY") {
    license_op <- utils::capture.output({
      xfun::in_dir(path, usethis::use_ccby_license())
    }, type = "message")
    usethis::ui_done("Added license")
  }

  ## copy qmd files ----
  qmd <- list.files(system.file("quarto", package = "booktem"),
                    "\\.qmd$",
                    full.names = TRUE)
  sapply(qmd, file.copy, path)
  usethis::ui_done("Added demo files")

  ## includes and R directories ----
  include <- system.file("quarto/include", package = "booktem")
  file.copy(include, file.path(path), recursive = TRUE)
  write(css, file.path(path, "include", "style.css"), append = TRUE)
  rfiles <- system.file("quarto/R", package = "booktem")
  file.copy(rfiles, file.path(path), recursive = TRUE)
  images <- system.file("quarto/images", package = "booktem")
  file.copy(images, file.path(path), recursive = TRUE)

  # .Rprofile ----
  rprofpath <- file.path(path, ".Rprofile")
  write("source(\"R/booktem_setup.R\")", file = rprofpath, append = TRUE)
  if (df_print == "dt") {
    write("source(\"R/dt_tables.R\")", file = rprofpath, append = TRUE)
  }
  if (webexercises) {
    write("source(\"R/webex.R\")", file = rprofpath, append = TRUE)
  } else {
    unlink(file.path(path, "webexercises.qmd"))
  }
  write("source(\"R/my_setup.R\")", file = rprofpath, append = TRUE)
  usethis::ui_done("Added auxillary files")

  # open project in RStudio ----
  if (open) usethis::proj_activate(path)

  # render book ----
  if (render) {
    usethis::ui_todo("Rendering book...")
    render_op <- tryCatch({
      utils::capture.output({
        xfun::in_dir(path, quarto::quarto_render(as_job = FALSE))
      })
    },
    error = function(e) {
      warning(e, call. = FALSE)
      return("")
    })

    ## check for success and show book ----
    if (!any(grepl("Output created: docs/index.html", render_op, fixed = TRUE))) {
      warning(render_op)
    } else {
      bookpath <- file.path(path, "docs", "index.html")
      usethis::ui_done("Book rendered at {normalizePath(bookpath)}")
      utils::browseURL(bookpath)
    }
  }

  if (!render & !open) {
    usethis::ui_done("Book created at {normalizePath(path)}")
  }

  invisible(path)
}
