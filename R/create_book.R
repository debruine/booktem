#' Create a Template Book
#'
#' Set up a book by creating a project at the specified path, rendering the demo book in Quarto, opening the book in a web browser, and opening the project in a new RStudio window.
#'
#' The argument `authors` can be text or a structured list, e.g.:
#'
#' ```
#' list(c("Lisa M.", "DeBruine", "0000-0002-7523-5539"),
#'      c("Daniel", "Lakens", "0000-0002-0247-239X"))
#' ```
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
#' @param authors authors of the book (see Details)
#' @param description short description of the book
#' @param output_dir directory to render output to
#' @param license what license to add (CC-BY only for now)
#' @param google_analytics the google analytics address
#' @param socials a list of social media URLs to put in the footer, named as a relevant icon (see Details)
#' @param downloads download types to include
#' @param sharing "twitter, facebook, linkedin"
#' @param margin_header defaults to ""
#' @param footer defaults to "license (YEAR) author"
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
                        authors = "Me",
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
                        footer = paste0(license, " (",
                                        format(Sys.Date(), "%Y"),
                                        ") ", author),
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
  version <- quarto::quarto_version()
  if (version < "1.3.56") {
    message("You might want to update quarto to version 1.3.56 or later to avoid some bugs with the bibliography files, see https://quarto.org/docs/get-started/")
  }

  requireNamespace("glossary")
  if (webexercises) requireNamespace("webexercises")

  # create project -----
  usethis::ui_todo("Setting up project...")
  usethis::create_project(path = path, open = FALSE)

  # add content ----
  usethis::ui_todo("Adding content...")

  # sort out authors ----
  alist <- list()
  if (is.list(authors)) {
    # authors as list, e.g.,
    # authors <- list(c("Lisa", "DeBruine"), c("Daniel", "Lakens"))
    alist <- lapply(authors, function(a) {
      do.call(author, as.list(a))
    })
    auth_txt_list <- sapply(alist, function(a) {
      trimws(paste(a$name$given, a$name$family))
    })
    auth_txt <- comma_and(auth_txt_list)

  } else if (length(authors) > 1) {
    # single author as vector, e.g.,
    # authors <- c("Lisa", "DeBruine")
    alist <- do.call(author, as.list(authors))
    auth_txt <- paste(alist$name$given, alist$name$family)
  } else {
    # authors as text, e.g.,
    # authors <- "Lisa DeBruine & Daniel Lakens"
    alist <- authors
    auth_txt <- authors
  }

  ## create _quarto.yml ----
  file <- system.file("quarto", "_quarto.yml", package = "booktem")
  yml <- yaml::read_yaml(file)

  yml$project$`output-dir` = output_dir

  yml$book$title = title
  yml$book$subtitle = subtitle
  yml$book$author = alist
  yml$book$decription = decription
  yml$book$license = license
  yml$book$`google-analytics` = google_analytics
  yml$book$downloads = downloads
  yml$book$sharing = sharing
  yml$book$`page-footer` = footer
  yml$book$`margin-header` = margin_header

  yml$format$html$`df-print` <- df_print
  yml$format$html$theme$light[[1]] <- light_theme
  yml$format$html$theme$dark[[1]] <- dark_theme

  if (length(socials) > 0) {
    social_links <- lapply(names(socials), function(icon) {
      list(icon = icon, href = socials[[icon]])
    })
    yml$book$`page-footer`$right <- social_links
  }

  write_yaml(quarto_yml, file.path(path, "_quarto.yml"))
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
  gstyle <- utils::capture.output(glossary::glossary_style())
  gstyle <- gstyle[2:(length(gstyle)-1)]
  write(gstyle, file.path(path, "include", "glossary.css"))
  write(css, file.path(path, "include", "style.css"), append = TRUE)
  rfiles <- system.file("quarto/R", package = "booktem")
  file.copy(rfiles, path, recursive = TRUE)
  images <- system.file("quarto/images", package = "booktem")
  file.copy(images, path, recursive = TRUE)

  # .Rprofile ----
  rprofpath <- file.path(path, ".Rprofile")
  write("source(\"R/booktem_setup.R\")", file = rprofpath, append = TRUE)
  if (df_print == "dt") {
    write("source(\"R/dt_tables.R\")", file = rprofpath, append = TRUE)
  }
  if (webexercises) {
    suppressMessages(
    webexercises::add_to_quarto(quarto_dir = path,
                                include_dir = file.path(path, "include"))
    )
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


#' List to Text
#'
#' Convert a list or vector to text with human-readable separators, e.g., "A, B & C".
#'
#' @param x The list or vector to convert
#' @param comma The text to use to separate all but the last item
#' @param and The text to use to separate the last item
#' @param oxford Whether to use an oxford comma before the last item
#'
#' @return A character string
#' @export
#'
#' @examples
#' comma_and(LETTERS[1:5])
#' comma_and(LETTERS[1:5], and = " and ")
#' comma_and(LETTERS[1:5], comma = "; ")
#'
#' # change and to use an oxford comma
#' my_list <- list("Nelson Mandela",
#'                 "an 800-year-old demigod",
#'                 "a dildo collector")
#' comma_and(my_list) # probably not what you mean
#' comma_and(my_list, oxford = TRUE)
comma_and <- function(x, comma = ", ", and = " & ", oxford = FALSE) {
  if (length(x) == 1) {
    txt <- x
  } else {
    last <- x[length(x)]
    first <- paste(x[1:(length(x)-1)], collapse = comma)
    if (oxford) and <- gsub(" +", " ", paste0(comma, and))
    txt <- paste0(first, and, last)
  }

  return(txt)
}


#' Write yaml with fixed logical values
#'
#' @param x the object to be converted
#' @param file either a character string naming a file or a connection open for writing
write_yaml <- function(x, file) {
  yaml::write_yaml(x, file, handlers = list(
    logical = function(x) {
      result <- ifelse(x, "true", "false")
      class(result) <- "verbatim"
      return(result)
    }
  ))
}
