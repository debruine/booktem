path <- tempdir()
setwd(path)
unlink("book", recursive = TRUE) # clean up

test_that("norender", {
  op <- capture_output(bookpath <- create_book(render = FALSE, open = FALSE))
  expect_equal(bookpath, "book")

  # check for basic files
  files <- list.files("book")
  expected <- c("_quarto.yml", "include", "index.qmd",
                "LICENSE.md", "R", "references.qmd")
  expect_true(all(expected %in% files))

  expect_false("docs" %in% files)

  # check _quarto.yml for replaced lines
  qyml <- readLines("book/_quarto.yml")
  expect_true(any(grepl("  output-dir: docs", qyml)))
  expect_true(any(grepl("  title: \"Book Template\"", qyml)))
  expect_true(any(grepl("  author: \"Me\"", qyml)))

  unlink("book", recursive = TRUE) # clean up
})

test_that("options", {
  args <- list(title = "My New Title",
               subtitle = "My New Subtitle",
               description = "My new description",
               author = "Lisa",
               output_dir = "./",
               webexercises = FALSE,
               render = FALSE, open = FALSE)
  bookpath <- do.call(create_book, args)

  sources <- readLines("book/.Rprofile")
  has_webex <- any(grepl("webex.R", sources, fixed = TRUE))
  expect_false(has_webex)

  # check _quarto.yml for replaced lines
  qyml <- readLines("book/_quarto.yml")
  expect_true(any(grepl(glue::glue("  output-dir: {args$output_dir}"), qyml)))
  expect_true(any(grepl(glue::glue("  title: \"{args$title}\""), qyml)))
  expect_true(any(grepl(glue::glue("  subtitle: \"{args$subtitle}\""), qyml)))
  expect_true(any(grepl(glue::glue("  description: \"{args$description}\""), qyml)))
  expect_true(any(grepl(glue::glue("  author: \"{args$author}\""), qyml)))

  unlink("book", recursive = TRUE) # clean up
})


test_that("render", {
  skip("Needs interactivity")

  op <- capture_output(bookpath <- create_book(open = TRUE))
  expect_equal(bookpath, "book")

  # check for basic files
  files <- list.files("book")
  expected <- c("_quarto.yml", "docs", "include", "index.qmd",
                "LICENSE.md", "R", "references.qmd")
  expect_true(all(expected %in% files))

  # check book rendered
  bookfiles <- list.files("book/docs")
  expected <- c("include", "index.html", "references.html",
                "search.json", "site_libs")
  expect_true(all(expected %in% bookfiles))

  unlink("book", recursive = TRUE) # clean up
})


