path <- tempdir()
setwd(path)
unlink("book", recursive = TRUE) # clean up

test_that("basic", {
  op <- capture_output(bookpath <- create_book(open = FALSE))
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

  # check _quarto.yml for replaced lines
  qyml <- readLines("book/_quarto.yml")
  expect_true(any(grepl("  output-dir: docs", qyml)))
  expect_true(any(grepl("  title: \"Book Template\"", qyml)))
  expect_true(any(grepl("  author: \"Me\"", qyml)))

  unlink("book", recursive = TRUE) # clean up
})
