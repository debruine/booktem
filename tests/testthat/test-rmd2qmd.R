test_that("rmd2qmd", {
  # set up
  d <- tempdir()
  rmd <- file.path(d, "test.Rmd")
  qmd <- file.path(d, "test.qmd")
  write("# Title {#title}\n\n[My reference](#title)\n\nOther reference \\@ref(title)\n", rmd)

  # rmd2qmd test
  rmd2qmd(d, d)
  expect_true(file.exists(qmd))

  # header test
  update_headers(d)
  txt <- readLines(qmd)

  # clean up
  unlink(rmd)
  unlink(qmd)
})
