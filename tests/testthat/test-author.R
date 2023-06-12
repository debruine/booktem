test_that("author", {
  test <- author(
    family = "DeBruine",
    given = "Lisa M.",
    orcid = "0000-0002-7523-5539",
    roles = c("Conceptualization", "Methodology")
  )

  expect_equal(test$name$family, "DeBruine")
  expect_equal(test$name$given, "Lisa M.")
  expect_equal(test$orcid, "0000-0002-7523-5539")
  expect_equal(test$roles, c("Conceptualization", "Methodology"))
})

test_that("author-abbrev-trim-extras", {
  test <- author(
    family = "  DeBruine",
    given = "Lisa M.  ",
    orcid = "https://orcid.org/0000-0002-7523-5539",
    roles = c("Con", "Met"),
    email = " debruine@gmail.com"
  )

  expect_equal(test$name$family, "DeBruine")
  expect_equal(test$name$given, "Lisa M.")
  expect_equal(test$orcid, "0000-0002-7523-5539")
  expect_equal(test$email, "debruine@gmail.com")
  expect_equal(test$roles, c("Conceptualization", "Methodology"))
})

test_that("extra names", {
  test <- author(
    given = "X",
    literal = "X van der Zon",
    family = "Zon",
    `dropping-particle` = "van",
    `non-dropping-particle` = "der"
  )

  expect_equal(test$name$family, "Zon")
  expect_equal(test$name$given, "X")
  expect_equal(test$name$literal, "X van der Zon")
  expect_equal(test$name$`dropping-particle`, "van")
  expect_equal(test$name$`non-dropping-particle`, "der")
})


test_that("comma_and", {
  expect_equal(comma_and(LETTERS[1:3]), "A, B & C")
  expect_equal(comma_and(LETTERS[1:3], and = " and "), "A, B and C")
  expect_equal(comma_and(LETTERS[1:3], comma = "; "), "A; B & C")

  expect_equal(comma_and("A"), "A")
  expect_equal(comma_and(c("A", "B")), "A & B")
  expect_equal(comma_and(list("A", "B")), "A & B")

  expect_equal(comma_and(c("A", "A", "A")), "A, A & A")
  expect_equal(comma_and(c("A", "A", "A"), oxford = TRUE),
               "A, A, & A")
})
