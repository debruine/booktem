
<!-- README.md is generated from README.Rmd. Please edit that file -->

# booktem

<!-- badges: start -->
<!-- badges: end -->

The goal of booktem is to make the creation of methods books quick and
easy.

## Installation

You can install the development version of booktem like so:

``` r
devtools::install_github("debruine/booktem")
```

## Example

Set up a book template:

``` r
library(booktem)

create_book(path = "demobook", 
            title = "My Demo Book",
            author = "My Name")
```

Set up a custom red, white and blue stripe on the top of the book.

``` r
stripe_colors <- c(red = "red",
                   white = "white",
                   blue = "blue")
stripe_css <- stripes(stripe_colors, height = 10)

create_book(path = "demobook", 
            title = "My Demo Book",
            author = "My Name",
            stripe_css = stripe_css)
```
