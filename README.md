
<!-- README.md is generated from README.Rmd. Please edit that file -->

# booktem

<!-- badges: start -->
<!-- badges: end -->

The goal of booktem is to make the creation of methods books quick and
easy. It uses a custom version of
[webexercises](https://psyteachr.github.io/webexercises/) built to work
with quarto (I’ll update that package to match soon).

## Installation

You can install the development version of booktem like so:

``` r
devtools::install_github("debruine/booktem")
```

## Example

Set up a book; this function creates a project at the specified path,
renders the demo book in Quarto, opens the book in a web browser, and
opens the project in a new RStudio window. The demo book gives examples
of the features included in the book.

``` r
library(booktem)

create_book(path = "demobook", 
            title = "My Demo Book",
            author = "My Name")
```

Set up a custom red, white and blue stripe on the top of the book
(instead of the psyTeachR rainbow stripe).

``` r
stripe_colors <- c(red = "red",
                   white = "white",
                   blue = "blue")
stripe_css <- stripes(stripe_colors, height = 10)

create_book(path = "demobook", 
            title = "My Demo Book",
            author = "My Name",
            css = stripe_css)
```

## Glossary

<style>
a.glossary, a:visited.glossary {
  color: #61589C !important;
}
</style>

Books are set up with some lightweight glossary functions. Edit the file
`include/glossary.yml` with your glossary terms like this:

    alpha: |
      (stats) The cutoff value for making a decision to reject the null hypothesis; (graphics) A value between 0 and 1 used to control the levels of transparency in a plot
    p-value: |
      The probability of seeing an effect at least as extreme as what you have, if the real effect was the value you are testing against (e.g., a null effect)

Hover over the highlighted terms to see the definition.

Look up a term from the glossary file with `glossary("alpha")`:
<a class='glossary' title='(stats) The cutoff value for making a decision to reject the null hypothesis; (graphics) A value between 0 and 1 used to control the levels of transparency in a plot'>alpha</a>

Display a different value for the term with
`glossary("alpha", "$\\alpha$")`:
<a class='glossary' title='(stats) The cutoff value for making a decision to reject the null hypothesis; (graphics) A value between 0 and 1 used to control the levels of transparency in a plot'>$\alpha$</a>

Use an inline definition instead of the glossary file with
`glossary("beta", def = "The second letter of the Greek alphabet")`:
<a class='glossary' title='The second letter of the Greek alphabet'>beta</a>

Just show the definition with `glossary("p-value", show_def = TRUE)`:
The probability of seeing an effect at least as extreme as what you
have, if the real effect was the value you are testing against (e.g., a
null effect)

Show the table of terms defined on this page with `glossary_table()`:

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
term
</th>
<th style="text-align:left;">
definition
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
alpha
</td>
<td style="text-align:left;">
(stats) The cutoff value for making a decision to reject the null
hypothesis; (graphics) A value between 0 and 1 used to control the
levels of transparency in a plot
</td>
</tr>
<tr>
<td style="text-align:left;">
beta
</td>
<td style="text-align:left;">
The second letter of the Greek alphabet
</td>
</tr>
<tr>
<td style="text-align:left;">
p-value
</td>
<td style="text-align:left;">
The probability of seeing an effect at least as extreme as what you
have, if the real effect was the value you are testing against (e.g., a
null effect)
</td>
</tr>
</tbody>
</table>
