update_headers <- function(path, recursive = TRUE) {
  # scan all files at path
  # find all "^# title {#label}"
  # replace with "^# title {#sec-label}"
  # find all "](#label)" or "\@ref(label)" and replace with "](#sec-label)" or @sec-label
}


update_figs <- function() {
  # find all "^(ref:label) reference text"
  # move reference text to matching fig.cap

  # find all "\@ref(fig:label)
  # replace with @fig-label
  # find all "```{r, fig:label"
  # replace with "```{r, fig-label"
}
