# booktem functions
# do not edit unless you're sure what you're doing!!!!!

if (requireNamespace("knitr", quietly = TRUE)) {
  suppressPackageStartupMessages({
    library(stats) # stops dplyr::filter being masked
    library(knitr)
  })

  ## set class for a chunk using class="className" ----
  knitr::knit_hooks$set(class = function(before, options, envir) {
    if (before) {
      sprintf("<div class = '%s'>", options$class)
    } else {
      "</div>"
    }
  })

  ## verbatim code chunks ----
  knitr::knit_hooks$set(verbatim = function(before, options, envir) {
    if (before) {
      sprintf("<div class='verbatim'><pre class='sourceCode r'><code class='sourceCode R'>&#96;&#96;&#96;{%s}</code></pre>", options$verbatim)
    } else {
      "<pre class='sourceCode r'><code class='sourceCode R'>&#96;&#96;&#96;</code></pre></div>"
    }
  })

}
