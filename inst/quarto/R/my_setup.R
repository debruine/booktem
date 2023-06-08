# book-specific code to include on every page

if (requireNamespace("knitr", quietly = TRUE)) {

  library(glossary)
  glossary::glossary_path("include/glossary.yml")
  glossary_popup("click") # "click", "hover" or "none"

  # default knitr options ----
  knitr::opts_chunk$set(
    echo       = TRUE,
    results    = "hold",
    out.width  = '100%',
    fig.width  = 8,
    fig.height = 5,
    fig.align  = 'center',
    dpi = 96
  )
}
