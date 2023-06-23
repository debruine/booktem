#' Quarto Author Schema
#'
#' Set up an author object using the quarto author schema <https://quarto.org/docs/journals/authors.html>.
#'
#' The author schema is as follows (the function currently does not do much checking). The metadata/roles attribute is specific to this package.
#'
#' ```
#' author:
#'   - id: "LDB"
#'     number: 1
#'     name:
#'       given: Lisa M.
#'       family: DeBruine
#'       literal: Lisa M. DeBruine
#'       dropping-particle: ""
#'       non-dropping-particle: ""
#'     url: https://debruine.github.io
#'     email: debruine@gmail.com
#'     fax: ""
#'     orcid: 0000-0002-7523-5539
#'     note: ""
#'     acknowledgements: ""
#'     attributes:
#'       corresponding: true
#'       equal-contributor: false
#'       deceased: false
#'     metadata:
#'       roles: [Conceptualization, Methodology]
#'     affiliations:
#'     - id: UofG-SPN
#'       number: 1
#'       name: University of Glasgow
#'       department: School of Psychology & Neuroscience
#'       address: 62 Hillhead Street
#'       city: Glasgow
#'       region: Scotland
#'       country: United Kingdom
#'       postal-code: G12 8QB
#'       url: https://www.gla.ac.uk/schools/psychologyneuroscience/
#' ```
#'
#' @param given a character string with the author's first name(s)
#' @param family a character string with the author's last name(s)
#' @param orcid the author's unique ORCiD (see https://orcid.org/)
#' @param roles a vector of roles from the CRediT taxonomy (see https://casrai.org/credit/); use credit_roles() to view the full list
#' @param ... further info to add to author object (see Details)
#'
#' @return a list
#' @export
#'
#' @examples
#' a <- author(
#'   "Lisa M.", "DeBruine",
#'   orcid = "0000-0002-7523-5539",
#'   roles = c("Conceptualization", "Methodology"),
#'   email = "debruine@gmail.com",
#'   affiliation = list(
#'     name = "University of Glasgow",
#'     department = "School of Psychology & Neuroscience"
#'   )
#' )
author <- function(given, family, orcid = NULL, roles = c(), ...) {
  role_names <- credit_roles("name")

  # check roles are in list
  if (is.numeric(roles)) {
    roles <- role_names[roles]
  }

  chk_roles <- names(roles)

  if (is.null(chk_roles)) { chk_roles <- roles }
  bad_roles <- chk_roles[!(chk_roles %in% role_names)]
  if (length(bad_roles)) {
    # check for abbreviations
    bad_roles <- bad_roles[!(tolower(bad_roles) %in% credit_roles("abbr"))]
    if (length(bad_roles)) {
      message("These roles do not exist in the CRediT taxonomy: ",
           paste(bad_roles, collapse = ", "), "\n  See http://credit.casrai.org/")
    }
    # convert to correct names
    chk_roles <- role_names[credit_roles("abbr") %in% tolower(chk_roles)]
  }

  if (!is.null(orcid)) orcid <- check_orcid(orcid)
  a <- list(
    orcid = orcid,
    name = list(family = trimws(family),
                given = trimws(given)),
    roles = chk_roles
  )

  # add extra name parts
  extras <- lapply(list(...), trimws)
  extra_names <- intersect(
    c("literal", "dropping-particle", "non-dropping-particle"),
    names(extras)
  )
  a$name <- c(a$name, extras[extra_names])

  # add all extras
  extras[extra_names] <- NULL

  a <- c(a, extras)

  return(a)
}




#' CRediT Roles
#'
#' @param display Whether to display the category names, explanations, or abbreviations
#'
#' @return list of roles
#' @export
#'
#' @examples
#' credit_roles()
credit_roles <- function(display = c("explain", "names", "abbr")) {
  roles <- list(
    "Conceptualization" = "Ideas; formulation or evolution of overarching research goals and aims.",
    "Data curation" = "Management activities to annotate (produce metadata), scrub data and maintain research data (including software code, where it is necessary for interpreting the data itself) for initial use and later re-use.",
    "Formal analysis" = "Application of statistical, mathematical, computational, or other formal techniques to analyse or synthesize study data.",
    "Funding acquisition" = "Acquisition of the financial support for the project leading to this publication.",
    "Investigation" = "Conducting a research and investigation process, specifically performing the experiments, or data/evidence collection.",
    "Methodology" = "Development or design of methodology; creation of models.",
    "Project administration" = "Management and coordination responsibility for the research activity planning and execution.",
    "Resources" = "Provision of study materials, reagents, materials, patients, laboratory samples, animals, instrumentation, computing resources, or other analysis tools.",
    "Software" = "Programming, software development; designing computer programs; implementation of the computer code and supporting algorithms; testing of existing code components.",
    "Supervision" = "Oversight and leadership responsibility for the research activity planning and execution, including mentorship external to the core team.",
    "Validation" = "Verification, whether as a part of the activity or separate, of the overall replication/reproducibility of results/experiments and other research outputs.",
    "Visualization" = "Preparation, creation and/or presentation of the published work, specifically visualization/data presentation.",
    "Writing - original draft" = "Preparation, creation and/or presentation of the published work, specifically writing the initial draft (including substantive translation).",
    "Writing - review & editing" = "Preparation, creation and/or presentation of the published work by those from the original research group, specifically critical review, commentary or revision -- including pre- or post-publication stages."
  )

  abbr <- c(
    "con",
    "dat",
    "ana",
    "fun",
    "inv",
    "met",
    "adm",
    "res",
    "sof",
    "sup",
    "val",
    "vis",
    "dra",
    "edi"
  )

  if ("explain" == display[1]) {
    for (i in 1:length(roles)) {
      cname <- names(roles)[i]
      cdesc <- roles[[i]]
      paste0("[", i, "/", abbr[i], "] ", cname, ": ", cdesc, "\n") %>%
        cat()
    }
  } else if ("abbr" == display[1]) {
    abbr
  } else {
    names(roles)
  }
}

#' Check validity of ORCiD
#'
#' @param orcid a 16-character ORCiD in bare or URL format
#'
#' @return a formatted 16-character ORCiD or FALSE
#' @export
#'
#' @examples
#' check_orcid("0000-0002-7523-5539")
#' check_orcid("0000-0002-0247-239X")
#' check_orcid("https://orcid.org/0000-0002-0247-239X")
#' check_orcid("0000-0002-0247-2394") # incorrect, return FALSE
check_orcid <- function(orcid) {
  baseDigits <- gsub("[^0-9X]", "", orcid)

  if (nchar(baseDigits) != 16) {
    warning("The ORCiD ", orcid, " is not valid.")
  }

  total <- 0
  for (i in 1:(nchar(baseDigits)-1)) {
    digit <- substr(baseDigits, i, i) %>% as.integer()
    total <- (total + digit) * 2
  }
  remainder <- total %% 11;
  result <- (12 - remainder) %% 11;
  result <- ifelse(result == 10, "X", result)

  if (result == substr(baseDigits, 16, 16)) {
    paste(substr(baseDigits, 1, 4),
          substr(baseDigits, 5, 8),
          substr(baseDigits, 9, 12),
          substr(baseDigits, 13, 16),
          sep = "-")
  } else {
    warning("The ORCiD ", orcid, " is not valid.")
  }
}



#' Get ORCiD from Name
#'
#' @param family The family (last) name to search for
#' @param given An optional given (first) name to search for. Initials will be converted from, e.g., L M to L\* M\*
#'
#' @return A vector of matching ORCiDs
#' @export
#'
#' @examples
#' get_orcid("DeBruine", "Lisa")
#'
get_orcid <- function(family, given = "*") {
  if (is.null(family) || trimws(family) == "") {
    stop("You must include a family name")
  }

  if (is.null(given) || trimws(given) == "") {
    given <- "*"
  }

  query <- "https://pub.orcid.org/v3.0/search/?q=family-name:%s+AND+given-names:%s"

  given2 <- given %>%
    trimws() %>%
    gsub("^(\\w)\\.?$", "\\1\\*", .) %>% # single initial
    gsub("^(.)\\.?\\s", "\\1\\* ", .) %>% # initial initial
    gsub("\\s(.)\\.?$", " \\1\\*", .) %>% # ending initial
    gsub("\\s(.)\\.?\\s", " \\1\\* ", .) %>% # internal initial
    utils::URLencode()

  family2 <- trimws(family) %>% utils::URLencode()
  url <- sprintf(query, family2, given2) %>% url("rb")
  on.exit(close(url))

  xml <- tryCatch(xml2::read_xml(url), error = function(e) {
    warning("You might not have an internet connection")
    return(list())
  })
  l <- xml2::as_list(xml)

  n <- length(l$search)
  if (n == 0) {
    message("No ORCID found for ", given, " ", family)
  } else if (n > 1) {
    message("Multiple (", n, ") ORCIDs found for ", given, " ", family)
  }

  sapply(l$search, function(res) {
    res$`orcid-identifier`$path
  }) %>% unlist() %>% unname()
}
