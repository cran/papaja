#' APA manuscript (6th edition)
#'
#' Template for creating an article according to APA guidelines (6th edition)
#' in PDF or DOCX format.
#'
#' @inheritParams rmarkdown::pdf_document
#' @param md_extensions Markdown extensions to be added or removed from the
#'   default definition of R Markdown. See the
#'   \code{\link[rmarkdown]{rmarkdown_format}} for additional details.
#' @param ... Further arguments to pass to
#'   \code{\link[bookdown]{pdf_document2}} or
#'   \code{\link[bookdown]{word_document2}}.
#' @details
#'   When creating PDF documents the YAML option `classoption` is passed
#'   to the class options of the LaTeX apa6 document class. In this case,
#'   additional options are available. Refer to the `apa6` document class
#'   \href{ftp://ftp.fu-berlin.de/tex/CTAN/macros/latex/contrib/apa6/apa6.pdf}{documentation}
#'   to find out about class options such as paper size or draft watermarks.
#'
#'   Please refer to the \href{https://frederikaust.com/papaja_man/r-markdown-components.html#yaml-front-matter}{\pkg{papaja} online-manual}
#'   for additional information on available YAML front matter settings.
#'   Note that the available settings for DOCX documents are more limited
#'   than for PDF documents.
#'
#'   When creating PDF documents the output device for figures defaults to
#'   \code{c("pdf", "png")}, so that each figure is saved in all four formats
#'   at a resolution of 300 dpi.
#' @return R Markdown output format to pass to [rmarkdown::render()].
#' @seealso [bookdown::pdf_document2()], [bookdown::word_document2()]
#' @export

apa6_pdf <- function(
  fig_caption = TRUE
  , number_sections = FALSE
  , toc = FALSE
  , keep_tex = TRUE
  , md_extensions = NULL
  , includes = NULL
  , ...
) {
  validate(fig_caption, check_class = "logical", check_length = 1)
  validate(number_sections, check_class = "logical", check_length = 1)
  validate(toc, check_class = "logical", check_length = 1)
  validate(keep_tex, check_class = "logical", check_length = 1)
  if(!is.null(includes)) {
    validate(includes, check_class = "list")
  } else {
    includes <- rmarkdown::includes()
  }

  apa6_header_includes <-  system.file(
    "rmarkdown", "templates", "apa6", "resources"
    , "apa6_header_includes.tex"
    , package = "papaja"
  )
  if(apa6_header_includes == "") stop("LaTeX header includes file not found.")

  includes$in_header <- c(includes$in_header, apa6_header_includes)

  if(is.null(md_extensions) || !grepl("raw\\_attribute", md_extensions)) {
    md_extensions <- paste0(md_extensions, "+raw_attribute")
  }

  # Call pdf_document() with the appropriate options
  config <- bookdown::pdf_document2(
    fig_caption = fig_caption
    , number_sections = number_sections
    , toc = toc
    , keep_tex = keep_tex
    , md_extensions = md_extensions
    , includes = includes
    , ...
  )

  # Set chunk defaults
  config$knitr$opts_chunk$echo <- FALSE
  config$knitr$opts_chunk$message <- FALSE
  config$knitr$opts_chunk$fig.cap <- " " # Ensures that figure environments are added
  config$knitr$opts_knit$rmarkdown.pandoc.to <- "latex"
  config$knitr$knit_hooks$inline <- inline_numbers

  config$knitr$opts_chunk$dev <- c("pdf", "png") # , "postscript", "tiff"
  config$knitr$opts_chunk$dpi <- 300
  config$clean_supporting <- FALSE # Always keep images files

  config$pre_knit <- function(input, ...) { modify_input_file(input = input) }

  ## Overwrite preprocessor to set CSL defaults
  saved_files_dir <- NULL

  # Preprocessor functions are adaptations from the RMarkdown package
  # (https://github.com/rstudio/rmarkdown/blob/master/R/pdf_document.R)
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
    # save files dir (for generating intermediates)
    saved_files_dir <<- files_dir

    args <- pdf_pre_processor(metadata, input_file, runtime, knit_meta, files_dir, output_dir)

    # Set citeproc = FALSE by default to invoke ampersand filter
    if(
      (is.null(metadata$replace_ampersands) || metadata$replace_ampersands) &&
      (is.null(metadata$citeproc) || metadata$citeproc)
    ) {
      metadata$citeproc <- FALSE
      assign("front_matter", metadata, pos = parent.frame())
    }

    args
  }

  post_processor <- function(metadata, input_file, output_file, clean, verbose) {

    output_text <- readLines(output_file, encoding = "UTF-8")

    # Correct abstract and note environment
    ## Note is added to the end of the document by Lua filter and needs to be
    ## moved to the preamble
    lua_addition_start <- which(grepl("^% papaja Lua-filter additions$", output_text))
    lua_addition_end <- which(grepl("^% End of papaja Lua-filter additions$", output_text))

    if(lua_addition_end - lua_addition_start > 1) {
      header_additions <- output_text[c((lua_addition_start + 1):(lua_addition_end - 1))]
      output_text <- output_text[-c(lua_addition_start:lua_addition_end)]
      begin_doc <- which(output_text == "\\begin{document}")
      output_text <- c(
        output_text[1:(begin_doc-1)]
        , header_additions
        , output_text[begin_doc:length(output_text)]
      )
    }
    output_text <- paste(output_text, collapse = "\n")

    output_text <- gsub(
      "\\\\begin\\{document\\}\n\\\\maketitle\n\\\\begin\\{abstract\\}(.+)\\\\end\\{abstract\\}"
      , paste0(
        "\\\\abstract{%\\1}\n\n"
        , "\n\n\\\\begin\\{document\\}\n\\\\maketitle"
      )
      , output_text
      , useBytes = TRUE
    )

    # Remove abstract environment if empty
    output_text <- gsub("\\\\abstract\\{\n\n\\}", "", output_text, useBytes = TRUE)

    # Remove pandoc listof...s
    if(sum(gregexpr("\\listoffigures", output_text, fixed = TRUE)[[1]] > 0)) {
      output_text <- sub("\\\\listoffigures", "", output_text, useBytes = TRUE) # Replace first occurrence
    }
    if(sum(gregexpr("\\listoftables", output_text, fixed = TRUE)[[1]] > 0)) {
      output_text <- sub("\\\\listoftables", "", output_text, useBytes = TRUE) # Replace first occurrence
    }

    # Prevent (re-)loading of geometry package
    output_text <- gsub("\\\\usepackage\\[?.*\\]?\\{geometry\\}", "", output_text, useBytes = TRUE)


    output_file_connection <- file(output_file)
    on.exit(close(output_file_connection))
    writeLines(output_text, output_file_connection, useBytes = TRUE)

    # Apply bookdown postprocesser and pass format options
    bookdown_post_processor <- bookdown::pdf_document2()$post_processor
    pp_env <- environment(bookdown_post_processor)
    assign("post", NULL, envir = pp_env) # Postprocessor is not self-contained
    assign("config", config, envir = pp_env) # Postprocessor is not self-contained
    assign("number_sections", number_sections, envir = pp_env)
    bookdown_post_processor(metadata = metadata, input = input_file, output = output_file, clean = clean, verbose = verbose)
  }

  config$pre_processor <- pre_processor
  config$post_processor <- post_processor

  config
}


#' @rdname apa6_pdf
#' @export

apa6_docx <- function(
  fig_caption = TRUE
  , number_sections = FALSE
  # , pandoc_args = NULL
  , md_extensions = NULL
  , ...
) {
  validate(fig_caption, check_class = "logical", check_length = 1)
  validate(number_sections, check_class = "logical", check_length = 1)

  # Get APA6 reference file
  ellipsis <- list(...)
  if(is.null(ellipsis$reference_docx)) {
    ellipsis$reference_docx <- system.file(
      "rmarkdown", "templates", "apa6", "resources"
      , "apa6_man.docx"
      , package = "papaja"
    )
    if(ellipsis$reference_docx == "") stop("No .docx-reference file found.")
  }


  # Call word_document() with the appropriate options
  config <- do.call(
    bookdown::word_document2
    , c(
      fig_caption = fig_caption
      # , pandoc_args = pandoc_args
      , md_extensions = md_extensions
      , number_sections = number_sections
      , ellipsis
    )
  )

  # Set chunk defaults
  config$knitr$opts_chunk$echo <- FALSE
  config$knitr$opts_chunk$message <- FALSE
  config$knitr$opts_knit$rmarkdown.pandoc.to <- "docx"
  config$knitr$knit_hooks$inline <- inline_numbers

  config$knitr$opts_chunk$dev <- c("png", "pdf") #, "svg", "tiff")
  config$knitr$opts_chunk$dpi <- 300
  config$clean_supporting <- FALSE # Always keep images files

  config$pre_knit <- function(input, ...) { modify_input_file(input=input) }

  ## Overwrite preprocessor to set CSL defaults
  saved_files_dir <- NULL
  .from <- rmarkdown::from_rmarkdown(fig_caption, md_extensions)

  # Preprocessor functions are adaptations from the RMarkdown package
  # (https://github.com/rstudio/rmarkdown/blob/master/R/pdf_document.R)
  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir, from = .from) {
    # save files dir (for generating intermediates)
    saved_files_dir <<- files_dir

    args <- word_pre_processor(metadata, input_file, runtime, knit_meta, files_dir, output_dir, from)

    # Set citeproc = FALSE by default to invoke ampersand filter
    if(
      (is.null(metadata$replace_ampersands) || metadata$replace_ampersands) &&
      (is.null(metadata$citeproc) || metadata$citeproc)
    ) {
      metadata$citeproc <- FALSE
      assign("front_matter", metadata, pos = parent.frame())
    }

    # Support pandoc numbersections option
    if(!is.null(metadata$numbersections) && isTRUE(metadata$numbersections)) {
      args <- c(args, "--lua-filter", rmarkdown::pkg_file_lua("number-sections.lua"))
    }

    args
  }

  post_processor <- function(metadata, input_file, output_file, clean, verbose) {

    # Add correct running head
    docx_files <- zip::zip_list(zipfile = output_file)$filename

    if(!is.null(metadata$shorttitle)) {
      running_head <- metadata$shorttitle
    } else {
      running_head <- metadata$title
    }

    zip::unzip(zipfile = output_file)
    on.exit(
      unlink(c("[Content_Types].xml", "_rels", "word", "docProps"), recursive = TRUE)
    )

    for(i in paste0("word/header", 2:3, ".xml")) {
      xml <- readLines(i, warn = FALSE)
      xml <- gsub("TITLE", toupper(running_head), xml, useBytes = TRUE)
      i_con <- file(i)
      writeLines(xml, con = i_con, useBytes = TRUE)
      close(i_con)
    }

    zip::zipr(
      zipfile = output_file
      , files = c("[Content_Types].xml", "_rels", "word", "docProps")
      , recurse = TRUE
      , include_directories = FALSE
    )
  }

  config$pre_processor <- pre_processor
  config$post_processor <- post_processor

  config
}

#' @describeIn apa6_pdf Format to create .docx-files. Alias of \code{apa6_docx}.
#' @export

apa6_word <- function(...) {
  apa6_docx(...)
}

#' @describeIn apa6_pdf Format to create .docx-files. Alias of \code{apa6_docx}.
#' @export

apa6_doc <- function(...) {
  apa6_docx(...)
}


# Set hook to print default numbers
inline_numbers <- function (x) {

  if(inherits(x, "difftime")) x <- as.numeric(x)
  if(is.numeric(x)) {
    printed_number <- ifelse(
      x == round(x)
      , as.character(x)
      , apa_num(x)
    )
    n <- length(printed_number)
    if(n == 1) {
      printed_number
    } else if(n == 2) {
      paste(printed_number, collapse = " and ")
    } else if(n > 2) {
      paste(paste(printed_number[1:(n - 1)], collapse = ", "), printed_number[n], sep = ", and ")
    }
  } else if(is.integer(x)) {
    x <- apa_num(x, numerals = x > 10)
  } else if(is.character(x)) {
    x
  } else {
    paste(as.character(x), collapse = ', ')
  }
}


# Preprocessor functions are adaptations from the RMarkdown package
# (https://github.com/rstudio/rmarkdown/blob/master/R/pdf_document.R)

set_default_csl <- function(x, version, metadata) {
  # Use APA6 CSL citations template if no other file is supplied
  has_csl <- function(text) {
    length(grep("^csl\\s*:.*$", text)) > 0
  }

  flavor <- list(NULL, "annotated")[[(!is.null(metadata$annotate_references) && metadata$annotate_references) + 1]]
  flavor <- c(
    flavor
    , list("no-disambiguation", NULL)[[(is.null(metadata$disambiguate_authors) || metadata$disambiguate_authors) + 1]]
  )

  csl_variant <- paste(c(paste0("apa", version), flavor), collapse = "-")

  if (!has_csl(readLines(x, warn = FALSE))) {
    csl_template <- system.file(
      "rmd", paste0(csl_variant, ".csl")
      , package = "papaja"
    )
    if(csl_template == "") stop("No CSL template file found.")
    return(c("--csl", rmarkdown::pandoc_path_arg(csl_template)))
  } else NULL
}

pdf_pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {

  # Add pandoc arguments
  args <- NULL

  if((!is.list(metadata$output) ||  !is.list(rmarkdown::metadata$output[[1]]) || is.null(metadata$output[[1]]$citation_package)) &
     (is.null(metadata$citeproc) || metadata$citeproc)) {

    ## Set CSL
    args <- set_default_csl(
      input_file
      , version = 6
      , metadata = metadata
    )
    csl_specified <- is.null(args)

    ## Set ampersand filter
    if((is.null(metadata$replace_ampersands) || metadata$replace_ampersands)) {
      if(csl_specified) {
        args <- c(args, "--csl", rmarkdown::pandoc_path_arg(tools::file_path_as_absolute(metadata$csl)))
      }

      args <- rmdfiltr::add_citeproc_filter(args)
      args <- rmdfiltr::add_replace_ampersands_filter(args)
    }
  }

  ## Set additional lua filters
  args <- rmdfiltr::add_wordcount_filter(args, error = FALSE)

  parse_metadata_filter <- system.file(
    "lua", "parse_metadata.lua"
    , package = "papaja"
  )
  args <- rmdfiltr::add_custom_filter(args, filter_path = parse_metadata_filter, lua = TRUE)

  if(isTRUE(metadata$quote_labels)) {
    label_quotes_filter <- system.file(
      "lua", "label_quotes.lua"
      , package = "papaja"
    )
    args <- rmdfiltr::add_custom_filter(args, filter_path = label_quotes_filter, lua = TRUE)
  }

  ## Set template variables and defaults
  if(is.null(metadata$documentclass)) {
    args <- c(args, "--variable", "documentclass:apa6")
  }

  if(!is.null(metadata[["class"]])) { # Depricated class options
    classoption <- paste(metadata[["class"]], collapse = ",")
    metadata$classoption <- paste(paste(metadata$classoption, collapse = ","), classoption, sep = ",")
  } else if(is.null(metadata$classoption)) {
    metadata$classoption <- "man"
  }

  if(isTRUE(metadata$mask)) {
    metadata$classoption <- paste0(metadata$classoption, ",mask")
    args <- c(args, "--variable", paste0("author-meta:", ""))
  }

  if(isTRUE(metadata$figsintext) || isTRUE(metadata$floatsintext)) {
    metadata$classoption <- paste0(metadata$classoption, ",floatsintext")
  }

  if(isTRUE(metadata$draft)) metadata$classoption <- paste0(metadata$classoption, ",draftall")

  args <- c(args, "--variable", paste0("classoption:", metadata$classoption))

  if (is.null(metadata$lang)) {
    lang_tag <- "en-EN"
  } else { # Depricated default lang options in papaja templates
    lang_tag <- switch(
      metadata$lang
      , english = "en-EN"
      , american = "en-US"
      , german = "de-DE"
      , metadata$lang
    )
  }

  args <- c(args, "--variable", paste0("lang:", lang_tag))

  if(is.null(metadata$title)) {
    args <- c(args, "--variable", "title:TITLE")
  }

  ## Surpresses redefinitino of paragraph and subparagraph
  if(is.null(metadata$subparagraph)) { # For compatibility with older pandoc versions
    args <- c(args, "--variable", "subparagraph:yes")
  }

  if(is.null(metadata$`block-headings`)) {
    args <- c(args, "--variable", "block-headings:no")
  }


  # Add necessary includes
  header_includes <- NULL
  after_body_includes <- NULL
  before_body_includes <- NULL


  ## Essential manuscript parts
  # if(!is.null(metadata$shorttitle)) {
  #   short_title <- paste0("\\shorttitle{", escape_latex(metadata$shorttitle), "}")
  # } else {
  # }
  # header_includes <- c(header_includes, short_title)

  # if(!is.null(metadata$leftheader)) {
  #   header_includes <- c(header_includes, paste0("\\leftheader{", escape_latex(metadata$leftheader), "}"))
  # }

  if(!is.null(metadata$keywords) || !is.null(metadata$wordcount)) {
    keywords <- paste(unlist(metadata$keywords), collapse = ", ")
    if(!is.null(metadata$wordcount)) {
      keywords <- paste0(keywords, "\\newline\\indent Word count: ", metadata$wordcount)
    }
    header_includes <- c(header_includes, paste0("\\keywords{", keywords, "}"))
  }

  ## Manuscript and table formatting
  if(
    ((!is.null(metadata$figsintext) & !isTRUE(metadata$figsintext)) ||
     (!is.null(metadata$floatsintext) & !isTRUE(metadata$floatsintext))) &&
    grepl("man", metadata$classoption)
  ) {
    header_includes <- c(
      header_includes
      , "\\DeclareDelayedFloatFlavor{ThreePartTable}{table}" # Make endfloat play with longtable
      # , "\\DeclareDelayedFloatFlavor{ltable}{table}" # Make endfloat play with lscape
      , "\\DeclareDelayedFloatFlavor{lltable}{table}" # Make endfloat play with lscape & longtable
      , "\\DeclareDelayedFloatFlavor*{longtable}{table}" # Make endfloat play with ordinary longtable (for kableExtra)
      # Patch \efloat@iwrite to use \protected@write (bug in endfloat package < 2.6)
      # Solution found at https://tex.stackexchange.com/questions/144372/error-when-using-endfloat-with-unicode-characters/144425
      # Details at https://github.com/axelsommerfeldt/endfloat/blob/master/README#L58
      , "\\makeatletter"
      , "\\renewcommand{\\efloat@iwrite}[1]{\\immediate\\expandafter\\protected@write\\csname efloat@post#1\\endcsname{}}"
      # , "`\\renewcommand{\\efloat@iwrite}[1]{\\immediate\\expandafter\\protected@write\\csname efloat@post#1\\endcsname{}}`{=latex}"
      , "\\makeatother"
    )
  }

  ## Additional options
  # Enable placement for table star environment
  if(any(grepl("jou", c(metadata$classoption, metadata$class)))) {
    header_includes <- c(header_includes, "\\usepackage{dblfloatfix}\n\n")
  }

  if(isTRUE(metadata$lineno) || isTRUE(metadata$linenumbers) ) {
    header_includes <- c(header_includes, "\\usepackage{lineno}\n\n\\linenumbers")
  }
  # Add after lineno to avoid LaTeX warning
  # https://tex.stackexchange.com/questions/447006/lineno-package-in-latex-causes-warning-message
  header_includes <- c(header_includes, "\\usepackage{csquotes}")

  if(!is.null(metadata$geometry)) {
    header_includes <- c(header_includes, paste0("\\geometry{", metadata$geometry, "}\n\n"))
  }

  if(isTRUE(metadata$footnotelist)) {
    header_includes <- c(
      header_includes
      , "\\usepackage{endnotes}"
      , "\\let\\footnote\\endnote"
    )

    after_body_includes <- c(after_body_includes, "\\clearpage", "\\theendnotes")
  }

  if(isTRUE(metadata$lof) || isTRUE(metadata$figurelist) || isTRUE(metadata$lot) || isTRUE(metadata$tablelist)) {
    header_includes <- c(header_includes, "\\usepackage[titles]{tocloft}")
  }

  if(isTRUE(metadata$lof) || isTRUE(metadata$figurelist)) {
    header_includes <- c(
      header_includes
      , "\\cftpagenumbersoff{figure}"
      , "\\renewcommand{\\cftfigpresnum}{\\itshape\\figurename\\enspace}"
      , "\\renewcommand{\\cftfigaftersnum}{.\\space}"
      , "\\setlength{\\cftfigindent}{0pt}"
      , "\\setlength{\\cftafterloftitleskip}{0pt}"
      , "\\settowidth{\\cftfignumwidth}{Figure 10.\\qquad}"
    )

    after_body_includes <- c(
      after_body_includes
      , "\\clearpage"
      , "\\renewcommand{\\listfigurename}{Figure captions}"
      , "\\listoffigures"
    )
  }

  if(isTRUE(metadata$lot) || isTRUE(metadata$tablelist)) {

    header_includes <- c(
      header_includes
      , "\\cftpagenumbersoff{table}"
      , "\\renewcommand{\\cfttabpresnum}{\\itshape\\tablename\\enspace}"
      , "\\renewcommand{\\cfttabaftersnum}{.\\space}"
      , "\\setlength{\\cfttabindent}{0pt}"
      , "\\setlength{\\cftafterloftitleskip}{0pt}"
      , "\\settowidth{\\cfttabnumwidth}{Table 10.\\qquad}"
    )

    after_body_includes <- c(
      after_body_includes
      , "\\clearpage"
      , "\\renewcommand{\\listtablename}{Table captions}"
      , "\\listoftables"
    )
  }

  tmp_includes_file <- function(x) {
    tmp_file <- tempfile(pattern = "includes_", tmpdir = tempdir(), fileext = ".tex")
    writeLines(x, con = tmp_file)
    tmp_file
  }

  header_includes <- c(header_includes, metadata$`header-includes`)
  if(length(header_includes) > 0) {
    args <- c(args, "--include-in-header", tmp_includes_file(header_includes))
  }

  before_body_includes <- c(before_body_includes, metadata$`before-includes`)
  if(length(before_body_includes) > 0) {
    args <- c(args, "--include-before", tmp_includes_file(before_body_includes))
  }

  after_body_includes <- c(after_body_includes, metadata$`after-includes`)
  if(length(after_body_includes) > 0) {
    args <- c(args, "--include-after", tmp_includes_file(after_body_includes))

  }

  args
}


word_pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir, from) {
  # Parse and modify YAML header
  input_text <- readLines(input_file, encoding = "UTF-8")
  yaml_params <- get_yaml_params(input_text)

  ## Create title page
  yaml_delimiters <- grep("^(---|\\.\\.\\.)\\s*$", input_text)
  augmented_input_text <- c(word_title_page(yaml_params), input_text[(yaml_delimiters[2] + 1):length(input_text)])

  ## Remove abstract to avoid redundancy introduced by pandoc
  yaml_params$abstract <- NULL

  ## Add modified YAML header
  augmented_input_text <- c("---", yaml::as.yaml(yaml_params), "---", augmented_input_text)
  # input_file_connection <- file(input_file)
  # on.exit(close(input_file_connection))
  # writeLines(augmented_input_text, input_file_connection, useBytes = TRUE)
  replace_yaml_front_matter(yaml_params, augmented_input_text, input_file)

  # Add pandoc arguments
  args <- NULL

  # Process markdown
  process_markdown <- utils::getFromNamespace("process_markdown", "bookdown")
  process_markdown(input_file, from, args, TRUE)

  if(is.null(metadata$citeproc) || metadata$citeproc) {

    ## Set CSL
    args <- set_default_csl(
      input_file
      , version = 6
      , metadata = metadata
    )
    csl_specified <- is.null(args)

    ## Set ampersand filter
    if((is.null(metadata$replace_ampersands) || metadata$replace_ampersands)) {
      if(csl_specified) {
        args <- c(args, "--csl", rmarkdown::pandoc_path_arg(tools::file_path_as_absolute(metadata$csl)))
      }

      args <- rmdfiltr::add_citeproc_filter(args)
      args <- rmdfiltr::add_replace_ampersands_filter(args)
    }
  }

  # Set additional lua filters
  args <- rmdfiltr::add_wordcount_filter(args, error = FALSE)

  docx_fixes_lua <-  system.file(
    "lua", "docx_fixes.lua"
    , package = "papaja"
  )
  if(docx_fixes_lua == "") stop("docx_fixes Lua-filter not found.")

  args <- rmdfiltr::add_custom_filter(args, filter_path = docx_fixes_lua, lua = TRUE, error = FALSE)

  args
}


get_yaml_params <- function(x) {
  yaml_delimiters <- grep("^(---|\\.\\.\\.)\\s*$", x)

  if(length(yaml_delimiters) >= 2 &&
     (yaml_delimiters[2] - yaml_delimiters[1] > 1) &&
     grepl("^---\\s*$", x[yaml_delimiters[1]])) {
    yaml_params <- yaml::yaml.load(paste(x[(yaml_delimiters[1] + 1):(yaml_delimiters[2] - 1)], collapse = "\n"))
    yaml_params
  } else NULL
}

replace_yaml_front_matter <- function(x, input_text, input_file) {
  yaml_delimiters <- grep("^(---|\\.\\.\\.)\\s*$", input_text)
  augmented_input_text <- c("---", yaml::as.yaml(x), "---", input_text[(yaml_delimiters[2] + 1):length(input_text)])


  input_file_connection <- file(input_file)
  on.exit(close(input_file_connection))
  writeLines(augmented_input_text, input_file_connection, useBytes = TRUE)
}


# Only for backward compatibility; we should recommend
# adding appendices manually in the documentation
modify_input_file <- function(input) {
  input_text <- readLines_utf8(con = basename(input))

  yaml_params <- get_yaml_params(input_text)

  appendices <- yaml_params$appendix

  if(!is.null(appendices)) {

    child_regex <- paste0(
      "child\\s*=\\s*['\"]", "(", appendices, ")", "['\"]"
    )

    missing_appendix <- sapply(
      child_regex
      , function(x) !grepl(x, paste0(input_text, collapse = "\n"))
    )
    missing_appendix <- setNames(missing_appendix, appendices)

    if(any(missing_appendix)) {
      appendix_section_line <- grep("^# \\(APPENDIX\\)", input_text)

      if(length(appendix_section_line) == 0) {
          input_text <- c(
            input_text
            , ""
            , ""
            , "\\newpage"
            , ""
            , "# (APPENDIX) Appendix {-}"
            , ""
          )
      }

      child_chunk <- function(x) {
        c(
          paste0("```{r child = \"", x, "\"}")
          , "```"
          , ""
        )
      }

      appendix_child_chunks <- unlist(
        lapply(appendices[missing_appendix], child_chunk)
      )

      input_text <- c(
        input_text
        , appendix_child_chunks
      )

      # latest changes due to issue #446
      writeLines(input_text, con = input, useBytes = TRUE)
    }
  }

  return(NULL)
}


#' @keywords internal

readLines_utf8 <- function(con) {
  if(is.character(con)) {
    con <- file(con, encoding = "utf8")
    on.exit(close(con))
  } else if(inherits(con, "connection")) {
    stop("If you want to use an already existing connection, you should use readLines(), directly.")
  }
  y <- try(readLines(con, encoding = "bytes"))
  if(inherits(y, "try-error")) stop("Reading from file ", encodeString(summary(con)$description, quote = "'"), " failed.")
  y
}
