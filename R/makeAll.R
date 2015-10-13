#' Generate news, celfiles, and help for website
#' 
#' Utility functions for generating the \code{News.html}, \code{celfiles.zip}, 
#' and \code{howto.html} files used in the website from \code{NEWS.Rd}, 
#' the celfiles in \code{"inst/extdata/celfiles"}, and
#' \code{vignettes/howto.Rmd}, respectively. 
#' Intented primarily for internal use.
#' 
#' @param makeCelfiles logical. Should \code{makeCelfiles()} be executed.
#'   Default \code{FALSE} is faster.
#' @param indir, outdir The in and output directories to read and write the 
#'   files from and to.
#' @param overwrite Overwrite existing files.
#' @param \dots Arguments passed to \code{knitr:knit2html}.
#' @return Writes to files in \code{outdir}. Invisibly returns the path to the 
#'   written file.
#' @note The functions assumes by default that the working directory is the
#'   root of the local \pkg{hemaClass} package. \cr
#'   The functions overwrites all previously present files.
#' @author Anders Ellern Bilgrau
#' @examples 
#' \dontrun{
#' makeAll()
#' 
#' # makeAll is an alias for:
#' makeNews()
#' makeCelfiles() 
#' makeHowto()
#' }
#' @export
makeAll <- function(makeCelfiles = FALSE) {
  makeNews()
  if (makeCelfiles) makeCelfiles()
  makeHowto()
}
  
#' @export
#' @rdname makeAll
makeNews <- function(indir = "inst", outdir = "inst/website/www") {
  # html.file <- system.file("NEWS.Rd", package = "hemaClass")
  infile <- file.path(indir, "NEWS.Rd")
  if (!file.exists(infile)) stop("Cannot find file ", infile)
  outfile <- file.path(outdir, "News.html")
  if (!file.exists(outfile)) stop("Cannot find file ", outfile)
  tools::Rd2HTML(infile, out = outfile) # Writes to disk
  
  # Extra modifications to file
  html <- readLines(outfile)
  html <- html[!grepl("R Documentation", html)]
  new.title <- '<strong>hemaClass.org</strong>'
  html <- gsub('Package <span class="pkg">hemaClass</span>', 
               new.title, html)
  
  # Add explanation text
  extra.text <- "Below are the latest news and changes for the
    <strong>hemaClass</strong> R-package."
  html <- append(html, extra.text, after = min(which(grepl(new.title, html))))
  
  
  # Fix headline levels
  html <- gsub("<h2>", "<h1>", html)
  html <- gsub("</h2>", "</h1>", html)
  html <- gsub("<h3>", "<h2>", html)
  html <- gsub("</h3>", "</h2>", html)
  
  # Prepend header
  html <- c(paste("<!-- Generated using hemaClass::makeNews()",
                  "from", infile, "on", Sys.Date(), "-->"),
            "<!-- Do not edit by hand! -->",
            paste("<!-- Edit",  infile, "instead. -->"),
            html)
  
  # Write changes
  writeLines(html, con = outfile) 
  
  return(invisible(outfile))
}

#' @export
#' @rdname makeAll
makeCelfiles <- function(outdir = "inst/website/www") {
  celfiles <- list.files(system.file("extdata/celfiles", package = "hemaClass"),
                         pattern = "\\.CEL$", full.names = TRUE)
  zipfile <- file.path(outdir, "celfiles.zip")
  zip(zipfile = zipfile, files = celfiles)
  return(invisible(zipfile))
}

#' @export
#' @rdname makeAll
makeHowto <- function(indir = "vignettes", outdir = "inst/website/www", ...) {
  
  infile <- file.path(indir, "howto.Rmd")
  if (!file.exists(infile)) stop("Cannot find file ", infile)
  outfile <- file.path(outdir, "howto.html")
  if (!file.exists(outfile)) stop("Cannot find file ", outfile)
    
  knitr::knit2html(input = infile, output = outfile, ...)  # Write to disk
  
  # Extra modifications to file
  # Comment out header sections (remove formatting for website):
  getind <- function(regex) min(which(grepl(regex, html, fixed = TRUE)))
  beginCommentAfter <- function(regex) {
    append(html, "<!-- ", after = min(getind(regex)))
  }
#   endCommentAfter <- function(regex) {
#     append(html, " -->", after = min(getind(regex)))
#   }
  beginCommentBefore <- function(regex) {
    append(html, "<!-- ", after = min(getind(regex)) - 1)
  }
  endCommentBefore <- function(regex) {
    append(html, " -->", after = min(getind(regex)) - 1)
  }
  
  html <- readLines(outfile)
  html <- beginCommentBefore("<script type=\"text/javascript\">")
  html <- endCommentBefore("<!-- Styles for R syntax highlighter -->")
  html <- beginCommentAfter("<!-- Styles for R syntax highlighter -->")
  html <- endCommentBefore("<!-- R syntax highlighter -->") 
  html <- beginCommentAfter("<!-- R syntax highlighter -->")
  html <- endCommentBefore("</head>")
  
  # Fix paths
  html <- gsub("../inst/website/www/", "", html)
  
  # Prepend header
  html <- c(paste("<!-- Generated using hemaClass::makeHowto()",
                  "from", infile, "on", Sys.Date(), "-->"),
            "<!-- Do not edit by hand! -->",
            paste("<!-- Edit",  infile, "instead. -->"),
            html)
  
  # Write to disk
  writeLines(html, con = outfile) 
  
  # Remove generated .md file in getwd()
  file.remove(gsub("\\.Rmd", ".md", basename(infile)))
 
  return(invisible(outfile)) 
}