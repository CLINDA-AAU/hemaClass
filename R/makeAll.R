#' Generate news, celfiles, and help for website
#' 
#' Functions for generating the \code{News.html}, \code{celfiles.zip}, and
#' \code{howto.html} files for the website from \code{NEWS.Rd}, the celfiles in 
#' \code{"/"}, an \code{vignettes/howto.Rmd}. Intented mostly for internal use.
#' 
#' @param indir, outdir The in and output directories to read and write the 
#'   files from and to.
#' @param overwrite Overwrite existing files.
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
#' #makeCelfiles() 
#' makeHowto()
#' }
#' @export
makeAll <- function() {
  makeNews()
  # makeCelfiles()
  makeHowto()
}
  
#' @export
#' @rdname makeAll
makeNews <- function(indir = "inst", outdir = "inst/website/www") {
  # html.file <- system.file("NEWS.Rd", package = "hemaClass")
  infile <- file.path(indir, "NEWS.Rd")
  if (!file.exists(infile)) stop("Cannot find file ", infile)
  outfile <- file.path(outdir, "News.html")
  if (!file.exists(infile)) stop("Cannot find file ", outfile)
  tools::Rd2HTML(infile, out = outfile) # Writes to disk
  
  # Extra modifications to file
  html <- readLines(outfile)
  html <- html[!grepl("R Documentation", html)]
  html <- gsub('Package <span class="pkg">hemaClass</span>', 
               '<strong>hemaClass.org</strong>', html)
  
  html <- c(paste("<!-- Generated using hemaClass::makeNews()",
                  "from inst/News.Rd", Sys.Date(), "-->"),
            "<!-- Do not edit by hand! -->",
            "<!-- Edit inst/News.Rd -->",
            html)
  
  # Fix headline levels
  html <- gsub("<h2>", "<h1>", html)
  html <- gsub("</h2>", "</h1>", html)
  html <- gsub("<h3>", "<h2>", html)
  html <- gsub("</h3>", "</h2>", html)
  
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
makeHowto <- function(indir = "vignettes", outdir = "inst/website/www") {
  
}