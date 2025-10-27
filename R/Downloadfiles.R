#' Download and store external data files for package use
#'
#' @description
#' Internal helper function used to download required data files (e.g., pretrained
#' embeddings or similarity matrices) from a remote URL into the package's
#' \code{data/} directory. If the specified file already exists, no download occurs.
#' If the download fails, a warning is issued instead of stopping execution.
#'
#' @param filename Character. The name of the file to be saved under the
#'   package's \code{data/} directory.
#' @param url Character. The full URL from which to download the file.
#'
#' @details
#' The function creates the \code{data/} directory inside the installed package path
#' (if it does not already exist), and saves the downloaded file there.
#'
#' This function is primarily intended for internal use during package installation
#' or updates, when large external datasets (such as gene or pathway embeddings)
#' need to be retrieved automatically.
#'
#' To make use of downloaded files at runtime, load them with \code{load()} or
#' \code{readRDS()} after confirming their presence via \code{system.file()}.
#'
#' @return
#' Invisibly returns the destination file path where the data is saved.
#' If the download fails, the function returns \code{NULL} and prints a warning.
#'
#' @examples
#' \dontrun{
#' # Example: download an example data file into package data directory
#' .Downloadfiles(
#'   filename = "gene_embedding_demo.Rdata",
#'   url = "https://example.org/data/gene_embedding_demo.Rdata"
#' )
#' }
#'
#' @keywords internal data download utility


.Downloadfiles <- function(filename, url) {
  cache_dir <- tools::R_user_dir("DEGEmbedR", which = "cache")
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  dest <- file.path(cache_dir, filename)

  if (!file.exists(dest)) {
    #message("Downloading data to cache: ", dest)
    tryCatch({
      utils::download.file(url, destfile = dest, mode = "wb", quiet = TRUE)
    }, error = function(e) {
      warning("Could not download data: ", e$message)
      return(invisible(NULL))
    })
  } else {
    #message("Using cached file: ", dest)
  }

    return(dest)
}
