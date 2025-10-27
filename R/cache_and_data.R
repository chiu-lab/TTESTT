
.pkg_name <- "DEGEmbedR"

.cache_dir <- function() {
  dir <- getOption("DEGEmbedR.cache_dir", tools::R_user_dir(.pkg_name, "cache"))
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

# If version update, redownload new files (stamp)
.version_string <- function() {
  as.character(utils::packageVersion(.pkg_name))
}

.stamp_path <- function() {
  file.path(.cache_dir(), sprintf("assets-%s.stamp", .version_string()))
}

# Generate Release Asset download link
.asset_url <- function() {
  ver <- .version_string()
  sprintf("https://github.com/chiu-lab/DEGEmbedR/releases/download/v%s/%s-data-%s.tar.gz",
          ver, .pkg_name, ver)
}

#Root on cache folder
get_data_root <- function() {
  .ensure_assets_downloaded()
  .cache_dir()
}

# unzip download files
.ensure_assets_downloaded <- function(force = FALSE) {
  if (!force && file.exists(.stamp_path())) return(invisible(TRUE))

  url <- .asset_url()
  tf  <- tempfile(fileext = ".tar.gz")
  message(sprintf("Downloading data assets for %s %s ...", .pkg_name, .version_string()))
  utils::download.file(url, destfile = tf, mode = "wb", quiet = TRUE)

  # unzip to root
  utils::untar(tf, exdir = .cache_dir())

  # clear stamp
  old_stamps <- Sys.glob(file.path(.cache_dir(), "assets-*.stamp"))
  if (length(old_stamps)) file.remove(old_stamps)

  file.create(.stamp_path())
  invisible(TRUE)
}

# Path to files
data_path <- function(...) {
  root <- get_data_root()
  file.path(root, ...)
}

# clear data
clear_data_cache <- function() {
  files <- list.files(.cache_dir(), full.names = TRUE, all.files = TRUE, no.. = TRUE)
  unlink(files, recursive = TRUE, force = TRUE)
  invisible(TRUE)
}
