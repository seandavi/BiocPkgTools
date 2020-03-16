#' Get data from Bioconductor
#'
#' @return A JSON string containing Bioconductor package details
#'
#' @importFrom jsonlite toJSON
#'
#' @export
#'
#' @examples
#' bioc_data <- get_bioc_data()
get_bioc_data <- function() {
    # if (has_cached_data() && cache_last_update_days() < 7) {
    #     return(get_cached_data())
    # }

    message("Downloading package data...")
    full_data <- process_data(
        pkg_list = BiocPkgTools::biocPkgList(),
        raw_dl_stats = BiocPkgTools::biocDownloadStats()
    )

    full_data <- full_data %>%
        dplyr::filter(!is.na(.data$tags))

    message("Package data download complete")

    json_data <- jsonlite::toJSON(full_data)
    # write_to_cache(json_data)

    return(json_data)
}

# process retrieved data into required data.frame columns
process_data <- function(pkg_list, raw_dl_stats) {
    dl_stats <- summarise_dl_stats(raw_dl_stats)

    pkg_link <- function(pkg) {
        vapply(
            pkg,
            function(x) {
                stringr::str_interp("https://bioconductor.org/packages/release/bioc/html/${x}.html")
            },
            character(1)
        )
    }

    # convert from factor to character to avoid inner_join warning
    pkg_list$Package <- as.character(pkg_list$Package)
    dl_stats$Package <- as.character(dl_stats$Package)

    full_data <- dplyr::inner_join(pkg_list, dl_stats, by = "Package") %>%
        dplyr::select(
            .data$Author,
            .data$Package,
            .data$License,
            .data$biocViews,
            .data$Description,
            .data$downloads_month,
            .data$downloads_total
        ) %>%
        dplyr::mutate(
            page = pkg_link(.data$Package)
        ) %>%
        dplyr::rename(
            authors = .data$Author,
            name = .data$Package,
            license = .data$License,
            tags = .data$biocViews,
            description = .data$Description
        )

    full_data$authors <- author_list_to_string(full_data$authors)

    full_data
}

# summarise download stats into monthly and total lifetime downloads
summarise_dl_stats <- function(dl_stats) {
    dl_stats %>%
        dplyr::group_by(.data$Package) %>%
        dplyr::summarise(
            downloads_month = dplyr::first(.data$Nb_of_downloads),
            downloads_total = sum(.data$Nb_of_downloads)
        )
}

# collapse list of names into a comma separated string
# final two authors separated by 'and'
author_list_to_string <- function(authors) {
    collapse_list <- function(x) unlist(x) %>% paste(collapse = ", ")
    sapply(authors, collapse_list) %>%
        stringr::str_replace(" and ", ", ") %>%
        stringr::str_replace(",([^,]*)$", " and\\1")
}

# get days since cached data was last updated
cache_last_update_days <- function() {
    cached_data_path <- system.file(
        "extdata",
        "data.Rds",
        package = "BiocExplorer"
    )
    cache_last_mod <- file.info(cached_data_path)$mtime

    difftime(Sys.time(), cache_last_mod, units = "days")
}

# get cached json data
get_cached_data <- function() {
    message("Getting data from cache")
    cached_data_path <- system.file(
        "extdata",
        "data.Rds",
        package = "BiocExplorer"
    )

    readRDS(cached_data_path)
}

# write json to cache location
write_to_cache <- function(json_data) {
    message("Writing data to cache")
    cached_data_path <- system.file(
        "extdata",
        package = "BiocExplorer"
    ) %>%
        file.path("data.Rds")

    saveRDS(json_data, cached_data_path)
}

# check if cached data is available
has_cached_data <- function() {
    cached_data_path <- system.file(
        "extdata",
        "data.Rds",
        package = "BiocExplorer"
    )

    return(cached_data_path != "")
}
