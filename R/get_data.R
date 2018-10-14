#' Get data from bioconductor
#'
#' @return json string containing bioconductor package details
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

    full_data$tags <- as.character(full_data$tags) %>%
        stringr::str_replace_all("[[:blank:]]", "") %>%
        stringr::str_replace_all("\n", "")

    full_data <- full_data %>%
        dplyr::filter(!is.na(tags))

    message("Package data download complete")

    json_data <- jsonlite::toJSON(full_data)
    # write_to_cache(json_data)

    return(json_data)
}

# process retrieved data into required data.frame columns
process_data <- function(pkg_list, raw_dl_stats) {
    dl_stats <- summarise_dl_stats(raw_dl_stats)

    pkg_link <- function(pkg) {
        stringr::str_interp(
            "http://bioconductor.org/packages/release/bioc/html/${pkg}.html"
        )
    }

    # convert from factor to character to avoid inner_join warning
    pkg_list$Package <- as.character(pkg_list$Package)
    dl_stats$Package <- as.character(dl_stats$Package)

    full_data <- dplyr::inner_join(pkg_list, dl_stats, by = "Package") %>%
        dplyr::select(
            Author,
            Package,
            License,
            biocViews,
            Description,
            downloads_month,
            downloads_total
        ) %>%
        dplyr::mutate(
            page = pkg_link(Package),
            package = stringr::str_split(biocViews, ", ")
        ) %>%
        dplyr::rename(
            authors = Author,
            name = Package,
            license = License,
            tags = biocViews,
            description = Description
        )

    full_data$authors <- author_list_to_string(full_data$authors)

    full_data
}

# summarise download stats into monthly and total lifetime downloads
summarise_dl_stats <- function(dl_stats) {
    dl_stats %>%
        dplyr::group_by(Package) %>%
        dplyr::summarise(
            downloads_month = dplyr::first(Nb_of_downloads),
            downloads_total = sum(Nb_of_downloads)
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
