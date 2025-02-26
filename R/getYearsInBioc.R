
getPkgYearsInBioc <- function(pkglist=NULL){

    bfc <- .get_cache()
    rid <- BiocFileCache::bfcquery(bfc, "bioc.pkg.years.info", "rname", exact = TRUE)$rid
    if (!length(rid)){
        pkginfo <- BiocFileCache::bfcnew(bfc, "bioc.pkg.years.info", ext = ".Rdata")
    }else{
        pkginfo <- BiocFileCache::bfcrpath(bfc, rids = rid)
    }
    
    if (!file.exists(pkginfo)) {
        NeededYearsData <- .generateBiocYearsInData(pkginfo)
    }else{       
        load(pkginfo)
        if(has_internet()){
            message("Checking for Bioc Release Update")
            NeededYearsData <- .updateYearsInBioc(NeededYearsData, pkginfo)
        }else{
            message("Cannot check for release update without internet.")
        }      
    }
    YearsInBiocDF <- .formatBiocYearsDF(NeededYearsData)
    if(is.null(pkglist)){
        return(YearsInBiocDF)
    }else{
        return(filter(YearsInBiocDF, package %in% pkglist))
    }
    
}

.formatBiocYearsDF <- function(NeededYearsData){

    config <- NeededYearsData[["config"]]
    manifestDF <- NeededYearsData[["manifestDF"]]

    manifestDFUnlist <- manifestDF |> unnest_longer(packages) |> 
        mutate(release = Release, version = major * 100 + minor, category = packages$category, package = packages$package, .keep = "none")

    current_version <- max(manifestDFUnlist$version)

    manifestByPkg <- manifestDFUnlist |> 
                 group_by(package) |>
                 summarize(category = category[which.max(version)],
                           first_version_available = release[which.min(version)],
                           last_version_available = release[which.max(version)]) |>
                  arrange(package)

    release_dates <- unlist(config[["release_dates"]])
    release_dates_date <- c(mdy(release_dates), NA)
    names(release_dates_date) = c(names(release_dates), config[["devel_version"]])
    since <- as.numeric(round(((Sys.Date() - release_dates_date) / 365.25)*2)/2)
    names(since) = c(names(release_dates), config[["devel_version"]])

    YearsInBiocDF <- manifestByPkg |> 
                    mutate(
                      first_version_release_date = release_dates_date[first_version_available],
                      last_version_release_date = release_dates_date[last_version_available],
                      approx_years_in = ifelse(last_version_available==config[["devel_version"]], since[first_version_available], NA),
                      years_before_rm = ifelse(last_version_available==config[["devel_version"]], NA,
                                        as.numeric(round((((last_version_release_date - first_version_release_date) / 365.25)*2)/2))),
                      last_version_available = ifelse(last_version_available==config[["devel_version"]], NA_character_,
                           last_version_available)) |>
                    select(package, category, first_version_available, first_version_release_date,
                           approx_years_in, last_version_available, last_version_release_date, years_before_rm)
    return(YearsInBiocDF) 
}

.updateYearsInBioc <- function(NeededYearsData, cachepathinfo){

    config <- NeededYearsData[["config"]]
    manifestDF <- NeededYearsData[["manifestDF"]]

    file_path <- 'https://bioconductor.org/config.yaml'
    current_config <- yaml::read_yaml(file_path)
    current_bioc_ver <- names(current_config$r_ver_for_bioc_ver)
    ## no info for 1.6/1.7 despite listing in config
    ## remove so that it does not try to generate information 
    neededReleases <- setdiff(current_bioc_ver, c("1.6", "1.7", manifestDF$Release))
    if(length(neededReleases) > 0){
        bioc_ver = data.frame(Release = neededReleases)
        newEntries <- cbind(bioc_ver, 
                        list_rbind(lapply(strsplit(bioc_ver$Release, ".", fixed = TRUE), 
                        (\(u) data.frame(major = as.integer(u[1]), 
                                         minor = as.integer(u[2])))))) |>
                  filter(major > 1 | (major == 1 & minor > 7)) |>
                  mutate(packages = map2(major, minor, .genBiocManifestDF))
        manifestDF <- rbind(manifestDF, newEntries)
        NeededYearsData[["manifestDF"]] <- manifestDF
        NeededYearsData[["config"]] <- current_config
        save(NeededYearsData, file=cachepathinfo)
    }
    return(NeededYearsData)
}

.generateBiocYearsInData <- function(cachefilepath){

    message("Internet Access Required For Data Generation \n",
            "  First time may take a few minutes, \n",
            "  Subsequent runs should be instantaneous.")

    ## get all packages for each release 

    file_path <- 'https://bioconductor.org/config.yaml'
    config <- yaml::read_yaml(file_path)
    bioc_ver <- data.frame(Release = names(config$r_ver_for_bioc_ver))
    manifestDF <- cbind(bioc_ver, 
                        list_rbind(lapply(strsplit(bioc_ver$Release, ".", fixed = TRUE), 
                        (\(u) data.frame(major = as.integer(u[1]), 
                                         minor = as.integer(u[2])))))) |>
                  filter(major > 1 | (major == 1 & minor > 7)) |>
                  mutate(packages = map2(major, minor, .genBiocManifestDF))
    NeededYearsData <- list(config = config, manifestDF = manifestDF)
    save(NeededYearsData, file=cachefilepath)
    return(NeededYearsData)
}


.genBiocManifestDF <- function(major, minor) {
    manifest_template <- "https://www.bioconductor.org/packages/{version}/{category}/src/contrib/PACKAGES"
    categories = c("bioc", "data/annotation", "data/experiment")
    ## the workflows category first appeared in 2.13
    if (major > 2 || (major == 2 && minor > 12))
    {
        categories <- append(categories, "workflows")
    }
    list_rbind(mapply(\(category, version) {
        
        print(glue("{category} / {version}"))
        
        con <- curl(glue(manifest_template))
        result <- read.dcf(con)
        close(con)
        tibble(category, package = result[,1])
    },
    categories, glue("{major}.{minor}"), SIMPLIFY = FALSE))
}
