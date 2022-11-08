biocConfigVersion <- function(
    version = "3.15", local = TRUE,
    yaml_dir = tools::R_user_dir("BiocPkgTools", which = "config")
) {
    if (!local)
        stop("Currently only local 'config.yaml' files are supported")
    date <- biocLastBuildDate(version)
    yaml_file <- file.path(yaml_dir, "config.yaml")
    download.file("https://bioconductor.org/config.yaml", yaml_file)
}

## download.file("https://bioconductor.org/config.yaml", "~/config.yaml")
## file.edit("~/config.yaml")
##
## ## add this to the yaml file
## rspm_ver_for_bioc_ver:
##   "3.15": "2022-10-19"
##
## ## set environment variable
## file.edit("~/.Renviron")
## #  BIOCONDUCTOR_CONFIG_FILE=/mnt/STORE1/bighome/mramos/config.yaml
## # opt <- getOption("BiocManager.snapshot", "RSPM")
