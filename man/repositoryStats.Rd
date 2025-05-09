% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/repositoryStats.R
\name{repositoryStats}
\alias{repositoryStats}
\alias{print.repositoryStats}
\title{Bioconductor Binary Repository Statistics}
\usage{
repositoryStats(
  version = BiocManager::version(),
  binary_repository = BiocManager::containerRepository(version),
  local = FALSE
)

\method{print}{repositoryStats}(x, ...)
}
\arguments{
\item{version}{(Optional) \code{character(1)} or \code{package_version}
indicating the \emph{Bioconductor} version (e.g., "3.8") for which
repositories are required.}

\item{binary_repository}{\code{character(1)} location of binary repository as
given by \code{BiocManager::containerRepository} (default)}

\item{local}{\code{logical(1)} whether to check the local file system for the
\code{PACKAGES} file's last modified date (default: \code{FALSE}).}

\item{x}{the object returned by \code{repositoryStats()}.}

\item{...}{further arguments passed to or from other methods (not used).}
}
\value{
a list of class \code{repositoryStats} with the following fields:
\itemize{
\item container: \code{character(1)} container label, e.g.,
\code{bioconductor_docker}, or NA if not evaluated on a supported container
\item bioconductor_version: \code{package_version} the
Bioconductor version provided by the user.
\item repository_exists: \code{logical(1)} TRUE if a binary repository
exists for the container and Bioconductor_Version version.
\item bioconductor_binary_repository: \code{character(1)} repository
location, if available, or NA if the repository does not exist.
\item n_software_packages: \code{integer(1)} number of software packages
in the Bioconductor source repository.
\item n_binary_packages: \code{integer(1)} number of binary packages
available. When a binary repository exists, this number is likely
to be larger than the number of source software packages, because
it includes the binary version of the source software packages, as
well as the (possibly CRAN) dependencies of the binary packages
\item n_binary_software_packages: \code{integer(1)} number of binary
packages derived from Bioconductor source packages. This number is
less than or equal to \code{n_software_packages}.
\item missing_binaries: \code{integer(1)} the number of Bioconductor
source software packages that are not present in the binary
repository.
\item out_of_date_binaries: \code{integer(1)} the number of Bioconductor
source software packages that are newer than their binary
counterpart. A newer source software package
might occur when the main Bioconductor build system has
updated a package after the most recent run of the binary
build system.
}
}
\description{
Summarize binary packages compatible with the Bioconductor or
Terra container in use.
}
\details{
For local repositories, use the \code{local = TRUE} argument. Local
repositories will typically start with the \verb{file://} URI. The function
checks the \code{mtime} of the output of \code{file.info} on the \code{PACKAGES} file in
the local repository. Otherwise, by default, it will check the
\code{last-modified} header of the \code{PACKAGES} file via \code{httr::HEAD()}.
}
\section{Methods (by generic)}{
\itemize{
\item \code{print(repositoryStats)}: Print a summary of package
availability in binary repositories.

}}
\examples{
stats <- repositoryStats() # obtain statistics
stats                       # display a summary
stats$container             # access an element for further computation

}
\author{
M. Morgan
}
