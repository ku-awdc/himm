.check.jags.home <- function(jags.home, major)
{
    ## Check that folder jags.home actually exists and contains the DLL
    ## in the appropriate sub-folder.

    ## Registry entries created by the JAGS instsaller may be invalid
    ## if the user removes JAGS by manually deleting files rather than
    ## using the uninstaller. So this function is used to check that
    ## the installation still exists.

    if (is.null(jags.home)) return(FALSE)
    if (!is.vector(jags.home, mode="character") || length(jags.home) != 1) {
        return(FALSE)
    }
    if (!file_test("-d", jags.home)) return(FALSE)

    bindir <- file.path(jags.home, .Platform$r_arch, "bin")
    jags.dll <- file.path(bindir, paste("libjags-", major,
                                        .Platform$dynlib.ext, sep=""))
    return(file.exists(jags.dll))
}


.findJAGS <- function(hive, major)
{
    ## Returns the registry key corresponding to the latest release of
    ## JAGS-major.x.y, or NULL if no release is found

    ## JAGS installer records JAGS install directory in 32-bit
    ## registry. This will switch to 64-bit registry after we drop
    ## 32-bit support.  So check both (Fix for problem in rjags_4-11).
    regkey <- try(readRegistry("SOFTWARE\\JAGS", hive = hive, maxdepth = 2,
                               view="64-bit"), silent = TRUE)
    if (inherits(regkey, "try-error")) {
        regkey <- try(readRegistry("SOFTWARE\\JAGS", hive = hive, maxdepth = 2,
                                   view="32-bit"), silent = TRUE)
    }
    if (inherits(regkey, "try-error")) {
        return(NULL)
    }

    keynames <- names(regkey)
    keynames <- keynames[grep(paste0("^JAGS-", major, "\\."), keynames)]
    if (length(keynames) == 0) {
        return(NULL)
    }
    else {
        keynames <- rev(keynames) #Search in reverse order of release number
        regkey <- regkey[keynames]
        for (i in seq(along=keynames)) {
            if(.check.jags.home(regkey[[i]][["InstallDir"]], major)) {
                return(regkey[i])
            }
        }
        return(NULL)
    }
}

.noJAGS <- function(major)
{
  paste("Failed to locate any version of JAGS version ", major, "\n\n",
        "The rjags package is just an interface to the JAGS library\n",
        "Make sure you have installed JAGS-", major,
        ".x.y.exe (for any x >=0, y>=0) from\n",
        "http://www.sourceforge.net/projects/mcmc-jags/files\n", sep="")
}

.onLoad <- function(lib, pkg)
{
### First task is to get installation directory of JAGS

    ## Major version of JAGS library should match major version
    ## of the rjags package
    jags.major <- packageVersion("rjags")$major

    ## Try environment variable first
    jags.home <- Sys.getenv("JAGS_HOME")
    if (nchar(jags.home) > 0) {
        if (!.check.jags.home(jags.home, jags.major)) {
            stop("The environment variable JAGS_HOME is set to\n", jags.home,
                 "\nbut no JAGS installation can be found there\n")
        }
    }
    else {
        ## Search the registry. We need to look for both machine-wide and
        ## user-specific installations

        key1 <- .findJAGS("HLM", jags.major)
        key2 <- .findJAGS("HCU", jags.major)

        if (is.null(key1)) {
            if (is.null(key2)) {
                stop(.noJAGS(jags.major))
            }
            else {
                latest <- key2
            }
        }
        else if (is.null(key2) || names(key2) < names(key1)) {
            latest <- key1
        }
        else {
            latest <- key2
        }

        jags.home <- latest[[1]][["InstallDir"]]
    }

### Add the JAGS bin to the windows PATH, if not already present

    path <- Sys.getenv("PATH")
    split.path <- strsplit(path, .Platform$path.sep)$PATH
    bindir <- file.path(jags.home, .Platform$r_arch, "bin")
    if (!any(split.path == bindir)) {
        path <- paste(bindir, path, sep=.Platform$path.sep)
        if (!Sys.setenv("PATH"=path)) {
            stop("Failed to add the rjags bin directory to the PATH:\n",
                 bindir)
        }
    }

### Load the dynlib
    library.dynam("himm", pkg, lib)

### And the JAGS module
    modloc <- gsub('/$','', file.path(lib, pkg, 'libs', .Platform$r_arch))
    rjags::load.module("himm", modloc, quiet=TRUE)

### And the Rcpp module
    Rcpp::loadModule("himm_module", TRUE)

}

.onAttach <- function(lib, pkg)
{
  packageStartupMessage("Loaded modules: ",
                        paste(rjags::list.modules(), collapse=","))
}


.onUnload <- function(libpath)
{
  rjags::unload.module("himm", quiet=TRUE)
  library.dynam.unload("himm", libpath)
}
