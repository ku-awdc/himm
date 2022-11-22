.onLoad <- function(lib, pkg)
{
    ## Load the dynlib
    library.dynam("himm", pkg, lib, local=FALSE)

    ## Load the JAGS module
    modloc <- gsub('/$','', file.path(lib, pkg, 'libs', .Platform$r_arch))
    rjags::load.module("himm", modloc, quiet=TRUE)

    ## And the Rcpp module
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

