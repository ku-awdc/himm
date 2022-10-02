#' Load the internal JAGS module
#'
#' @name load_module
#' @aliases load_module unload_module
#'
#' @details
#' This module provides the following distributions for JAGS:
#'
#' LOMAX:  dlom(alpha, sigma)
#'
#' \deqn{
#'   p(x) = \frac{\alpha}{\sigma} \left(1 + \frac{x}{\sigma}\right)^{-\left(\alpha+1\right)}
#' }{
#'   p(x) = (\alpha / \sigma) (1 + (x / \sigma)) ^ -(\alpha+1)
#' }
#'
#' \deqn{\alpha > 0, \sigma > 0, x > 0}{\alpha > 0, \sigma > 0, x > 0}
#'

#' @rdname load_module
#' @export
load_module <- function(modloc, pkgname){

	# Find and load the shared library:
	slibpath <- file.path(modloc, paste(pkgname, .Platform$dynlib.ext, sep=''))
	cat("Loading shared library from:  ", slibpath, "\n", sep="")
	success <- try({
	  dyn.load(slibpath)
	  rjags::load.module(pkgname,modloc)
	})

	if(inherits(success, 'try-error')){

		rvers <- paste('version ', R.version$major, sep='')

		return(paste("The dynlib could not be loaded - perhaps the package was not built using the same versions of R [", rvers, "] and JAGS as available on this system?", sep=''))

	}

	package_env$slibpath <- slibpath
	invisible(TRUE)

}

#' @rdname load_module
#' @export
unload_module <- function(){

	if(is.null(package_env$slibpath)){
		warning('Unable to load the dynlib as it has not been loaded')
		invisible(FALSE)
	}

	slibpath <- package_env$slibpath
	cat("Unloading shared library from:  ", slibpath, "\n", sep="")
	success <- try(dyn.unload(slibpath))
	if(inherits(success, 'try-error'))
		warning("The internal dynlib could not be unloaded")

	package_env$slibpath <- NULL
	invisible(TRUE)
}

package_env <- new.env()
