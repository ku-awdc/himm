.onLoad <- function(libname, pkgname){

	# Get and save the library location, getting rid of any trailing / caused by r_arch being empty:
	modloc <- gsub('/$','', file.path(libname, pkgname, 'libs', if(.Platform$r_arch!="") .Platform$r_arch else ""))
	# load_module(modloc, pkgname)
	himm_env$module <- list(modloc=modloc, pkgname=pkgname, slibpath=NULL)

}

.onAttach <- function(libname, pkgname){

}

.onDetach <- function(libpath){

  unload_module()

}

himm_env <- as.environment(list(module = list(modloc=NULL, pkgname=NULL, slibpath=NULL)))
