".onAttach" <- function(lib, pkg) {
	packageStartupMessage(sprintf("# Package %s (%s) loaded [try SPEINews()].", 
		pkg, packageDescription(pkg) $Version))
}
