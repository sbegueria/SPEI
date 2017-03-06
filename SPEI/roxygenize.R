#! /usr/bin/Rscript

# file roxigenize.R
#
# This file roxygenizes all documentation wriiten in "Roxygen" format.
#
# author: Emanuele Cordano on 16-01-2014
#
#This program is free software: you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation, either version 3 of the License, or
#(at your option) any later version.
#
#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.
#
#You should have received a copy of the GNU General Public License
#along with this program.  If not, see <http://www.gnu.org/licenses/>.

###############################################################################
library(roxygen2)

options(repos = c(CRAN="http://cran.r-project.org"))


######path <- "/home/ecor/Dropbox/R-packages" 
##path <- "/media/VSHARED/workspace/Rstarter/Rcode"
path <- "/home/cordaem/workspace/Rstarter/Rcode"
pkg_name <- "SPEI"
pkg_dir <- paste(path,pkg_name,pkg_name,sep="/")
cran_pkg_path <- paste(path,"toCran",sep="/")
cran_pkg_dir <- paste(cran_pkg_path,pkg_name,sep="/")


##roxygenize(pkg_dir,clean=TRUE)

#roxygen.dir=pkg_dir copy.package=FALSE,unlink.target=FALSE

## installation
oo <- installed.packages()
if (pkg_name %in% oo[,"Package"]) {
	
	
	vv <-as.character(packageVersion(pkg_name))
	vv1 <- as.character(packageVersion(pkg_name,lib.loc=paste(path,pkg_name,sep="/")))
	print(vv)
	print(vv1)
	if (compareVersion(vv1,vv)>=0) {
		
		
		print("removing")
		remove.packages(pkg_name)
		install.packages(pkg_dir,type="source",repos=NULL)
	}
	
	
} else { 
	
	install.packages(pkg_dir,type="source",repos=NULL)
}

## 
toCran <- TRUE
if (toCran) { 
	
			system(paste("cp -R",pkg_dir,cran_pkg_path,sep=" ")) 
			cran_pkg_hidden <- paste(cran_pkg_dir,".git*",sep="/")
			system(paste("rm -rf",cran_pkg_hidden,sep=" "))
			cran_pkg_unuseful <- paste(cran_pkg_dir,"LICENSE",sep="/")
			system(paste("rm -rf",cran_pkg_unuseful,sep=" "))
			cran_pkg_unuseful <- paste(cran_pkg_dir,"LICENSE",sep="/")
			system(paste("rm -rf",cran_pkg_unuseful,sep=" "))
			cran_pkg_unuseful <- paste(cran_pkg_dir,"Read-and-delete-me",sep="/")
			system(paste("rm -rf",cran_pkg_unuseful,sep=" "))
			cran_pkg_unuseful <- paste(cran_pkg_dir,"roxygenize*",sep="/")
			system(paste("rm -rf",cran_pkg_unuseful,sep=" "))
			
			
			toremove <- c(".DS_Store","inst/.DS_Store","inst/doc/.DS_Store","inst/doc/examples/.DS_Store","inst/doc/examples/snowstuff/.DS_Store","inst/template/.DS_Store","inst/template/friuli/.DS_Store", ".Rapp.history")
			cran_pkg_unuseful <- paste(cran_pkg_dir,toremove,sep="/")
			for (it in cran_pkg_unuseful) {
				
				system(paste("rm -rf",it,sep=" "))
				
				
			}
			
	}




