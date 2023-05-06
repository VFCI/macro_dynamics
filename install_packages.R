## Install all required packages

package_list = c('devtools','Matrix','MASS','abind','coda','openxlsx', 'dplyr','ggplot2','reshape2', 'xtable', 'lpirfs', 'sovereign','HI', 'mvnfast', 'VARsignR', 'openxlsx', 'vars', 'ggplot2', 'ggpubr', 'gridExtra')

for (x in package_list){
    if (identical(system.file(package=x),"")){
		install.packages(x,dep=TRUE,quiet = TRUE)
		if (identical(system.file(package=x),"")){
			devtools::install_version(x,version=NULL,repos="http://cran.r-project.org",dependencies=FALSE)
		}
	}
    print("OK")
    
    library(x,character.only=TRUE)
}

