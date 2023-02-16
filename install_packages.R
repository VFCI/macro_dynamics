## Install all required packages

package_list = c('Matrix','MASS','parallel','abind','coda','openxlsx', 'dplyr','ggplot2','reshape2', 'xtable', 'lpirfs', 'sovereign', 'VARsignR', 'openxlsx', 'vars', 'ggplot2', 'ggpubr', 'gridExtra')

for (x in package_list){
    if (!require(x,character.only = TRUE))
	{
		install.packages(x,dep=TRUE)
		if(!require(x,character.only = TRUE)) stop("Package not found")
	}
    return("OK")
    library(x)
}

