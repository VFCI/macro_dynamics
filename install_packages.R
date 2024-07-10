package_list = c('devtools','Matrix','MASS','abind','coda','openxlsx','zoo','dplyr','ggplot2','reshape2', 'xtable', 'lpirfs', 'sovereign','HI', 'mvnfast', 'VARsignR', 'openxlsx', 'vars', 'ggplot2', 'ggpubr','dint','gridExtra','PerformanceAnalytics','tidyquant','tsibble','gsl','copula','svars','here', 'renv','corrplot','mvnormalTest')

for (x in package_list){
    if (identical(system.file(package=x),"")){
		install.packages(x,dep=TRUE,quiet = TRUE)
		if (identical(system.file(package=x),"")){
			devtools::install_version(x,version=NULL,repos="http://cran.r-project.org",dependencies=FALSE)
		}
	}
    print(paste(x,"OK"))
    
    library(x,character.only=TRUE)
}
# install.packages("jsonlite", repos = c("https://jeroen.r-universe.dev", "https://cloud.r-project.org"))
# devtools::install_github("paulponcet/tribe")

renv::restore(prompt=FALSE)
renv::install("https://github.com/jrnold/stataXml/")

