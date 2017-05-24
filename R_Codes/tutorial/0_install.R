## For MAC users: 

# install 'devtools' if required
if (!require(devtools)) install.packages("devtools")  
devtools::install_github("CCS-Lab/hBayesDM")


## For Windows users:

# Download Rtools: https://cran.r-project.org/bin/windows/Rtools/
# Then install hBayesDM
install.packages("hBayesDM", dependencies=TRUE)
