# Shiny application instructions
These are our internal instructions and reference on maintaining the **hemaClass** Shiny server.

## Log onto server as super user
Start a terminal and log onto the server as a super user by running:
```sh
ssh -X <username>@oncoclass.hpc.aau.dk
#ssh -X sfl@oncoclass.hpc.aau.dk
```
With the appropriate username.
When prompted, enter your password.

## Installing the latest version of R
Start a terminal and start a super user session:
```sh
sudo su 
```

Add the R mirror to the source list via:
```sh
echo "deb http://mirrors.dotsrc.org/cran/bin/linux/ubuntu trusty/ #enabled-manually" >> /etc/apt/sources.list
```
Exit the super user session by
```sh
exit
```
To add the key
```sh
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
```


## Installing R
To install R, do
```sh
sudo apt-get update && sudo apt-get install r-base r-base-dev
```
## Installing Shiny 
According to http://www.rstudio.com/products/shiny/download-server/,
open port 3838 for shiny:
```sh
sudo ufw allow 3838
sudo ufw allow 8787
sudo ufw allow 8080
sudo ufw allow 80
```

## Installing devtools:
```sh
apt-get -y build-dep libcurl4-gnutls-dev
apt-get -y install libcurl4-gnutls-dev
```

## Install the packages needed to run the scripts
Start R by:
```sh
sudo -i R
```
Run the following R-script described in the root `README.md`.
```R
source("http://bioconductor.org/biocLite.R")
biocLite("affy")
biocLite("affyio")
biocLite("preprocessCore")

# Then form CRAN
install.packages("shiny")
install.packages("matrixStats")
install.packages("Rcpp")
install.packages("RcppArmadillo")
install.packages("RcppEigen")
install.packages("testthat")
install.packages("gdata")
install.packages("survival")
install.packages("WriteXLS")

# Finally the package is installed.
install.packages("devtools")  # Uncomment if devtools is not installed
devtools::install_github("oncoclass/hemaClass", 
                        dependencies = TRUE)

devtools::install_github("analytixware/shinysky")

# To gain support for reading xlsx files
gdata::installXLSXsupport(perl = "perl", verbose = FALSE)
```


# General editing

## Installing shiny applications

In a local terminal, and run one of the following command to copy the current website to your user folder on the server.
```sh
scp -r /hemaClass/inst/website/ <username>@oncoclass.hpc.aau.dk:~/
#scp -r /Users/Falgreen/Documents/R-packages/hemaClass/inst/website/ sfl@oncoclass.hpc.aau.dk:~/
#scp -r /Users/mboegsted/Documents/R-packages/hemaClass/inst/website/ m_boegsted@dcm.aau.dk@oncoclass.hpc.aau.dk:~/
#scp -r /Users/anders/Documents/hemaClass/inst/website/ abilgrau@math.aau.dk@oncoclass.hpc.aau.dk:~/
```
Remember to use the correct paths. Log onto the server as a sudo user:
```sh
ssh -X <username>@oncoclass.hpc.aau.dk
#ssh -X sfl@oncoclass.hpc.aau.dk
#ssh -X m_boegsted@dcm.aau.dk@oncoclass.hpc.aau.dk
#ssh -X abilgrau@math.aau.dk@oncoclass.hpc.aau.dk
```
After you have logged in you can copy the file into the shiny server folder:
```sh
sudo cp -r  ~/website/ /srv/shiny-server/hemaClass/
```
Remember to clean up your home dir by
```sh
rm -r ~/website/
```
to save precious server space.

## Update website only (not database)
In order to only submit changes made to the website only and not the database (i.e. `server.R` and `ui.R`) use
```sh
scp /hemaClass/inst/website/*.R <username>@oncoclass.hpc.aau.dk:~/website/
#scp -r /Users/Falgreen/Documents/R-packages/hemaClass/inst/website/*.R sfl@oncoclass.hpc.aau.dk:~/website/
#scp -r /Users/mboegsted/Documents/R-packages/hemaClass/inst/website/*.R m_boegsted@dcm.aau.dk@oncoclass.hpc.aau.dk:~/website/
#scp /Users/anders/Documents/hemaClass/inst/website/*.R abilgrau@math.aau.dk@oncoclass.hpc.aau.dk:~/website/

ssh -X <username>@oncoclass.hpc.aau.dk
#ssh -X sfl@oncoclass.hpc.aau.dk
#ssh -X m_boegsted@dcm.aau.dk@oncoclass.hpc.aau.dk
#ssh -X abilgrau@math.aau.dk@oncoclass.hpc.aau.dk

sudo cp -r  ~/website/*.R /srv/shiny-server/hemaClass/website
```

## Install new version of the **hemaClass** package
To install the newest version of the **hemaClass** package, run:
```sh
sudo -i R
```
In R, run:
```R
devtools::install_github("oncoclass/hemaClass", dependencies = TRUE)
```

# Misc. apache stuff
To enable the oncoclass configuration file for apache2 write
```sh
sudo a2ensite oncoclass.conf
```
Restarting the apache server:
```sh
sudo service apache2 restart
```
Restarting the shiny server:
```sh
sudo restart shiny-server
```
