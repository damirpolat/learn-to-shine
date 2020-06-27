FROM rocker/tidyverse:4.0.0-ubuntu18.04

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \ 
		tzdata \
		gdebi-core \
		subversion \
		default-jdk \
		default-jre \
	&& R CMD javareconf		

# Download and install shiny server
RUN wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
		gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb && \
    . /etc/environment && \ 
    R -e "install.packages(c('shiny', 'rmarkdown'))"



# install R packages
RUN R -e "install.packages('shinythemes')"
RUN R -e "install.packages('shinydashboard')"

# copy app files to image
COPY shiny-server.sh /usr/bin/shiny-server.sh
RUN chmod +x /usr/bin/shiny-server.sh
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY possible-bug.R /srv/shiny-server/

# select port
EXPOSE 3838

# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/possible-bug.R', host='0.0.0.0', port=3838)"]
