FROM openanalytics/r-base

MAINTAINER Jason Yang "sound_118@msn.com"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev 

# system library dependency for word cloud app
RUN apt-get update && apt-get install -y \
    libxml2-dev 

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://mirrors.tongji.edu.cn/CRAN/')"

# install dependencies of survival analysis app
RUN R -e "install.packages(c('thematic','bslib','shinyjs','shinyBS','shinycssloaders','shinyscreenshot','htmltools','plotly','survminer','DT','survival','gt') , repos='https://mirrors.tongji.edu.cn/CRAN/')"

# copy the app to the image
RUN mkdir /root/rsurv
COPY rsurv /root/rsurv

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/rsurv')"]
