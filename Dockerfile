FROM rocker/geospatial:4.1.0
RUN apt-get update && apt-get install -y  gdal-bin git-core libcairo2-dev libcurl4-openssl-dev libgdal-dev libgeos-dev libgeos++-dev libgit2-dev libicu-dev libpng-dev libpq-dev libproj-dev libssl-dev libudunits2-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://packagemanager.rstudio.com/all/__linux__/focal/latest'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site
RUN echo "options(warn = 2);" >> $R_HOME/etc/Rprofile.site

# renv part
RUN Rscript -e 'remotes::install_github("rstudio/renv")'

RUN mkdir /build_zone
WORKDIR /build_zone

# We do it one so that we have some cache when building the image
COPY renv.lock renv.lock
RUN R -e 'renv::restore()'
ADD . /build_zone

# Re-restore (in case of new deps) & install local
RUN Rscript -e 'renv::restore();remotes::install_local(repos = "https://packagemanager.rstudio.com/all/__linux__/focal/latest", upgrade = "never")'

# Remove option to warn and stop
RUN echo "options(warn = -1);" >> $R_HOME/etc/Rprofile.site

EXPOSE 80

# For some reasons I can't explain the package is not installed when using renv
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');source('app.R', echo = TRUE)"
