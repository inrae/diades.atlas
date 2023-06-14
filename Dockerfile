FROM rocker/geospatial:4.3.0
RUN apt-get update && apt-get install -y  gdal-bin git-core libcairo2-dev libcurl4-openssl-dev libgdal-dev libgeos-dev libgeos++-dev libgit2-dev libicu-dev libpng-dev libpq-dev libproj-dev libssl-dev libudunits2-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
# RUN echo "options(renv.config.pak.enabled = TRUE, repos = c(CRAN = 'https://packagemanager.rstudio.com/all/__linux__/focal/latest'), download.file.method = 'libcurl', Ncpus = 4)" >> /usr/local/lib/R/etc/Rprofile.site
RUN mkdir -p /usr/local/lib/R/etc
RUN echo "options(repos = c(CRAN = 'https://packagemanager.rstudio.com/all/__linux__/focal/latest'), download.file.method = 'libcurl', Ncpus = 4)" >> /usr/local/lib/R/etc/Rprofile.site
RUN echo "options(warn = 2);Sys.setenv(RENV_PATHS_CACHE='~/cache')" >> $R_HOME/etc/Rprofile.site

# We don't need no rserver
RUN apt-get remove --purge -y rstudio-server

# renv part

RUN Rscript -e 'remotes::install_github("rstudio/renv")'
# RUN Rscript -e 'remotes::install_version("renv", version = "0.13.2")'

RUN mkdir /build_zone
RUN mkdir ~/cache
WORKDIR /build_zone

# We do it one so that we have some cache when building the image
# ADD . /build_zone
COPY renv.lock renv.lock
COPY dev/switch_renv_repo.R switch_renv_repo.R
RUN Rscript switch_renv_repo.R && rm switch_renv_repo.R
RUN R -e 'renv::restore(packages = "renv")'
RUN R -e 'renv::restore()'
ADD . /build_zone

# Remove option to warn and stop
RUN echo "options(warn = -1);" >> $R_HOME/etc/Rprofile.site
# Re-restore (in case of new deps) & install local
RUN R -e 'list.files();renv::restore();remotes::install_deps(upgrade = "never")'
RUN R -e 'list.files();renv::restore();remotes::install_local(upgrade = "never")'
RUN R -e 'if(file.exists(".Renviron")) file.remove(".Renviron")'

EXPOSE 80

# For some reasons I can't explain the package is not installed when using renv
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');source('app.R', echo = TRUE)"
