FROM rocker/geospatial:4.1.0
RUN apt-get update && apt-get install -y  gdal-bin git-core libcairo2-dev libcurl4-openssl-dev libgdal-dev libgeos-dev libgeos++-dev libgit2-dev libicu-dev libpng-dev libpq-dev libproj-dev libssl-dev libudunits2-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://packagemanager.rstudio.com/all/__linux__/focal/latest'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site
RUN echo "options(warn = 2);" >> $R_HOME/etc/Rprofile.site

# Dependencies as extracted from DESCRIPTION file as usual
# RUN Rscript -e 'install.packages("remotes")'
# RUN Rscript -e 'remotes::install_version("stringr",upgrade="never", version = "1.4.0")'
# RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.4.2")'
# RUN Rscript -e 'remotes::install_version("magrittr",upgrade="never", version = "2.0.1")'
# RUN Rscript -e 'remotes::install_version("future",upgrade="never", version = "1.21.0")'
# RUN Rscript -e 'remotes::install_version("cartography",upgrade="never", version = "2.4.2")'
# RUN Rscript -e 'remotes::install_github("Thinkr-open/golem@aaae5c8788802a7b4aef4df23691902a286dd964")'

# renv part
RUN Rscript -e 'remotes::install_github("rstudio/renv")'
RUN mkdir /build_zone
WORKDIR /build_zone
COPY renv.lock renv.lock
RUN R -e 'renv::restore()'
ADD . /build_zone

# # RUN Rscript -e 'source("renv/activate.R")'
# # Still need to restore in case sub-dependencies versions are not good
# RUN Rscript -e 'renv::restore(library = .libPaths())'
RUN Rscript -e 'renv::restore();remotes::install_local(repos = "https://packagemanager.rstudio.com/all/__linux__/focal/latest", upgrade = "never")'

# Remove option to warn and stop
# RUN head -n -1 $R_HOME/etc/Rprofile.site
RUN echo "options(warn = -1);" >> $R_HOME/etc/Rprofile.site

EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');source('app.R', echo = TRUE)"
