FROM rocker/r-ver:3.6.0

RUN apt-get update && apt-get -y install \
        libcurl4-openssl-dev \
        libgdal-dev \
        libprotobuf-dev \
        libssh-dev \
        libudunits2-dev \
        libv8-dev \
        libxml2-dev \
        protobuf-compiler \
        zlib1g-dev

COPY docker/bin /usr/local/bin/
RUN sed  -i'' '/mran.microsoft.com/d' /usr/local/lib/R/etc/Rprofile.site

RUN install_packages --repo=https://mrc-ide.github.io/drat \
    DiagrammeR \
    Matrix \
    R6 \
    RcppEigen \
    TMB \
    assertthat \
    covr \
    data.tree \
    dplyr \
    forcats \
    ggplot2 \
    gridExtra \
    here \
    knitr \
    lubridate \
    magrittr \
    mockery \
    mvtnorm \
    rdhs \
    readr \
    rlang \
    rmarkdown \
    scales \
    sf \
    spdep \
    specio \
    survey \
    testthat \
    tidyr \
    tidyverse \
    tmbstan \
    traduire \
    viridis \
    withr \
    yaml \
    zip \
    zoo

RUN install_packages --repo=https://inla.r-inla-download.org/R/stable \
    INLA

RUN install_packages remotes && install_remote \
    bergant/datamodelr \
    mrc-ide/eppasm \
    reside-ic/traduire

COPY . /src
