FROM mrcide/naomi_base:latest

COPY docker/bin /usr/local/bin/
COPY docker/Rprofile.site /usr/local/lib/R/etc/Rprofile.site

RUN install_packages --repo=https://inla.r-inla-download.org/R/stable \
    INLA

ARG GIT_SHA='UNKNOWN'
ARG GIT_BRANCH='UNKNOWN'
ARG NAOMI_VERSION='UNKNOWN'
ENV NAOMI_GIT_HASH $GIT_SHA
ENV NAOMI_GIT_BRANCH $GIT_BRANCH
ENV NAOMI_VERSION $NAOMI_VERSION

COPY . /src
