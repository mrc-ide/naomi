FROM mrcide/naomi-base:latest

COPY docker/bin /usr/local/bin/

ARG GIT_SHA='UNKNOWN'
ARG GIT_BRANCH='UNKNOWN'
ARG NAOMI_VERSION='UNKNOWN'
ENV NAOMI_GIT_HASH $GIT_SHA
ENV NAOMI_GIT_BRANCH $GIT_BRANCH
ENV NAOMI_VERSION $NAOMI_VERSION

RUN install_packages --repo=https://mrc-ide.r-universe.dev \
    mockr \
    rvest \
    pkgbuild \
    testthat.buildkite \
    openxlsx

RUN install_remote \
    mrc-ide/naomi.resources

## Model run will try to parallelise over as many threads as are available
## potentially slowing the application, manually limit threads to 1
ENV OMP_NUM_THREADS=1

COPY . /src
