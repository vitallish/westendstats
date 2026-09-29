ARG RSTATS_DEV_TAG=r4.5.3-20260528
FROM westendstatistics/rstats-dev:${RSTATS_DEV_TAG} AS base

USER root
# xz-utils is required to unpack the arf installer's tar.xz archive
RUN apt-get update && apt-get install -y --no-install-recommends xz-utils \
  && rm -rf /var/lib/apt/lists/*
USER vscode

# install arf
RUN curl --proto '=https' --tlsv1.2 -LsSf https://github.com/eitsupi/arf/releases/latest/download/arf-console-installer.sh | sh

USER root
# Other packages to install - move away from dev when ready
RUN install2.r --error \
  broom.helpers \
  jgd
RUN Rscript -e "install.packages('sess', repos = c('https://reditorsupport.r-universe.dev', getOption('repos')))"

ARG RSTATS_DEV_TAG
ENV RSTATS_DEV_TAG="${RSTATS_DEV_TAG}"

COPY prompt.sh /tmp/prompt.sh
RUN cat /tmp/prompt.sh >> /home/vscode/.bashrc && rm /tmp/prompt.sh

USER vscode
