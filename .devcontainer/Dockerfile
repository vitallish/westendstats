ARG RSTATS_DEV_TAG=r4.5.3-20260528
FROM westendstatistics/rstats-dev:${RSTATS_DEV_TAG} AS base

USER root
# Other packages to install - move away from dev when ready
# RUN install2.r --error \
# mice

ARG RSTATS_DEV_TAG
ENV RSTATS_DEV_TAG="${RSTATS_DEV_TAG}"

COPY prompt.sh /tmp/prompt.sh
RUN cat /tmp/prompt.sh >> /home/vscode/.bashrc && rm /tmp/prompt.sh

USER vscode
