FROM rocker/tidyverse:latest

## create directories
RUN mkdir -p /telegramBot

## copy files
COPY /coursebot/install.R /coursebot/install.R
COPY /coursebot/init.R /coursebot/init.R
COPY /coursebot/texts /coursebot/texts
COPY /coursebot/keys /coursebot/keys
COPY .Renviron .Renviron

## install R-packages
RUN Rscript /coursebot/install.R
CMD Rscript /coursebot/init.R
