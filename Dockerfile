# FROM plotly/heroku-docker-r:3.6.2_heroku18
# FROM virtualstaticvoid/heroku-docker-r:build
# FROM virtualstaticvoid/heroku-docker-r:4.1.0-build
FROM rstudio/r-base:devel-focal
# FROM rocker/r-bspm:testing
# FROM eddelbuettel/r2u
RUN apt update -qq

COPY . /app/

RUN if [ -f '/app/apt-packages' ]; then apt-get update -q && cat /app/apt-packages | xargs apt-get -qy install && rm -rf /var/lib/apt/lists/*; fi;              

RUN apt install --yes --no-install-recommends gpg-agent  	# to add the key
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys A1489FE2AB99A21A

## Second: add the repo
RUN echo "deb [arch=amd64] https://dirk.eddelbuettel.com/cranapt focal main" > /etc/apt/sources.list.d/cranapt.list
RUN apt update

## Third: ensure R 4.2.0 is used
RUN echo "deb [arch=amd64] https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/" > /etc/apt/sources.list.d/edd-misc.list
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 67C2D66C4B1D4339

## Fourth: add pinning to ensure package sorting
RUN echo "Package: *" > /etc/apt/preferences.d/99cranapt
RUN echo "Pin: release o=CRAN-Apt Project" >> /etc/apt/preferences.d/99cranapt
RUN echo "Pin: release l=CRAN-Apt Packages" >> /etc/apt/preferences.d/99cranapt
RUN echo "Pin-Priority: 700"  >> /etc/apt/preferences.d/99cranapt

## Fifth: install bspm and enable it
RUN Rscript -e 'install.packages("bspm")'
RUN RHOME=$(R RHOME)
RUN echo "suppressMessages(bspm::enable())" >> ${RHOME}/etc/Rprofile.site
RUN echo "options(bspm.sudo=TRUE)" >> ${RHOME}/etc/Rprofile.site
#
# on build, copy application files
#COPY . /app/

#run apt install --yes --no-install-recommends wget; wget -q -O- https://eddelbuettel.github.io/r2u/assets/dirk_eddelbuettel_key.asc \
#    | tee -a /etc/apt/trusted.gpg.d/cranapt_key.asc
    
#run echo "deb [arch=amd64] https://dirk.eddelbuettel.com/cranapt focal main" \
#    > /etc/apt/sources.list.d/cranapt.list; apt update
# for installing additional dependencies etc.
RUN if [ -f '/app/onbuild' ]; then bash /app/onbuild; fi; 

# look for /app/apt-packages and if it exists, install the packages contained
#RUN if [ -f '/app/apt-packages' ]; then apt-get update -q && cat /app/apt-packages | xargs apt-get -qy install && rm -rf /var/lib/apt/lists/*; fi;              

# look for /app/init.R and if it exists, execute it
RUN if [ -f '/app/init.R' ]; then /usr/bin/R --no-init-file --no-save --quiet --slave -f /app/init.R; fi; 

# here app.R needs to match the name of the file which contains your app              
CMD cd /app && /usr/bin/R --no-save -f /app/app.R
