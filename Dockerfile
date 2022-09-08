# FROM plotly/heroku-docker-r:3.6.2_heroku18
# FROM virtualstaticvoid/heroku-docker-r:build
# FROM virtualstaticvoid/heroku-docker-r:4.1.0-build
# FROM rstudio/r-base:devel-focal
# FROM rocker/r-bspm:testing
FROM eddelbuettel/r2u:focal

#
# on build, copy application files
COPY . /app/

#run apt install --yes --no-install-recommends wget; wget -q -O- https://eddelbuettel.github.io/r2u/assets/dirk_eddelbuettel_key.asc \
#    | tee -a /etc/apt/trusted.gpg.d/cranapt_key.asc
    
#run echo "deb [arch=amd64] https://dirk.eddelbuettel.com/cranapt focal main" \
#    > /etc/apt/sources.list.d/cranapt.list; apt update
# for installing additional dependencies etc.
RUN if [ -f '/app/onbuild' ]; then bash /app/onbuild; fi; 

# look for /app/apt-packages and if it exists, install the packages contained
RUN if [ -f '/app/apt-packages' ]; then apt-get update -q && cat /app/apt-packages | xargs apt-get -qy install && rm -rf /var/lib/apt/lists/*; fi;              

# look for /app/init.R and if it exists, execute it
RUN if [ -f '/app/init.R' ]; then /usr/bin/R --no-init-file --no-save --quiet --slave -f /app/init.R; fi; 

# here app.R needs to match the name of the file which contains your app              
CMD cd /app && /usr/bin/R --no-save -f /app/app.R
