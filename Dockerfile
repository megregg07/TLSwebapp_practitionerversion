FROM rocker/shiny:4.5.1

RUN apt-get update -y
RUN apt-get install libglu1-mesa -y
#RUN apt-get install make -y
RUN apt-get install libnlopt-dev -y

RUN apt-get update -y && apt-get install -y \
    texlive-latex-recommended \
    texlive-fonts-recommended \
    texlive-latex-extra
    
WORKDIR /srv/shiny-server/

RUN R -e "install.packages('pbkrtest',repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('car',repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('lme4',repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shiny',repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinythemes',repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ICSNP',repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('heplots',repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('dplyr',repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('geometry',repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('MASS',repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggplot2',repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('tidyr',repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('tools',repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('patchwork',repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('Cairo',repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('stringr',repos='http://cran.rstudio.com/')"

# still need to debug command below
#RUN R -e "tinytex::install_tinytex(force=TRUE)"


COPY . /srv/shiny-server

RUN chmod -R 775 /srv/shiny-server
RUN chgrp -R shiny /srv/shiny-server

EXPOSE 3838
