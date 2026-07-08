FROM openanalytics/r-ver:4.5.2

# Force the container to use UTF-8 encoding so it recognizes Unicode characters
ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8

RUN echo "\noptions(shiny.port=3838, shiny.host='0.0.0.0')" >> /usr/local/lib/R/etc/Rprofile.site

# System libraries (Added fonts-dejavu, fonts-liberation, and fontconfig)
RUN apt-get update && apt-get install --no-install-recommends -y \
pandoc \
libcairo2-dev \
libxt-dev \
libssl-dev \
libcurl4-openssl-dev \
libxml2-dev \
libglu1-mesa-dev \
libgl1-mesa-dev \
chromium \
texinfo \
texlive-latex-base \
texlive-latex-recommended \
texlive-latex-extra \
texlive-fonts-recommended \
texlive-extra-utils \
fonts-dejavu \
fonts-liberation \
fontconfig \
&& rm -rf /var/lib/apt/lists/*

# Add all required R packages
RUN R -q -e "options(warn=2); install.packages(c('shiny', 'shinythemes', 'shinyjs', 'DT', 'ICSNP', 'heplots', 'dplyr', 'geometry', 'MASS', 'ggplot2', 'tidyr', 'patchwork', 'Cairo', 'stringr', 'RColorBrewer', 'kableExtra'))"

# Install R code
COPY . /app
WORKDIR /app

EXPOSE 3838

CMD ["R", "-q", "-e", "shiny::runApp('/app')"]