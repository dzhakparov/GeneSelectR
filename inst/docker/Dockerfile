# Use rocker/rstudio as the base image
FROM rocker/rstudio:4.3.1

# System dependencies
RUN apt-get update && apt-get install -y wget \ 
    libxml2-dev \
    r-cran-devtools \
    libxt6 \
    zlib1g-dev \
    libbz2-dev \
    liblzma-dev \
    libpcre3-dev \
    libicu-dev \
    libjpeg-dev \
    libpng-dev \
    libxml2-dev \
    libglpk-dev


# Install Miniconda3 for Python
RUN wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -O ~/miniconda.sh && \
    bash ~/miniconda.sh -b -p /opt/conda && \
    rm ~/miniconda.sh

# Add conda to PATH
ENV PATH="/opt/conda/bin:$PATH"

# Create a conda environment and install Python packages
RUN conda create -n GeneSelectR_env python=3.8 && \
    echo "source activate GeneSelectR_env" > ~/.bashrc && \
    conda install -n GeneSelectR_env -c conda-forge 'numpy<=1.19' 'scikit-learn<=0.22.1' pandas boruta_py scikit-optimize
    
# Install a specific version of RColorBrewer
# RUN R -e "install.packages('https://cran.r-project.org/src/contrib/Archive/RColorBrewer/RColorBrewer_1.1-3.tar.gz', repos = NULL, type = 'source')"

# RUN R -e "install.packages('RColorBrewer')" 
RUN R -e "install.packages('remotes'); remotes::install_github('dzhakparov/GeneSelectR')" 

# Install R dependencies
# RUN R -e "install.packages(c('devtools'), repos = 'http://cran.rstudio.com/')"

# BiocManager and Bioconductor packages
RUN R -e "install.packages(c('BiocManager'), repos = 'http://cran.rstudio.com/')"
# RUN R -e "BiocManager::install(c('clusterProfiler', 'GO.db', 'simplifyEnrichment', 'UpSetR', 'AnnotationHub', 'ensembldb', 'org.Hs.eg.db'))"
# RUN R -e "remotes::install_github('dzhakparov/GeneSelectR')"
# Expose the RStudio port
EXPOSE 8787

CMD ["/init"]

