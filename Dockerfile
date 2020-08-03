#Download base image haskell 8.8
FROM haskell:8.8

###############################################################################
# Install dependencies

RUN apt-get update && apt-get install -y \
  vim \
  less \
  libblas-dev \
  liblapack-dev

# Welcome message
RUN echo '[ ! -z "$TERM" -a -r /etc/welcome ] && cat /etc/welcome' \
    >> /etc/bash.bashrc \
    ; echo "\
===================================================================\n\
= Artifact: CAMP: Cost-Aware Multiparty Session Protocols         =\n\
===================================================================\n\
\n\
This is the artifact for the paper 'CAMP: Cost-Aware Multiparty \n\
Session Protocols' .\n\
\n\
PWD=/home/oopsla20-artifact/CAMP \n\
\n\
This directory contains the source code for our tool, as well as \n\
the benchmark data, and protocols used in our paper. Moreover, we \n\
provide the scripts that we used for performing interpolation of  \n\
sequential costs, and deriving the cost equations that we use in our \n\
examples. Please find in README.md a more detailed description. \n\
\n\
  * README.md  ............. Description of this artifact \n\
  * LICENSE  ............... BSD-3 License for our code \n\
  * run.sh  ................ Generate the predicted cost table \n\
  * src  ................... Source code of the libraries \n\
  * app  ................... Source code of the scripts \n\
  * examples  .............. Source code of the protocols \n\
  * benchmark_data  ........ Data used in the paper \n\
\n"\
    > /etc/welcome

###############################################################################
# Artifact user

# Add oopsla20-artifact user
RUN useradd -ms /bin/bash oopsla20-artifact

###############################################################################
# Download and build artifact

USER oopsla20-artifact

RUN echo 'export PATH=/opt/ghc/8.6.5/bin/:/home/oopsla20-artifact/.local/bin:$PATH' >> \
      /home/oopsla20-artifact/.bashrc && \
    mkdir -p /home/oopsla20-artifact/.vim/autoload ~/.vim/bundle && \
    curl -LSso /home/oopsla20-artifact/.vim/autoload/pathogen.vim \
      https://tpo.pe/pathogen.vim && \
    echo 'execute pathogen#infect()' >> /home/oopsla20-artifact/.vimrc && \
    echo 'syntax on' >> /home/oopsla20-artifact/.vimrc && \
    echo 'filetype plugin indent on' >> /home/oopsla20-artifact/.vimrc && \
    echo 'colorscheme default' >> /home/oopsla20-artifact/.vimrc && \
    git clone https://github.com/neovimhaskell/haskell-vim.git \
       /home/oopsla20-artifact/.vim/bundle/haskell-vim && \
    git clone https://github.com/camp-cost/camp \
       /home/oopsla20-artifact/CAMP && \
    echo 'system-ghc: true' >> /home/oopsla20-artifact/CAMP/stack.yaml

WORKDIR /home/oopsla20-artifact/CAMP
RUN stack build
ENV TERM xterm-256color

CMD ["/bin/bash", "-l"]
