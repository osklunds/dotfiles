
FROM opensuse/tumbleweed:latest

### Zypper

# Not optimal to split perhaps, but it makes additions during development faster
RUN zypper in -y xauth
RUN zypper in -y xclock
RUN zypper in -y adobe-sourcecodepro-fonts # Prettier fonts in emacs
RUN zypper in -y git
RUN zypper in -y ripgrep
RUN zypper in -y wget
RUN zypper in -y docker
RUN zypper in -y docker-compose
RUN zypper in -y libstdc++-devel
RUN zypper in -y gcc-c++
RUN zypper in -y emacs
RUN zypper in -y emacs-x11
RUN zypper in -y rustup
RUN zypper in -y clang
RUN zypper in -y cmake
RUN zypper in -y --force-resolution diff # default doesn't work well with vdiff
RUN zypper in -y ImageMagick
RUN zypper in -y ipa-mincho-fonts # Japanese fonts, still not working though
RUN zypper in -y vim # in case emacs doesn't start, as backup

RUN zypper in -y firefox
RUN zypper in -y nautilus

### Other programs

RUN rustup default stable && rustup component add rust-analyzer


### Config

RUN usermod -s /bin/bash root
ENV SHELL "/bin/bash"

