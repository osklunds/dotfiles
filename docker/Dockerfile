
FROM opensuse/tumbleweed:latest

### Zypper

# Not optimal to split perhaps, but it makes additions during development faster
RUN zypper in -y xauth
RUN zypper in -y xclock
RUN zypper in -y firefox
RUN zypper in -y adobe-sourcecodepro-fonts # Prettier fonts in emacs
RUN zypper in -y git
RUN zypper in -y ripgrep
RUN zypper in -y wget
RUN zypper in -y docker
RUN zypper in -y docker-compose
RUN zypper in -y libstdc++-devel
RUN zypper in -y emacs
RUN zypper in -y emacs-x11
RUN zypper in -y rustup
RUN zypper in -y clang
RUN zypper in -y cmake
RUN zypper in -y --force-resolution diff # default doesn't work well with vdiff
RUN zypper in -y ImageMagick
RUN zypper in -y ipa-mincho-fonts # Japanese fonts


### Other programs

RUN rustup default stable && rustup component add rust-analyzer


### Config

RUN usermod -s /bin/bash root


### Dirs and symlinks

# Need to have repos in a path that:
# 1. Can have user permissions on host
# 2. Can be the same in host and container
# So that all relative paths in dockerfiles/compose files inside
# own_repos and others_repos can be resolved when running docker inside docker.
RUN mkdir /docker_root
RUN ln -s /docker_root/own_repos /root/own_repos
RUN ln -s /docker_root/others_repos /root/others_repos

# User configuration
RUN ln -s /root/own_repos/dotfiles/.bashrc /root/.bashrc
RUN ln -s /root/own_repos/dotfiles/.emacs /root/.emacs
RUN ln -s /root/own_repos/dotfiles/.gitconfig /root/.gitconfig
ENV SHELL "/bin/bash"


### Start

CMD /root/own_repos/dotfiles/docker/container_start.sh
