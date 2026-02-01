
FROM opensuse/tumbleweed:latest

# Not optimal to split perhaps, but it makes additions during development faster
RUN zypper in -y xauth
RUN zypper in -y xclock
RUN zypper in -y firefox
RUN zypper in -y adobe-sourcecodepro-fonts
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

RUN rustup default stable && rustup component add rust-analyzer

RUN usermod -s /bin/bash root 

RUN ln -s /root/dotfiles/.bashrc /root/.bashrc
RUN ln -s /root/dotfiles/.emacs /root/.emacs
RUN ln -s /root/dotfiles/.gitconfig /root/.gitconfig

ENV SHELL "/bin/bash"

CMD /root/dotfiles/docker/container_start.sh
