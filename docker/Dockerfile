
FROM opensuse/tumbleweed:latest

RUN zypper in -y openssh xclock xauth emacs emacs-x11 firefox \
    git ripgrep adobe-sourcecodepro-fonts

RUN usermod -s /bin/bash root 

# By the way, after cloning this repo, these are the only symlinks you need to
# do to "install" this repo.
RUN ln -s /dotfiles/.bashrc /root/.bashrc
RUN ln -s /dotfiles/.emacs /root/.emacs

# Run this on host
# xhost +local:docker
