
FROM opensuse/tumbleweed:latest

RUN zypper in -y openssh xclock xauth emacs emacs-x11 firefox

RUN ln -s /dotfiles/.bashrc /root/.bashrc

# Run this on host
# xhost +local:docker
