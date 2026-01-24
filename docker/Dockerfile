
FROM opensuse/tumbleweed:latest

RUN zypper in -y openssh xclock xauth emacs emacs-x11 firefox

RUN useradd -m docker 
ENV HOME /home/docker

# Run this on host
# xhost +local:docker
