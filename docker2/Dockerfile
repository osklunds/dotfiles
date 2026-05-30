
FROM opensuse/tumbleweed:latest

### Zypper

# Not optimal to split perhaps, but it makes additions during development faster
RUN zypper in -y xauth
RUN zypper in -y xclock
RUN zypper in -y git
RUN zypper in -y wget
RUN zypper in -y nautilus

### Config

RUN usermod -s /bin/bash root
ENV SHELL "/bin/bash"

