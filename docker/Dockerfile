
FROM opensuse/tumbleweed:latest

RUN zypper in -y openssh xclock xauth

RUN echo "root:test" | chpasswd

RUN useradd -m docker 
RUN echo "docker:test" | chpasswd 
RUN usermod -s /bin/bash docker 
# RUN usermod -aG sudo docker 
ENV HOME /home/docker

# ENV XAUTHORITY $HOME/.Xauthority 

EXPOSE 22
