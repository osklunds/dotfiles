
FROM opensuse/tumbleweed:latest

RUN zypper in -y openssh xclock xauth emacs emacs-x11 firefox \
    git ripgrep adobe-sourcecodepro-fonts

RUN usermod -s /bin/bash root 

RUN ln -s /root/dotfiles/.bashrc /root/.bashrc
RUN ln -s /root/dotfiles/.emacs /root/.emacs
RUN ln -s /root/dotfiles/.gitconfig /root/.gitconfig

ENV SHELL "/bin/bash"

CMD /root/dotfiles/docker/container_start.sh
