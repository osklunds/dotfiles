
FROM opensuse/tumbleweed:latest

### Credits
# https://www.redhat.com/en/blog/podman-inside-container
# https://samuel.forestier.app/blog/security/podman-rootless-in-podman-rootless-the-debian-way

### Zypper

# Not optimal to split perhaps, but it makes additions during development faster
RUN zypper in -y xauth
RUN zypper in -y xclock
RUN zypper in -y git
RUN zypper in -y wget
RUN zypper in -y nautilus
RUN zypper in -y podman
RUN zypper in -y podman-compose
RUN zypper in -y sudo
RUN zypper in -y crun
RUN zypper --non-interactive in --force-resolution -t pattern xfce
RUN zypper in -y xinit
RUN zypper in -y tigervnc
RUN zypper in -y dbus-launch

### User config

RUN useradd podman -s /bin/bash

RUN echo podman:1001:5000 > /etc/subuid;
RUN echo podman:1001:5000 > /etc/subgid;

RUN echo "podman ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/podman && \
    chmod 0440 /etc/sudoers.d/podman

### Podman config

# Copied from https://raw.githubusercontent.com/containers/image_build/refs/heads/main/podman/containers.conf
RUN cat > /etc/containers/containers.conf <<'EOF'
[containers]
netns="host"
userns="host"
ipcns="host"
utsns="host"
cgroupns="host"
cgroups="disabled"
log_driver = "k8s-file"
[engine]
cgroup_manager = "cgroupfs"
events_logger="file"
runtime="crun"
EOF

# Copied from https://raw.githubusercontent.com/containers/image_build/refs/heads/main/podman/podman-containers.conf
# But critical change: added runtime="crun"
RUN mkdir -p /home/podman/.config/containers/
RUN cat > /home/podman/.config/containers/containers.conf <<'EOF'
[containers]
volumes = [
	"/proc:/proc",
]
default_sysctls = []
[engine]
runtime="crun"
EOF

### VNC and XFCE4

# RUN mkdir -p /home/podman/.vnc
# RUN cat > /home/podman/.vnc/xstartup << 'EOF'
# #!/bin/sh
# unset SESSION_MANAGER
# unset DBUS_SESSION_BUS_ADDRESS
# exec startxfce4
# EOF

# RUN chmod +x /home/podman/.vnc/xstartup
# RUN printf "mypassword\nmypassword\nn\n" | vncpasswd

# CMD ["vncserver", ":1", "-geometry", "1280x800", "-depth", "24", "-fg"]

CMD ["/sbin/init"]

### Needs to be last

# USER podman
