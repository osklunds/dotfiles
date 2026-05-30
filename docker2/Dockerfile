
FROM opensuse/tumbleweed:latest

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

RUN useradd podman -s /bin/bash
RUN echo podman:1001:5000 > /etc/subuid;
RUN echo podman:1001:5000 > /etc/subgid;

RUN echo "podman ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/podman && \
    chmod 0440 /etc/sudoers.d/podman

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

RUN mkdir -p /home/podman/.local/share/containers && \
    chown podman:podman -R /home/podman && \
    chmod 0644 /etc/containers/containers.conf
# VOLUME /home/podman/.local/share/containers

USER podman

