def attach [] {
  [STDIN, STDOUT, STDERR]
}

def blkio-weight [] {
  10..100
}

def cgroups [] {
  ["enabled", "disabled", "no-conmon", "split"]
}

def image-volume [] {
  ["bind", "tmpfs", "ignore"]
}

def memory-swappines [] {
  -1..100
}

def oom-score-adj [] {
  -1000..1000
}

def restart [] {
  ["always", "no", "on-failure", "unless-stopped"]
}

def sdnotify [] {
  ["container", "conmon", "ignore"]
}

def systemd [] {
  ["true", "false", "always"]
}

# Run a command in a new container
#
# Description:
#   Runs a command in a new container from the given image
#
# Usage:
#   podman run [options] IMAGE [COMMAND [ARG...]]
#
# Examples:
#   podman run imageID ls -alF /etc
#   podman run --network=host imageID dnf -y install java
#   podman run --volume /var/hostdir:/var/ctrdir -i -t fedora /bin/bash
export extern "main run" [
  --add-host: string                  # Add a custom host-to-IP mapping (host:ip) (default [])
  --annotation: string                # Add annotations to container (key=value)
  --arch: string                      # use ARCH instead of the architecture of the machine for choosing images
  --attach (-a): string@attach        # Attach to STDIN, STDOUT or STDERR
  --authfile: string                  # Path of the authentication file. Use REGISTRY_AUTH_FILE environment variable to override
  --blkio-weight: string@blkio-weight # Block IO weight (relative weight) accepts a weight value between 10 and 1000.
  --blkio-weight-device: string       # Block IO weight (relative device weight, format: DEVICE_NAME:WEIGHT)
  --cap-add: string                   # Add capabilities to the container
  --cap-drop: string                  # Drop capabilities from the container
  --cgroup-conf: string               # Configure cgroup v2 (key=value)
  --cgroup-parent: string             # Optional parent cgroup for the container
  --cgroupns: string                  # cgroup namespace to use
  --cgroups: string@cgroups           # control container cgroup configuration ("enabled"|"disabled"|"no-conmon"|"split")
                                      # (default "enabled")
  --chrootdirs: string                # Chroot directories inside the container
  --cidfile: path                     # Write the container ID to the file
  --conmon-pidfile: path              # Path to the file that will receive the PID of conmon
  --cpu-period: number                # Limit the CPU CFS (Completely Fair Scheduler) period
  --cpu-quota: number                 # Limit the CPU CFS (Completely Fair Scheduler) quota
  --cpu-rt-period: number             # Limit the CPU real-time period in microseconds
  --cpu-rt-runtime: number            # Limit the CPU real-time runtime in microseconds
  --cpu-shares (-c): number           # CPU shares (relative weight)
  --cpus: number                      # Number of CPUs. The default is 0.000 which means no limit
  --cpuset-cpus: string               # CPUs in which to allow execution (0-3, 0,1)
  --cpuset-mems: string               # Memory nodes (MEMs) in which to allow execution (0-3, 0,1). Only effective on NUMA systems.
  --detach (-d)                       # Run container in background and print container ID
  --detach-keys: string               # Override the key sequence for detaching a container. Format is
                                      # a single character [a-Z] or a comma separated sequence of `ctrl-<value>`,
                                      # where `<value>` is one of: `a-cf`, `@`, `^`, `[`, `\`, `]`, `^`
                                      # or `_` (default "ctrl-p,ctrl-q")
  --device: string                    # Add a host device to the container
  --device-cgroup-rule: string        # Add a rule to the cgroup allowed devices list
  --device-read-bps: string           # Limit read rate (bytes per second) from a device (e.g. --device-read-bps=/dev/sda:1mb)
  --device-read-iops: string          # Limit read rate (IO per second) from a device (e.g. --device-read-iops=/dev/sda:1000)
  --device-write-bps: string          # Limit write rate (bytes per second) to a device (e.g. --device-write-bps=/dev/sda:1mb)
  --device-write-iops: string         # Limit write rate (IO per second) to a device (e.g. --device-write-iops=/dev/sda:1000)
  --disable-content-trust             # This is a Docker specific option and is a NOOP
  --dns: string                       # Set custom DNS servers
  --dns-option: string                # Set custom DNS options
  --dns-search: string                # Set custom DNS search domains
  --entrypoint: string                # Overwrite the default ENTRYPOINT of the image
  --env (-e): list                    # Set environment variables in container (default
                                      # [PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin,TERM=xterm])
  --env-file: path                    # Read in a file of environment variables
  --env-host                          # Use all current host environment variables in container
  --env-merge: string                 # Preprocess environment variables from image before injecting them into the container
  --expose: string                    # Expose a port or a range of ports
  --gidmap: string                    # GID map to use for the user namespace
  --group-add: string                 # Add additional groups to the primary container process.
                                      # 'keep-groups' allows container processes to use supplementary groups.
  --health-cmd: string                # set a healthcheck command for the container ('none' disables the existing healthcheck)
  --health-interval: string           # set an interval for the healthchecks (a value of disable results in no automatic timer setup)
                                      # (default "30s")
  --health-on-failure: string         # action to take once the container turns unhealthy (default "none")
  --health-retries: number            # the number of retries allowed before a healthcheck is considered to be unhealthy (default 3)
  --health-start-period: string       # the initialization time needed for a container to bootstrap (default "0s")
  --health-timeout: string            # the maximum time allowed to complete the healthcheck before an interval is
                                      # considered failed (default "30s")
  --help
  --hostname (-h): string             # Set container hostname
  --hostuser: string                  # Host user account to add to /etc/passwd within container
  --http-proxy                        # Set proxy environment variables in the container based on the host proxy vars (default true)
  --image-volume: string@image-volume # Tells podman how to handle the builtin image volumes ("bind"|"tmpfs"|"ignore") (default "bind")
  --init                              # Run an init binary inside the container that forwards signals and reaps processes
  --init-path: path                   # Path to the container-init binary
  --interactive (-i)                  # Keep STDIN open even if not attached
  --ip: string                        # Specify a static IPv4 address for the container
  --ip6: string                       # Specify a static IPv6 address for the container
  --ipc: string                       # IPC namespace to use
  --label (-l): string                # Set metadata on container
  --label-file: path                  # Read in a line delimited file of labels
  --log-driver: string                # Logging driver for the container (default "k8s-file")
  --log-opt: string                   # Logging driver options
  --mac-address: string               # Container MAC address (e.g. 92:d0:c6:0a:29:33)
  --memory (-m): string               # Memory limit (format: <number>[<unit>], where unit = b (bytes), k (kibibytes),
                                      # m (mebibytes), or g (gibibytes))
  --memory-reservation: string        # Memory soft limit (format: <number>[<unit>], where unit = b (bytes), k (kibibytes),
                                      # m (mebibytes), or g (gibibytes))
  --memory-swap: string               # Swap limit equal to memory plus swap: '-1' to enable unlimited swap
  --memory-swappiness: number@memory-swappines # Tune container memory swappiness (0 to 100, or -1 for system default) (default -1)
  --mount: string                     # Attach a filesystem mount to the container
  --name: string                      # Assign a name to the container
  --network: string                   # Connect a container to a network
  --network-alias: string             # Add network-scoped alias for the container
  --no-healthcheck                    # Disable healthchecks on container
  --no-hosts                          # Do not create /etc/hosts within the container, instead use the version from the image
  --oom-kill-disable                  # Disable OOM Killer
  --oom-score-adj: int@oom-score-adj  # Tune the host's OOM preferences (-1000 to 1000)
  --os: string                        # use OS instead of the running OS for choosing images
  --passwd                            # add entries to /etc/passwd and /etc/group (default true)
  --passwd-entry: string              # Entry to write to /etc/passwd
  --personality: string               # Configure execution domain using personality (e.g., LINUX/LINUX32)
  --pid: string                       # PID namespace to use
  --pidfile: string                   # Write the container process ID to the file
  --pids-limit: number                # Tune container pids limit (set -1 for unlimited)
  --platform: string                  # Specify the platform for selecting the image.  (Conflicts with --arch and --os)
  --pod: string                       # Run container in an existing pod
  --pod-id-file: path                 # Read the pod ID from the file
  --preserve-fds: number              # Pass a number of additional file descriptors into the container
  --privileged                        # Give extended privileges to container
  --publish (-p): string              # Publish a container's port, or a range of ports, to the host (default [])
  --publish-all (-P)                  # Publish all exposed ports to random ports on the host interface
  --pull: string                      # Pull image policy (default "missing")
  --quiet (-q)                        # Suppress output information when pulling images
  --read-only                         # Make containers root filesystem read-only
  --read-only-tmpfs                   # When running containers in read-only mode mount a read-write tmpfs on /run,
                                      # /tmp and /var/tmp (default true)
  --replace                           # If a container with the same name exists, replace it
  --requires: string                  # Add one or more requirement containers that must be started before this container will start
  --restart: string@restart           # Restart policy to apply when a container exits ("always"|"no"|"on-failure"|"unless-stopped")
  --rm                                # Remove container (and pod if created) after exit
  --rmi                               # Remove container image unless used by other containers
  --rootfs                            # The first argument is not an image but the rootfs to the exploded container
  --sdnotify: string@sdnotify         # control sd-notify behavior ("container"|"conmon"|"ignore") (default "container")
  --seccomp-policy: string            # Policy for selecting a seccomp profile (experimental) (default "default")
  --secret: string                    # Add secret to container
  --security-opt: string              # Security Options
  --shm-size: string                  # Size of /dev/shm (format: <number>[<unit>], where unit = b (bytes), k (kibibytes),
                                      # m (mebibytes), or g (gibibytes)) (default "65536k")
  --sig-proxy                         # Proxy received signals to the process (default true)
  --stop-signal: string               # Signal to stop a container. Default is SIGTERM
  --stop-timeout: number              # Timeout (in seconds) that containers stopped by user command have to exit. If exceeded,
                                      # the container will be forcibly stopped via SIGKILL. (default 10)
  --subgidname: string                # Name of range listed in /etc/subgid for use in user namespace
  --subuidname: string                # Name of range listed in /etc/subuid for use in user namespace
  --sysctl: string                    # Sysctl options
  --systemd: string@systemd           # Run container in systemd mode ("true"|"false"|"always") (default "true")
  --timeout: number                   # Maximum length of time a container is allowed to run.
                                      # The container will be killed automatically after the time expires.
  --tls-verify                        # Require HTTPS and verify certificates when contacting registries for pulling images
  --tmpfs: string                     # Mount a temporary filesystem (tmpfs) into a container
  --tty (-t)                          # Allocate a pseudo-TTY for container
  --tz: string                        # Set timezone in container
  --uidmap: string                    # UID map to use for the user namespace
  --ulimit: string                    # Ulimit options (default [nproc=32768:32768])
  --umask: string                     # Set umask in container (default "0022")
  --unsetenv: string                  # Unset environment default variables in container
  --unsetenv-all                      # Unset all default environment variables in container
  --user (-u): string                 # Username or UID (format: <name|uid>[:<group|gid>])
  --userns: string                    # User namespace to use
  --uts: string                       # UTS namespace to use
  --variant: string                   # Use VARIANT instead of the running architecture variant for choosing images
  --volume (-v): string               # Bind mount a volume into the container
  --volumes-from: string              # Mount volumes from the specified container(s)
  --workdir (-w): string              # Working directory inside the container
  image: string                       # Container image
  ...command                          # Command to run in the container followed by optional arguments
]
