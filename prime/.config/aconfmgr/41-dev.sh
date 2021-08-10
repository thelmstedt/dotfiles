# Packages
AddPackage ansible # Radically simple IT automation platform
AddPackage cblas # C interface to BLAS
AddPackage dbus-python # Python bindings for DBUS
AddPackage crictl # A CLI for CRI-compatible container runtimes
AddPackage critest # A benchmarking CLI for CRI-compatible container runtimes
AddPackage docker # Pack, ship and run any application as a lightweight container
AddPackage docker-compose # Fast, isolated development environments using Docker
AddPackage flameshot # Powerful yet simple to use screenshot software
AddPackage gcc # The GNU Compiler Collection - C and C++ frontends
AddPackage gcc-fortran # Fortran front-end for GCC
AddPackage git # the fast distributed version control system
AddPackage git-filter-repo # Quickly rewrite git repository history (filter-branch replacement)
AddPackage git-lfs # Git extension for versioning large files
AddPackage gradle # Powerful build system for the JVM
AddPackage helm # The Kubernetes Package Manager
AddPackage imagemagick # An image viewing/manipulation program
AddPackage jdk-openjdk # OpenJDK Java 15 development kit
AddPackage jdk8-openjdk # OpenJDK Java 8 development kit
AddPackage kubeadm # A tool for quickly installing Kubernetes and setting up a secure cluster
AddPackage kubectl # A command line tool for communicating with a Kubernetes API server
AddPackage make # GNU make utility to maintain groups of programs
AddPackage maven # Java project management and project comprehension tool
AddPackage memcached # Distributed memory object caching system
AddPackage minikube # A tool that makes it easy to run Kubernetes locally
AddPackage minio # Object storage server compatible with Amazon S3
AddPackage nginx # Lightweight HTTP server and IMAP/POP3 proxy server
AddPackage nodejs # Evented I/O for V8 javascript
AddPackage npm # A package manager for javascript
AddPackage openblas # An optimized BLAS library based on GotoBLAS2 1.13 BSD
AddPackage packer # tool for creating identical machine images for multiple platforms from a single source configuration
AddPackage patch # A utility to apply patch files to original sources
AddPackage perf # Linux kernel performance auditing tool
AddPackage pgbouncer # Lightweight connection pooler for PostgreSQL
AddPackage postgresql # Sophisticated object-relational DBMS
AddPackage pyenv # Easily switch between multiple versions of Python
AddPackage python-pip # The PyPA recommended tool for installing Python packages
AddPackage python-pipenv # Sacred Marriage of Pipfile, Pip, & Virtualenv.
AddPackage python-poetry # Python dependency management and packaging made easy
AddPackage python-virtualenvwrapper # Extensions to Ian Bicking's virtualenv tool
AddPackage python2-pip # The PyPA recommended tool for installing Python packages
AddPackage python2-virtualenv # Virtual Python Environment builder
AddPackage reflector # A Python 3 module and script to retrieve and filter the latest Pacman mirror list.
AddPackage ruby # An object-oriented language for quick and easy programming
AddPackage ruby-bundler # Manages an application's dependencies through its entire life, across many machines, systematically and repeatably.
AddPackage rustup # The Rust toolchain installer
AddPackage strace # A diagnostic, debugging and instructional userspace tracer
AddPackage tig # Text-mode interface for Git.
AddPackage tomcat-native # Optional component for Tomcat to use certain native resources for performance, compatibility
AddPackage vagrant # Build and distribute virtualized development environments
AddPackage visualvm # Visual tool integrating several commandline JDK tools and lightweight profiling capabilities


# AUR

AddPackage --foreign aws-cli-v2-bin # Universal Command Line Interface for Amazon Web Services version 2
AddPackage --foreign git-delta-bin # A viewer for git and diff output
AddPackage --foreign vpn-slice # vpnc-script replacement for easy and secure split-tunnel VPN setup
AddPackage --foreign wkhtmltopdf-static # Shell utility to convert HTML to PDF using Webkit and Qt (upstream static build)

# Files
CopyFile /etc/ansible/ansible.cfg
CopyFile /etc/ImageMagick-7/policy.xml
CopyFile /etc/minio/minio.conf
CopyFile /etc/nginx/nginx.conf
CopyFile /etc/nginx/sites-enabled/local.tmv.io
CopyFile /etc/systemd/system/memcached.service.d/override.conf

# Services
CreateLink /etc/systemd/system/multi-user.target.wants/docker.service /usr/lib/systemd/system/docker.service
CreateLink /etc/systemd/system/multi-user.target.wants/memcached.service /usr/lib/systemd/system/memcached.service
CreateLink /etc/systemd/system/multi-user.target.wants/minio.service /usr/lib/systemd/system/minio.service
CreateLink /etc/systemd/system/multi-user.target.wants/nginx.service /usr/lib/systemd/system/nginx.service
CreateLink /etc/systemd/system/multi-user.target.wants/postgresql.service /usr/lib/systemd/system/postgresql.service