# Packages
AddPackage ansible # Radically simple IT automation platform
AddPackage boost # Free peer-reviewed portable C++ source libraries (development headers)
AddPackage cblas # C interface to BLAS
AddPackage dbus-python # Python bindings for DBUS
AddPackage cargo-watch # Utility for Cargo to compile projects when sources change
AddPackage crictl # A CLI for CRI-compatible container runtimes
AddPackage critest # A benchmarking CLI for CRI-compatible container runtimes
AddPackage difftastic # An experimental diff tool that compares files based on their syntax
AddPackage docker # Pack, ship and run any application as a lightweight container
AddPackage docker-compose # Fast, isolated development environments using Docker
AddPackage flameshot # Powerful yet simple to use screenshot software
AddPackage gcc # The GNU Compiler Collection - C and C++ frontends
AddPackage gcc-fortran # Fortran front-end for GCC
AddPackage git # the fast distributed version control system
AddPackage git-delta # Syntax-highlighting pager for git and diff output
AddPackage git-filter-repo # Quickly rewrite git repository history (filter-branch replacement)
AddPackage git-lfs # Git extension for versioning large files
AddPackage go # Core compiler tools for the Go programming language
AddPackage gradle # Powerful build system for the JVM
AddPackage helm # The Kubernetes Package Manager
AddPackage imagemagick # An image viewing/manipulation program
AddPackage jdk-openjdk # OpenJDK Java 15 development kit
AddPackage jdk8-openjdk # OpenJDK Java 8 development kit
AddPackage kubeadm # A tool for quickly installing Kubernetes and setting up a secure cluster
AddPackage kubectl # A command line tool for communicating with a Kubernetes API server
AddPackage libgccjit # Just-In-Time Compilation with GCC backend
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
AddPackage postgresql # Sophisticated object-relational DBMS
AddPackage postgresql-old-upgrade # PostgreSQL build for migrating between major versions with pg_upgrade
AddPackage pyenv # Easily switch between multiple versions of Python
AddPackage python-pip # The PyPA recommended tool for installing Python packages
AddPackage python-pipenv # Sacred Marriage of Pipfile, Pip, & Virtualenv.
AddPackage python-poetry # Python dependency management and packaging made easy
AddPackage python-virtualenvwrapper # Extensions to Ian Bicking's virtualenv tool
AddPackage python-bcc # BPF Compiler Collection - Python 3 bindings
AddPackage python-pytorch # Tensors and Dynamic neural networks in Python with strong GPU acceleration (with AVX2 CPU optimizations)
AddPackage reflector # A Python 3 module and script to retrieve and filter the latest Pacman mirror list.
AddPackage rapidjson # Fast JSON parser/generator for C++ with both SAX/DOM style API
AddPackage redis # An in-memory database that persists on disk
AddPackage ruby # An object-oriented language for quick and easy programming
AddPackage ruby-bundler # Manages an application's dependencies through its entire life, across many machines, systematically and repeatably.
AddPackage rustup # The Rust toolchain installer
AddPackage sqlite-analyzer # An analysis program for sqlite3 database files
AddPackage strace # A diagnostic, debugging and instructional userspace tracer
AddPackage terraform # HashiCorp tool for building and updating infrastructure as code idempotently
AddPackage tig # Text-mode interface for Git.
AddPackage tomcat-native # Optional component for Tomcat to use certain native resources for performance, compatibility
AddPackage typescript # TypeScript is a language for application scale JavaScript development
AddPackage vagrant # Build and distribute virtualized development environments
AddPackage visualvm # Visual tool integrating several commandline JDK tools and lightweight profiling capabilities
AddPackage yarn # Fast, reliable, and secure dependency management


# AUR

AddPackage --foreign aws-cli-v2-bin # Universal Command Line Interface for Amazon Web Services version 2
AddPackage --foreign flamegraph # Flame Graphs visualize profiled code
AddPackage --foreign libffi7 # Portable foreign function interface library (ABI version 7)
AddPackage --foreign vpn-slice # vpnc-script replacement for easy and secure split-tunnel VPN setup
AddPackage --foreign wkhtmltopdf-static # Shell utility to convert HTML to PDF using Webkit and Qt (upstream static build)

# Files
CopyFile /etc/ImageMagick-7/policy.xml
CopyFile /etc/minio/minio.conf
CopyFile /etc/nginx/nginx.conf
CopyFile /etc/nginx/sites-enabled/local.tmv.io

SetFileProperty /etc/minio/minio.conf mode 644
SetFileProperty /etc/redis group redis
SetFileProperty /etc/redis mode 775
SetFileProperty /etc/redis/sentinel.conf group redis
SetFileProperty /etc/redis/sentinel.conf mode 664

# Services
CreateLink /etc/systemd/system/multi-user.target.wants/docker.service /usr/lib/systemd/system/docker.service
CreateLink /etc/systemd/system/multi-user.target.wants/memcached@11211.service /usr/lib/systemd/system/memcached@.service
CreateLink /etc/systemd/system/multi-user.target.wants/minio.service /usr/lib/systemd/system/minio.service
CreateLink /etc/systemd/system/multi-user.target.wants/postgresql.service /usr/lib/systemd/system/postgresql.service
CreateLink /etc/systemd/system/multi-user.target.wants/redis.service /usr/lib/systemd/system/redis.service
