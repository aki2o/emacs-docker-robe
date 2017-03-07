# What's this?

This is a extension of Emacs that let robe work under docker container.  
You'll be able to develop using robe under docker container in the following conditions.  
-   Developing in host with volume mounted to the docker container which runs the product code.
-   The docker container expose a port for docker-robe.el
-   Host configures a port forwarding to the docker container port.
-   It's able to copy `robe-ruby-path` into the docker container same path using docker command.

# Install

### If use package.el

2017/03/07 Not yet regist.  

### If use el-get.el

2017/03/07 Not yet regist.  

### If use auto-install.el

```lisp
(auto-install-from-url "https://raw.github.com/aki2o/emacs-docker-robe/master/docker-robe.el")
```
-   In this case, you need to install each of the following dependency.

### Manually

Download docker-robe.el and put it on your load-path.  
-   In this case, you need to install each of the following dependency.

### Dependency

-   [inf-ruby.el](https://github.com/nonsequitur/inf-ruby)
-   [robe.el](https://github.com/dgutov/robe)

1.  robe.el version

    This extension requires the robe.el version includes <https://github.com/dgutov/robe/pull/100>.

# Configuration

```lisp
(require 'docker-robe)
(docker-robe:activate)
```

# Usage

You do not need anthing else using robe.  
docker-robe.el appends the following few steps into `robe-start`.  
1.  select container
    -   select the docker container names which product code run in.

2.  select port
    -   select a exposed port of the container.

3.  config source volume mapping
    -   whether to map the mount path on the container to local path.
    -   If mapping the path, input the local path.
