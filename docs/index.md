MANUAL
======
[![Build Status](https://travis-ci.org/DANS-KNAW/easy-deposit-api.png?branch=master)](https://travis-ci.org/DANS-KNAW/easy-deposit-api)

SYNOPSIS
--------

    easy-deposit-api run-service
                          

DESCRIPTION
-----------
This service offers a JSON-based deposit API that lets the client manage deposits on behalf of an EASY
account. An important use case for this is the [easy-deposit-ui]. However, this service could also be
used by other clients.

For details about the service API see the [OpenAPI specification].

[OpenAPI specification]: ./api.html
[easy-deposit-ui]: https://github.com/DANS-KNAW/easy-deposit-ui

ARGUMENTS
---------

          --help                Show help message
          --version             Show version of this program

    Subcommand: run-service - Starts the EASY Bag Store as a daemon that services
                              HTTP requests
          --help   Show help message
    ---


INSTALLATION AND CONFIGURATION
------------------------------
The preferred way of install this module is using the RPM package. This will install the binaries to
`/opt/dans.knaw.nl/easy-deposit-api` and the configuration files to `/etc/opt/dans.knaw.nl/easy-deposit-api`.

To install the module on systems that do not support RPM, you can copy and unarchive the tarball to the target host.
You will have to take care of placing the files in the correct locations for your system yourself. For instructions
on building the tarball, see next section.


BUILDING FROM SOURCE
--------------------

Prerequisites:

* Java 8 or higher
* Maven 3.3.3 or higher
* RPM 

Steps:

    git clone https://github.com/DANS-KNAW/easy-deposit-api.git
    cd easy-deposit-api
    mvn install

If the `rpm` executable is found at `/usr/local/bin/rpm`, the build profile that includes the RPM 
packaging will be activated. If `rpm` is available, but at a different path, then activate it by using
Maven's `-P` switch: `mvn -Pprm install`.

Alternatively, to build the tarball execute:

    mvn clean install assembly:single
