## Documentation 

This repo contains the documentation for the au.id.cxd.math library.

It is able to interactively call the library through R java.

One catch however, rJava on OSX initialises JDK1.6 but we really want JDK1.8

This article explains how to achieve this:

http://paulklemm.com/blog/2017-02-12-fix-onload-failed-rjava-macos/

it uses the reconfiguration command

  sudo R CMD javareconf
  
