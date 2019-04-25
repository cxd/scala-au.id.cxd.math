## Running Scala in Markdown.

R has the ability to customise different engines, and has support for scala.

This works without jvmr.

When running the jvm itself, it is possible to use jenv and set the local environment of the project to use the jdk1.8.  

When running scala, it is possible to choose different versions of scala by customising the path. 

For example:

```
{r, engine="scala", message=FALSE, results='hide', engine.path='/usr/local/opt/scala@2.11/bin/scala',  engine.opts='-savecompiled -classpath /Users/cd/Projects/scala/scala-au.id.cxd.math-gh-pages/scala-au.id.cxd.math/target/*.jar', cache=TRUE}
```

The above declaration defines the scala engine at a different path to the default install. Allowing older versions of scala to be used.

Another alternate method of running scala in notebooks is given by rscala. In which case configuration of the environment will use the scala found on the path. This project has been updated to support scala 2.12 hence the main assembly jar is built for both 2.11 and 2.12.

The overall procedure is described here:

https://www.zstat.pl/2018/07/27/scala-in-knitr/

Additionally the configuration of the rJava runtime may, on OSX have some difficulties with defining the right rJava.so link. The process of resolving this is described here:
https://github.com/s-u/rJava/issues/151

In short if using java 10 it may be necessary to:

```
install_name_tool -change   /Library/Java/JavaVirtualMachines/jdk-9.jdk/Contents/Home/lib/server/libjvm.dylib   /Library/Java/JavaVirtualMachines/jdk-10.0.2.jdk/Contents/Home/lib/server/libjvm.dylib /Library/Frameworks/R.framework/Resources/library/rJava/libs/rJava.so
```

and

```
sudo R CMD javareconf
```

Then deploy rJava and rscala.

Note if the environment is configured for scala 2.12 the dependencies in use need to be compiled for that target.

