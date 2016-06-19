#!/usr/bin/env bash
echo Create scaladoc
cd /Users/cd/Projects/scala/au.id.cxd.math/
sbt doc

echo Copy to site
cd /Users/cd/Projects/scala/scala-au.id.cxd.math-gh-pages/scala-au.id.cxd.math/
pwd
rsync --delete -azzv /Users/cd/Projects/scala/au.id.cxd.math/math/target/scala-2.11/api ./latest/math/
git add .
git commit -m "updated documents" -a
git push origin gh-pages