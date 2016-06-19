#!/usr/bin/env bash
rsync --delete -azzv /Users/cd/Projects/scala/au.id.cxd.math/math/target/scala-2.11/api ./latest/math
git add .
git commit -m "updated documents" -a
git push origin gh-pages