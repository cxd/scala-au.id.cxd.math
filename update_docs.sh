#!/usr/bin/env bash
rsync --delete -azzv /Users/cd/Projects/scala/au.id.cxd.math/target/scala-2.11/api ./latest/
git add .
git commit -m "updated documents" -a
git push origin gh-pages