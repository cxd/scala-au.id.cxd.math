#!/bin/bash

pandoc -f latex -t html au.id.cxd.math.tex > au-id-cxd-math.html
echo "<script type=\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>" >> au-id-cxd-math.html
