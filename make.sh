#!/bin/bash -x
#pandoc -t beamer -f markdown+lhs -i presentation.markdown -o presentation.pdf
pandoc -t slidy -f markdown+lhs typeclasses.markdown.lhs -o typeclasses.html --self-contained
