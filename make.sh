#!/bin/bash -x
#pandoc -t beamer -f markdown+lhs -i presentation.markdown -o presentation.pdf
pandoc -t slidy -f markdown+lhs typeclasses.markdown.lhs -o typeclasses.html --self-contained
pandoc -t slidy -f markdown+lhs just_haskell_or_nothing.markdown.lhs -o just_haskell_or_nothing.html --self-contained
pandoc -t slidy -f markdown flawless_feature_release_at_enterprise_internet_companies.markdown -o flawless_feature_release_at_enterprise_internet_companies.html --self-contained
