#!/bin/bash
#./freddypro.sh remove
## sed -i 's|suppressPackageStartupMessages|#suppressPackageStartupMessages|g' ~/.Rprofile
## sed -i 's|source|#source|g' ~/.Rprofile
R --vanilla CMD build fr3PG
## package=`ls *.tar*`
## R --vanilla CMD check --as-cran $package
## #./freddypro.sh install
## sed -i 's|#||g' ~/.Rprofile
## cp -r FREddyPro/ $github/FREddyPro/
cp -r fr3PG/* ../fr3PG
