#!/bin/sh

rm -f version.ml
cd ../../src/ml
echo "let genes = \"`wc -c *.ml *.mll *.mly|grep total|sed -e 's/total//g' -e 's/ //g'`\";;" > ../../build/ml/version.ml
echo "let compile_ver = \"`date`\";;" >> ../../build/ml/version.ml
echo "let compile_mac = \"`uname -a`\";;" >> ../../build/ml/version.ml
echo "let compile_use = \"`whoami`\";;" >> ../../build/ml/version.ml
echo "let mod_versions = [" >> ../../build/ml/version.ml
grep "\$VERSION"  *.ml *.mll *.mly | \
     sed -f ../../build/ml/mkver.sed >> ../../build/ml/version.ml
cd -
echo "];;" >> version.ml
