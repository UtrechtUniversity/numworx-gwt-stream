#!/bin/sh
D=s3://test-dwo-nl/apps
cd target/StreamGWT
S="Stream.css main.js StreamWidget.html"
for i in $S
do aws --profile prod s3 cp --acl public-read  $i $D/
done
scp $S $USER@gemini.science.uu.nl:/science/wwwprojects/FI-Sites/www-dev/dwo/apps/

S=streamgwt
aws --profile prod s3 cp --acl public-read --recursive $S $D/$S

rsync -a --delete $S $USER@gemini.science.uu.nl:/science/wwwprojects/FI-Sites/www-dev/dwo/apps/
