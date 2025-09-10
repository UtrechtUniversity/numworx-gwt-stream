#!/bin/sh
D=s3://test-dwo-nl/jars
cd target/
S=stream.jar
aws --profile prod s3 cp --acl public-read  $S $D/$S

scp $S $USER@gemini.science.uu.nl:/science/wwwprojects/FI-Sites/www-dev/dwo/jars/

