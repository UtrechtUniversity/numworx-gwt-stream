#!/bin/sh

cd target/StreamGWT
S="Stream.css main.js StreamWidget.html" 
cp $S $HOME/Public/apps/
S=streamgwt
rm -rf $HOME/Public/apps/$S
cp -r  $S $HOME/Public/apps/
