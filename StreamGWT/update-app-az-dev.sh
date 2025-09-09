set -ex
cd target/StreamGWT
PATH=$PATH:/usr/local/bin
X=echo
X=
if test -f streamgwt/streamgwt.nocache.js
then
# BSD (MacOSX)
#	EXP=$(date -v+1d +%Y-%m-%d)
# posix (ubuntu)
#	EXP=$(date -d 'next day' +%Y-%m-%d)
. ~/aws.env
SAS=$(az storage container generate-sas --account-name numworxacc --name test  --auth-mode key  --permissions dlrw --expiry $EXP --account-key $KEY)
SAS=$(echo $SAS|tr -d '"')

azcopy copy Stream.css https://numworxacc.blob.core.windows.net/test/apps/?"$SAS"
azcopy copy StreamWidget.html https://numworxacc.blob.core.windows.net/test/apps/?"$SAS"
azcopy copy main.js https://numworxacc.blob.core.windows.net/test/apps/?"$SAS"
azcopy copy streamgwt https://numworxacc.blob.core.windows.net/test/apps/?"$SAS" --recursive=true






else
	echo $2 missing in $(pwd)
fi
