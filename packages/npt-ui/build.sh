#! /bin/bash

OUTPUTDIR="$1"

SCRIPT_PATH=${0%/*}

cd $SCRIPT_PATH && 

if [ ! -d "$OUTPUTDIR" ]; then
	echo "Error: the output directory ($OUTPUTDIR) doesn't exist"
	exit 1
fi

yarn && yarn run prod-build --env.outputPath=$OUTPUTDIR

