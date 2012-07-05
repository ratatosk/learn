#!/bin/sh

# script to automatically convert pictures to be used for training
# uses "convert" from imagemagick package.

SRC="$1"
DST="$2"

if [ -z "$DST" -o -z "$SRC" ]; then
    echo "Wrong usage"
    exit 1
fi

I=0

for FROM in $SRC/*; do
    FILENAME=`basename "$FROM" | sed 's/\.[^\.]*$//'`
    TO="$DST/$FILENAME.tiff"
    echo "converting $FROM to $TO..."
    convert "$FROM" -resize 32x32\! -colorspace Gray "$TO"
done