#!/usr/bin/env sh

for i in input/*.xml 
do
    sh test/try.sh "$i"
done

