#!/bin/bash

for n in `seq 93 113`;
do
    wget -np --no-clobber --reject='index.html*' -l1 --no-check-certificate --convert-links --random-wait -r -p -E -e robots=off -U mozilla https://www.govtrack.us/data/us/$n/people.xml
    mv www.govtrack.us/data/us/$n/people.xml ../people/people$n.xml
    rm ./www.govtrack.us
done
