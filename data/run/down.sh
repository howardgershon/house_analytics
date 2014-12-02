for n in `seq 93 113`;
do
    wget -np --no-clobber --reject='index.html*' -l1 --no-check-certificate --convert-links --random-wait -r -p -E -e robots=off -U mozilla https://www.govtrack.us/data/us/$n/stats/
    mv www.govtrack.us/data/us/$n/stats/ ../$n
    rm -R ./www.govtrack.us/
done
