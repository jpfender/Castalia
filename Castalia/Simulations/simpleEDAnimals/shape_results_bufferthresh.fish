#! /usr/bin/fish

function common_subs
    sed -i.bak -e '1,/all values/ d' $argv
    sed -i.bak -e '/^\s*$/ d' $argv
    sed -i.bak -e 's/,/\t/g' $argv
    sed -i.bak -e 's/sampleInterval//g' $argv
    sed -i.bak -e 's/buffer//g' $argv
    sed -i.bak -e 's/ | /\t/g' $argv
    sed -i.bak -e '/^83/ d' $argv
    sed -i.bak -e '/^104/ d' $argv
end

set infile $argv

echo "Calculating Base Values..."

cr -i $infile.txt -s "Base values" --sum --all -o 2 > $infile-BaseValues.txt
common_subs $infile-BaseValues.txt
sed -i.bak -e '1 s/.*/SAMPLE_INTERVAL	BUFFER_THRESHOLD	RUN	EVENTS	FALSE_NEGATIVES	FALSE_POSITIVES	NEGATIVES	PACKETS_SENT	POSITIVES	TRUE_NEGATIVES	TRUE_POSITIVES/' $infile-BaseValues.txt
sed -i.bak -e 's/ /\t/g' $infile-BaseValues.txt
sed -i.bak -e 's/-/ /g' $infile-BaseValues.txt
gawk -i inplace 'BEGIN{FS="\t";OFS="\t";} {print $3,$1,$2,$4,$5,$6,$7,$8,$9,$10,$11}' $infile-BaseValues.txt
sed -i.bak -e '/\s*0\s*0\s*0\s*0\s*0\s*0/ d' $infile-BaseValues.txt
sed -i.bak -e '/\s*0\s*$/ d' $infile-BaseValues.txt
echo "Produced file: $infile-BaseValues.txt"

gawk 'BEGIN{FS="\t";OFS="\t";} {print $1,$2,$3,$4,$8}' $infile-BaseValues.txt > $infile-Packets.txt
echo "Produced file: $infile-Packets.txt"

cat bufthreshplots.r | string replace -r '[0-9][0-9][0-9][0-9][0-9][0-9]-[0-9][0-9][0-9][0-9][0-9][0-9]' $infile > bufthreshplots.r.bak
mv bufthreshplots.r.bak bufthreshplots.r
Rscript bufthreshplots.r
