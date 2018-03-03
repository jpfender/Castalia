#! /usr/bin/fish

function common_subs
    sed -i.bak -e '1,/all values/ d' $argv
    sed -i.bak -e '/^\s*$/ d' $argv
    sed -i.bak -e 's/filter//g' $argv
    sed -i.bak -e 's/,/\t/g' $argv
    sed -i.bak -e 's/numSources//g' $argv
    sed -i.bak -e 's/sensDistance//g' $argv
    sed -i.bak -e 's/field//g' $argv
    sed -i.bak -e 's/ | /\t/g' $argv
    sed -i.bak -e '/^83/ d' $argv
    sed -i.bak -e '/^104/ d' $argv
end

set infile $argv

echo "Calculating Base Values..."

cr -i $infile.txt -s "Base values" --sum --all -o 2 > $infile-BaseValues.txt
common_subs $infile-BaseValues.txt
sed -i.bak -e 's/nodes//g' $infile-BaseValues.txt
sed -i.bak -e '1 s/.*/FILTER	NUMSOURCES	SENSINGDISTANCE	RUN	EVENTS	FALSE_NEGATIVES	FALSE_POSITIVES	NEGATIVES	PACKETS_SENT	POSITIVES	TRUE_NEGATIVES	TRUE_POSITIVES/' $infile-BaseValues.txt
sed -i.bak -e 's/ /\t/g' $infile-BaseValues.txt
sed -i.bak -e 's/-/ /g' $infile-BaseValues.txt
gawk -i inplace 'BEGIN{FS="\t";OFS="\t";} {print $4,$1,$2,$3,$5,$6,$7,$8,$9,$10,$11,$12}' $infile-BaseValues.txt
sed -i.bak -e '/\s*0\s*0\s*0\s*0\s*0\s*0/ d' $infile-BaseValues.txt
sed -i.bak -e '/\s*0\s*$/ d' $infile-BaseValues.txt
echo "Produced file: $infile-BaseValues.txt"

sed -e '/Off/ d' $infile-BaseValues.txt > $infile-BaseValues-filterOn.txt
sed -i -e '/Ideal/ d' $infile-BaseValues-filterOn.txt
echo "Produced file: $infile-BaseValues-filterOn.txt"

gawk 'BEGIN{FS="\t";OFS="\t";} {print $1,$2,$3,$4,$9}' $infile-BaseValues.txt > $infile-Packets.txt
echo "Produced file: $infile-Packets.txt"

echo "Calculating RX values..."

cr -i $infile.txt -s "RX" --all -o 2 > $infile-RX.txt
common_subs $infile-RX.txt
sed -i.bak -e 's/nodes//g' $infile-RX.txt
sed -i.bak -e '1 s/.*/FILTER	NUMSOURCES	SENSINGDISTANCE	RUN	FAILED_NO_INT	FAILED_WITH_INT	FAILED_BELOW_SENS	FAILED_NON_RX	RECEIVED_DESPITE_INT	RECEIVED_NO_INT/' $infile-RX.txt
sed -i.bak -e 's/ /\t/g' $infile-RX.txt
sed -i.bak -e 's/-/ /g' $infile-RX.txt
gawk -i inplace 'BEGIN{FS="\t";OFS="\t";} {print $4,$1,$2,$3,$5,$6,$7,$8,$9,$10}' $infile-RX.txt
sed -i.bak -e '/\s*0\s*0\s*0\s*0\s*0\s*0/ d' $infile-RX.txt
echo "Produced file: $infile-RX.txt"

echo "Calculating Buffer Overflow values..."

cr -i $infile.txt -s "Buffer" --all --sum -o 2 > $infile-BO.txt
common_subs $infile-BO.txt
sed -i.bak -e '1 s/.*/FILTER	NUMSOURCES	RUN	SENSDISTANCE30	SENSDISTANCE40	SENSDISTANCE50/' $infile-BO.txt
sed -i.bak -e 's/ /\t/g' $infile-BO.txt
sed -i.bak -e 's/-/ /g' $infile-BO.txt
gawk -i inplace 'BEGIN{FS="\t";OFS="\t";} {print $3,$1,$2,$4,$5,$6}' $infile-BO.txt
echo "Produced file: $infile-BO.txt"

echo "Calculating Event Delivery Rate..."

cr -i $infile.txt -s "Determinants" --all -o 2 > $infile-Determinants.txt
common_subs $infile-Determinants.txt
sed -i.bak -e '1 s/.*/FILTER	NUMSOURCES	RUN	SENSDISTANCE30	SENSDISTANCE40	SENSDISTANCE50/' $infile-Determinants.txt
sed -i.bak -e 's/ /\t/g' $infile-Determinants.txt
sed -i.bak -e 's/-/ /g' $infile-Determinants.txt
gawk -i inplace 'BEGIN{FS="\t";OFS="\t";} {print $3,$1,$2,$4,$5,$6}' $infile-Determinants.txt
echo "Produced file: $infile-Determinants.txt"

cat newplots.r | string replace -r '[0-9][0-9][0-9][0-9][0-9][0-9]-[0-9][0-9][0-9][0-9][0-9][0-9]' $infile > newplots.r.bak
mv newplots.r.bak newplots.r
Rscript newplots.r
