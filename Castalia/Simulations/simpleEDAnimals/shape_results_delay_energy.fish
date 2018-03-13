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

echo "Calculating Delay..."

cr -i $infile.txt -s "Delay" --all -o 2 > $infile-DELAY.txt
common_subs $infile-DELAY.txt
sed -i.bak -e '1 s/.*/FILTER	NUMSOURCES	RUN	SENSDISTANCE30	SENSDISTANCE40	SENSDISTANCE50/' $infile-DELAY.txt
sed -i.bak -e 's/ /\t/g' $infile-DELAY.txt
sed -i.bak -e 's/-/ /g' $infile-DELAY.txt
gawk -i inplace 'BEGIN{FS="\t";OFS="\t";} {print $3,$1,$2,$4,$5,$6}' $infile-DELAY.txt
sed -i.bak -e '/\s*0\s*0\s*0\s*/ d' $infile-DELAY.txt
echo "Produced file: $infile-DELAY.txt"

echo "Calculating Consumed Energy..."

cr -i $infile.txt -s "Consumed Energy" --all -o 2 > $infile-ENERGY.txt
common_subs $infile-ENERGY.txt
sed -i.bak -e '1 s/.*/FILTER	NUMSOURCES	RUN	SENSDISTANCE30	SENSDISTANCE40	SENSDISTANCE50/' $infile-ENERGY.txt
sed -i.bak -e 's/ /\t/g' $infile-ENERGY.txt
sed -i.bak -e 's/-/ /g' $infile-ENERGY.txt
gawk -i inplace 'BEGIN{FS="\t";OFS="\t";} {print $3,$1,$2,$4,$5,$6}' $infile-ENERGY.txt
sed -i.bak -e '/\s*0\s*0\s*0\s*/ d' $infile-ENERGY.txt
echo "Produced file: $infile-ENERGY.txt"

echo "Calculating Estimated Network Lifetime..."

cr -i $infile.txt -s "Estimated network lifetime" --all -o 2 > $infile-LIFETIME.txt
common_subs $infile-LIFETIME.txt
sed -i.bak -e '1 s/.*/FILTER	NUMSOURCES	RUN	SENSDISTANCE30	SENSDISTANCE40	SENSDISTANCE50/' $infile-LIFETIME.txt
sed -i.bak -e 's/ /\t/g' $infile-LIFETIME.txt
sed -i.bak -e 's/-/ /g' $infile-LIFETIME.txt
gawk -i inplace 'BEGIN{FS="\t";OFS="\t";} {print $3,$1,$2,$4,$5,$6}' $infile-LIFETIME.txt
sed -i.bak -e '/\s*0\s*0\s*0\s*/ d' $infile-LIFETIME.txt
echo "Produced file: $infile-LIFETIME.txt"

cat delay_energy_plots.r | string replace -r '[0-9][0-9][0-9][0-9][0-9][0-9]-[0-9][0-9][0-9][0-9][0-9][0-9]' $infile > delay_energy_plots.r.bak
mv delay_energy_plots.r.bak delay_energy_plots.r
Rscript delay_energy_plots.r
