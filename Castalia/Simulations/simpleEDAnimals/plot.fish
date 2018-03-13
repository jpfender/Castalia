#! /usr/bin/fish

set FILENAME 170124-110109.txt

set NUMSOURCES 2 3 4 5
set DIST dist66 distRand distUniform
set SAMPLEINTERVAL 5 10 20

for source in $NUMSOURCES
    for dist in $DIST
        for interval in $SAMPLEINTERVAL
            #echo "cr -i $FILENAME  -s \"packet filtering breakdown\"    --sum -f \"numSources$source,$dist,sampleInterval$interval\" | cpl -o packet-filtering-numSources$source-$dist-sampleInterval$interval.png     --xrotate 45 -s stacked -l \"outside width -4\""

            #echo "cr -i $FILENAME  -s \"duplicate detection breakdown\" --sum -f \"numSources$source,$dist,sampleInterval$interval\" | cpl -o duplicate-detection-numSources$source-$dist-sampleInterval$interval-static.png  --xrotate 45 -s stacked --yrange 50000 -l \"outside width -4\""
            #echo "cr -i $FILENAME  -s \"duplicate detection breakdown\" --sum -f \"numSources$source,$dist,sampleInterval$interval\" | cpl -o duplicate-detection-numSources$source-$dist-sampleInterval$interval-flex.png  --xrotate 45 -s stacked -l \"outside width -4\""

            echo "cr -i $FILENAME  -s \"Accuracy\"                  -f \"numSources$source,$dist,sampleInterval$interval\" | cpl -o accuracy-numSources$source-$dist-sampleInterval$interval.png    --yrange 1.0"
            echo "cr -i $FILENAME  -s \"Negative Predictive Value\" -f \"numSources$source,$dist,sampleInterval$interval\" | cpl -o NPV-numSources$source-$dist-sampleInterval$interval.png         --yrange 1.0"
            echo "cr -i $FILENAME  -s \"Positive Predictive Value\" -f \"numSources$source,$dist,sampleInterval$interval\" | cpl -o PPV-numSources$source-$dist-sampleInterval$interval.png         --yrange 1.0"
            echo "cr -i $FILENAME  -s \"Sensitivity\"               -f \"numSources$source,$dist,sampleInterval$interval\" | cpl -o sensitivity-numSources$source-$dist-sampleInterval$interval.png --yrange 1.0"
            echo "cr -i $FILENAME  -s \"Specificity\"               -f \"numSources$source,$dist,sampleInterval$interval\" | cpl -o specificity-numSources$source-$dist-sampleInterval$interval.png --yrange 1.0"
        end
    end
end
