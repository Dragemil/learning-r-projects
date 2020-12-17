mkdir data
(
    cd data
    for i in {01..12}
    do 
        wget "https://s3.amazonaws.com/tripdata/JC-2019$i-citibike-tripdata.csv.zip"
        unzip -u "JC-2019$i-citibike-tripdata.csv.zip"
        rm "JC-2019$i-citibike-tripdata.csv.zip"
    done
)
