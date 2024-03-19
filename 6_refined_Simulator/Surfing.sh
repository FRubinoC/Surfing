make clean
make

mkdir IoT-System
mkdir IoT-System/nodes

START=1
FILENAME="test1"
NODENUMBER=5

while getopts n:f: flag;
do
    case "${flag}" in
        n) NODENUMBER=${OPTARG};;
        f) FILENAME="${OPTARG}";;
    esac
done

for (( num=$START; num<=$NODENUMBER; num++ ))
do
    mkdir IoT-System/nodes/$num
done


cat test/$FILENAME.txt | ./main.native

