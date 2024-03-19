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

docker network remove IoT-net
docker network create --driver=bridge --subnet=172.28.0.0/16 --ip-range=172.28.1.0/8 --gateway=172.28.1.254 IoT-net

cp test/$FILENAME.txt .
mv $FILENAME.txt toBeRun

cat toBeRun | ./main.native management

