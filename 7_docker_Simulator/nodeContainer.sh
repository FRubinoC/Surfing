NODE=0

while getopts n:f: flag;
do
    case "${flag}" in
        n) NODE="${OPTARG}";;
    esac
done

cp -r _build IoT-System/nodes/$NODE
cd IoT-System/nodes/$NODE
mv _build appbuild
cd ../../..
cp toBeRun IoT-System/nodes/$NODE

echo "FROM ubuntu" > IoT-System/nodes/$NODE/Dockerfile
echo "RUN set -x && apt-get update && apt-get upgrade" >> IoT-System/nodes/$NODE/Dockerfile
echo "ADD . /appbuild" >> IoT-System/nodes/$NODE/Dockerfile
echo "ADD . /toBeRun" >> IoT-System/nodes/$NODE/Dockerfile
echo "WORKDIR appbuild" >> IoT-System/nodes/$NODE/Dockerfile
echo "RUN mkdir IoT-System && mkdir IoT-System/nodes && mkdir IoT-System/nodes/$NODE" >> IoT-System/nodes/$NODE/Dockerfile
echo "RUN chmod +x appbuild/main.native" >> IoT-System/nodes/$NODE/Dockerfile
echo "CMD cat toBeRun | ./appbuild/main.native $NODE" >> IoT-System/nodes/$NODE/Dockerfile

cd IoT-System/nodes/$NODE
docker build -t iotnode$NODE .
cd ../../..

gnome-terminal --title=node$NODE -- sh -c "docker run --network IoT-net --rm -it --name iotnode$NODE --ip 172.28.1.$NODE iotnode$NODE; bash"