#!/bin/bash

docker run -d -p8888:8888 -v $PWD/sage_save_local:/home/sage/local_save sagemath/sagemath:latest sage-jupyter 2>&1 | tee run_result

sleep 1

container_id=`cat run_result`
rm run_result

url=`docker logs $container_id 2>&1 | grep "^ *or" |  grep "http.*$" -o`

echo url: $url

python -mwebbrowser $url
