#!/bin/sh

set -e

ADMIN_USER=admin
ADMIN_PASS=admin
BUILD_RUN_TIME=30
BUILD_INTERVAL=5

DOCKER_IMAGE=zsol/hackage-server

server_id=$(sudo docker run -d -p 8080 ${DOCKER_IMAGE} sh -c "./dist/build/hackage-server/hackage-server init --static-dir=datafiles --admin=\"${ADMIN_USER}:${ADMIN_PASS}\" ; ./dist/build/hackage-server/hackage-server run --static-dir=datafiles")

server_ip=$(sudo docker inspect $server_id | grep IPAddress | cut -d'"' -f4)
server_local_port=$(sudo docker port $server_id 8080)

sleep 2 # TODO: poll until web server comes up

sudo docker run -d ${DOCKER_IMAGE} sh -c "echo -e \"${ADMIN_USER}\n${ADMIN_PASS}\" | ./dist/build/hackage-build/hackage-build init http://${server_ip}:8080; ./dist/build/hackage-build/hackage-build build --run-time=${BUILD_RUN_TIME} --interval=${BUILD_INTERVAL} --continuous"

echo "You can access your local hackage at http://localhost:${server_local_port}"
