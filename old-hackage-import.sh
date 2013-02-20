#!/bin/bash

# number of cores to run the server on
CORES=4

# number of concurrent upload jobs
JOBS=10


STATE_DIR=./state
STATIC_DIR=./static-files

IMPORTDATA_DIR=./import-data
ADMIN_PASSWD=admin
#IMPORTER_PASSWD=importer

SERVER_HOST=localhost
SERVER_PORT=8080
SERVER_URL=http://admin:${ADMIN_PASSWD}@${SERVER_HOST}:${SERVER_PORT}

LOG_DIR=./
SERVER_LOG=${LOG_DIR}/server.log
IMPORT_LOG=${LOG_DIR}/import.log

echo "initialising server..."
hackage-server init --static=${STATIC_DIR} --state=${STATE_DIR} --admin=admin:${ADMIN_PASSWD} > ${SERVER_LOG}
echo "running server..."
hackage-server run -v3 --static=${STATIC_DIR} --state=${STATE_DIR} --delay-cache-updates=60 +RTS -N${CORES} >> ${SERVER_LOG} 2>&1 &

echo "Waiting a sec for the server to start..."
sleep 2

#curl -u admin:${ADMIN_PASSWD} -X PUT http://localhost:8080/users/importclient
#curl -u admin:${ADMIN_PASSWD} -X PUT http://localhost:8080/users/importclient/htpasswd -d "${IMPORTER_PASSWD}"
echo "Making 'admin' user a member of the mirrorers group"
curl -u admin:${ADMIN_PASSWD} -X PUT ${SERVER_URL}/packages/mirrorers/user/admin > ${IMPORT_LOG} 2>&1

echo "importing users..."
time hackage-import users ${SERVER_URL} --htpasswd=${IMPORTDATA_DIR}/passwd/hackage.htpasswd --all-uploaders --addresses=${IMPORTDATA_DIR}/passwd/hackage.addresses --jobs=${JOBS} >> ${IMPORT_LOG}

echo "importing package metadata..."
time hackage-import metadata ${SERVER_URL} --index=${IMPORTDATA_DIR}/archive/00-index.tar.gz --jobs=${JOBS} >> ${IMPORT_LOG}

echo "importing package owner data..."
time hackage-import metadata ${SERVER_URL} --upload-log=${IMPORTDATA_DIR}/archive/log --jobs=${JOBS} >> ${IMPORT_LOG}

echo "importing package tarballs..."
time hackage-import tarball ${SERVER_URL} ${IMPORTDATA_DIR}/archive/*/*/*.tar.gz --jobs=${JOBS}  >> ${IMPORT_LOG}

echo "importing package documentation..."
time hackage-import docs ${SERVER_URL} ${IMPORTDATA_DIR}/cache/docs/*-docs.tar.gz --jobs=${JOBS}  >> ${IMPORT_LOG}

echo "importing package deprecation info..."
time hackage-import deprecation ${SERVER_URL} ${IMPORTDATA_DIR}/archive/*/*/tags >> ${IMPORT_LOG}

echo "importing distro info..."
time hackage-import distro ${SERVER_URL} ${IMPORTDATA_DIR}/archive/00-distromap/* >> ${IMPORT_LOG}

echo "Checkpointing server state..."
kill -USR1 `pidof hackage-server`
echo "Waiting..."
sleep 30
echo "Shutting down server..."
kill `pidof hackage-server`
