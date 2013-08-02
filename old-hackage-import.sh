#!/bin/bash

HACKAGE_SERVER=hackage-server
HACKAGE_IMPORT=hackage-import

# number of cores to run the server on
CORES=1

# number of concurrent upload jobs
JOBS=10


STATE_DIR=./state
STATIC_DIR=./datafiles

IMPORTDATA_DIR=./import-data
ADMIN_PASSWD=admin

SERVER_HOST=localhost
SERVER_PORT=8080
SERVER_URL=http://admin:${ADMIN_PASSWD}@${SERVER_HOST}:${SERVER_PORT}

LOG_DIR=./logs
SERVER_LOG=${LOG_DIR}/server.log
IMPORT_LOG=${LOG_DIR}/import.log

echo "initialising server..."
${HACKAGE_SERVER} init --static=${STATIC_DIR} --state=${STATE_DIR} --admin=admin:${ADMIN_PASSWD} > ${SERVER_LOG}
echo "running server..."
${HACKAGE_SERVER} run -v3 --port=${SERVER_PORT} --static=${STATIC_DIR} --state=${STATE_DIR} --delay-cache-updates=60 +RTS -N${CORES} >> ${SERVER_LOG} 2>&1 &

echo "Waiting a sec for the server to start..."
sleep 2

echo "Making 'admin' user a member of the mirrorers group"
curl -u admin:${ADMIN_PASSWD} -X PUT ${SERVER_URL}/packages/mirrorers/user/admin > ${IMPORT_LOG} 2>&1
echo "Making 'admin' user a member of the trustees group"
curl -u admin:${ADMIN_PASSWD} -X PUT ${SERVER_URL}/packages/trustees/user/admin >> ${IMPORT_LOG} 2>&1

echo "importing users..."
time ${HACKAGE_IMPORT} users ${SERVER_URL} --htpasswd=${IMPORTDATA_DIR}/passwd/hackage.htpasswd --all-uploaders --addresses=${IMPORTDATA_DIR}/passwd/hackage.addresses --jobs=${JOBS} >> ${IMPORT_LOG}

echo "importing package metadata..."
time ${HACKAGE_IMPORT} metadata ${SERVER_URL} --index=${IMPORTDATA_DIR}/archive/00-index.tar.gz --jobs=${JOBS} >> ${IMPORT_LOG}

echo "importing package owner data..."
time ${HACKAGE_IMPORT} metadata ${SERVER_URL} --upload-log=${IMPORTDATA_DIR}/archive/log --jobs=${JOBS} >> ${IMPORT_LOG}

echo "importing package tarballs..."
time find ${IMPORTDATA_DIR}/archive -name '*.tar.gz' -print0 | xargs -0 \
  ${HACKAGE_IMPORT} tarball ${SERVER_URL} --jobs=${JOBS}  >> ${IMPORT_LOG}

echo "importing package documentation..."
time find ${IMPORTDATA_DIR}/docs -name '*-docs.tar.gz' -print0 | xargs -0 \
  ${HACKAGE_IMPORT} docs ${SERVER_URL} --jobs=${JOBS}  >> ${IMPORT_LOG}

echo "importing package deprecation info..."
time find ${IMPORTDATA_DIR}/archive -name 'tags' -print0 | xargs -0 \
  ${HACKAGE_IMPORT} deprecation ${SERVER_URL} >> ${IMPORT_LOG}

echo "importing distro info..."
time ${HACKAGE_IMPORT} distro ${SERVER_URL} ${IMPORTDATA_DIR}/archive/00-distromap/* >> ${IMPORT_LOG}

echo "importing download counts..."
time ${HACKAGE_IMPORT} downloads ${SERVER_URL} ${IMPORTDATA_DIR}/download-logs/*.gz >> ${IMPORT_LOG}

echo "Checkpointing server state..."
kill -USR1 `pidof hackage-server`
echo "Waiting..."
sleep 30
echo "Shutting down server..."
kill `pidof hackage-server`
