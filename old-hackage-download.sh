#!/bin/bash

# Script to get all the old hackage data

IMPORTDATA_DIR=./import-data
IMPORTDATA_DIR=$(realpath ${IMPORTDATA_DIR})

mkdir -p ${IMPORTDATA_DIR}/passwd/ ${IMPORTDATA_DIR}/archive/ ${IMPORTDATA_DIR}/docs/

echo "Syncing passwd files..."
rsync -r -v hackage.haskell.org:/srv/www/hackage.haskell.org/passwd/ ${IMPORTDATA_DIR}/passwd/

echo "Syncing archive..."
rsync -r -v -z --skip-compress=gz -f'- latest/' -f'- logs/' hackage.haskell.org:/srv/www/hackage.haskell.org/public_html/packages/archive/ ${IMPORTDATA_DIR}/archive/


echo "Updating doc tarballs..."
pushd ${IMPORTDATA_DIR}/docs
for docdir in ${IMPORTDATA_DIR}/archive/*/*/doc/
do
  pkgverdir=$(dirname $docdir)
  pkgver=$(basename $pkgverdir)
  pkgname=$(basename $(dirname $pkgverdir))
  docdir_new=${pkgname}-${pkgver}-docs
  tarball=${docdir_new}.tar.gz

  if [[ -d ${docdir}/html ]]; then
    true
    if ! [[ -h ${docdir_new} ]]; then
      ln -f -s ${docdir}/html ${docdir_new}
    fi
    if ! [[ -f ${tarball} ]]; then
      echo ${tarball}
      tar -h -c ${docdir_new} -zf ${tarball} --exclude=frames.html --exclude=mini_* --owner=nobody --group=users --format=ustar
    else
      echo -n '.'
    fi
  else
    echo !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    echo ${docdir}/html does not exist
  fi
done
popd
