#!/bin/bash

# Script to get all the old hackage data

IMPORTDATA_DIR=./import-data

mkdir -p ${IMPORTDATA_DIR}/passwd/ ${IMPORTDATA_DIR}/archive/

echo "Syncing passwd files..."
rsync -r -v hackage.haskell.org:/srv/www/hackage.haskell.org/passwd/ ${IMPORTDATA_DIR}/passwd/

echo "Syncing archive..."
rsync -r -v -z --skip-compress=gz -f'- latest/' -f'- logs/' hackage.haskell.org:/srv/www/hackage.haskell.org/public_html/packages/archive/ ${IMPORTDATA_DIR}/archive/

