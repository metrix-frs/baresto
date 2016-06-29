#!/bin/bash

set -e
trap 'exit' ERR

if [ -z "$1" ]; then
  echo "No version supplied"
  exit 1
fi

version="$1"

sed -ri "s/\"version\": \"[^\s]*\"/\"version\": \"$version\"/" package.json
sed -ri "s/\"version\": \"[^\s]*\"/\"version\": \"$version\"/" bower.json
sed -ri "s/^versionStr = \"v[^\s]*\"/versionStr = \"v$version\"/" src/Version.purs

git add package.json bower.json src/Version.purs
git commit -m "version $version"
git push
git tag "v$version"
git push origin "v$version"
