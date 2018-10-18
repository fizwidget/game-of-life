#!/bin/bash

echo "- Checking out deployment branch..."
git checkout gh-pages
echo

echo "- Merging master into deployment branch"
git merge master -m "Merging master"
echo

echo "- Building..."
yarn build --optimize
echo

echo "- Comitting new artefacts..."
git add elm.js
git commit -m "Updating build artefacts"
echo

echo "- Pushing built result..."
git push -f
echo

echo "- Checking out master..."
git checkout master
echo

echo "- Deployment successful (ﾉ◕ヮ◕)ﾉ*:･ﾟ✧"