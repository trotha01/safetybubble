#!/bin/bash
set -e

# compile
elm make Main.elm --yes --output=elm.js

# remove elm.js from .gitignore
# compatible with both GNU and BSD/Mac sed
sed -i.bak '/elm.js/d' .gitignore
rm .gitignore.bak

echo "ls"
ls

# init branch and commit
git init
git config user.name "Trevor Rothaus (via Travis CI)"
git config user.email "trotha01@gmail.com"
git checkout -b gh-pages
git add .
git status
git commit -m "Deploy to GitHub Pages [skip ci]"
git push --force "https://${GITHUB_TOKEN}@github.com/trotha01/safetybubble.git" gh-pages

# Upload to itch.io

# download butler
curl https://dl.itch.ovh/butler/linux-amd64/head/butler --output butler
chmod 755 butler

# zip relevant files
zip -v safetybubble.zip *.html *.css *.js

./butler push safetybubble.zip trotha01/safety-bubble:HTML5
