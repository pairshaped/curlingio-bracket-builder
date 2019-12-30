elm-app build
git add --all
git commit -a "Pages build"
git push
git subtree push --prefix build origin gh-pages
