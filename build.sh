elm-app build
git add --all
git commit -am "Pages build"
git push
git subtree push --prefix build origin gh-pages
