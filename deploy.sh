gco deploy
git merge master
elm-app build
touch build/.nojekyll
cp CNAME build/CNAME
git add --all
git commit -am "Pages build"
git push
git subtree push --prefix build origin gh-pages
gco master

# If there's ever a merge issue because github added a file, we can force push over it.
# git push origin `git subtree split --prefix build master`:gh-pages --force
