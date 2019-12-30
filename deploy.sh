elm-app build
touch build/.nojekyll
cp CNAME build/
git add --all
git commit -am "Pages build"
git push
git subtree push --prefix build origin gh-pages

# If there's ever a merge issue because github added a file, we can force push over it.
# git push origin `git subtree split --prefix build master`:gh-pages --force
