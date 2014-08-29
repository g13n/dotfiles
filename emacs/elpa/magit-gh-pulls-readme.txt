This is an experimental magit extension for manipulating GitHub pull requests

In order to use this, configure your repository like:
  $ git config --add magit.extension gh-pulls
    # or whatever is required to load that extension in magit for
    # this repository
  $ git config magit.gh-pulls-repo = <user>/<repo>
    # your github repository

and of course, load magit-gh-pulls.el

There are currently 3 bindings for pull requests:
# g g refreshes the list of pull requests
# g f fetches the commits associated with the pull request at point
# g b helps you creating a topic branch from a review request

Then, you can do whatever you want with the commit objects associated with
the pull request (merge, cherry-pick, diff, ...)
