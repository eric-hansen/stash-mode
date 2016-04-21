# stash-mode

A script to help automate creating pull requests in Stash and BitBucket.

## Install

Until I can put this on MELPA just clone the repo or copy/paste it into a file and require it.

## Variables

* `stash-username` - The username you use for Stash (OAuth is not supported yet)
* `stash-password` - The password for the username
* `stash-url` - The base URL for Stash (i.e.: http://stash.example.com or http://www.example.com/stash), no trailing slash.
* `stash-reviewers` - Username(s) of people who are to review this code, if any

## How To Use

Make sure there's a .git somewhere for the purpose of running Git commands.  This will get the current branch and history of commits since the last push and populate the title and description, respectively.  When you're ready to create a PR, just run the `stash-mode/stash-pull-request` command and provide the destination branch.

On success it will display a PR link in the minibuffer, on failure it won't do anything informative for now.

## TODO

* Better error handling on request errors
* Put this on MELPA