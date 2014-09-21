reddit-scrape
=============

Fetch top links from multiple subreddits.

Purpose
-------

I made this because I got annoyed at the default multi-reddit behavior
of combining subreddits and sorting according to absolute upvote
count.  If you mix some small subreddit like "r/nononono" with
"r/gifs", you'll never see any posts from "r/nononono" because there
are so many fewer people visiting and upvoting it.  The central
feature of `reddit-scrape` is that it normalizes post scores by
subreddit.

Rather than doing something really useful like patching the reddit
code to be able to sort scores in a more useful way, I threw this
thing together.

Installation
------------


    git clone https://github.com/peddie/reddit-scrape
    cd reddit-scrape
    cabal sandbox init
    cabal install --only-dep
    cabal configure
    cabal build

    cp dist/build/reddit-scrape/reddit-scrape <some bin directory>

Usage
-----

Run `reddit-scrape -h` for a list of command-line flags.  By default,
it removes youtube video links (I want to watch tiny clips with no
sound and minimal context) and converts gifs to better-compressed and
zoomable gfycat WebM videos.  The default action is to simply print
out a list of posts with a score, subreddit, title and full link.  You
can also have it pass all the URLs it prints to a specified command.

I usually invoke it in a way that won't print anything to stdout but
will open a new tab for each post with a normalized score over 1.0
(i.e. each post more interesting than the monthly average for its
subreddit):

    reddit-scrape -t 1.0 -o -q

If you want to pick and choose by title or otherwise browse, the
default command line options should work for you.
