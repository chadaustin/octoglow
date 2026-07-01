# Hi!

Octoglow is a slideshow for your personal photos that runs right in your web browser.

## Why another slideshow?

My wife and I enjoy displaying family photos on the TV, for example, when hosting parties or when visitors are over.  For years, the Google Picasa screensaver served our needs perfectly.  However, when we updated our media PC from Windows XP to Windows 7, the screensaver stopped working reliably.  Indeed, it appears unmaintained.

We tried a few other screensavers, but every one had a deal-breaking flaw.  Either they would run refuse to show any photos until the entire NAS was scanned (400 GB of photos!) or they would run terribly on the media PC's on-die Intel GPU.

Then, when the NAS ran Ubuntu 12.04, we used minidlna and the XBox 360's DLNA support.  However, minidlna was removed in Ubuntu 14.04 (Update: <a href="https://bugs.launchpad.net/ubuntu/+source/minidlna/+bug/1309651">it's back now</a>.).

Finally I decided I could do a better job with a basic server (written in Haskell) and a very simple HTML5 page with CSS3 for the photo transitions.  It runs on Windows and Mac and serves all of our needs.

## How do I run it?

On your server, install a recent ghc and cabal, and run `cabal install --only-dependencies && cabal run <path-to-photos-directory>`.  Then load `http://<yourserver>:9999` in a browser.  Mouse to the upper left corner of the window to see the configuration UI.

## What's up with the name?

Honestly, GitHub suggested it.
