Seacat, a web framework written in Haskell
==========================================

Seacat is a little web framework written in Haskell, which evolved
rather naturally from me writing Λδ (defailed below). I think the
naturality of producing a little framework, rather than developing in
an ad hoc manner, is a testament to why Haskell is good for web
development: you actually sit and think about what you're doing.

Building
--------

If you have a recent version of cabal, you can use sandboxes to manage
the dependencies.

    cabal sandbox init
    cabal install --only-dependencies

If not, you'll just have to install the dependencies with all your
other packages.

    cabal install --only-dependencies

Once you have the dependencies, you can build it the normal way,

    cabal configure
    cabal build

Installing
----------

You can install to the sandbox (good for playing with Λδ), or with the
rest of your packages. If you made a sandbox, cabal will install there
automatically.

    cabal install

Running
-------

There are a couple of different commands for doing different things,
in general Seacat execution takes the form,

    <seacatbin> <command> [/path/to/configfile]

If the configuration file is omitted, the defaults are used. The
commands are as follows,

 - migrate: Perform a database migration (includes creating a
            database).
 - populate: Populate the database with sample data.
 - runserver: Run the Seacat server.

Configuration
-------------

See examples/seacat.conf

Documentation
-------------

Sources have Haddock comments, and you can produce pretty HTML
documentation from this,

    cabal haddock

Λδ, an imageboard written in Haskell
====================================

Λδ (or Lambdadelta) is an imageboard program written in Haskell. Most
imageboards are written in PHP or Perl, but I like applying Haskell to
things most people would consider me crazy for, hence this.

This is still heavily a WIP, as most of what I've done so far has been
building a microframework from the ground up, rather than actually
writing the imageboard bit.

Building
--------

If you used a sandbox for Seacat you can use the same one for Λδ by
just copying the `cabal.sandbox.config` file.

    cabal install --only-dependencies
    cabal configure
    cabal build
 
Running
-------

As above, but the name of the executable is `lambdadelta`,

    lambdadelta <command> [/path/to/configfile]

Expectations
------------

There are some directoryes that Λδ expects to exist, relative to
`file_root` in the configuration,

 - /board/, home for all board-specific things
 - /board/src/, images uploaded to a board
 - /board/thumb/, thumbnails of images uploaded to a board
 - /banners/, banners to display
 - /style.css, stylesheet

Configuration
-------------

See examples/lambdadelta.conf
