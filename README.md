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

Running
-------

There are a couple of different commands for doing different things,
in general Λδ execution takes the form,

    lambdadelta <command> [/path/to/configfile]

If the configuration file is omitted, the defaults are used. The
commands are as follows,

 - migrate: Perform a database migration (includes creating a
            database).
 - populate: Populate the database with sample data.
 - runserver: Run the Λδ server.

Configuration
-------------

See examples/lambdadelta.conf
