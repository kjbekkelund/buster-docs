# Buster.JS Documentation #

This is the app that runs http://busterjs.org.

## Working with the docs ##

To work with the documentation, simply clone the repo, link with NPM and run the server:

    git clone https://github.com/busterjs/buster-docs.git
    cd buster-docs
    npm link
    node bin/server

We deploy a static copy of the site. You can sanity check the static copy if you have `wget` installed (make sure the server is already running on the default port 8090):

    bin/build

There's also a binary to publish documentation using `rsync` but you need your keys on the server to do that.

## Contributing documentation ##

Please feel free to add documentation, fix spelling errors or whatever. To contribute back, do a pull request, or simply send us a patch on busterjs-dev@googlegroups.com (doesn't require a separate 'clone' on GitHub/'fork' on Gitorious). We're happy to accept your changes in medium, as long as it's git commits.

