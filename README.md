## First Setup

### Install Node.js

    $ apt-get install -y nodejs
    $ apt-get install -y nodejs-legacy # for debian users
    $ apt-get install -y npm

### Install Bower and Gulp

    $ npm install -g bower
    $ npm install -g gulp

### Install Ruby and SASS

    $ apt-get install ruby
    $ gem install sass

### Clone and Get Dependencies

    $ git clone ssh://git@gitlab.mdrexl.net:10022/holger/lobster.git
    $ cd lobster/
    $ npm install
    $ bower update

## Deploy

    $ gulp

## Serve

Install (core)[http://gitlab.mdrexl.net/holger/core].

Assuming you have cloned core next to where you have cloned lobster:

    $ cd core/
    $ stack build
    $ stack exec core-server -- -s ../lobster/public
