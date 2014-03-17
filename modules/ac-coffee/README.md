ac-coffee
=========

Emacs auto-complete source for CoffeeScript.

Current version uses a simple laxer-based approach and provides little information other than keyword identifier (e.g. variable, method names) list. 
That's enough for most common usage. 
The next version will be grammar-based.

![Snapshot](http://project.catx.me/ac-coffee/snapshot.png "ac-coffee")

Requirements
--------

* Node.js
* Coffee
* Emacs 24+
* auto-complete

Known Issues
-------

* Current version doesn't provide scope information yet, which means suggestions is made only by token name.
* Does not start completion on ```.```.

Installation
--------

1. Clone this repo
2. ~~In repo folder, run ```npm install coffee-script```~~ (node_modules folder already included in repo)
3. Configure ```.emacs```

```lisp
;; My Coffee AC Source
(add-to-list 'load-path "path/to/ac-coffee/")
(require 'ac-coffee)
```

Done!
