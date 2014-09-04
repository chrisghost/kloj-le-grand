Kloj le Grand
=============

Clojure bot for [vindinium](http://vindinium.org)

## Usage

```
$ lein repl
```

```clojure
> ; (re)load the code
> (require 'vindinium.core :reload)

> ; run a training game with 80 turns
> (vindinium.core/-main "training" secretkey 80)

> ; run 50 arena games
> (vindinium.core/-main "arena" secretkey 50)
```

