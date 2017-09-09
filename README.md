# hello-gamekit
`trivial-gamekit` basic example from [Getting Started](https://borodust.org/projects/trivial-gamekit/getting-started/) guide.


## Loading and running

Install `cl-bodge` quicklisp distribution if it isn't installed yet:

```lisp
(ql-dist:install-dist "http://bodge.borodust.org/dist/org.borodust.bodge.txt")
```

Or just in case do:

```lisp
(ql:update-all-dists)
```

Then load the example itself:
```lisp
(ql:quickload :hello-gamekit)
```

And run it!
```lisp
(gamekit:start 'hello-gamekit:hello-gamekit)
```


## Assets
* Snake head [image](http://www.clipartpanda.com/clipart_images/snake-cartoon-black-and-white-29075943)
* Honk [sound](https://freesound.org/people/Tomlija/sounds/105419/)
