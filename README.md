## Usage

```
$ cd nih
$ sbt run
```

```
$ cd scalaz
$ sbt run
```

```
$ cd cats
$ sbt run
```

## `map` and `ap`

```
.--------------.         .-----------------------.
|  Category _  |         |     Category F[_]     |
|--------------|         |-----------------------|
| a: A         |         |   aF: F[A]            |
| b: B         |         |   bF: F[B]            |
| f: A => B  ~~~~~ map ~~~~> fF: F[A] => F[B]  <~~~ ap ~.
|              |         |                       |      |
|              |         |   gF: F[A => B]  ~~~~~~~~~~~~'
'--------------'         '-----------------------'
```

## References

* `Validation` in Scalaz: http://eed3si9n.com/learning-scalaz/Validation.html
* `Validated` in Cats: http://typelevel.org/cats/tut/validated.html
* `<*>` in Haskell: http://hackage.haskell.org/package/base-4.8.2.0/docs/Control-Applicative.html#v:-60--42--62-
* `<$>` in Haskell: http://hackage.haskell.org/package/base-4.8.2.0/docs/Data-Functor.html#v:-60--36--62-
