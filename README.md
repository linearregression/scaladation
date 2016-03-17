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

## *n*-ary functions, for *n >= 3*

`ap` converts a value `F[A => B]` to `F[A] => F[B]`, but what about
lifted functions of higher arity?

The `B` in `F[A => B]` is a type variable, so in practice it can have
any type, including that of a function.

For a *3*-ary function, it helps to imagine `F[X => Y => Z]` simplified
to `F[A => B]` where `A` represents `X`, and `B` represents `Y => Z`.

If we `ap` this value with an `F[X]`, we'll end up with an `F[Y => Z]`,
which is ready to be `ap`'d with an `F[Y]` to finally produce an `F[Z]`.

## References

* `Validation` in Scalaz: http://eed3si9n.com/learning-scalaz/Validation.html
* `Validated` in Cats: http://typelevel.org/cats/tut/validated.html
* `<*>` in Haskell: http://hackage.haskell.org/package/base-4.8.2.0/docs/Control-Applicative.html#v:-60--42--62-
* `<$>` in Haskell: http://hackage.haskell.org/package/base-4.8.2.0/docs/Data-Functor.html#v:-60--36--62-
