# <div align="center">Dagmore 🍕 ⛑</div>

> _She was like a candle in the wind... unreliable._ - [Dean Learner](https://en.wikipedia.org/wiki/Dean_Learner)

Dagmore is a DSL for constructing heterogenous computations representable as
[DAGs](https://en.wikipedia.org/wiki/Directed_acyclic_graph). It bears a _lot_
of similarity to [Dagless](https://github.com/i-am-tom/dagless), its spiritual
older brother, bug uses rank-2 polymorphism to provide similar guarantees
_without the compile-time overhead_ or `IxMonad`/`do-notation` trickery!

---

See the [Test](https://github.com/i-am-tom/dagmore/tree/master/test/Test)
directory for fully-worked examples!

```haskell
main = run do
  acceleration <- register @Acceleration
  mass         <- register @Mass
  displacement <- register @Displacement

  force <- using (mass, acceleration) $ \(m, a) -> do
    Mass         m' <- m
    Acceleration a' <- a

    pure (Force (m' * a'))

  displacement <- fetch @Displacement

  using (force, displacement) $ \(f, d) -> do
    Force        f' <- f
    Displacement d' <- d

    pure (Energy (f' * d'))
```
