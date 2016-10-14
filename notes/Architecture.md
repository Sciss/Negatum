The objective is to refactor the things in Mutagen(Tx) in a way
that allows maximum flexibility of composition from within Mellite.

We should provide objects for all stages, i.e. GP evolution,
SOM building, playback trajectory.

## Evolution

- Suggest to avoid having a mutable tx chromosome such as the one in
  MutagenTx, based lucre-topology. It was complex, slow, and ate too
  much space.
- Suggest having "flat" input, i.e. values of `SynthGraph`
- Suggest evaluation be based on FScape-next instead of Strugatzki.
  That would automatically give us full programmability inside Mellite.
- Parameters could be given through attr-map using key conventions?
  For example, the set of allowed UGens could simply be given by
  a `SynthGraphObj`, the population by an `IntObj`, etc.

Can the whole thing be expressed as a set of related FScape graph
elements? (Does that make sense?)

Generator parameters:

    population      : Int           = 500
    constProb       : Double        = 0.5
    minNumVertices  : Int           = 64
    maxNumVertices  : Int           = 256
    nonDefaultProb  : Double        = 0.95
    allowedUGens    : Set[String]   = Set.empty

The advantage of FScape is that we can easily patch attr-map
entries to inputs, and even apply transformations to them, and that
the graph is "closed", so there is not problem adding new types of
side-effecting input and output. The disadvantage is that we cannot
express the data of the GP in terms of `GE` (unless we add a byte
type and use byte-array-serialization).

```scala
val gen  = Generate(n = 500, constProb = 0.5, ..., allowed = "proto")
val eval = Evaluate(in = gen) { in =>
  ...
}
val sel  = Select(gen = gen, eval = eval, ...)
```

There is actually no problem passing in `gen` as argument. It could
expand to other thing than `StreamIn`.

```scala
val folder = Folder("key")
val gen    = Generate(n = 500 - folder.size, ...)
val fOut   = folder.addAll(gen)
val eval   = Evaluate(in = fOut) { in => ... }
```

