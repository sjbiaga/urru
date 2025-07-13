Stochastic Pi-calculus in SCala aka sPISC ala RISC
==================================================

The π-calculus maps one to one on `Scala` for-comprehensions
"inside" the Cats Effect's `IO[_]` monad.

The stochastic branch adds rates to actions in comparison with
the [π-calculus](https://github.com/sjbiaga/pisc/tree/main).
This branch uses _cancellation_ to discard actions.
Another [branch](https://github.com/sjbiaga/pisc/tree/stochastic-flatMap)
heavily uses `flatMap`'s and comparison with `null` to discard actions.

After code generation, the π-calculus "processes" could be
programmatically typed as `Scala` code using `CE` `IO`.

The for-comprehensions vertically put the prefix (after "`for`")
and the composition/summation (before "`yield`").

Channels for names work as [CE tutorial](https://typelevel.org/cats-effect/docs/tutorial)'s
producer/consumer but no queue, only `takers` and `offerers`, which can be at most one.

Composition: parallel modelled with - `NonEmptyList.fromListUnsafe(...).parSequence`.

Summation: *probabilistic* choice modelled with - `parSequence`.

[Guarded] Replication: modelled with - `parSequence` and `lazy val` [or `def`].

The source code is divided in two: the parser in `Calculus.scala` and the
`Scala` source code generator in `Program.scala`.


Calculus
--------

The π-calculus process expressions are exactly as in the literature, with
both ASCII and UTF-8 characters, and slight variations. There is "match" and
"mismatch", but also there is `if then else` or the sugared Elvis operator.
Forcibly, _restriction_ is "considered" a _prefix_, besides input/output
prefixes per se.

The BNF formal grammar for processes is the following.

    LINE           ::= EQUATION | DEFINITION | DIRECTIVE
    EQUATION       ::= INVOCATION "=" CHOICE
    DEFINITION     ::= "⟦<CODE>" [ TEMPLATE ] "<CODE>⟧" PARAMS [ POINTERS ] "=" CHOICE
    DIRECTIVE      ::= "⟦" KEY = ( VALUE | "(" VALUE { "," VALUE } ")" ) "⟧"
    CHOICE         ::= PARALLEL { "+" PARALLEL }
    PARALLEL       ::= SEQUENTIAL { "|" SEQUENTIAL }
    SEQUENTIAL     ::= PREFIXES [ LEAF | "(" CHOICE ")" ]
    LEAF           ::= "[" NAME ("="|"≠") NAME "]" CHOICE
                     | "if" NAME ("="|"≠") NAME "then" CHOICE "else" CHOICE
                     | NAME ("="|"≠") NAME "?" CHOICE ":" CHOICE
                     | "!" [ "." μ "." ] CHOICE
                     | CAPITAL
                     | INVOCATION
                     | INSTANTIATION
    CAPITAL        ::= IDENTIFIER [ "(" [ NAMES ] ")" ] ( POINTERS | "{" "}" )
    INSTANTIATION  ::= "⟦<CODE>" INSTANCE "<CODE>⟧" [ POINTERS ]
    INVOCATION     ::= IDENTIFIER PARAMS
    PARAMS         ::= [ "(" NAMES ")" ]
    POINTERS       ::= "{" NAMES "}"
    NAMES          ::= NAME { "," NAME }
    NAMESʹ         ::= [ NAME ] { "," [ NAME ] }

The BNF formal grammar for prefixes is the following.

    PREFIXES       ::= { PREFIX }
    PREFIX         ::= μ "."
                     | "ν" "(" NAMES ")"
    μ              ::= "τ" [ @ RATE ] [ EXPRESSION ]
                     | NAME [ @ RATE ] "<" NAME ">" [ EXPRESSION ]
                     | NAME [ @ RATE ] "(" NAME ")" [ EXPRESSION ]
                     | NAME <CONS> "(" NAMESʹ ")" [ EXPRESSION ]
    EXPRESSION     ::= "/*" ... "*/"

Lexically, `ident` is a channel name - (an identifier) starting with lowercase letter;
capital `IDENT` is an agent identifier starting with uppercase letter. Both may contain
single and double quotes.

A source file with the "`.pisc`" extension consists of equations, binding an agent identifier
with an optional list of "formal" (bound names) parameters, to a process expression. Because
the use of parentheses in a _restriction_ would lead to ambiguities, it is forced to start
with the UTF-8 character "ν". "()" is _inaction_ or the _empty sum_.
"τ" is the _silent transition_.

Lines starting with a hash `#` character are (line) comments. Blank lines are ignored.
Lines starting with an `@` character are intermixed as `Scala` code. Lines ending with
backslash continue on the next line.

Summation (`CHOICE`) has lower precedence than composition (`PARALLEL`).

The output prefix uses angular parentheses and has the form `NAME<NAME>.`, while
the input prefix uses the round parentheses and has the form `NAME(NAME).`. A _`name`_
in parentheses can also be a (constant) `String` literal, a (boxed in a) `BigDecimal` number,
or a [`Scalameta`](https://scalameta.org) term as a `Scala` comment between `/*` and `*/`.

The _`rate`_ of an action ("τ" or prefix) can be optionally annotated with `@`
and an infinite-symbol ("∞"), a "top"-symbol ("⊤"), a `Scala` identifier,
a (boxed in a) `BigDecimal` number, or any [`Scalameta`](https://scalameta.org) term
as a `Scala` comment between `/*` and `*/`.

A match has the form `[NAME=NAME]` and a mismatch the same, but
using the `NOT EQUAL TO` Unicode `≠` character. `NAME=NAME` or `NAME≠NAME` is a
_test_,that can be used also as `if NAME(=|≠)NAME then CHOICE else CHOICE` or
as the syntactic sugar `NAME(=|≠)NAME ? CHOICE : CHOICE` Elvis ternary operator.

Stack safe is the [guarded] _replication_ unary operator `! [ "." μ"." ] CHOICE`;
the guard `"." μ "."` is optional, and it starts with a `"."` so that it is
distinguished from other prefixes.

The name before parentheses (angular or round) must be a channel name.

For output, the name in angular parentheses is optional, if empty being `null`.
This may be used to cease guarded replication with input prefix guard: i.e., if a
`null` is received, the (stack-safe, recursive) replication stops.

Note that input/output prefixes and the silent transition are followed by a dot,
whereas restriction is not; also, inaction, invocation, (mis)match, `if then else`
and replication are "leaves".

Between "τ" and "." in a silent transition, there can be a `Scalameta` term for
which a `for` generator `_ <- IO { term }` is inserted _after_ the transition,
or any list of `Enumerator`s which are added _after_ the transition. Any symbol
that is found in these terms is considered a _free_ name.

Between output prefix closing angular parenthesis and ".", there can be a
`Scalameta` term as an `IO[Any]` piece of code, that is executed under supervision
(so that cancellation does not outlive the `cats.effect.std.Supervisor[IO]`),
just after ending the output but before returning from it.

Between input prefix closing round parenthesis and ".", there can be a
`Scalameta` function-term as a `T`-parameterized `T => IO[T]` piece of code,
that is applied the prior result from the input, obtaining the `IO[T]` that is
executed under supervision (so that cancellation does not outlive the
`cats.effect.std.Supervisor[IO]`) providing the actual result, just after ending
the input but before returning from it.

Unlike the rest of the agents, the `Main` agent has the command line arguments
spliced as `vararg` parameter(s).


Stochastic
----------

The execution of a stochastic π-calculus program is handled in the files:
`loop.scala`, `stats.scala` and `spi.scala`. The `Main` agent is invoked from the
final generated source file wherein `main.scala.in` was `cat`enated.

From `main.scala.in`, two fibers are launched in `background` and used as `Resource`s,
such that terminating the program cancels them. Even when a process is _discarded_,
a `Deferred[IO, Option[(Double, -)]]` completed with `None` is used for this
situation, hence the fiber does not block semantically (so the program would not
exit any longer).

Silent transitions, input and output prefixes ("actions") are associated with
an `UUID` (`Universally Unique IDentifier`) by inheriting the trait `Act`. The
only other expression that has `enabled` actions is... summation (or choice),
by extending `Sum`.

A state contains a `Set` of `enabled` actions `(UUIDs`). _Parsing_ the equations
binding agents to process expressions, results in finding the enabled actions
for all summations - including those corresponding to the bound agents. States
constrain that each "precursor" _completed_ action (i.e., that is not excluded
or discarded) prior to its "expiration", immediately enables other "successor"
actions, including possibly, again itself.

Unlike π-calculus, where communication occurs unhindered whenever a channel inputs
and outputs, in stochastic π-calculus actions have rates, and depending on these
there is a "probabilistic" _election_ which determine which channels are elected for
input/output. Therefore, it is important that there should be a "moment" for this
election to consider _all_ enabled actions when selecting the pairs to communicate,
or otherwise - if some enabled actions are provisionally "pending" - none.

Thus, in advance, it is always known which (as well as how many of _the same_)
enabled actions are just pending before blocking for their completion;
the latter may result in either success, or in actions being discarded (as not
having been probabilistically chosen) - by completion with `None`.

Pending actions will soon reach blocking for their completion. As long as there
are pending actions enabled but not yet blocked for completion, _none_ of the
other enabled actions actually blocking, do participate in what corresponds to
probabilistic choice - the election mentioned above: there is patience for that
"moment" to arrive. Of course, it may be hard to catch (for unguarded replication).

Even if many of the same action (i.e., with the same key) are enabled, these are
distinguished upon completion by a unique `UUID`, either per agent invocation, or
per replication - the scope. And due to the atomicity of updates, the management
of the data structure for the enabled actions, is coherent among all actions thus
"firing" in parallel. This makes actions behave as multisets.

The file `StochasticPi.scala` is used to create a _directed multigraph_ with nodes
the _union_ of action and sum _type_, which at runtime gives rise to a
_transition system_ with states - multisets of enabled actions -, for the purpose of
enabling the actions in the successor state, immediately prior to the "expiration"
of the precursor enabled action. From action to successive action or sum, there is
an edge, even if there are restrictions in between (picture these latter translation
in `Scala`).

Upon the expiration of the last action from a sequence of prefixes, the enabled
actions are enabled _before_ they are next to be "fired" (in parallel) - which
means blocking for their completion.

The directed multigraph has multiple edges, because for the case of guarded
replication there is also one edge from the guard to itself (a loop) and to the
replicated process. This means that after expiration of the guard action, the
enabled actions are again the guard itself and the enabled actions of the process
that fires up in parallel.

Besides the _enabled_ actions, `StochasticPi.scala` is used to create also the
_discarded_ actions. Each choice corresponds to a set of enabled actions. The
set of enabled actions of the summation is but the union of the former. However,
a somewhat "duplicated" algorithm creates the discarded actions: and, with this
occasion, alse the _excluded_ actions.

Thus, assume a choice of the summation; the union of the set of _all_ enabled
actions of the choices to the left; and, the union of the set of _all_ enabled
actions of the choices to the right. Then for each key in the summation, the
discarded actions is the union of "left" with "right".

As an action is part of many summations on the way up to the top level,
each time there is more than one choice, to the same key there will be added
more "left/right" discarded actions/keys.

What is the benefit? At execution, when a key is part of the enabled actions,
performing the action corresponding to that key means two things. First, the
actions that are in parallel remain the same. Second, whenever the key is
part of a composition (maybe more than one sequence), then the fact that
this parallel composition is a choice in a summation, demands that all other
actions in the other "left" and "right" choices be discarded. So, there may be
many summations in which the expired _key_ discards other keys.

The discarded, excluded, and enabled actions are then embedded as an immutable map
from `String` to `Set[String]`in the generated output file. These `magical` maps
are declared as `π-trick`, `π-elvis` and `π-spell`, respectively.

The contention between the enabled actions occurs as follows. First, it "disables"
the excluded actions associated, if any. Each action in a sequence is a `for`
generator that calls a method in `spi.scala`; the key of this
action is passed as second argument. The method is within a unique scope (`^`)
to distinguish between different actions that correspond to the same key (multisets).
It creates a `Deferred[IO, Option[(Double, -)]]` and offers these together with the
action `rate` associated with the key, or enqueues a quadruple in the `/` `Queue`.
That does not mean the rate will be used, because the action may be _discarded_.
Then, it blocks semantically on the `Deferred.get` method. If discarded, it will
be `canceled`, but the cancellation will not outlive the `Supervisor`'s lifecycle
which the fiber `use`s for this purpose in a parameter to a (nested) `parSequence`.

A background fiber blocks on (`take`ing or) dequeuing this `Queue`. It then updates
the `%` map of all enabled actions by merely mapping the string `^ + key` (where `^`
stands for the unique scope) to a triple `Ref` & `Deferred` & rate - prior, it
decreases the number associated with the `key` from the (dual) `%` map.

There are two kinds of keys:

- the size of an `UUID`, each action has hard-coded a unique key; in the dual `%`
  map, these correspond to numbers: a number increases (`π-enable` method in
  `spi.scala`) as more actions - with the same key, i.e., in parallel - are enabled,
  but decreases (`poll` method in `loop.scala`) as the actions are "fired": when
  the number/counter reaches zero, a key is removed from the `%` map (also, when
  keys are `π-discard`ed);

- double the size of the former, concatenating the `UUID` of the current scope
  (`implicit ^`) with actions' keys, and replacing the value in the dual `map` by
  mapping `^ + key` to the triple `Ref` & `Deferred` & rate, each "fired" action
  has also associated a runtime unique key; to the "same actions", there may
  correspond concomitantly a number _and_ a triple in the dual `%` map.

It is crucial the [enabled] actions are enabled (rate absent, but enough for
the loose key to be in the `%` map) already when actions are fired in parallel.
A second background fiber is blocked on a different `Queue` that the other
background fiber releases, and then (blocking) polls for a next "offer".

This second background fiber then awaits for all enabled actions to be "reached"
or "fired", and thus have associated a rate rather than a number (the value type
of the `%` map is a `Scala 3` _union_ type). If the multisets are not empty,
then it will block on the `Queue` shared with the first background fiber.

As soon as the multisets are empty, the second background fiber computes
[statistically](https://github.com/scalanlp/breeze) - starting from the
rate(s) and/or weight(s) - the _delay(s)_ (or duration(s) or "delta(s)") that
correspond(s) to the fastest action(s). In plural, because the programmer can
set a degree of parallelism - the number of cores - allowing for multiple
communications to occur (simultaneously), as long as these do not discard each
other - only those satisfying this condition are returned: for each (pair), it
then uses a key to get an associated `Deferred`, and the delay to _complete_ a
`Deferred`; it does this twice, for actions of opposite polarities, unless it's
just the case of a single "τ". One obvious constraint is that the two actions
(in a pair) do not discard each other.

Meanwhile, all methods called (in parallel, either summation or composition)
from a `for` generator, having `offered` the action rate, are (and must _all_
be) blocked on `Deferred.get`'s method. As soon as one (pair of) `Deferred` is
`complete`d, the rest - upon being discarded - will be semantically unblocked
_and_ canceled. Upon `complete`ion of a pair, a `CyclicBarrier[IO](3)` is also
passed, such that all three fibers (a parallel fiber from the loop, the positive
polarity action and the negative polarity action) `await`, and only continue as
soon as - after the "communication" but not before its result -, the keys to be
discarded are discarded and the keys to be enabled are enabled.


Traces
------

Traces are enabled by the `"traces"` directive. If active, the keys of actions
are appended: the channel name, ("τ" for the silent action), the agent name from
the current equation, a label, the action's rate, and the filename where traces
are directed. However, it is not until runtime that these keys may be possibly
used. The traces' output is a `.csv` file with the following columns:

    start,end,name,polarity,key,replication,label,rate,delay,duration,agent,filename

The first two column are two timestamps, the `start` and `end` of the action in
nanoseconds. The _channel name_ is the third column (or "τ"). The polarity of
the action (`false` for output, `true` for input, empty for `τ`) is the fourth
column. The fifth stores the unique _key_. The sixth tells whether the action is
the guard of a _replication_. The seventh is a _label_ that is a string of tags,
which differentiate between the elements of summations and compositions. The
eighth is the _rate_. The ninth is the _delay_ of the action (silent or
communication). The tenth may be `0.0` for immediate actions, `NaN` for passive
actions or the same as the ninth for active actions. The eleventh is the
originating _agent name_. The final twelfth column is a _filename_ (with
possible to be ignored commas) or empty.

There may be silent actions which are inserted after the other are labelled:
these are not traceable, but nor should they need be; if they are still needed,
they must be inserted explicitly.


Program
-------

A new name - will be available in the Scala scope:

    for
      x <- ν
      .
      .
      .
    yield
      ()

The inaction - `IO.unit`.

A long prefix path - "`v(x).x<5>.x(y).τ@(1).x@∞(z).z<y>.`":

    for
      x      <- ν
      _      <- x(⊤(1L), BigDecimal(5))("2b9b3d1a-9b17-4c3f-b126-268ec639a8a7")
      (y, _) <- x(⊤(1L))("eaab7d89-cf7e-4286-95aa-35adb187df55")
      _      <- τ(`ℝ⁺`(BigDecimal(1))("e34022d6-89f5-4148-92ba-f471db56749b"))
      (z, _) <- x(∞(1L))("8ce85b1d-d213-442d-8520-68f0f1db25af")
      _      <- z(⊤(1L), y)("d998269b-9edf-4129-9921-ab8647f3d6d1")
      .
      .
      .
    yield
      ()

Note that `UUID` second argument is absent.

One may intercalate "`println`"s:

    for
      x      <- ν
      _      <- IO.println(s"new x=$x")
      t      <- x(⊤(1), BigDecimal(5))("2b9b3d1a-9b17-4c3f-b126-268ec639a8a7")
      _      <- IO.println(s"passive output duration = $t")
      (y, _) <- x(⊤(1L))("eaab7d89-cf7e-4286-95aa-35adb187df55")
      _      <- IO.println("input x(y)")
      t      <- τ(`ℝ⁺`(BigDecimal(1))("e34022d6-89f5-4148-92ba-f471db56749b"))
      _      <- IO.println(s"silent transition duration = $t")
      (z, t) <- x(∞(1L))("8ce85b1d-d213-442d-8520-68f0f1db25af")
      _      <- IO.println(s"immediate input duration = $t")
      _      <- z(⊤(1L), y)("d998269b-9edf-4129-9921-ab8647f3d6d1")
      .
      .
      .
    yield
      ()

A [mis]match `[x = y] P` translates as:

    for
      .
      .
      .
      _ <- if !(x == y) then IO.unit else
           for
             . // P
             .
             .
           yield
             ()
    yield
      ()

An `if then else` translates `if x = y then P else Q` as:

    for
      .
      .
      .
      _ <- ( if (x == y)
             then
               for // P
                 .
                 .
                 .
               yield
                 ()
             else
               for // Q
                 .
                 .
                 .
               yield
                 ()
           )
    yield
      ()

Each replication operator uses a unique variable pattern
named "`_<uuid>`" to translate lazily `! . π . P` as:

    for
      _<uuid> <- IO {
        lazy val _<uuid>: String => IO[Any] = { implicit ^ =>
          NonEmptyList
            .fromListUnsafe(
              List(
                .  // P
                .
                .
              ,
                for
                  π
                  _ <- _<uuid>(`π-uuid`)
                yield
                  ()
              )
            )
            .parSequence
        }
        <uuid>
      }
      π
      _ <- _<uuid>(`π-uuid`)
    yield
      ()

where "`uuid`" is some generated `java.util.UUID`.

Agent identifiers (literals) start with uppercase, while
channel names start with lowercase.


Apps (examples)
---------------

The `examples` folder *must* have three sub-folders:

    ./examples/
       pisc/
       in/
       out/

The root project folder contains five files: `loop.scala`, `stats.scala`, `spi.scala`,
and `main.scala.in`.

!!!Warning: do not delete them!!!

One can edit'em, though they're ready to generate a main `App`.

To get and run the examples, one can `source` the functions from `bin/spi.sh`.

To run an example, `cd` to `examples` and execute:

    ./examples $ spi ex.scala

To get the final source file `ex.scala` (from `out/ex.scala.out`), run:

    ./examples $ spio ex

To get the intermediary `in/ex.scala.in` file, execute the `run` command in the `sbt` shell:

    sbt:Stochastic π-Calculus2Scala> run ex

where `example/pisc/ex.pisc` contains the stochastic π-calculus source (equations binding
agents to process expressions).
