Undo-Redo-Undo-Redo
===================

A `Game` is composed of colored "*items*" that correspond to:

- lines between pairs of points, in the case of `game-flow`;
- folding blocks in the case of `game-fold`, and
- compact (i.e., w/o holes) blocks in the case of `game-fill`.

Per game, each item holds a "linked list" that grows by each move: it is called
a "path", because the `case class` - named `Path` for a game - has a `parent`
field through which the "list" is linked back to `None` - the "empty" start of
the game. (In fact, there is a `Path` per item with each restart of a game.)

An instance of `Path` has two fields of type `Option[?]` named "`undo`" and
"`redo`": following back through "`undo`" is just as following back through
"`parent`"; however, upon multiple "`undo`s", not only does the path update
to the parent, but also a "`redo`" list is composed such that, following it
through to the end, the reverse list of "lost" `undo`s can be traversed (and
so, "`redo`ne").

                       _
       ,---------------|-.
       |       _         |
       |       | next    |
       |    ,--|---.     |
       |  A | undo |     |
       |    `------'     |
       |       ^         |
       /-------|---------'
      /        |       ^
     Path      |       | parent
      \        |       |
       \-------|-------|-.
       |       | next    |
       |    ,--|---.     |
       |  B | undo |<............
       |    `------'     |      |
       | ,------.        |      | next
       | |      |        |   ,--|---.
       | | redo ------------>| undo | C
       | |      |        |   `------'
       | `--|---'        |      ^
       |    | next       |      |
       `----|------------'      |
            v                   |
         ,------.               | next
         |      |            ,--|---.
       E | redo ------------>| undo | D
         |      |   undo     `------'
         `--|---'
            | next
            _


An "`undo`" has a `move` field thats holds the corresponding move, i.e., that
can be "undone". This field is preserved even in a "`redo`", because the latter
holds the "`undo`" in its `undo` field, and so the move - via `undo.move`. For
`game-fill`, a move in the opposite direction as the previous, it is intercepted
also an `undo`.

A "`redo`" is much like a move. Following a "`redo`", a new instance of `Path`
is created with the `undo` field taken from its now-parent's `redo`, and this
"`redo`" is of no further use, although kept as such in the now-parent `Path`.

                       _
       ,---------------|-.
       |       _         |
       |       | next    |
       |    ,--|---.     |
       |  A | undo |     |
       |    `------'     |
       |       ^         |
       `-------|---------'
               |       ^
               |       | parent
               |       |
       ,-------|-------|-.
       |       |         |
       |       | next    |
       |    ,--|---.     |
       |  B | undo |     | (<- unused redo)
       |    `------'     |
       |       ^         |
       /-------|---------'
      /        |       ^
     Path      |       | parent
      \        |       |
       \-------|-------|-.
       |       | next    |
       |    ,--|---.     |
       |  C | undo |<............
       |    `------'     |      |
       | ,------.        |      | next
       | |      | E      |   ,--|---.
       | | redo ------------>| undo | D
       | |      |   undo |   `------'
       | `--|---'        |
       |    | next       |
       `----|------------'
            _

When a move is performed, the "`redo`" list vanishes, unless the move is the same
with that of the "`redo`". Here, not only the two moves must be the same, but they
must also be performed with the same _intensity_ (from the part of the player).
This amounts to the local context of the move, that can be quantified with regard to
the status of the grid: clues and other items. In the simplest case, the move is free
absolutely, when the intensity of the `redo` move (kept in its `undo`) is that this
`undo` would be just a backtrack (without "clashes") from the part of the player.

The state of a game is a mutable list, with an index for each item. The color is
a negative number, calculated by negating the index (divided by 2, for the `game-flow`)
and subtracting one. The index is a positive number, obtained from the color by the same
calculation: for the `game-flow`, further multiplied by 2 and added either 0 or 1.

Just - The Past Simple
----------------------

The "`undo`s" are essential because they are added in a mutable list in the parent,
alternating with the corresponding "`redo`": this list becomes (part of) the "past"
(simple) for an instance of `Path` created upon a (new) move. It can be accessed from
the current instance of the `Path` (from the game state) through the `parent` field.
It holds objects of type `Just`, but in reality, the `Undo` and `Redo` `case class`es
only, inherit this trait.

Consider a move which is `undo`ne: this `undo` is added at the end of the mutable
list - of the instance of `Path` that has now become current (and was former _the_
`parent`). Next, consider a `redo`. But this performs the move prior to the `undo`:
as well, this is also added at the end of the mutable list. Now, the "same" `undo`
again - added at the end of the mutable list. In general, cycling the `undo`/`redo`
prior to a new distinct move, will correspond to successive elements, either `undo`
or `redo`, for the same move, at the end of the mutable list that is (becomes)
the "simple past" - for any new instance of `Path`.

An element of the "simple past" holds an instance of its `Path` as well. So, consider
a new distinct move: this move will be performed also for all the "just" mutable lists
encountered up through the `parent` field, on each element's own `Path`. A quick
inductive thinking reveals that there is an invariant for all these "just" elements,
which is that they correspond at all times to the actual just one grid, and thus
need not store a grid, resulting in a low memory footprint.

This is not all the advantage, though. After a - say, an - `undo` is added as an
element of the "just" mutable list, a new distinct move which is `undo`ne, will not
only create a subsequent `undo` added after this mentioned one, but also in the
"just" list of the current `Path` of this one. Hence, there appear to be three
combinations: the first `undo` by itself, the second `undo` by itself, and the
first `undo` together with the second `undo`. Of course, these correspond to the
same item, at the same depth, but distinct moves.

In this manner, rather than complicatedly generating combinations from a "just"
mutable list, a tree-like structure is obtained, whose simple traversal (albeit
not quite so direct) allows to apply a callback on the collected path of multiple
`undo`s (and `redo`s), and have these analysed in conjunction. Moreover, these
can be (de)serialized as `case class`es, thus storing actually the tree.
However, in memory, the tree will fast grow exponentially with each `undo`/`redo`,
because it really captures all possible combinations.

The memory grows exponentially, because when the immutable `undo`s/`redo`s are
added, the sequence of moves, `undo`s and `redo`s, is replayed (or reflected,
but not from the very beginning, as a further undo would remove a `Path` together
with its " just") commencing with a fresh instance of `Path`, in each case: it is
this `copy` of an `undo` or `redo` `case class` that is added at the end of the
"just" mutable list. The rest of the fields (i.e., besides the `path: Mutable[?]`
and `identifier: Mutable[Long]` fields) are ignored when an `undo` or `redo` becomes
"part of the past simple".

Since the methods invoked on the `Path` `case class` are the same even for
replaying, an `Option[Long]` parameter `id` tells whether to avoid replaying. The
invariant of there existing only a grid is preserved as follows.

For an `undo`, replaying (when reverting from a current child to its parent) means
performing

1. the child's `undo` field's move with a fresh (empty) `Path`;
2. the moves from the `undo` for each `redo` "forward";
3. the `undo`s for each `redo` "backward"; and,
4. because replaying occurs on the child, also just one more `undo` (reflecting
   the pass from child to parent).

[The resulting `undo`-as-just is added at the end to the parent's `just` mutable
list (to be such simple past for further child `Path`s).]

For a `redo`, replaying (when creating a child from its current parent) means
performing

1. the `undo` field move with a fresh (empty) `Path`;
2. the moves from the `undo` for each `redo` "forward";
3. the `undo`s for each `redo` "backward"; and,
4. because replaying occurs on the parent, also just one `redo` (reflecting
   the pass from parent to child).

[The resulting `redo`-as-just is added at the end to the current `just` mutable
list, but which becomes a parent `Path`.]

In either case, the resulting `Path` of the replayed and copied `undo` or `redo`,
is from now on bi-similar with the returned `Path`; or at most until the instance
of `Path` to whose `just` mutable list they are added, it is "gone" - if `undo`ne.
This is when "repeal" occurs: TODO.

Pending Batch(es)
-----------------

Without exception of some game, a move may require `undo`'s on other items, until
the move is possible. For the `game-flow`, a move is always possible, i.e.,
other line(s) "snap" until reducing to at most the pair of clue points (of course,
a move _into_ any of the pair of clue points is impossible); however, when the
two halfs of the same color would (snap and) join at ninety degrees upon a bridge
clue, this move is prohibited: not that it couldn't make a pending, but otherwise
there might be two pendings (one "collinear", one "not collinear"), whereas at most
one pending is assumed. For the `game-fold`, a move may not be possible, because
a block can at most fold to its initial shape, that cannot be eliminated. Since
for `game-fill`, a block can be taken out from the grid onto the pad, also a move
is always possible (of course, not taking into account when a block _cannot_ be
"moved" or dropped from the pad onto the grid).

For the cases when `undo`s must precede a possible move in order to make it occur,
it is resorted to the very methods of `undo` (and `redo`), only which do not
alter the past simple, nor are considered from the point of view of intensities: they
are called `batch undo` and `batch redo`.

Because a batch may be independent of other batches, there is a stack-like
structure - the "`pending`" - that is cleared under certain conditions (which
obviously interfere with some "untouchable" batch); otherwise, the pending
`batch undo`s may be `batch redo`ne, if *only* in the _reverse_ order in which
they occurred.

A (pending) batch associates an item with other items that are `undo`ne upon the
item's move. If possible ("batchable", depending on the game), the "top"
(of the stack-list) "pending" batch accumulates in a `HashMap` the index of
each `undo`ne item, mapped to how many `undo`s (a batch will `redo`).

Thus, an element of the pending mutable stack-list, is a pair:

- the current move's item's index;
- a mapping from the indexes of the (batch) `undo`ne items to an `undo` count.

The conditions under which the "pending" is cleared are as follows:

- the current move's item's index already exists in the "pending" list,
  as the first of a pair - and there can be but one;

- the current move's item's index appears in the "pending" list,
  contained in the second `HashMap` of a pair - an interference that cancels
  some batch(es);

- the "pending" stack-list contains one or more elements, but the
  current `undo`'s is not on the top of the stack; otherwise, a
  `batch redo` occurs.

A `batch redo` iterates over the set of indexes (of the items that
were `undo`ne), and for each index, performs so many `redo`s as
mapped at the index.

For the `game-flow`, if the current move's line actually "snaps" the
same-color paired line, this is not considered a `batch undo`, but the
join of the two lines (of the same color).

Have - The Past Perfect
-----------------------

There is a single `case class` (per game) that inherits the `Have` (past perfect)
trait: a `Board` is the grid without any rules or player, which anything can pile up
on, in no matter what order or how many times: inconsistencies are extensional.
The grid of a `Have` (`Board`) is reached by the player, and upon an `undo`,
it is added to the parent `Path`'s `have` mutable list: it "has been", so there
is no longer any intensity.

Whenever there is a move by the player, this move piles up on every `Board`
in the `have` mutable list. Nothing extensional corresponds to `undo`/`redo`.

Clues
-----
