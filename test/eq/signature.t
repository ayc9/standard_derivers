Test 1: Given a record type a, expose equal_a
  $ test1="
  > type a = {
  >   x: int ;
  >   y: bool }[@@deriving equal]"
  $ echo "$test1" > test.mli
  $ driver test.mli 
  type a = {
    x: int ;
    y: bool }[@@deriving equal]
  include
    sig
      [@@@ocaml.warning "-32"]
      val equal_a : a -> ((a)[@merlin.hide ]) -> bool
    end[@@ocaml.doc "@inline"][@@merlin.hide ]

Test 2: Given a record type a, expose equal_a
  $ test2="
  > type b = {
  >   x: int ;
  >   y: bool }[@@deriving compare]"
  $ echo "$test2" > test.mli
  $ driver test.mli 
  type b = {
    x: int ;
    y: bool }[@@deriving compare]
  include
    sig
      [@@@ocaml.warning "-32"]
      val compare_b : b -> ((b)[@merlin.hide ]) -> int
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
