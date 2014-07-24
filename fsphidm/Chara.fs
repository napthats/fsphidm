module Chara

open PhiMap
open PhiClient


type CharaAction =
    | Say of string
    | Go of (Direction * bool) //bool is for a withTurn flag

type Chara = 
    abstract GetAction : unit -> CharaAction option
    //abstract Walk : Direction * bool -> bool
    //abstract Say : string -> unit

