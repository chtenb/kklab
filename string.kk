import std/core/string
import std/core/char
import list

pub fun chars/trim-start(str : string, chars : list<char>) : string
  str.list.trim-start(chars, (==)).string

pub fun chars/trim-end(str : string, chars : list<char>) : string
  str.list.trim-end(chars, (==)).string

pub fun chars/trim(str : string, chars : list<char>) : string
  str.list.trim-end(chars, (==)).trim-start(chars, (==)).string

pub fun substring(str : string, start : int, length : int) : string
  str.slice.subslice(start, start + length).string
