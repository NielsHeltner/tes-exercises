type key = string
type value = int
type dictionary = key -> value

val empty : dictionary
val add : dictionary -> key -> value -> dictionary
val find : dictionary -> key -> value
