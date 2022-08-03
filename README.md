# Hash Map

Stable hash maps for Motoko based on "deterministic hash map" algorithm by Tyler Close

```ts
import Iter "mo:base/Iter";
import Nat "mo:base/Nat";
import Principal "mo:base/Principal";
import Map "mo:hashmap/Map";

let { ihash; nhash; thash; phash; calcHash } = Map;

let map1 = Map.new<Nat, Nat>();
let map2 = Map.new<Int, Nat>();
let map3 = Map.new<Text, Nat>();
let map4 = Map.new<Principal, Nat>();

Map.set(map1, nhash, 1, 1);
Map.set(map2, ihash, -1, 2);
Map.set(map3, thash, "aaa", 3);
Map.set(map4, phash, Principal.fromText("aaaaa-aa"), 4);

ignore Map.put(map3, thash, "bbb", 5);
ignore Map.put(map3, thash, "ccc", 6);

ignore Map.has(map1, nhash, 1);

ignore Map.get(map2, ihash, -1);

ignore Map.size(map3);

Map.delete(map3, thash, "bbb");

ignore Map.remove(map3, thash, "ccc");

for (key in Map.keys(map1)) {};
for (value in Map.vals(map1)) {};
for ((key, value) in Map.entries(map1)) {};

let map5 = Map.filter<Int, Nat>(map2, func(key, value) { key != 1 });

let map6 = Map.map<Nat, Nat, Text>(map1, func(key, value) { Nat.toText(value) });

let map7 = Map.mapFilter<Nat, Nat, Text>(map1, func(key, value) { if (key != 1) ?Nat.toText(value) else null });

let iter = Iter.map<Nat, (Nat, Nat)>(Iter.fromArray([]), func(value) { (value, value) });

let map8 = Map.fromIter<Nat, Nat>(iter, nhash);

Map.forEach<Nat, Nat>(map1, func(key, value) { Map.set(map8, nhash, key, value) });

ignore Map.some<Nat, Nat>(map1, func(key, value) { key != 1 });

ignore Map.every<Nat, Nat>(map1, func(key, value) { key != 1 });

ignore Map.find<Nat, Nat>(map1, func(key, value) { key == 1 });

ignore Map.findLast<Nat, Nat>(map1, func(key, value) { key == 1 });

let hash = calcHash(thash, "ddd");

for (map in [Map.new<Text, Nat>()].vals()) Map.set(map, hash, "ddd", 2);

Map.clear(map1);
```
