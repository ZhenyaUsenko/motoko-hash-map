# Hash Map

Stable hash maps for Motoko based on "deterministic hash map" algorithm by Tyler Close

```rust
import Nat "mo:base/Nat";
import Principal "mo:base/Principal";
import Map "mo:map/Map";

let { nhash; thash; phash } = Map;

let map1 = Map.new<Nat, Nat>();
let map2 = Map.new<Int, Nat>();
let map3 = Map.new<Text, Nat>();
let map4 = Map.new<Principal, Nat>();

Map.set(map1, nhash, 1, 1);
Map.set(map2, nhash, -1, 2);
Map.set(map3, thash, "aaa", 3);
Map.set(map4, phash, Principal.fromText("aaaaa-aa"), 4);

ignore Map.put(map3, thash, "bbb", 5);
ignore Map.put(map3, thash, "ccc", 6);

ignore Map.has(map1, nhash, 1);

ignore Map.get(map2, nhash, -1);

ignore Map.size(map3);

Map.delete(map3, thash, "bbb");

ignore Map.remove(map3, thash, "ccc");

for (key in Map.keys(map1)) {};
for (value in Map.vals(map1)) {};
for ((key, value) in Map.entries(map1)) {};

let map5 = Map.filter(map2, func(key: Int, value: Nat): Bool {
  return key != 1;
});

let map6: Map.Map<Nat, Text> = Map.map(map1, func(key: Nat, value: Nat): Text {
  return Nat.toText(value);
});

let map7: Map.Map<Nat, Text> = Map.mapFilter(map1, func(key: Nat, value: Nat): ?Text {
  return if (key != 1) ?Nat.toText(value) else null;
});

Map.clear(map1);
```
