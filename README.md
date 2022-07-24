# Hash Map

Stable hash maps for Motoko based on "deterministic hash map" algorithm by Tyler Close

```rust
import Nat "mo:base/Nat";
import Principal "mo:base/Principal";
import Map "mo:map/Map";

let map = Map.new<Nat>();

Map.set(map, #nat 1, 1);
Map.set(map, #int (-1), 2);
Map.set(map, #text "aaa", 3);
Map.set(map, #principal (Principal.fromText("aaaaa-aa")), 4);

ignore Map.put(map, #text "bbb", 5);
ignore Map.put(map, #text "ccc", 6);

ignore Map.has(map, #nat 1);

ignore Map.get(map, #int (-1));

ignore Map.size(map);

Map.delete(map, #text "bbb");

ignore Map.remove(map, #text "ccc");

for (key in Map.keys(map)) {};
for (value in Map.vals(map)) {};
for ((key, value) in Map.entries(map)) {};

let map2 = Map.filter(map, func(value: Nat, key: Map.Key): Bool {
  return switch (key) { case (#text _) true; case (_) false };
});

let map3: Map.Map<Text> = Map.map(map, func(value: Nat, key: Map.Key): Text {
  return Nat.toText(value);
});

let map4: Map.Map<Text> = Map.mapFilter(map, func(value: Nat, key: Map.Key): ?Text {
  return switch (key) { case (#text _) ?Nat.toText(value); case (_) null };
});

Map.clear(map);
```
