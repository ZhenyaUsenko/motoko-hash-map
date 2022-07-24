import Array "mo:base/Array";
import IC "mo:base/ExperimentalInternetComputer";
import Hash "mo:base/Hash";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Map "./Map";
import Set "./Set";

shared actor class HashMap() {
  type Key = { #principal: Principal; #text: Text; #nat: Nat; #int: Int };

  type Item<V> = { #item: (Key, V, Nat32, Nat32); #nextIndex: Nat32 };

  type Map<V> = { body: ([Nat32], [Item<V>], Nat32, Nat32, Nat32) };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public query func test(): async Map<Nat> {
    let map = Map.new<Nat>();

    Map.set(map, #nat 1, 1);
    Map.set(map, #nat 2, 2);
    Map.set(map, #nat 3, 3);
    Map.set(map, #nat 4, 4);
    Map.set(map, #nat 5, 5);

    Map.delete(map, #nat 2);
    Map.delete(map, #nat 3);
    Map.delete(map, #nat 5);

    return { body = (Array.freeze(map.body.0), Array.freeze(map.body.1), map.body.2, map.body.3, map.body.4) };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public query func testPerf(): async Nat64 {
    return IC.countInstructions(func() {
      let map = Map.new<Nat>();

      for (item in Iter.range(0, 100000)) Map.set(map, #nat item, item);

      for (item in Iter.range(0, 100000)) Map.set(map, #nat item, item + 1);
    });
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public query func testSetPerf(): async Nat64 {
    return IC.countInstructions(func() {
      let set = Set.new();

      for (item in Iter.range(0, 100000)) Set.add(set, #nat item);

      for (item in Iter.range(0, 100000)) Set.add(set, #nat item);
    });
  };
};
