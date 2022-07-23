import Array "mo:base/Array";
import IC "mo:base/ExperimentalInternetComputer";
import Hash "mo:base/Hash";
import Nat "mo:base/Nat";
import Iter "mo:base/Iter";
import Map "./Map";
import Tree "./StableRBTree";

shared actor class HashMap() {
  type Key = {
    #principal: Principal;
    #text: Text;
    #nat: Nat;
    #int: Int;
  };

  type Item<V> = {
    key: ?Key;
    value: ?V;
    hash: Nat32;
    next: ?Item<V>;
  };

  type Map<V> = {
    buckets: [?Item<V>];
    data: [?Item<V>];
    bucketsSize: Nat32;
    dataSize: Nat32;
    actualSize: Nat;
    size: Nat;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public query func test(): async Nat64 {
    return IC.countInstructions(func() {
      let map = Map.new<Nat>();

      for (item in Iter.range(0, 100000)) Map.set(map, #nat item, item);

      for (item in Iter.range(0, 100000)) Map.set(map, #nat item, item + 1);
    });
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public query func testTree(): async Nat64 {
    return IC.countInstructions(func() {
      var tree = Tree.init<Nat, Nat>();

      for (item in Iter.range(0, 100000)) tree := Tree.put(tree, Nat.compare, item, item);

      for (item in Iter.range(0, 100000)) tree := Tree.put(tree, Nat.compare, item, item + 1);
    });
  };
};