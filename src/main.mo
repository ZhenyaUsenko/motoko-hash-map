import Map "./Map";
import Array "mo:base/Array";
import Hash "mo:base/Hash";

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
    hash: ?Hash.Hash;
    next: ?Item<V>;
  };

  type Map<V> = {
    buckets: [?Item<V>];
    data: [?Item<V>];
    actualSize: Nat;
    size: Nat;
  };

  func itemToShared(item: ?Map.Item<Nat>): ?Item<Nat> {
    return switch (item) {
      case (?item) return ?{
        key = item.key;
        value = item.value;
        hash = item.hash;
        next = itemToShared(item.next);
      };

      case (_) return null;
    };
  };

  public query func test(): async Map<Nat> {
    let map = Map.new<Nat>();

    Map.set(map, #nat 1, 1);
    Map.set(map, #nat 2, 2);
    Map.set(map, #nat 3, 3);
    Map.set(map, #nat 4, 4);
    Map.set(map, #nat 5, 5);

    Map.delete(map, #nat 2);
    Map.delete(map, #nat 3);
    Map.delete(map, #nat 4);
    Map.delete(map, #nat 5);

    return {
      buckets = Array.map(Array.freeze(map.buckets), itemToShared);
      data = Array.map(Array.freeze(map.data), itemToShared);
      actualSize = map.actualSize;
      size = map.size;
    };
  };
};