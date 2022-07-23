import Array "mo:base/Array";
import Int "mo:base/Int";
import Iter "mo:base/Iter";
import Nat32 "mo:base/Nat32";
import Option "mo:base/Option";
import Principal "mo:base/Principal";
import Text "mo:base/Text";
import Types "./types";

module {
  public type Key = Types.Key;
  public type Map<V> = Types.Map<V>;

  let { toNat } = Nat32;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  
  func getHash(key: Key): Nat32 {
    return switch (key) {
      case (#principal key) Principal.hash(key);
      case (#text key) Text.hash(key);
      case (#nat key) Int.hash(key);
      case (#int key) Int.hash(key);
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func rehash<V>(map: Map<V>) {
    let (_, data, capacity, _, size) = map.map;

    let newCapacity = if (size < capacity / 4) capacity / 2 else if (size > capacity * 3 / 4) capacity * 2 else capacity;
    var newTakenSize = 0:Nat32;

    let newBuckets: Types.Buckets = Array.init(toNat(newCapacity), 0:Nat32);
    let newData: Types.Data<V> = Array.init(toNat(newCapacity), #nextIndex (0:Nat32));

    for (item in data.vals()) switch (item) {
      case (#item (key, value, hash, _)) {
        let bucketIndex = toNat(hash % newCapacity);

        newData[toNat(newTakenSize)] := #item (key, value, hash, newBuckets[bucketIndex]);
        newBuckets[bucketIndex] := newTakenSize + 1;

        newTakenSize += 1;
      };

      case (_) {};
    };

    map.map := (newBuckets, newData, newCapacity, newTakenSize, size);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func get<V>(map: Map<V>, key: Key): ?V {
    let (buckets, data, capacity, _, _) = map.map;

    var index = buckets[toNat(getHash(key) % capacity)];

    loop if (index != 0) {
      let dataIndex = toNat(index - 1);

      switch (data[dataIndex]) {
        case (#item (itemKey, value, _, nextIndex)) {
          if (itemKey == key) return ?value;

          index := nextIndex;
        };

        case (#nextIndex nextIndex) index := nextIndex;
      };
    } else {
      return null;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func replace<V>(map: Map<V>, key: Key, value: V): ?V {
    let (buckets, data, prevCapacity, prevTakenSize, _) = map.map;

    let hash = getHash(key);
    var index = buckets[toNat(hash % prevCapacity)];

    loop if (index != 0) {
      let dataIndex = toNat(index - 1);

      switch (data[dataIndex]) {
        case (#item (itemKey, prevValue, hash, nextIndex)) {
          if (itemKey == key) {
            data[dataIndex] := #item (itemKey, value, hash, nextIndex);

            return ?prevValue;
          };

          index := nextIndex;
        };

        case (#nextIndex nextIndex) index := nextIndex;
      };
    } else {
      if (prevTakenSize == prevCapacity) rehash(map);

      let (buckets, data, capacity, takenSize, size) = map.map;

      let bucketIndex = toNat(hash % capacity);

      data[toNat(takenSize)] := #item (key, value, hash, buckets[bucketIndex]);
      buckets[bucketIndex] := takenSize + 1;

      map.map := (buckets, data, capacity, takenSize + 1, size + 1);

      return null;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func remove<V>(map: Map<V>, key: Key): ?V {
    let (buckets, data, capacity, takenSize, size) = map.map;

    var index = buckets[toNat(getHash(key) % capacity)];

    loop if (index != 0)  {
      let dataIndex = toNat(index - 1);

      switch (data[dataIndex]) {
        case (#item (itemKey, value, _, nextIndex)) {
          if (itemKey == key) {
            data[dataIndex] := #nextIndex nextIndex;

            map.map := (buckets, data, capacity, takenSize, size - 1);

            if (size - 1 < capacity / 4) rehash(map);

            return ?value;
          };

          index := nextIndex;
        };

        case (#nextIndex nextIndex) index := nextIndex;
      };
    } else {
      return null;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func entries<V>(map: Map<V>): Iter.Iter<(Key, V)> {
    let (_, data, _, takenSize, _) = map.map;

    var index = 0:Nat32;

    return {
      next = func(): ?(Key, V) {
        loop if (index < takenSize) switch (data[toNat(index)]) {
          case (#item (key, value, _, _)) {
            index += 1;

            return ?(key, value);
          };

          case (_) index += 1;
        } else {
          return null;
        };
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func map<V1, V2>(map: Map<V1>, fn: (value: V1, key: Key) -> V2): Map<V2> {
    let (buckets, data, capacity, takenSize, size) = map.map;

    let newBuckets: Types.Buckets = Array.init(toNat(capacity), 0:Nat32);
    let newData: Types.Data<V2> = Array.init(toNat(capacity), #nextIndex (0:Nat32));

    for (index in buckets.keys()) {
      newBuckets[index] := buckets[index];

      switch (data[index]) {
        case (#item (key, value, hash, nextIndex)) newData[index] := #item (key, fn(value, key), hash, nextIndex);
        case (#nextIndex nextIndex) newData[index] := #nextIndex nextIndex;
      };
    };

    return { var map = (newBuckets, newData, capacity, takenSize, size) };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func mapFilter<V1, V2>(map: Map<V1>, fn: (value: V1, key: Key) -> ?V2): Map<V2> {
    let (_, data, capacity, _, size) = map.map;

    let newData: Types.Data<V2> = Array.init(toNat(capacity), #nextIndex (0:Nat32));
    var newCapacity = 2:Nat32;
    var newSize = 0:Nat32;

    for (item in data.vals()) switch (item) {
      case (#item (key, prevValue, hash, _)) switch (fn(prevValue, key)) {
        case (?value) {
          newData[toNat(newSize)] := #item (key, value, hash, 0);
          
          newSize += 1;

          if (newSize == newCapacity) newCapacity *= 2;
        };

        case (_) {};
      };

      case (_) {};
    };

    let newMap: Map<V2> = { var map = ([var], newData, newCapacity, newSize, newSize) };

    rehash(newMap);

    return newMap;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func new<V>(): Map<V> {
    return { var map = (Array.init(2, 0:Nat32), Array.init(2, #nextIndex (0:Nat32)), 2, 0, 0) };
  };

  public func clear<V>(map: Map<V>) {
    map.map := (Array.init(2, 0:Nat32), Array.init(2, #nextIndex (0:Nat32)), 2, 0, 0);
  };

  public func size<V>(map: Map<V>): Nat {
    return toNat(map.map.4);
  };

  public func has<V>(map: Map<V>, key: Key): Bool {
    return Option.isSome(get(map, key));
  };

  public func set<V>(map: Map<V>, key: Key, value: V) {
    ignore replace(map, key, value);
  };

  public func delete<V>(map: Map<V>, key: Key) {
    ignore remove(map, key);
  };

  public func keys<V>(map: Map<V>): Iter.Iter<Key> {
    return Iter.map(entries(map), func(item: (Key, V)): Key { item.0 });
  };

  public func vals<V>(map: Map<V>): Iter.Iter<V> {
    return Iter.map(entries(map), func(item: (Key, V)): V { item.1 });
  };

  public func filter<V>(map: Map<V>, fn: (value: V, key: Key) -> Bool): Map<V> {
    return mapFilter(map, func(value: V, key: Key): ?V { if (fn(value, key)) ?value else null })
  };
};
