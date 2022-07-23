import Array "mo:base/Array";
import Int "mo:base/Int";
import Nat32 "mo:base/Nat32";
import Option "mo:base/Option";
import Principal "mo:base/Principal";
import Text "mo:base/Text";
import Types "./types";

module {
  public type Key = Types.Key;
  public type Item<V> = Types.Item<V>;
  public type Map<V> = Types.Map<V>;

  let toNat = Nat32.toNat;

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
    let (buckets, data, capacity, _, size) = map.data;

    let newCapacity = if (size < capacity / 4) Nat32.max(capacity / 2, 4) else if (size >= capacity / 4 * 3) capacity * 2 else capacity;
    var newTakenSize = 0:Nat32;

    let newBuckets: [var Nat32] = Array.init(toNat(newCapacity), 0:Nat32);
    let newData: [var Item<V>] = Array.init(toNat(newCapacity), #nextIndex(0:Nat32));

    label dataLoop for (item in data.vals()) switch (item) {
      case (#item (key, value, hash, _)) {
        let bucketIndex = toNat(hash % newCapacity);

        newData[toNat(newTakenSize)] := #item (key, value, hash, newBuckets[bucketIndex]);
        newBuckets[bucketIndex] := newTakenSize + 1;

        newTakenSize += 1;
      };

      case (_) break dataLoop;
    };

    map.data := (newBuckets, newData, newCapacity, newTakenSize, size);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func new<V>(): Map<V> {
    return { var data = (Array.init(4, 0:Nat32), Array.init(4, #nextIndex(0:Nat32)), 4, 0, 0) };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func get<V>(map: Map<V>, key: Key): ?V {
    let (buckets, data, capacity, _, _) = map.data;

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
    let (buckets, data, prevCapacity, prevTakenSize, _) = map.data;

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

      let (buckets, data, capacity, takenSize, size) = map.data;

      let bucketIndex = toNat(hash % capacity);

      data[toNat(takenSize)] := #item (key, value, hash, buckets[bucketIndex]);
      buckets[bucketIndex] := takenSize + 1;

      map.data := (buckets, data, capacity, takenSize + 1, size + 1);

      return null;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func remove<V>(map: Map<V>, key: Key): ?V {
    let (buckets, data, capacity, takenSize, size) = map.data;

    var index = buckets[toNat(getHash(key) % capacity)];

    loop if (index != 0)  {
      let dataIndex = toNat(index - 1);

      switch (data[dataIndex]) {
        case (#item (itemKey, value, _, nextIndex)) {
          if (itemKey == key) {
            data[dataIndex] := #nextIndex nextIndex;

            map.data := (buckets, data, capacity, takenSize, size - 1);

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

  public func size<V>(map: Map<V>): Nat {
    return toNat(map.data.4);
  };

  public func has<V>(map: Map<V>, key: Key): Bool {
    return Option.isSome(get(map, key));
  };

  public func set<V>(map: Map<V>, key: Key, value: V) {
    let _ = replace(map, key, value);
  };

  public func delete<V>(map: Map<V>, key: Key) {
    let _ = remove(map, key);
  };
};
