import Iter "mo:base/Iter";
import Prim "mo:prim";
import Utils "./utils";

module {
  type HashUtils<K> = (
    getHash: (K) -> Nat32,
    areEqual: (K, K) -> Bool,
  );

  type Entry<K, V> = (
    key: K,
    value: V,
    hash: Nat32,
    nextIndex: Nat32,
  );

  type Slot<K, V> = {
    #item: Entry<K, V>;
    #nextIndex: Nat32;
  };

  public type Map<K, V> = {
    var body: (
      buckets: [var Nat32],
      data: [var Slot<K, V>],
      capacity: Nat32,
      takenSize: Nat32,
      size: Nat32,
    );
  };

  public let { nhash; thash; phash; bhash } = Utils;

  let toNat = Prim.nat32ToNat;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func iter<K, V, T>(map: Map<K, V>, fn: (Entry<K, V>) -> T): Iter.Iter<T> {
    let (_, data, _, takenSize, _) = map.body;

    var index = 0:Nat32;

    return {
      next = func(): ?T {
        loop if (index < takenSize) switch (data[toNat(index)]) {
          case (#item item) {
            index += 1;

            return ?fn(item);
          };

          case (_) index += 1;
        } else {
          return null;
        };
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func rehash<K, V>(map: Map<K, V>) {
    let (_, data, capacity, _, size) = map.body;

    let newCapacity = if (size < capacity / 4) capacity / 2 else if (size > capacity * 3 / 4) capacity * 2 else capacity;
    var newTakenSize = 0:Nat32;

    let newBuckets: [var Nat32] = Prim.Array_init(toNat(newCapacity), 0:Nat32);
    let newData: [var Slot<K, V>] = Prim.Array_init(toNat(newCapacity), #nextIndex (0:Nat32));

    for (item in data.vals()) switch (item) {
      case (#item (key, value, hash, _)) {
        let bucketIndex = toNat(hash % newCapacity);

        newData[toNat(newTakenSize)] := #item (key, value, hash, newBuckets[bucketIndex]);

        newTakenSize += 1;

        newBuckets[bucketIndex] := newTakenSize;
      };

      case (_) {};
    };

    map.body := (newBuckets, newData, newCapacity, newTakenSize, size);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func get<K, V>(map: Map<K, V>, (getHash, areEqual): HashUtils<K>, key: K): ?V {
    let (buckets, data, capacity, _, _) = map.body;

    var index = buckets[toNat(getHash(key) % capacity)];

    loop if (index == 0) return null else switch (data[toNat(index - 1)]) {
      case (#item (itemKey, value, _, nextIndex)) {
        if (areEqual(itemKey, key)) return ?value;

        index := nextIndex;
      };

      case (#nextIndex nextIndex) index := nextIndex;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func put<K, V>(map: Map<K, V>, (getHash, areEqual): HashUtils<K>, key: K, value: V): ?V {
    let (buckets, data, capacity, takenSize, size) = map.body;

    let hash = getHash(key);
    let bucketIndex = toNat(hash % capacity);
    let firstIndex = buckets[bucketIndex];
    var index = firstIndex;

    loop if (index == 0) {
      let newTakenSize = takenSize + 1;

      data[toNat(takenSize)] := #item (key, value, hash, firstIndex);
      buckets[bucketIndex] := newTakenSize;

      map.body := (buckets, data, capacity, newTakenSize, size + 1);

      if (newTakenSize == capacity) rehash(map);

      return null;
    } else {
      let dataIndex = toNat(index - 1);

      switch (data[dataIndex]) {
        case (#item (itemKey, prevValue, hash, nextIndex)) {
          if (areEqual(itemKey, key)) {
            data[dataIndex] := #item (itemKey, value, hash, nextIndex);

            return ?prevValue;
          };

          index := nextIndex;
        };

        case (#nextIndex nextIndex) index := nextIndex;
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func remove<K, V>(map: Map<K, V>, (getHash, areEqual): HashUtils<K>, key: K): ?V {
    let (buckets, data, capacity, takenSize, size) = map.body;

    var index = buckets[toNat(getHash(key) % capacity)];

    loop if (index == 0) return null else {
      let dataIndex = toNat(index - 1);

      switch (data[dataIndex]) {
        case (#item (itemKey, value, _, nextIndex)) {
          if (areEqual(itemKey, key)) {
            let newSize = size - 1;

            data[dataIndex] := #nextIndex nextIndex;

            map.body := (buckets, data, capacity, takenSize, newSize);

            if (newSize < capacity / 4) rehash(map);

            return ?value;
          };

          index := nextIndex;
        };

        case (#nextIndex nextIndex) index := nextIndex;
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func map<K, V1, V2>(map: Map<K, V1>, fn: (V1, K) -> V2): Map<K, V2> {
    let (buckets, data, capacity, takenSize, size) = map.body;

    let newBuckets: [var Nat32] = Prim.Array_init(toNat(capacity), 0:Nat32);
    let newData: [var Slot<K, V2>] = Prim.Array_init(toNat(capacity), #nextIndex (0:Nat32));

    for (index in buckets.keys()) {
      newBuckets[index] := buckets[index];

      switch (data[index]) {
        case (#item (key, value, hash, nextIndex)) newData[index] := #item (key, fn(value, key), hash, nextIndex);
        case (#nextIndex nextIndex) newData[index] := #nextIndex nextIndex;
      };
    };

    return { var body = (newBuckets, newData, capacity, takenSize, size) };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func mapFilter<K, V1, V2>(map: Map<K, V1>, fn: (V1, K) -> ?V2): Map<K, V2> {
    let (_, data, capacity, _, _) = map.body;

    let newData: [var Slot<K, V2>] = Prim.Array_init(toNat(capacity), #nextIndex (0:Nat32));
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

    let newMap: Map<K, V2> = { var body = ([var], newData, newCapacity, newSize, newSize) };

    rehash(newMap);

    return newMap;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func new<K, V>(): Map<K, V> {
    return { var body = (Prim.Array_init(2, 0:Nat32), Prim.Array_init(2, #nextIndex (0:Nat32)), 2, 0, 0) };
  };

  public func clear<K, V>(map: Map<K, V>) {
    map.body := (Prim.Array_init(2, 0:Nat32), Prim.Array_init(2, #nextIndex (0:Nat32)), 2, 0, 0);
  };

  public func size<K, V>(map: Map<K, V>): Nat {
    return toNat(map.body.4);
  };

  public func has<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, key: K): Bool {
    return switch (get(map, hashUtils, key)) { case (null) false; case (_) true };
  };

  public func set<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, key: K, value: V) {
    ignore put(map, hashUtils, key, value);
  };

  public func delete<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, key: K) {
    ignore remove(map, hashUtils, key);
  };

  public func keys<K, V>(map: Map<K, V>): Iter.Iter<K> {
    return iter(map, func((key, _, _, _): Entry<K, V>): K { key });
  };

  public func vals<K, V>(map: Map<K, V>): Iter.Iter<V> {
    return iter(map, func((_, value, _, _): Entry<K, V>): V { value });
  };

  public func entries<K, V>(map: Map<K, V>): Iter.Iter<(K, V)> {
    return iter(map, func((key, value, _, _): Entry<K, V>): ((K, V)) { (key, value) });
  };

  public func filter<K, V>(map: Map<K, V>, fn: (V, K) -> Bool): Map<K, V> {
    return mapFilter(map, func(value: V, key: K): ?V { if (fn(value, key)) ?value else null });
  };
};
