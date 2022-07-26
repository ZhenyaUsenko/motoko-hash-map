import Iter "mo:base/Iter";
import Prim "mo:prim";
import Utils "./utils";

module {
  type HashUtils<K> = (
    getHash: (key: K) -> Nat32,
    areEqual: (key1: K, key2: K) -> Bool,
  );

  type Entry<K, V> = (
    key: K,
    value: V,
    hash: Nat32,
    nextIndex: Nat32,
  );

  type Slot<K, V> = {
    #entry: Entry<K, V>;
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

  func iter<K, V, T>(map: Map<K, V>, fn: (entry: Entry<K, V>) -> T): Iter.Iter<T> {
    let (_, data, _, takenSize, _) = map.body;

    var index = 0:Nat32;

    return {
      next = func(): ?T {
        loop if (index >= takenSize) return null else switch (data[toNat(index)]) {
          case (#entry entry) {
            index += 1;

            return ?fn(entry);
          };

          case (_) index += 1;
        };
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func rehash<K, V>(map: Map<K, V>) {
    let (_, data, capacity, _, size) = map.body;

    let newCapacity = if (size < capacity / 4) capacity / 2 else if (size > capacity * 3 / 4) capacity * 2 else capacity;
    var newTakenSize = 0:Nat32;

    let newBuckets = Prim.Array_init<Nat32>(toNat(newCapacity), 0);
    let newData = Prim.Array_init<Slot<K, V>>(toNat(newCapacity), #nextIndex 0);

    for (entry in data.vals()) switch (entry) {
      case (#entry (key, value, hash, _)) {
        let bucketIndex = toNat(hash % newCapacity);

        newData[toNat(newTakenSize)] := #entry (key, value, hash, newBuckets[bucketIndex]);

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
      case (#entry (entryKey, value, _, nextIndex)) {
        if (areEqual(entryKey, key)) return ?value;

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

      data[toNat(takenSize)] := #entry (key, value, hash, firstIndex);
      buckets[bucketIndex] := newTakenSize;

      map.body := (buckets, data, capacity, newTakenSize, size + 1);

      if (newTakenSize == capacity) rehash(map);

      return null;
    } else {
      let dataIndex = toNat(index - 1);

      switch (data[dataIndex]) {
        case (#entry (entryKey, prevValue, hash, nextIndex)) {
          if (areEqual(entryKey, key)) {
            data[dataIndex] := #entry (entryKey, value, hash, nextIndex);

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
        case (#entry (entryKey, value, _, nextIndex)) {
          if (areEqual(entryKey, key)) {
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

  public func map<K, V1, V2>(map: Map<K, V1>, fn: (key: K, value: V1) -> V2): Map<K, V2> {
    let (buckets, data, capacity, takenSize, size) = map.body;

    let newBuckets = Prim.Array_init<Nat32>(toNat(capacity), 0);
    let newData = Prim.Array_init<Slot<K, V2>>(toNat(capacity), #nextIndex 0);

    for (index in buckets.keys()) {
      newBuckets[index] := buckets[index];

      switch (data[index]) {
        case (#entry (key, value, hash, nextIndex)) newData[index] := #entry (key, fn(key, value), hash, nextIndex);
        case (#nextIndex nextIndex) if (nextIndex != 0) newData[index] := #nextIndex nextIndex;
      };
    };

    return { var body = (newBuckets, newData, capacity, takenSize, size) };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func mapFilter<K, V1, V2>(map: Map<K, V1>, fn: (key: K, value: V1) -> ?V2): Map<K, V2> {
    let (_, data, _, takenSize, _) = map.body;

    let newBuckets = Prim.Array_init<Nat32>(0, 0);
    let newData = Prim.Array_init<Slot<K, V2>>(toNat(takenSize), #nextIndex 0);
    var newCapacity = 2:Nat32;
    var newSize = 0:Nat32;

    for (entry in data.vals()) switch (entry) {
      case (#entry (key, prevValue, hash, _)) switch (fn(key, prevValue)) {
        case (?value) {
          newData[toNat(newSize)] := #entry (key, value, hash, 0);

          newSize += 1;

          if (newSize == newCapacity) newCapacity *= 2;
        };

        case (_) {};
      };

      case (_) {};
    };

    let newMap = { var body = (newBuckets, newData, newCapacity, newSize, newSize) };

    rehash(newMap);

    return newMap;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func new<K, V>(): Map<K, V> {
    return { var body = (Prim.Array_init<Nat32>(2, 0), Prim.Array_init<Slot<K, V>>(2, #nextIndex 0), 2, 0, 0) };
  };

  public func clear<K, V>(map: Map<K, V>) {
    map.body := (Prim.Array_init<Nat32>(2, 0), Prim.Array_init<Slot<K, V>>(2, #nextIndex 0), 2, 0, 0);
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

  public func filter<K, V>(map: Map<K, V>, fn: (key: K, value: V) -> Bool): Map<K, V> {
    return mapFilter(map, func(key: K, value: V): ?V { if (fn(key, value)) ?value else null });
  };
};
