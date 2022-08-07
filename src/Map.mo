import Iter "mo:base/Iter";
import Prim "mo:prim";
import Utils "./utils";

module {
  public type Entry<K, V> = (
    key: ?K,
    value: ?V,
    hash: Nat,
    nextIndex: Nat,
  );

  public type Map<K, V> = {
    var body: (
      buckets: [var Nat],
      data: [var Entry<K, V>],
      capacity: Nat,
      takenSize: Nat,
      size: Nat,
    );
  };

  public type HashUtils<K> = Utils.HashUtils<K>;

  public let { ihash; nhash; thash; phash; bhash; lhash; useHash; calcHash } = Utils;

  let { Array_init = initArray } = Prim;

  let NULL_INDEX = 0x3fffffff;

  let NULL_ENTRY = (null, null, 0, NULL_INDEX);

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func rehash<K, V>(map: Map<K, V>) {
    let (_, data, capacity, _, size) = map.body;

    let newCapacity = if (size < capacity / 4) capacity / 2 else if (size > capacity * 3 / 4) capacity * 2 else capacity;

    if (newCapacity > NULL_INDEX) Prim.trap("Map capacity limit reached (2 ** 29)");

    let newBuckets = initArray<Nat>(newCapacity, NULL_INDEX);
    let newData = initArray<Entry<K, V>>(newCapacity, NULL_ENTRY);
    var index = 0;

    for (entry in data.vals()) switch (entry) {
      case (null, _, _, _) {};

      case (key, value, hash, _) {
        let bucketIndex = hash % newCapacity;

        newData[index] := (key, value, hash, newBuckets[bucketIndex]);
        newBuckets[bucketIndex] := index;

        index += 1;
      };
    };

    map.body := (newBuckets, newData, newCapacity, size, size);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func get<K, V>(map: Map<K, V>, (getHash, areEqual): HashUtils<K>, key: K): ?V {
    let (buckets, data, capacity, _, _) = map.body;

    var index = buckets[getHash(key) % capacity];

    loop if (index == NULL_INDEX) return null else switch (data[index]) {
      case (?entryKey, value, _, nextIndex) {
        if (areEqual(entryKey, key)) return value;

        index := nextIndex;
      };

      case (_, _, _, nextIndex) index := nextIndex;
    };
  };

  public func has<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, key: K): Bool {
    return switch (get(map, hashUtils, key)) { case (null) false; case (_) true };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func put<K, V>(map: Map<K, V>, (getHash, areEqual): HashUtils<K>, key: K, value: V): ?V {
    let (buckets, data, capacity, takenSize, size) = map.body;

    let hash = getHash(key);
    let bucketIndex = hash % capacity;
    let firstIndex = buckets[bucketIndex];
    var index = firstIndex;

    loop if (index == NULL_INDEX) {
      let newTakenSize = takenSize + 1;

      data[takenSize] := (?key, ?value, hash, firstIndex);
      buckets[bucketIndex] := takenSize;

      map.body := (buckets, data, capacity, newTakenSize, size + 1);

      if (newTakenSize == capacity) rehash(map);

      return null;
    } else switch (data[index]) {
      case (?entryKey, prevValue, hash, nextIndex) {
        if (areEqual(entryKey, key)) {
          data[index] := (?entryKey, ?value, hash, nextIndex);

          return prevValue;
        };

        index := nextIndex;
      };

      case (_, _, _, nextIndex) index := nextIndex;
    };
  };

  public func set<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, key: K, value: V) {
    ignore put(map, hashUtils, key, value);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func remove<K, V>(map: Map<K, V>, (getHash, areEqual): HashUtils<K>, key: K): ?V {
    let (buckets, data, capacity, takenSize, size) = map.body;

    var index = buckets[getHash(key) % capacity];

    loop if (index == NULL_INDEX) return null else switch (data[index]) {
      case (?entryKey, prevValue, _, nextIndex) {
        if (areEqual(entryKey, key)) {
          let newSize: Nat = size - 1;

          data[index] := if (nextIndex != NULL_INDEX) (null, null, 0, nextIndex) else NULL_ENTRY;

          map.body := (buckets, data, capacity, takenSize, newSize);

          if (newSize < capacity / 4) rehash(map);

          return prevValue;
        };

        index := nextIndex;
      };

      case (_, _, _, nextIndex) index := nextIndex;
    };
  };

  public func delete<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, key: K) {
    ignore remove(map, hashUtils, key);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func map<K, V1, V2>(map: Map<K, V1>, fn: (key: K, value: V1) -> V2): Map<K, V2> {
    let (buckets, data, capacity, takenSize, size) = map.body;

    let newBuckets = initArray<Nat>(capacity, NULL_INDEX);
    let newData = initArray<Entry<K, V2>>(capacity, NULL_ENTRY);

    for (index in buckets.keys()) {
      newBuckets[index] := buckets[index];

      switch (data[index]) {
        case (?key, ?prevValue, hash, nextIndex) newData[index] := (?key, ?fn(key, prevValue), hash, nextIndex);

        case (_, _, _, nextIndex) if (nextIndex != NULL_INDEX) newData[index] := (null, null, 0, nextIndex);
      };
    };

    return { var body = (newBuckets, newData, capacity, takenSize, size) };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func mapFilter<K, V1, V2>(map: Map<K, V1>, fn: (key: K, value: V1) -> ?V2): Map<K, V2> {
    let (_, data, _, takenSize, _) = map.body;

    let newBuckets = initArray<Nat>(0, NULL_INDEX);
    let newData = initArray<Entry<K, V2>>(takenSize, NULL_ENTRY);
    var newCapacity = 2;
    var newSize = 0;

    for (entry in data.vals()) switch (entry) {
      case (?key, ?prevValue, hash, _) switch (fn(key, prevValue)) {
        case (null) {};

        case (value) {
          newData[newSize] := (?key, value, hash, NULL_INDEX);

          newSize += 1;

          if (newSize == newCapacity) newCapacity *= 2;
        };
      };

      case (_) {};
    };

    let newMap = { var body = (newBuckets, newData, newCapacity, newSize, newSize) };

    rehash(newMap);

    return newMap;
  };

  public func filter<K, V>(map: Map<K, V>, fn: (key: K, value: V) -> Bool): Map<K, V> {
    return mapFilter<K, V, V>(map, func(key, value) { if (fn(key, value)) ?value else null });
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func iter<K, V, T>(map: Map<K, V>, fn: (key: K, value: V) -> T): Iter.Iter<T> {
    let (_, data, _, takenSize, _) = map.body;

    var index = 0;

    return {
      next = func(): ?T {
        loop if (index >= takenSize) return null else switch (data[index]) {
          case (?key, ?value, _, _) {
            index += 1;

            return ?fn(key, value);
          };

          case (_) index += 1;
        };
      };
    };
  };

  public func keys<K, V>(map: Map<K, V>): Iter.Iter<K> {
    return iter<K, V, K>(map, func(key, value) { key });
  };

  public func vals<K, V>(map: Map<K, V>): Iter.Iter<V> {
    return iter<K, V, V>(map, func(key, value) { value });
  };

  public func entries<K, V>(map: Map<K, V>): Iter.Iter<(K, V)> {
    return iter<K, V, (K, V)>(map, func(key, value) { (key, value) });
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func forEach<K, V>(map: Map<K, V>, fn: (key: K, value: V) -> ()) {
    let (_, data, _, _, _) = map.body;

    for (entry in data.vals()) switch (entry) { case (?key, ?value, _, _) fn(key, value); case (_) {} };
  };

  public func some<K, V>(map: Map<K, V>, fn: (key: K, value: V) -> Bool): Bool {
    let (_, data, _, _, _) = map.body;

    for (entry in data.vals()) switch (entry) { case (?key, ?value, _, _) if (fn(key, value)) return true; case (_) {} };

    return false;
  };

  public func every<K, V>(map: Map<K, V>, fn: (key: K, value: V) -> Bool): Bool {
    let (_, data, _, _, _) = map.body;

    for (entry in data.vals()) switch (entry) { case (?key, ?value, _, _) if (not fn(key, value)) return false; case (_) {} };

    return true;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func find<K, V>(map: Map<K, V>, fn: (key: K, value: V) -> Bool): ?(K, V) {
    let (_, data, _, _, _) = map.body;

    for (entry in data.vals()) switch (entry) { case (?key, ?value, _, _) if (fn(key, value)) return ?(key, value); case (_) {} };

    return null;
  };

  public func findLast<K, V>(map: Map<K, V>, fn: (key: K, value: V) -> Bool): ?(K, V) {
    let (_, data, _, takenSize, _) = map.body;

    var index = takenSize;

    while (index != 0) {
      index -= 1;

      switch (data[index]) { case (?key, ?value, _, _) if (fn(key, value)) return ?(key, value); case (_) {} };
    };

    return null;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func new<K, V>(): Map<K, V> {
    return { var body = (initArray<Nat>(2, NULL_INDEX), initArray<Entry<K, V>>(2, NULL_ENTRY), 2, 0, 0) };
  };

  public func clear<K, V>(map: Map<K, V>) {
    map.body := (initArray<Nat>(2, NULL_INDEX), initArray<Entry<K, V>>(2, NULL_ENTRY), 2, 0, 0);
  };

  public func fromIter<K, V>(iter: Iter.Iter<(K, V)>, hashUtils: HashUtils<K>): Map<K, V> {
    let map = new<K, V>();

    for ((key, value) in iter) ignore put(map, hashUtils, key, value);

    return map;
  };

  public func size<K, V>(map: Map<K, V>): Nat {
    let (_, _, _, _, size) = map.body;

    return size;
  };
};
