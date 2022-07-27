import Iter "mo:base/Iter";
import Prim "mo:prim";
import Utils "./utils";

module {
  public type Entry<K> = (
    key: ?K,
    hash: Nat32,
    nextIndex: Nat32,
  );

  public type Set<K> = {
    var body: (
      buckets: [var Nat32],
      data: [var Entry<K>],
      capacity: Nat32,
      takenSize: Nat32,
      size: Nat32,
    );
  };

  public type HashUtils<K> = Utils.HashUtils<K>;

  public let { ihash; nhash; thash; phash; bhash; lhash; calcHash } = Utils;

  let toNat = Prim.nat32ToNat;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func rehash<K>(map: Set<K>) {
    let (_, data, capacity, _, size) = map.body;

    let newCapacity = if (size < capacity / 4) capacity / 2 else if (size > capacity * 3 / 4) capacity * 2 else capacity;
    var newTakenSize = 0:Nat32;

    let newBuckets = Prim.Array_init<Nat32>(toNat(newCapacity), 0);
    let newData = Prim.Array_init<Entry<K>>(toNat(newCapacity), (null, 0, 0));

    for (entry in data.vals()) switch (entry) {
      case (null, _, _) {};

      case (key, hash, _) {
        let bucketIndex = toNat(hash % newCapacity);

        newData[toNat(newTakenSize)] := (key, hash, newBuckets[bucketIndex]);

        newTakenSize += 1;

        newBuckets[bucketIndex] := newTakenSize;
      };
    };

    map.body := (newBuckets, newData, newCapacity, newTakenSize, size);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func has<K>(map: Set<K>, (getHash, areEqual): HashUtils<K>, key: K): Bool {
    let (buckets, data, capacity, _, _) = map.body;

    var index = buckets[toNat(getHash(key) % capacity)];

    loop if (index == 0) return false else switch (data[toNat(index - 1)]) {
      case (?entryKey, _, nextIndex) {
        if (areEqual(entryKey, key)) return true;

        index := nextIndex;
      };

      case (_, _, nextIndex) index := nextIndex;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func put<K>(map: Set<K>, (getHash, areEqual): HashUtils<K>, key: K): Bool {
    let (buckets, data, capacity, takenSize, size) = map.body;

    let hash = getHash(key);
    let bucketIndex = toNat(hash % capacity);
    let firstIndex = buckets[bucketIndex];
    var index = firstIndex;

    loop if (index == 0) {
      let newTakenSize = takenSize + 1;

      data[toNat(takenSize)] := (?key, hash, firstIndex);
      buckets[bucketIndex] := newTakenSize;

      map.body := (buckets, data, capacity, newTakenSize, size + 1);

      if (newTakenSize == capacity) rehash(map);

      return false;
    } else {
      let dataIndex = toNat(index - 1);

      switch (data[dataIndex]) {
        case (?entryKey, _, nextIndex) {
          if (areEqual(entryKey, key)) return true;

          index := nextIndex;
        };

        case (_, _, nextIndex) index := nextIndex;
      };
    };
  };

  public func add<K>(map: Set<K>, hashUtils: HashUtils<K>, key: K) {
    ignore put(map, hashUtils, key);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func remove<K>(map: Set<K>, (getHash, areEqual): HashUtils<K>, key: K): Bool {
    let (buckets, data, capacity, takenSize, size) = map.body;

    var index = buckets[toNat(getHash(key) % capacity)];

    loop if (index == 0) return false else {
      let dataIndex = toNat(index - 1);

      switch (data[dataIndex]) {
        case (?entryKey, _, nextIndex) {
          if (areEqual(entryKey, key)) {
            let newSize = size - 1;

            data[dataIndex] := (null, 0, nextIndex);

            map.body := (buckets, data, capacity, takenSize, newSize);

            if (newSize < capacity / 4) rehash(map);

            return true;
          };

          index := nextIndex;
        };

        case (_, _, nextIndex) index := nextIndex;
      };
    };
  };

  public func delete<K>(map: Set<K>, hashUtils: HashUtils<K>, key: K) {
    ignore remove(map, hashUtils, key);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func filter<K>(map: Set<K>, fn: (key: K) -> Bool): Set<K> {
    let (_, data, capacity, _, _) = map.body;

    let newBuckets = Prim.Array_init<Nat32>(0, 0);
    let newData = Prim.Array_init<Entry<K>>(toNat(capacity), (null, 0, 0));
    var newCapacity = 2:Nat32;
    var newSize = 0:Nat32;

    for (entry in data.vals()) switch (entry) {
      case (?key, hash, _) if (fn(key)) {
        newData[toNat(newSize)] := (?key, hash, 0);

        newSize += 1;

        if (newSize == newCapacity) newCapacity *= 2;
      };

      case (_) {};
    };

    let newMap = { var body = (newBuckets, newData, newCapacity, newSize, newSize) };

    rehash(newMap);

    return newMap;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func keys<K>(map: Set<K>): Iter.Iter<K> {
    let (_, data, _, takenSize, _) = map.body;

    var index = 0:Nat32;

    return {
      next = func(): ?K {
        loop if (index >= takenSize) return null else switch (data[toNat(index)]) {
          case (?key, _, _) {
            index += 1;

            return ?key;
          };

          case (_) index += 1;
        };
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func forEach<K>(map: Set<K>, fn: (key: K) -> ()) {
    let (_, data, _, _, _) = map.body;

    for (entry in data.vals()) switch (entry) { case (?key, _, _) fn(key); case (_) {} };
  };

  public func some<K>(map: Set<K>, fn: (key: K) -> Bool): Bool {
    let (_, data, _, _, _) = map.body;

    for (entry in data.vals()) switch (entry) { case (?key, _, _) if (fn(key)) return true; case (_) {} };

    return false;
  };

  public func every<K>(map: Set<K>, fn: (key: K) -> Bool): Bool {
    let (_, data, _, _, _) = map.body;

    for (entry in data.vals()) switch (entry) { case (?key, _, _) if (not fn(key)) return false; case (_) {} };

    return true;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func new<K>(): Set<K> {
    return { var body = (Prim.Array_init<Nat32>(2, 0), Prim.Array_init<Entry<K>>(2, (null, 0, 0)), 2, 0, 0) };
  };

  public func clear<K>(map: Set<K>) {
    map.body := (Prim.Array_init<Nat32>(2, 0), Prim.Array_init<Entry<K>>(2, (null, 0, 0)), 2, 0, 0);
  };

  public func fromIter<K>(iter: Iter.Iter<K>, hashUtils: HashUtils<K>): Set<K> {
    let map = new<K>();

    for (key in iter) ignore put(map, hashUtils, key);

    return map;
  };

  public func size<K>(map: Set<K>): Nat {
    let (_, _, _, _, size) = map.body;

    return toNat(size);
  };
};
