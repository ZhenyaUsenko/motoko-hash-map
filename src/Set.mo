import Iter "mo:base/Iter";
import Prim "mo:prim";
import Utils "./utils";

module {
  public type Entry<K> = (
    key: ?K,
    hash: Nat,
    nextIndex: Nat,
  );

  public type Set<K> = {
    var body: (
      buckets: [var Nat],
      data: [var Entry<K>],
      capacity: Nat,
      takenSize: Nat,
      size: Nat,
    );
  };

  public type HashUtils<K> = Utils.HashUtils<K>;

  public let { ihash; nhash; thash; phash; bhash; lhash; useHash; calcHash } = Utils;

  let { Array_init = initArray } = Prim;

  let NULL_INDEX = 0x3fffffff;

  let NULL_ENTRY = (null, 0, NULL_INDEX);

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func rehash<K>(set: Set<K>) {
    let (_, data, capacity, _, size) = set.body;

    let newCapacity = if (size < capacity / 4) capacity / 2 else if (size > capacity * 3 / 4) capacity * 2 else capacity;

    if (newCapacity > NULL_INDEX) Prim.trap("Set capacity limit reached (2 ** 29)");

    let newBuckets = initArray<Nat>(newCapacity, NULL_INDEX);
    let newData = initArray<Entry<K>>(newCapacity, NULL_ENTRY);
    var index = 0;

    for (entry in data.vals()) switch (entry) {
      case (null, _, _) {};

      case (key, hash, _) {
        let bucketIndex = hash % newCapacity;

        newData[index] := (key, hash, newBuckets[bucketIndex]);
        newBuckets[bucketIndex] := index;

        index += 1;
      };
    };

    set.body := (newBuckets, newData, newCapacity, size, size);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func has<K>(set: Set<K>, (getHash, areEqual): HashUtils<K>, key: K): Bool {
    let (buckets, data, capacity, _, _) = set.body;

    var index = buckets[getHash(key) % capacity];

    loop if (index == NULL_INDEX) return false else switch (data[index]) {
      case (?entryKey, _, nextIndex) {
        if (areEqual(entryKey, key)) return true;

        index := nextIndex;
      };

      case (_, _, nextIndex) index := nextIndex;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func put<K>(set: Set<K>, (getHash, areEqual): HashUtils<K>, key: K): Bool {
    let (buckets, data, capacity, takenSize, size) = set.body;

    let hash = getHash(key);
    let bucketIndex = hash % capacity;
    let firstIndex = buckets[bucketIndex];
    var index = firstIndex;

    loop if (index == NULL_INDEX) {
      let newTakenSize = takenSize + 1;

      data[takenSize] := (?key, hash, firstIndex);
      buckets[bucketIndex] := takenSize;

      set.body := (buckets, data, capacity, newTakenSize, size + 1);

      if (newTakenSize == capacity) rehash(set);

      return false;
    } else switch (data[index]) {
      case (?entryKey, _, nextIndex) {
        if (areEqual(entryKey, key)) return true;

        index := nextIndex;
      };

      case (_, _, nextIndex) index := nextIndex;
    };
  };

  public func add<K>(set: Set<K>, hashUtils: HashUtils<K>, key: K) {
    ignore put(set, hashUtils, key);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func remove<K>(set: Set<K>, (getHash, areEqual): HashUtils<K>, key: K): Bool {
    let (buckets, data, capacity, takenSize, size) = set.body;

    var index = buckets[getHash(key) % capacity];

    loop if (index == NULL_INDEX) return false else switch (data[index]) {
      case (?entryKey, _, nextIndex) {
        if (areEqual(entryKey, key)) {
          let newSize: Nat = size - 1;

          data[index] := if (nextIndex != NULL_INDEX) (null, 0, nextIndex) else NULL_ENTRY;

          set.body := (buckets, data, capacity, takenSize, newSize);

          if (newSize < capacity / 4) rehash(set);

          return true;
        };

        index := nextIndex;
      };

      case (_, _, nextIndex) index := nextIndex;
    };
  };

  public func delete<K>(set: Set<K>, hashUtils: HashUtils<K>, key: K) {
    ignore remove(set, hashUtils, key);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func filter<K>(set: Set<K>, fn: (key: K) -> Bool): Set<K> {
    let (_, data, _, takenSize, _) = set.body;

    let newBuckets = initArray<Nat>(0, NULL_INDEX);
    let newData = initArray<Entry<K>>(takenSize, NULL_ENTRY);
    var newCapacity = 2;
    var newSize = 0;

    for (entry in data.vals()) switch (entry) {
      case (?key, hash, _) if (fn(key)) {
        newData[newSize] := (?key, hash, NULL_INDEX);

        newSize += 1;

        if (newSize == newCapacity) newCapacity *= 2;
      };

      case (_) {};
    };

    let newSet = { var body = (newBuckets, newData, newCapacity, newSize, newSize) };

    rehash(newSet);

    return newSet;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func keys<K>(set: Set<K>): Iter.Iter<K> {
    let (_, data, _, takenSize, _) = set.body;

    var index = 0;

    return {
      next = func(): ?K {
        loop if (index >= takenSize) return null else switch (data[index]) {
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

  public func forEach<K>(set: Set<K>, fn: (key: K) -> ()) {
    let (_, data, _, _, _) = set.body;

    for (entry in data.vals()) switch (entry) { case (?key, _, _) fn(key); case (_) {} };
  };

  public func some<K>(set: Set<K>, fn: (key: K) -> Bool): Bool {
    let (_, data, _, _, _) = set.body;

    for (entry in data.vals()) switch (entry) { case (?key, _, _) if (fn(key)) return true; case (_) {} };

    return false;
  };

  public func every<K>(set: Set<K>, fn: (key: K) -> Bool): Bool {
    let (_, data, _, _, _) = set.body;

    for (entry in data.vals()) switch (entry) { case (?key, _, _) if (not fn(key)) return false; case (_) {} };

    return true;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func find<K>(set: Set<K>, fn: (key: K) -> Bool): ?K {
    let (_, data, _, _, _) = set.body;

    for (entry in data.vals()) switch (entry) { case (?key, _, _) if (fn(key)) return ?key; case (_) {} };

    return null;
  };

  public func findLast<K>(set: Set<K>, fn: (key: K) -> Bool): ?K {
    let (_, data, _, takenSize, _) = set.body;

    var index = takenSize;

    while (index != 0) {
      index -= 1;

      switch (data[index]) { case (?key, _, _) if (fn(key)) return ?key; case (_) {} };
    };

    return null;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func new<K>(): Set<K> {
    return { var body = (initArray<Nat>(2, NULL_INDEX), initArray<Entry<K>>(2, NULL_ENTRY), 2, 0, 0) };
  };

  public func clear<K>(set: Set<K>) {
    set.body := (initArray<Nat>(2, NULL_INDEX), initArray<Entry<K>>(2, NULL_ENTRY), 2, 0, 0);
  };

  public func fromIter<K>(iter: Iter.Iter<K>, hashUtils: HashUtils<K>): Set<K> {
    let set = new<K>();

    for (key in iter) ignore put(set, hashUtils, key);

    return set;
  };

  public func size<K>(set: Set<K>): Nat {
    let (_, _, _, _, size) = set.body;

    return size;
  };
};
