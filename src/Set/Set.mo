import Iter "mo:base/Iter";
import Prim "mo:prim";
import Utils "../utils";

module {
  public type Entry<K> = (
    links: [var Entry<K>],
    key: K,
    hash: Nat32,
  );

  public type Set<K> = {
    var body: (
      buckets: [var Entry<K>],
      capacity: Nat32,
      edgeEntry: Entry<K>,
    );
    var size: Nat32;
  };

  public type HashUtils<K> = Utils.HashUtils<K>;

  public let { ihash; nhash; thash; phash; bhash; lhash; useHash; calcHash } = Utils;

  let { Array_tabulate = tabulateArray; Array_init = initArray; nat32ToNat = nat; clzNat32 = clz; trap } = Prim;

  func unwrap<T>(value: ?T): T { switch (value) { case (?value) value; case (_) trap("unreachable") } };

  let PREV = 0; let NEXT = 1; let BUCKET_NEXT = 2;

  let NULL_HASH = 0xffffffff:Nat32;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func rehash<K>(set: Set<K>) {
    let (buckets, capacity, edgeEntry) = set.body;

    let newCapacity = 2 **% (33 -% clz(set.size));
    let newBuckets = initArray<Entry<K>>(nat(newCapacity), edgeEntry);
    var entry = edgeEntry.0[NEXT];

    loop {
      let (links, key, hash) = entry;

      if (hash == NULL_HASH) return set.body := (newBuckets, newCapacity, edgeEntry) else {
        let bucketIndex = nat(hash % newCapacity);

        links[BUCKET_NEXT] := newBuckets[bucketIndex];
        newBuckets[bucketIndex] := entry;
        entry := links[NEXT];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func has<K>(set: Set<K>, (getHash, areEqual, getNullKey): HashUtils<K>, keyParam: K): Bool {
    let (buckets, capacity, edgeEntry) = set.body;

    let hashParam = getHash(keyParam);
    var entry = buckets[nat(hashParam % capacity)];

    loop {
      let (links, key, hash) = entry;

      if (hash == hashParam and areEqual(key, keyParam)) {
        return true;
      } else if (hash == NULL_HASH) {
        return false;
      } else {
        entry := links[BUCKET_NEXT];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func putHelper<K>(set: Set<K>, (getHash, areEqual, getNullKey): HashUtils<K>, keyParam: K, DPREV: Nat, DNEXT: Nat): Bool {
    let (buckets, capacity, edgeEntry) = set.body;

    let hashParam = getHash(keyParam);
    let bucketIndex = nat(hashParam % capacity);
    let bucketEntry = buckets[bucketIndex];
    var entry = bucketEntry;

    loop {
      let (links, key, hash) = entry;

      if (hash == NULL_HASH) {
        let newSize = set.size +% 1;
        let prevEntry = edgeEntry.0[DPREV];

        let newLinks = if (DPREV == PREV) [var prevEntry, edgeEntry, bucketEntry] else [var edgeEntry, prevEntry, bucketEntry];
        let newEntry = (newLinks, keyParam, hashParam);

        prevEntry.0[DNEXT] := newEntry;
        edgeEntry.0[DPREV] := newEntry;
        buckets[bucketIndex] := newEntry;
        set.size := newSize;

        if (newSize == capacity) rehash(set);

        return false;
      } else if (hash == hashParam and areEqual(key, keyParam)) {
        return true;
      } else {
        entry := links[BUCKET_NEXT];
      };
    };
  };

  public func put<K>(set: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    return putHelper(set, hashUtils, keyParam, PREV, NEXT);
  };

  public func putFront<K>(set: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    return putHelper(set, hashUtils, keyParam, NEXT, PREV);
  };

  public func add<K>(set: Set<K>, hashUtils: HashUtils<K>, keyParam: K) {
    ignore putHelper(set, hashUtils, keyParam, PREV, NEXT);
  };

  public func addFront<K>(set: Set<K>, hashUtils: HashUtils<K>, keyParam: K) {
    ignore putHelper(set, hashUtils, keyParam, NEXT, PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func remove<K>(set: Set<K>, (getHash, areEqual, getNullKey): HashUtils<K>, keyParam: K): Bool {
    let (buckets, capacity, edgeEntry) = set.body;

    let hashParam = getHash(keyParam);
    let bucketIndex = nat(hashParam % capacity);
    var entry = buckets[bucketIndex];
    var bucketPrev = edgeEntry;

    loop {
      let (links, key, hash) = entry;

      if (hash == hashParam and areEqual(key, keyParam)) {
        let size = set.size;
        let prevEntry = links[PREV];
        let nextEntry = links[NEXT];

        prevEntry.0[NEXT] := nextEntry;
        nextEntry.0[PREV] := prevEntry;
        set.size := size -% 1;

        if (bucketPrev.2 == NULL_HASH) buckets[bucketIndex] := links[BUCKET_NEXT] else bucketPrev.0[BUCKET_NEXT] := links[BUCKET_NEXT];
        if (size == capacity / 8) rehash(set);

        return true;
      } else if (hash == NULL_HASH) {
        return false;
      } else {
        bucketPrev := entry;
        entry := links[BUCKET_NEXT];
      };
    };
  };

  public func delete<K>(set: Set<K>, hashUtils: HashUtils<K>, keyParam: K) {
    ignore remove(set, hashUtils, keyParam);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func popHelper<K>(set: Set<K>, DPREV: Nat, DNEXT: Nat): ?K {
    let size = set.size;

    if (size == 0) return null;

    let (buckets, capacity, edgeEntry) = set.body;

    let bucketIndex = nat(edgeEntry.0[DPREV].2 % capacity);
    var entry = buckets[bucketIndex];
    var bucketPrev = edgeEntry;

    loop {
      let (links, key, hash) = entry;

      if (links[DNEXT].2 == NULL_HASH) {
        let prevEntry = links[DPREV];

        prevEntry.0[DNEXT] := edgeEntry;
        edgeEntry.0[DPREV] := prevEntry;
        set.size := size -% 1;

        if (bucketPrev.2 == NULL_HASH) buckets[bucketIndex] := links[BUCKET_NEXT] else bucketPrev.0[BUCKET_NEXT] := links[BUCKET_NEXT];
        if (size == capacity / 8) rehash(set);

        return ?key;
      } else {
        bucketPrev := entry;
        entry := links[BUCKET_NEXT];
      };
    };
  };

  public func pop<K>(set: Set<K>): ?K {
    return popHelper(set, PREV, NEXT);
  };

  public func popFront<K>(set: Set<K>): ?K {
    return popHelper(set, NEXT, PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func peek<K>(set: Set<K>): ?K {
    let (buckets, capacity, edgeEntry) = set.body;

    let (links, key, hash) = edgeEntry.0[PREV];

    return if (hash == NULL_HASH) null else ?key;
  };

  public func peekFront<K>(set: Set<K>): ?K {
    let (buckets, capacity, edgeEntry) = set.body;

    let (links, key, hash) = edgeEntry.0[NEXT];

    return if (hash == NULL_HASH) null else ?key;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func filter<K>(set: Set<K>, fn: (K) -> Bool): Set<K> {
    let (buckets, capacity, edgeEntry) = set.body;

    let newEdgeEntry = createEdgeEntry<K>(edgeEntry.1);
    var entry = edgeEntry.0[NEXT];
    var prevEntry = newEdgeEntry;
    var newSize = 0:Nat32;

    loop {
      let (links, key, hash) = entry;

      if (hash == NULL_HASH) {
        let newSet: Set<K> = { var body = ([var], 0, newEdgeEntry); var size = newSize };

        newEdgeEntry.0[PREV] := prevEntry;
        rehash(newSet);

        return newSet;
      } else if (fn(key)) {
        let newEntry = ([var prevEntry, newEdgeEntry, newEdgeEntry], key, hash);

        prevEntry.0[NEXT] := newEntry;
        prevEntry := newEntry;
        entry := links[NEXT];
        newSize +%= 1;
      } else {
        entry := links[NEXT];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func keys<K>(set: Set<K>): Iter.Iter<K> = object {
    let (buckets, capacity, edgeEntry) = set.body;

    var entry = edgeEntry;

    public func next(): ?K {
      entry := entry.0[NEXT];

      let (links, key, hash) = entry;

      return if (hash == NULL_HASH) null else ?key;
    };
  };

  public func keysDesc<K>(set: Set<K>): Iter.Iter<K> = object {
    let (buckets, capacity, edgeEntry) = set.body;

    var entry = edgeEntry;

    public func next(): ?K {
      entry := entry.0[PREV];

      let (links, key, hash) = entry;

      return if (hash == NULL_HASH) null else ?key;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func existHelper<K>(set: Set<K>, fn: (K) -> Bool, DNEXT: Nat): Bool {
    let (buckets, capacity, edgeEntry) = set.body;

    var entry = edgeEntry;

    loop {
      entry := entry.0[DNEXT];

      let (links, key, hash) = entry;

      if (hash == NULL_HASH) return false else if (fn(key)) return true;
    };
  };

  public func forEach<K>(set: Set<K>, fn: (K) -> ()) {
    ignore existHelper<K>(set, func(key) { fn(key); false }, NEXT);
  };

  public func forEachDesc<K>(set: Set<K>, fn: (K) -> ()) {
    ignore existHelper<K>(set, func(key) { fn(key); false }, PREV);
  };

  public func some<K>(set: Set<K>, fn: (K) -> Bool): Bool {
    return existHelper<K>(set, fn, NEXT);
  };

  public func someDesc<K>(set: Set<K>, fn: (K) -> Bool): Bool {
    return existHelper<K>(set, fn, PREV);
  };

  public func every<K>(set: Set<K>, fn: (K) -> Bool): Bool {
    return not existHelper<K>(set, func(key) { not fn(key) }, NEXT);
  };

  public func everyDesc<K>(set: Set<K>, fn: (K) -> Bool): Bool {
    return not existHelper<K>(set, func(key) { not fn(key) }, PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func find<K>(set: Set<K>, fn: (K) -> Bool): ?K {
    let (buckets, capacity, edgeEntry) = set.body;

    var entry = edgeEntry;

    loop {
      entry := entry.0[NEXT];

      let (links, key, hash) = entry;

      if (hash == NULL_HASH) return null else if (fn(key)) return ?key;
    };
  };

  public func findDesc<K>(set: Set<K>, fn: (K) -> Bool): ?K {
    let (buckets, capacity, edgeEntry) = set.body;

    var entry = edgeEntry;

    loop {
      entry := entry.0[PREV];

      let (links, key, hash) = entry;

      if (hash == NULL_HASH) return null else if (fn(key)) return ?key;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func fromIter<K>(iter: Iter.Iter<K>, hashUtils: HashUtils<K>): Set<K> {
    let newSet = new<K>(hashUtils);

    for (key in iter) add(newSet, hashUtils, key);

    return newSet;
  };

  public func toArray<K, T>(set: Set<K>, fn: (K) -> ?T): [T] {
    let (buckets, capacity, edgeEntry) = set.body;

    let array = initArray<?T>(nat(set.size), null);
    var entry = edgeEntry.0[NEXT];
    var arraySize = 0:Nat32;

    loop {
      let (links, key, hash) = entry;

      if (hash == NULL_HASH) {
        return tabulateArray<T>(nat(arraySize), func(i) {
          switch (array[i]) { case (?item) item; case (_) trap("unreachable") };
        });
      } else switch (fn(key)) {
        case (null) entry := links[NEXT];

        case (newValue) {
          array[nat(arraySize)] := newValue;
          entry := links[NEXT];
          arraySize +%= 1;
        };
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func createEdgeEntry<K>(nullKey: K): Entry<K> {
    let nullEntry: Entry<K> = ([var], nullKey, NULL_HASH);
    let newEdgeEntry: Entry<K> = ([var nullEntry, nullEntry], nullKey, NULL_HASH);

    newEdgeEntry.0[PREV] := newEdgeEntry;
    newEdgeEntry.0[NEXT] := newEdgeEntry;

    return newEdgeEntry;
  };

  public func new<K>((getHash, areEqual, getNullKey): HashUtils<K>): Set<K> {
    let newEdgeEntry = createEdgeEntry<K>(getNullKey());

    return { var body = ([var newEdgeEntry, newEdgeEntry], 2, newEdgeEntry); var size = 0 };
  };

  public func clear<K>(set: Set<K>) {
    let (buckets, capacity, edgeEntry) = set.body;

    let newEdgeEntry = createEdgeEntry<K>(edgeEntry.1);

    set.body := ([var newEdgeEntry, newEdgeEntry], 2, newEdgeEntry);
    set.size := 0;
  };

  public func size<K>(set: Set<K>): Nat {
    return nat(set.size);
  };
};
