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

  let PREV = 0; let NEXT = 1; let BUCKET_NEXT = 2;

  let NULL_HASH = 0xffffffff:Nat32;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func rehash<K>(set: Set<K>) {
    let body = set.body;

    let newCapacity = 2 **% (33 -% clz(set.size));
    let newBuckets = initArray<Entry<K>>(nat(newCapacity), body.2);
    var entry = body.2.0[NEXT];

    loop {
      if (entry.2 == NULL_HASH) return set.body := (newBuckets, newCapacity, body.2) else {
        let bucketIndex = nat(entry.2 % newCapacity);

        entry.0[BUCKET_NEXT] := newBuckets[bucketIndex];
        newBuckets[bucketIndex] := entry;
        entry := entry.0[NEXT];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func has<K>(set: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    let body = set.body;

    let hashParam = hashUtils.0(keyParam);
    var entry = body.0[nat(hashParam % body.1)];

    loop {
      if (entry.2 == hashParam and hashUtils.1(entry.1, keyParam)) {
        return true;
      } else if (entry.2 == NULL_HASH) {
        return false;
      } else {
        entry := entry.0[BUCKET_NEXT];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func putHelper<K>(set: Set<K>, hashUtils: HashUtils<K>, keyParam: K, DPREV: Nat, DNEXT: Nat): Bool {
    let body = set.body;

    let hashParam = hashUtils.0(keyParam);
    let bucketIndex = nat(hashParam % body.1);
    let bucketEntry = body.0[bucketIndex];
    var entry = bucketEntry;

    loop {
      if (entry.2 == NULL_HASH) {
        let newSize = set.size +% 1;
        let prevEntry = body.2.0[DPREV];

        let newLinks = if (DPREV == PREV) [var prevEntry, body.2, bucketEntry] else [var body.2, prevEntry, bucketEntry];
        let newEntry = (newLinks, keyParam, hashParam);

        prevEntry.0[DNEXT] := newEntry;
        body.2.0[DPREV] := newEntry;
        body.0[bucketIndex] := newEntry;
        set.size := newSize;

        if (newSize == body.1) rehash(set);

        return false;
      } else if (entry.2 == hashParam and hashUtils.1(entry.1, keyParam)) {
        return true;
      } else {
        entry := entry.0[BUCKET_NEXT];
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

  public func remove<K>(set: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    let body = set.body;

    let hashParam = hashUtils.0(keyParam);
    let bucketIndex = nat(hashParam % body.1);
    var entry = body.0[bucketIndex];
    var bucketPrev = body.2;

    loop {
      if (entry.2 == hashParam and hashUtils.1(entry.1, keyParam)) {
        let size = set.size;
        let prevEntry = entry.0[PREV];
        let nextEntry = entry.0[NEXT];

        prevEntry.0[NEXT] := nextEntry;
        nextEntry.0[PREV] := prevEntry;
        set.size := size -% 1;

        if (bucketPrev.2 == NULL_HASH) body.0[bucketIndex] := entry.0[BUCKET_NEXT] else bucketPrev.0[BUCKET_NEXT] := entry.0[BUCKET_NEXT];
        if (size == body.1 / 8) rehash(set);

        return true;
      } else if (entry.2 == NULL_HASH) {
        return false;
      } else {
        bucketPrev := entry;
        entry := entry.0[BUCKET_NEXT];
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

    let body = set.body;

    let bucketIndex = nat(body.2.0[DPREV].2 % body.1);
    var entry = body.0[bucketIndex];
    var bucketPrev = body.2;

    loop {
      if (entry.0[DNEXT].2 == NULL_HASH) {
        let prevEntry = entry.0[DPREV];

        prevEntry.0[DNEXT] := body.2;
        body.2.0[DPREV] := prevEntry;
        set.size := size -% 1;

        if (bucketPrev.2 == NULL_HASH) body.0[bucketIndex] := entry.0[BUCKET_NEXT] else bucketPrev.0[BUCKET_NEXT] := entry.0[BUCKET_NEXT];
        if (size == body.1 / 8) rehash(set);

        return ?entry.1;
      } else {
        bucketPrev := entry;
        entry := entry.0[BUCKET_NEXT];
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
    let entry = set.body.2.0[PREV];

    return if (entry.2 == NULL_HASH) null else ?entry.1;
  };

  public func peekFront<K>(set: Set<K>): ?K {
    let entry = set.body.2.0[NEXT];

    return if (entry.2 == NULL_HASH) null else ?entry.1;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func filter<K>(set: Set<K>, fn: (K) -> Bool): Set<K> {
    let body = set.body;

    let newEdgeEntry = createEdgeEntry<K>(body.2.1);
    var entry = body.2.0[NEXT];
    var prevEntry = newEdgeEntry;
    var newSize = 0:Nat32;

    loop {
      if (entry.2 == NULL_HASH) {
        let newSet: Set<K> = { var body = ([var], 0, newEdgeEntry); var size = newSize };

        newEdgeEntry.0[PREV] := prevEntry;
        rehash(newSet);

        return newSet;
      } else if (fn(entry.1)) {
        let newEntry = ([var prevEntry, newEdgeEntry, newEdgeEntry], entry.1, entry.2);

        prevEntry.0[NEXT] := newEntry;
        prevEntry := newEntry;
        entry := entry.0[NEXT];
        newSize +%= 1;
      } else {
        entry := entry.0[NEXT];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func keys<K>(set: Set<K>): Iter.Iter<K> = object {
    var entry = set.body.2;

    public func next(): ?K {
      entry := entry.0[NEXT];

      return if (entry.2 == NULL_HASH) null else ?entry.1;
    };
  };

  public func keysDesc<K>(set: Set<K>): Iter.Iter<K> = object {
    var entry = set.body.2;

    public func next(): ?K {
      entry := entry.0[PREV];

      return if (entry.2 == NULL_HASH) null else ?entry.1;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func existHelper<K>(set: Set<K>, fn: (K) -> Bool, DNEXT: Nat): Bool {
    var entry = set.body.2;

    loop {
      entry := entry.0[DNEXT];

      if (entry.2 == NULL_HASH) return false else if (fn(entry.1)) return true;
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
    var entry = set.body.2;

    loop {
      entry := entry.0[NEXT];

      if (entry.2 == NULL_HASH) return null else if (fn(entry.1)) return ?entry.1;
    };
  };

  public func findDesc<K>(set: Set<K>, fn: (K) -> Bool): ?K {
    var entry = set.body.2;

    loop {
      entry := entry.0[PREV];

      if (entry.2 == NULL_HASH) return null else if (fn(entry.1)) return ?entry.1;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func fromIter<K>(iter: Iter.Iter<K>, hashUtils: HashUtils<K>): Set<K> {
    let newSet = new<K>(hashUtils);

    for (key in iter) add(newSet, hashUtils, key);

    return newSet;
  };

  public func toArray<K, T>(set: Set<K>, fn: (K) -> ?T): [T] {
    let array = initArray<?T>(nat(set.size), null);
    var entry = set.body.2.0[NEXT];
    var arraySize = 0:Nat32;

    loop {
      if (entry.2 == NULL_HASH) {
        return tabulateArray<T>(nat(arraySize), func(i) {
          switch (array[i]) { case (?item) item; case (_) trap("unreachable") };
        });
      } else switch (fn(entry.1)) {
        case (null) entry := entry.0[NEXT];

        case (newValue) {
          array[nat(arraySize)] := newValue;
          entry := entry.0[NEXT];
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

  public func new<K>(hashUtils: HashUtils<K>): Set<K> {
    let newEdgeEntry = createEdgeEntry<K>(hashUtils.2());

    return { var body = ([var newEdgeEntry, newEdgeEntry], 2, newEdgeEntry); var size = 0 };
  };

  public func clear<K>(set: Set<K>) {
    let newEdgeEntry = createEdgeEntry<K>(set.body.2.1);

    set.body := ([var newEdgeEntry, newEdgeEntry], 2, newEdgeEntry);
    set.size := 0;
  };

  public func size<K>(set: Set<K>): Nat {
    return nat(set.size);
  };
};
