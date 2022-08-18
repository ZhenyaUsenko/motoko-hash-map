import Iter "mo:base/Iter";
import Prim "mo:prim";
import Utils "../utils";

module {
  public type Entry<K> = (
    links: [var Entry<K>],
    key: ?K,
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

  let { Array_init = initArray; nat32ToNat = nat; clzNat32 = clz } = Prim;

  let VALUE = 0; let PREV = 0; let NEXT = 1; let BUCKET_NEXT = 2;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func rehash<K>(set: Set<K>) {
    let (_, _, edgeEntry) = set.body;

    let newCapacity = 2 **% (33 -% clz(set.size));
    let newBuckets = initArray<Entry<K>>(nat(newCapacity), edgeEntry);
    var entry = edgeEntry.0[NEXT];

    loop switch (entry) {
      case (links, ?_, hash) {
        let bucketIndex = nat(hash % newCapacity);

        links[BUCKET_NEXT] := newBuckets[bucketIndex];
        newBuckets[bucketIndex] := entry;
        entry := links[NEXT];
      };

      case (_) return set.body := (newBuckets, newCapacity, edgeEntry);
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func has<K>(set: Set<K>, (getHash, areEqual): HashUtils<K>, keyParam: K): Bool {
    let (buckets, capacity, _) = set.body;

    let hashParam = getHash(keyParam);
    var entry = buckets[nat(hashParam % capacity)];

    loop switch (entry) {
      case (links, ?key, hash) {
        if (hash == hashParam and areEqual(key, keyParam)) return true;

        entry := links[BUCKET_NEXT];
      };

      case (_) return false;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func putHelper<K>(set: Set<K>, (getHash, areEqual): HashUtils<K>, keyParam: K, DIR_PREV: Nat, DIR_NEXT: Nat): Bool {
    let (buckets, capacity, edgeEntry) = set.body;

    let hashParam = getHash(keyParam);
    let bucketIndex = nat(hashParam % capacity);
    let bucketEntry = buckets[bucketIndex];
    var entry = bucketEntry;

    loop switch (entry) {
      case (links, ?key, hash) {
        if (hash == hashParam and areEqual(key, keyParam)) return true;

        entry := links[BUCKET_NEXT];
      };

      case (_) {
        let newSize = set.size +% 1;
        let prevEntry = edgeEntry.0[DIR_PREV];

        let newLinks = if (DIR_PREV == PREV) [var prevEntry, edgeEntry, bucketEntry] else [var edgeEntry, prevEntry, bucketEntry];
        let newEntry: Entry<K> = (newLinks, ?keyParam, hashParam);

        prevEntry.0[DIR_NEXT] := newEntry;
        edgeEntry.0[DIR_PREV] := newEntry;
        buckets[bucketIndex] := newEntry;
        set.size := newSize;

        if (newSize == capacity) rehash(set);

        return false;
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

  public func remove<K>(set: Set<K>, (getHash, areEqual): HashUtils<K>, keyParam: K): Bool {
    let (buckets, capacity, edgeEntry) = set.body;

    let hashParam = getHash(keyParam);
    let bucketIndex = nat(hashParam % capacity);
    var entry = buckets[bucketIndex];
    var bucketPrevEntry = edgeEntry;

    loop switch (entry) {
      case (links, ?key, hash) {
        if (hash == hashParam and areEqual(key, keyParam)) {
          let size = set.size;
          let prevEntry = links[PREV];
          let nextEntry = links[NEXT];

          prevEntry.0[NEXT] := nextEntry;
          nextEntry.0[PREV] := prevEntry;
          set.size := size -% 1;

          switch (bucketPrevEntry) {
            case (_, null, _) buckets[bucketIndex] := links[BUCKET_NEXT];
            case (_) bucketPrevEntry.0[BUCKET_NEXT] := links[BUCKET_NEXT];
          };

          if (size == capacity / 8) rehash(set);

          return true;
        };

        bucketPrevEntry := entry;
        entry := links[BUCKET_NEXT];
      };

      case (_) return false;
    };
  };

  public func delete<K>(set: Set<K>, hashUtils: HashUtils<K>, keyParam: K) {
    ignore remove(set, hashUtils, keyParam);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func popHelper<K>(set: Set<K>, DIR_PREV: Nat, DIR_NEXT: Nat): ?K {
    let size = set.size;

    if (size == 0) return null;

    let (buckets, capacity, edgeEntry) = set.body;

    let bucketIndex = nat(edgeEntry.0[DIR_PREV].2 % capacity);
    var entry = buckets[bucketIndex];
    var bucketPrevEntry = edgeEntry;

    loop switch (entry.0[DIR_NEXT]) {
      case (_, null, _) {
        let (links, key, _) = entry;
        let prevEntry = links[DIR_PREV];

        prevEntry.0[DIR_NEXT] := edgeEntry;
        edgeEntry.0[DIR_PREV] := prevEntry;
        set.size := size -% 1;

        switch (bucketPrevEntry) {
          case (_, null, _) buckets[bucketIndex] := links[BUCKET_NEXT];
          case (_) bucketPrevEntry.0[BUCKET_NEXT] := links[BUCKET_NEXT];
        };

        if (size == capacity / 8) rehash(set);

        return key;
      };

      case (_) {
        bucketPrevEntry := entry;
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
    let (_, _, edgeEntry) = set.body;

    return edgeEntry.0[PREV].1;
  };

  public func peekFront<K>(set: Set<K>): ?K {
    let (_, _, edgeEntry) = set.body;

    return edgeEntry.0[NEXT].1;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func filter<K>(set: Set<K>, fn: (key: K) -> Bool): Set<K> {
    let (_, capacity, edgeEntry) = set.body;
    let (_, _, newEdgeEntry) = new<K>().body;

    var entry = edgeEntry.0[NEXT];
    var prevEntry = newEdgeEntry;
    var newSize = 0:Nat32;

    loop switch (entry) {
      case (links, ?key, hash) if (fn(key)) {
        let newEntry: Entry<K> = ([var prevEntry, newEdgeEntry, newEdgeEntry], ?key, hash);

        prevEntry.0[NEXT] := newEntry;
        prevEntry := newEntry;
        entry := links[NEXT];
        newSize +%= 1;
      } else {
        entry := links[NEXT];
      };

      case (_) {
        let newSet: Set<K> = { var body = ([var], 0, newEdgeEntry); var size = newSize };

        newEdgeEntry.0[PREV] := prevEntry;
        rehash(newSet);

        return newSet;
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func keys<K>(set: Set<K>): Iter.Iter<K> = object {
    let (_, _, edgeEntry) = set.body;

    var entry = edgeEntry;

    public func next(): ?K {
      entry := entry.0[NEXT];

      return entry.1;
    };
  };

  public func keysDesc<K>(set: Set<K>): Iter.Iter<K> = object {
    let (_, _, edgeEntry) = set.body;

    var entry = edgeEntry;

    public func next(): ?K {
      entry := entry.0[PREV];

      return entry.1;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func existHelper<K>(set: Set<K>, DIR: Nat, fn: (key: K) -> Bool): Bool {
    let (_, _, edgeEntry) = set.body;

    var entry = edgeEntry;

    loop {
      entry := entry.0[DIR];

      switch (entry) { case (_, ?key, _) if (fn(key)) return true; case (_) return false };
    };
  };

  public func forEach<K>(set: Set<K>, fn: (key: K) -> ()) {
    ignore existHelper<K>(set, NEXT, func(key) { fn(key); false });
  };

  public func forEachDesc<K>(set: Set<K>, fn: (key: K) -> ()) {
    ignore existHelper<K>(set, PREV, func(key) { fn(key); false });
  };

  public func some<K>(set: Set<K>, fn: (key: K) -> Bool): Bool {
    return existHelper<K>(set, NEXT, fn);
  };

  public func someDesc<K>(set: Set<K>, fn: (key: K) -> Bool): Bool {
    return existHelper<K>(set, PREV, fn);
  };

  public func every<K>(set: Set<K>, fn: (key: K) -> Bool): Bool {
    return not existHelper<K>(set, NEXT, func(key) { not fn(key) });
  };

  public func everyDesc<K>(set: Set<K>, fn: (key: K) -> Bool): Bool {
    return not existHelper<K>(set, PREV, func(key) { not fn(key) });
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func findHelper<K>(set: Set<K>, DIR: Nat, fn: (key: K) -> Bool): ?K {
    let (_, _, edgeEntry) = set.body;

    var entry = edgeEntry.0[DIR];

    loop switch (entry) {
      case (links, ?key, _) {
        if (fn(key)) return ?key;

        entry := links[DIR];
      };

      case (_) return null;
    };
  };

  public func find<K>(set: Set<K>, fn: (key: K) -> Bool): ?K {
    return findHelper<K>(set, NEXT, fn);
  };

  public func findDesc<K>(set: Set<K>, fn: (key: K) -> Bool): ?K {
    return findHelper<K>(set, PREV, fn);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func new<K>(): Set<K> {
    let nullEntry: Entry<K> = ([var], null, 0);
    let newEdgeEntry: Entry<K> = ([var nullEntry, nullEntry, nullEntry], null, 0);

    newEdgeEntry.0[PREV] := newEdgeEntry;
    newEdgeEntry.0[NEXT] := newEdgeEntry;

    return { var body = ([var newEdgeEntry, newEdgeEntry], 2, newEdgeEntry); var size = 0 };
  };

  public func clear<K>(set: Set<K>) {
    set.body := new<K>().body;
    set.size := 0;
  };

  public func fromIter<K>(iter: Iter.Iter<K>, hashUtils: HashUtils<K>): Set<K> {
    let newSet = new<K>();

    for (key in iter) add(newSet, hashUtils, key);

    return newSet;
  };

  public func size<K>(set: Set<K>): Nat {
    return nat(set.size);
  };
};
