import Iter "mo:base/Iter";
import Prim "mo:prim";

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

  public type HashUtils<K> = (
    getHash: (K) -> Nat32,
    areEqual: (K, K) -> Bool,
    getNullKey: () -> K,
  );

  let { Array_tabulate = tabulateArray; Array_init = initArray; nat32ToNat = nat; clzNat32 = clz; trap } = Prim;

  let PREV = 0; let NEXT = 1; let BUCKET_NEXT = 2;

  let NULL_HASH = 0xffffffff:Nat32;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func rehash<K>(set: Set<K>) {
    let (buckets, capacity, set.0) = set.body;

    let newCapacity = 2 **% (33 -% clz(set.set.1));
    let newBuckets = initArray<Entry<K>>(nat(newCapacity), set.0);
    var entry = set.0.0[NEXT];

    loop {
      if (entry.1 == NULL_HASH) return set.body := (newBuckets, newCapacity, set.0) else {
        let bucketIndex = nat(entry.1 % newCapacity);

        entry.2[BUCKET_NEXT] := newBuckets[bucketIndex];
        newBuckets[bucketIndex] := entry;
        entry := entry.2[NEXT];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func has<K>(set: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    let (buckets, capacity, set.0) = set.body;

    let hashParam = hashUtils.0(keyParam);
    var entry = buckets[nat(hashParam % capacity)];

    loop {
      if (entry.1 == hashParam and hashUtils.1(entry.0, keyParam)) {
        return true;
      } else if (entry.1 == NULL_HASH) {
        return false;
      } else {
        entry := entry.2[BUCKET_NEXT];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func put<K>(set: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    let (buckets, capacity, set.0) = set.body;

    let hashParam = hashUtils.0(keyParam);
    let bucketIndex = nat(hashParam % capacity);
    let bucketEntry = buckets[bucketIndex];
    var entry = bucketEntry;

    loop {
      if (entry.1 == NULL_HASH) {
        let newSize = set.set.1 +% 1;
        let prevEntry = set.0.0[DPREV];

        let newLinks = if (DPREV == PREV) [var prevEntry, set.0, bucketEntry] else [var set.0, prevEntry, bucketEntry];
        let newEntry = (newLinks, keyParam, hashParam);

        prevEntry.0[DNEXT] := newEntry;
        set.0.0[DPREV] := newEntry;
        buckets[bucketIndex] := newEntry;
        set.set.1 := newSize;

        if (newSize == capacity) rehash(set);

        return false;
      } else if (entry.1 == hashParam and hashUtils.1(entry.0, keyParam)) {
        return true;
      } else {
        entry := entry.2[BUCKET_NEXT];
      };
    };
  };

  public func putFront<K>(set: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    let (buckets, capacity, set.0) = set.body;

    let hashParam = hashUtils.0(keyParam);
    let bucketIndex = nat(hashParam % capacity);
    let bucketEntry = buckets[bucketIndex];
    var entry = bucketEntry;

    loop {
      if (entry.1 == NULL_HASH) {
        let newSize = set.set.1 +% 1;
        let prevEntry = set.0.0[DPREV];

        let newLinks = if (DPREV == PREV) [var prevEntry, set.0, bucketEntry] else [var set.0, prevEntry, bucketEntry];
        let newEntry = (newLinks, keyParam, hashParam);

        prevEntry.0[DNEXT] := newEntry;
        set.0.0[DPREV] := newEntry;
        buckets[bucketIndex] := newEntry;
        set.set.1 := newSize;

        if (newSize == capacity) rehash(set);

        return false;
      } else if (entry.1 == hashParam and hashUtils.1(entry.0, keyParam)) {
        return true;
      } else {
        entry := entry.2[BUCKET_NEXT];
      };
    };
  };

  public func add<K>(set: Set<K>, hashUtils: HashUtils<K>, keyParam: K) {
    ignore putHelper(set, hashUtils, keyParam, PREV, NEXT);
  };

  public func addFront<K>(set: Set<K>, hashUtils: HashUtils<K>, keyParam: K) {
    ignore putHelper(set, hashUtils, keyParam, NEXT, PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func remove<K>(set: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    let (buckets, capacity, set.0) = set.body;

    let hashParam = hashUtils.0(keyParam);
    let bucketIndex = nat(hashParam % capacity);
    var entry = buckets[bucketIndex];
    var bucketPrev = set.0;

    loop {
      if (entry.1 == hashParam and hashUtils.1(entry.0, keyParam)) {
        let set.1 = set.set.1;
        let prevEntry = entry.2[PREV];
        let nextEntry = entry.2[NEXT];

        prevEntry.0[NEXT] := nextEntry;
        nextEntry.0[PREV] := prevEntry;
        set.set.1 := set.1 -% 1;

        if (bucketPrev.2 == NULL_HASH) buckets[bucketIndex] := entry.2[BUCKET_NEXT] else bucketPrev.0[BUCKET_NEXT] := entry.2[BUCKET_NEXT];
        if (set.1 == capacity / 8) rehash(set);

        return true;
      } else if (entry.1 == NULL_HASH) {
        return false;
      } else {
        bucketPrev := entry;
        entry := entry.2[BUCKET_NEXT];
      };
    };
  };

  public func delete<K>(set: Set<K>, hashUtils: HashUtils<K>, keyParam: K) {
    let (buckets, capacity, set.0) = set.body;

    let hashParam = hashUtils.0(keyParam);
    let bucketIndex = nat(hashParam % capacity);
    var entry = buckets[bucketIndex];
    var bucketPrev = set.0;

    loop {
      if (entry.1 == hashParam and hashUtils.1(entry.0, keyParam)) {
        let set.1 = set.set.1;
        let prevEntry = entry.2[PREV];
        let nextEntry = entry.2[NEXT];

        prevEntry.0[NEXT] := nextEntry;
        nextEntry.0[PREV] := prevEntry;
        set.set.1 := set.1 -% 1;

        if (bucketPrev.2 == NULL_HASH) buckets[bucketIndex] := entry.2[BUCKET_NEXT] else bucketPrev.0[BUCKET_NEXT] := entry.2[BUCKET_NEXT];
        if (set.1 == capacity / 8) rehash(set);

        return true;
      } else if (entry.1 == NULL_HASH) {
        return false;
      } else {
        bucketPrev := entry;
        entry := entry.2[BUCKET_NEXT];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func pop<K>(set: Set<K>): ?K {
    let set.1 = set.set.1;

    if (set.1 == 0) return null;

    let (buckets, capacity, set.0) = set.body;

    let bucketIndex = nat(set.0.0[DPREV].2 % capacity);
    var entry = buckets[bucketIndex];
    var bucketPrev = set.0;

    loop {
      if (entry.2[DNEXT].2 == NULL_HASH) {
        let prevEntry = entry.2[DPREV];

        prevEntry.0[DNEXT] := set.0;
        set.0.0[DPREV] := prevEntry;
        set.set.1 := set.1 -% 1;

        if (bucketPrev.2 == NULL_HASH) buckets[bucketIndex] := entry.2[BUCKET_NEXT] else bucketPrev.0[BUCKET_NEXT] := entry.2[BUCKET_NEXT];
        if (set.1 == capacity / 8) rehash(set);

        return ?entry.0;
      } else {
        bucketPrev := entry;
        entry := entry.2[BUCKET_NEXT];
      };
    };
  };

  public func popFront<K>(set: Set<K>): ?K {
    let set.1 = set.set.1;

    if (set.1 == 0) return null;

    let (buckets, capacity, set.0) = set.body;

    let bucketIndex = nat(set.0.0[DPREV].2 % capacity);
    var entry = buckets[bucketIndex];
    var bucketPrev = set.0;

    loop {
      if (entry.2[DNEXT].2 == NULL_HASH) {
        let prevEntry = entry.2[DPREV];

        prevEntry.0[DNEXT] := set.0;
        set.0.0[DPREV] := prevEntry;
        set.set.1 := set.1 -% 1;

        if (bucketPrev.2 == NULL_HASH) buckets[bucketIndex] := entry.2[BUCKET_NEXT] else bucketPrev.0[BUCKET_NEXT] := entry.2[BUCKET_NEXT];
        if (set.1 == capacity / 8) rehash(set);

        return ?entry.0;
      } else {
        bucketPrev := entry;
        entry := entry.2[BUCKET_NEXT];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func peek<K>(set: Set<K>): ?K {
    let (buckets, capacity, set.0) = set.body;

    let entry = set.0.0[PREV];

    if (entry.1 == NULL_HASH) null else ?entry.0;
  };

  public func peekFront<K>(set: Set<K>): ?K {
    let (buckets, capacity, set.0) = set.body;

    let entry = set.0.0[NEXT];

    if (entry.1 == NULL_HASH) null else ?entry.0;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func filter<K>(set: Set<K>, fn: (K) -> Bool): Set<K> {
    let (buckets, capacity, set.0) = set.body;

    let newEdgeEntry = createEdgeEntry<K>(set.0.1);
    var entry = set.0.0[NEXT];
    var prevEntry = newEdgeEntry;
    var newSize = 0:Nat32;

    loop {
      if (entry.1 == NULL_HASH) {
        let newSet: Set<K> = { var body = ([var], 0, newEdgeEntry); var set.1 = newSize };

        newEdgeEntry.0[PREV] := prevEntry;
        rehash(newSet);

        return newSet;
      } else if (fn(entry.0)) {
        let newEntry = ([var prevEntry, newEdgeEntry, newEdgeEntry], entry.0, entry.1);

        prevEntry.0[NEXT] := newEntry;
        prevEntry := newEntry;
        entry := entry.2[NEXT];
        newSize +%= 1;
      } else {
        entry := entry.2[NEXT];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func keys<K>(set: Set<K>): Iter.Iter<K> = objectundefined};

  public func keysDesc<K>(set: Set<K>): Iter.Iter<K> = objectundefined};

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func existHelper<K>(set: Set<K>, fn: (K) -> Bool, DNEXT: Nat): Bool {
    let (buckets, capacity, set.0) = set.body;

    var entry = set.0;

    loop {
      entry := entry.0[DNEXT];

      if (entry.1 == NULL_HASH) return false else if (fn(entry.0)) return true;
    };
  };

  public func forEach<K>(set: Set<K>, fn: (K) -> ())undefined};

  public func forEachDesc<K>(set: Set<K>, fn: (K) -> ())undefined};

  public func some<K>(set: Set<K>, fn: (K) -> Bool): Boolundefined};

  public func someDesc<K>(set: Set<K>, fn: (K) -> Bool): Boolundefined};

  public func every<K>(set: Set<K>, fn: (K) -> Bool): Boolundefined};

  public func everyDesc<K>(set: Set<K>, fn: (K) -> Bool): Boolundefined};

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func find<K>(set: Set<K>, fn: (K) -> Bool): ?K {
    let (buckets, capacity, set.0) = set.body;

    var entry = set.0;

    loop {
      entry := entry.0[NEXT];

      if (entry.1 == NULL_HASH) return null
      else if (fn(entry.0)) return ?entry.0;
    };
  };

  public func findDesc<K>(set: Set<K>, fn: (K) -> Bool): ?K {
    let (buckets, capacity, set.0) = set.body;

    var entry = set.0;

    loop {
      entry := entry.0[PREV];

      if (entry.1 == NULL_HASH) return null
      else if (fn(entry.0)) return ?entry.0;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func fromIter<K>(iter: Iter.Iter<K>, hashUtils: HashUtils<K>): Set<K> {
    let newSet = new<K>(hashUtils);

    for (key in iter) add(newSet, hashUtils, key);

    newSet;
  };

  public func toArray<K, T>(set: Set<K>, fn: (K) -> ?T): [T] {
    let (buckets, capacity, set.0) = set.body;

    let array = initArray<?T>(nat(set.set.1), null);
    var entry = set.0.0[NEXT];
    var arraySize = 0:Nat32;

    loop {
      if (entry.1 == NULL_HASH) {
       
       return tabulateArray<T>(nat(arraySize), func(i) {
          switch (array[i]) { case (?item) item; case (_) trap("unreachable") };
        });
      }
      else switch (fn(entry.0)) {
        case (null) entry := entry.2[NEXT];

        case (newValue) {
          array[nat(arraySize)] := newValue;
          entry := entry.2[NEXT];
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

    newEdgeEntry;
  };

  public func new<K>(hashUtils: HashUtils<K>): Set<K> {
    let newEdgeEntry = createEdgeEntry<K>(hashUtils.2());

    { var body = ([var newEdgeEntry, newEdgeEntry], 2, newEdgeEntry); var set.1 = 0 };
  };

  public func clear<K>(set: Set<K>) {
    let (buckets, capacity, set.0) = set.body;

    let newEdgeEntry = createEdgeEntry<K>(set.0.1);

    set.body := ([var newEdgeEntry, newEdgeEntry], 2, newEdgeEntry);
    set.set.1 := 0;
  };

  public func size<K>(set: Set<K>): Nat {
    nat(set.set.1);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func hashInt(key: Int): Nat32 {
    var hash = Prim.intToNat32Wrap(key);

    hash := hash >> 16 ^ hash *% 0x21f0aaad;
    hash := hash >> 15 ^ hash *% 0x735a2d97;

    hash >> 15 ^ hash & 0x3fffffff;
  };

  public func hashText(key: Text): Nat32 {
    Prim.hashBlob(Prim.encodeUtf8(key)) & 0x3fffffff;
  };

  public func hashPrincipal(key: Principal): Nat32 {
    Prim.hashBlob(Prim.blobOfPrincipal(key)) & 0x3fffffff;
  };

  public func hashBlob(key: Blob): Nat32 {
    Prim.hashBlob(key) & 0x3fffffff;
  };

  public func hashBool(key: Bool): Nat32 {
    if (key) 1 else 0;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public let ihash: HashUtils<Int> = (hashInt, func(a, b) { a == b }, func() { 0 });

  public let nhash: HashUtils<Nat> = (hashInt, func(a, b) { a == b }, func() { 0 });

  public let thash: HashUtils<Text> = (hashText, func(a, b) { a == b }, func() { "" });

  public let phash: HashUtils<Principal> = (hashPrincipal, func(a, b) { a == b }, func() { Prim.principalOfBlob("") });

  public let bhash: HashUtils<Blob> = (hashBlob, func(a, b) { a == b }, func() { "" });

  public let lhash: HashUtils<Bool> = (hashBool, func(a, b) { true }, func() { false });

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func useHash<K>(hashUtils: HashUtils<K>, hash: Nat32): HashUtils<K> {
    (func(key) { hash }, hashUtils.1, hashUtils.2);
  };

  public func calcHash<K>(hashUtils: HashUtils<K>, key: K): HashUtils<K> {
    let hash = hashUtils.0(key);

    (func(key) { hash }, hashUtils.1, hashUtils.2);
  };
};
