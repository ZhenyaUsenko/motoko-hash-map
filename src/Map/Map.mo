import Iter "mo:base/Iter";
import Prim "mo:prim";
import Utils "../utils";

module {
  public type Entry<K, V> = (
    links: [var Entry<K, V>],
    key: K,
    value: [var V],
    hash: Nat32,
  );

  public type Map<K, V> = {
    var body: (
      buckets: [var Entry<K, V>],
      capacity: Nat32,
      edgeEntry: Entry<K, V>,
    );
    var size: Nat32;
  };

  public type HashUtils<K> = Utils.HashUtils<K>;

  public let { ihash; nhash; thash; phash; bhash; lhash; useHash; calcHash } = Utils;

  let { Array_tabulate = tabulateArray; Array_init = initArray; nat32ToNat = nat; clzNat32 = clz; trap } = Prim;

  let VALUE = 0; let PREV = 0; let NEXT = 1; let BUCKET_NEXT = 2;

  let NULL_HASH = 0xffffffff:Nat32;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func rehash<K, V>(map: Map<K, V>) {
    let (buckets, capacity, edgeEntry) = map.body;

    let newCapacity = 2 **% (33 -% clz(map.size));
    let newBuckets = initArray<Entry<K, V>>(nat(newCapacity), edgeEntry);
    var entry = edgeEntry.0[NEXT];

    loop {
      let (links, key, value, hash) = entry;

      if (hash == NULL_HASH) return map.body := (newBuckets, newCapacity, edgeEntry) else {
        let bucketIndex = nat(hash % newCapacity);

        links[BUCKET_NEXT] := newBuckets[bucketIndex];
        newBuckets[bucketIndex] := entry;
        entry := links[NEXT];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func get<K, V>(map: Map<K, V>, (getHash, areEqual, getNullKey): HashUtils<K>, keyParam: K): ?V {
    let (buckets, capacity, edgeEntry) = map.body;

    let hashParam = getHash(keyParam);
    var entry = buckets[nat(hashParam % capacity)];

    loop {
      let (links, key, value, hash) = entry;

      if (hash == hashParam and areEqual(key, keyParam)) return ?value[VALUE] else if (hash == NULL_HASH) return null else {
        entry := links[BUCKET_NEXT];
      };
    };
  };

  public func has<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    return switch (get(map, hashUtils, keyParam)) { case (null) false; case (_) true };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func putHelper<K, V>(map: Map<K, V>, (getHash, areEqual, getNullKey): HashUtils<K>, keyParam: K, valueParam: V, DPREV: Nat, DNEXT: Nat): ?V {
    let (buckets, capacity, edgeEntry) = map.body;

    let hashParam = getHash(keyParam);
    let bucketIndex = nat(hashParam % capacity);
    let bucketEntry = buckets[bucketIndex];
    var entry = bucketEntry;

    loop {
      let (links, key, value, hash) = entry;

      if (hash == NULL_HASH) {
        let newSize = map.size +% 1;
        let prevEntry = edgeEntry.0[DPREV];

        let newLinks = if (DPREV == PREV) [var prevEntry, edgeEntry, bucketEntry] else [var edgeEntry, prevEntry, bucketEntry];
        let newEntry = (newLinks, keyParam, [var valueParam], hashParam);

        prevEntry.0[DNEXT] := newEntry;
        edgeEntry.0[DPREV] := newEntry;
        buckets[bucketIndex] := newEntry;
        map.size := newSize;

        if (newSize == capacity) rehash(map);

        return null;
      } else if (hash == hashParam and areEqual(key, keyParam)) {
        let prevValue = value[VALUE];

        value[VALUE] := valueParam;

        return ?prevValue;
      } else {
        entry := links[BUCKET_NEXT];
      };
    };
  };

  public func put<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V): ?V {
    return putHelper(map, hashUtils, keyParam, valueParam, PREV, NEXT);
  };

  public func putFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V): ?V {
    return putHelper(map, hashUtils, keyParam, valueParam, NEXT, PREV);
  };

  public func set<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V) {
    ignore putHelper(map, hashUtils, keyParam, valueParam, PREV, NEXT);
  };

  public func setFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V) {
    ignore putHelper(map, hashUtils, keyParam, valueParam, NEXT, PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func remove<K, V>(map: Map<K, V>, (getHash, areEqual, getNullKey): HashUtils<K>, keyParam: K): ?V {
    let (buckets, capacity, edgeEntry) = map.body;

    let hashParam = getHash(keyParam);
    let bucketIndex = nat(hashParam % capacity);
    var entry = buckets[bucketIndex];
    var bucketPrev = edgeEntry;

    loop {
      let (links, key, value, hash) = entry;

      if (hash == hashParam and areEqual(key, keyParam)) {
        let size = map.size;
        let prevEntry = links[PREV];
        let nextEntry = links[NEXT];

        prevEntry.0[NEXT] := nextEntry;
        nextEntry.0[PREV] := prevEntry;
        map.size := size -% 1;

        if (bucketPrev.3 == NULL_HASH) buckets[bucketIndex] := links[BUCKET_NEXT] else bucketPrev.0[BUCKET_NEXT] := links[BUCKET_NEXT];
        if (size == capacity / 8) rehash(map);

        return ?value[VALUE];
      } else if (hash == NULL_HASH) {
        return null;
      } else {
        bucketPrev := entry;
        entry := links[BUCKET_NEXT];
      };
    };
  };

  public func delete<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K) {
    ignore remove(map, hashUtils, keyParam);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func updateHelper<K, V>(map: Map<K, V>, (getHash, areEqual, getNullKey): HashUtils<K>, keyParam: K, fn: (K, ?V) -> V, DPREV: Nat, DNEXT: Nat): V {
    let (buckets, capacity, edgeEntry) = map.body;

    let hashParam = getHash(keyParam);
    let bucketIndex = nat(hashParam % capacity);
    let bucketEntry = buckets[bucketIndex];
    var entry = bucketEntry;

    loop {
      let (links, key, value, hash) = entry;

      if (hash == NULL_HASH) {
        let newSize = map.size +% 1;
        let prevEntry = edgeEntry.0[DPREV];

        let newValue = fn(keyParam, null);
        let newLinks = if (DPREV == PREV) [var prevEntry, edgeEntry, bucketEntry] else [var edgeEntry, prevEntry, bucketEntry];
        let newEntry = (newLinks, keyParam, [var newValue], hashParam);

        prevEntry.0[DNEXT] := newEntry;
        edgeEntry.0[DPREV] := newEntry;
        buckets[bucketIndex] := newEntry;
        map.size := newSize;

        if (newSize == capacity) rehash(map);

        return newValue;
      } else if (hash == hashParam and areEqual(key, keyParam)) {
        let newValue = fn(keyParam, ?value[VALUE]);

        value[VALUE] := newValue;

        return newValue;
      } else {
        entry := links[BUCKET_NEXT];
      };
    };
  };

  public func update<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, getValue: (K, ?V) -> V): V {
    return updateHelper(map, hashUtils, keyParam, getValue, PREV, NEXT);
  };

  public func updateFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, getValue: (K, ?V) -> V): V {
    return updateHelper(map, hashUtils, keyParam, getValue, NEXT, PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func popHelper<K, V>(map: Map<K, V>, DPREV: Nat, DNEXT: Nat): (?K, ?V) {
    let size = map.size;

    if (size == 0) return (null, null);

    let (buckets, capacity, edgeEntry) = map.body;

    let bucketIndex = nat(edgeEntry.0[DPREV].3 % capacity);
    var entry = buckets[bucketIndex];
    var bucketPrev = edgeEntry;

    loop {
      let (links, key, value, hash) = entry;

      if (links[DNEXT].3 == NULL_HASH) {
        let prevEntry = links[DPREV];

        prevEntry.0[DNEXT] := edgeEntry;
        edgeEntry.0[DPREV] := prevEntry;
        map.size := size -% 1;

        if (bucketPrev.3 == NULL_HASH) buckets[bucketIndex] := links[BUCKET_NEXT] else bucketPrev.0[BUCKET_NEXT] := links[BUCKET_NEXT];
        if (size == capacity / 8) rehash(map);

        return (?key, ?value[VALUE]);
      } else {
        bucketPrev := entry;
        entry := links[BUCKET_NEXT];
      };
    };
  };

  public func pop<K, V>(map: Map<K, V>): (?K, ?V) {
    return popHelper(map, PREV, NEXT);
  };

  public func popFront<K, V>(map: Map<K, V>): (?K, ?V) {
    return popHelper(map, NEXT, PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func peek<K, V>(map: Map<K, V>): (?K, ?V) {
    let (buckets, capacity, edgeEntry) = map.body;

    let (links, key, value, hash) = edgeEntry.0[PREV];

    return if (hash == NULL_HASH) (null, null) else (?key, ?value[VALUE]);
  };

  public func peekFront<K, V>(map: Map<K, V>): (?K, ?V) {
    let (buckets, capacity, edgeEntry) = map.body;

    let (links, key, value, hash) = edgeEntry.0[NEXT];

    return if (hash == NULL_HASH) (null, null) else (?key, ?value[VALUE]);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func map<K, V1, V2>(map: Map<K, V1>, fn: (K, V1) -> V2): Map<K, V2> {
    let (buckets, capacity, edgeEntry) = map.body;

    let newEdgeEntry = createEdgeEntry<K, V2>(edgeEntry.1);
    let newBuckets = initArray<Entry<K, V2>>(nat(capacity), newEdgeEntry);
    var entry = edgeEntry.0[NEXT];
    var prevEntry = newEdgeEntry;

    loop {
      let (links, key, value, hash) = entry;

      if (hash == NULL_HASH) {
        newEdgeEntry.0[PREV] := prevEntry;

        return { var body = (newBuckets, capacity, newEdgeEntry); var size = map.size };
      } else {
        let bucketIndex = nat(hash % capacity);

        let newEntry = ([var prevEntry, newEdgeEntry, newBuckets[bucketIndex]], key, [var fn(key, value[VALUE])], hash);

        prevEntry.0[NEXT] := newEntry;
        newBuckets[bucketIndex] := newEntry;
        prevEntry := newEntry;
        entry := links[NEXT];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func mapFilter<K, V1, V2>(map: Map<K, V1>, fn: (K, V1) -> ?V2): Map<K, V2> {
    let (buckets, capacity, edgeEntry) = map.body;

    let newEdgeEntry = createEdgeEntry<K, V2>(edgeEntry.1);
    var entry = edgeEntry.0[NEXT];
    var prevEntry = newEdgeEntry;
    var newSize = 0:Nat32;

    loop {
      let (links, key, value, hash) = entry;

      if (hash == NULL_HASH) {
        let newMap: Map<K, V2> = { var body = ([var], 0, newEdgeEntry); var size = newSize };

        newEdgeEntry.0[PREV] := prevEntry;
        rehash(newMap);

        return newMap;
      } else switch (fn(key, value[VALUE])) {
        case (?newValue) {
          let newEntry = ([var prevEntry, newEdgeEntry, newEdgeEntry], key, [var newValue], hash);

          prevEntry.0[NEXT] := newEntry;
          prevEntry := newEntry;
          entry := links[NEXT];
          newSize +%= 1;
        };

        case (_) entry := links[NEXT];
      };
    };
  };

  public func filter<K, V>(map: Map<K, V>, fn: (K, V) -> Bool): Map<K, V> {
    return mapFilter<K, V, V>(map, func(key, value) { if (fn(key, value)) ?value else null });
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func iterateHelper<K, V, T>(map: Map<K, V>, fn: (K, [var V]) -> T, DNEXT: Nat): Iter.Iter<T> = object {
    let (buckets, capacity, edgeEntry) = map.body;

    var entry = edgeEntry;

    public func next(): ?T {
      entry := entry.0[DNEXT];

      let (links, key, value, hash) = entry;

      return if (hash == NULL_HASH) null else ?fn(key, value);
    };
  };

  public func keys<K, V>(map: Map<K, V>): Iter.Iter<K> {
    return iterateHelper<K, V, K>(map, func(key, value) { key }, NEXT);
  };

  public func keysDesc<K, V>(map: Map<K, V>): Iter.Iter<K> {
    return iterateHelper<K, V, K>(map, func(key, value) { key }, PREV);
  };

  public func vals<K, V>(map: Map<K, V>): Iter.Iter<V> {
    return iterateHelper<K, V, V>(map, func(key, value) { value[VALUE] }, NEXT);
  };

  public func valsDesc<K, V>(map: Map<K, V>): Iter.Iter<V> {
    return iterateHelper<K, V, V>(map, func(key, value) { value[VALUE] }, PREV);
  };

  public func entries<K, V>(map: Map<K, V>): Iter.Iter<(K, V)> {
    return iterateHelper<K, V, (K, V)>(map, func(key, value) { (key, value[VALUE]) }, NEXT);
  };

  public func entriesDesc<K, V>(map: Map<K, V>): Iter.Iter<(K, V)> {
    return iterateHelper<K, V, (K, V)>(map, func(key, value) { (key, value[VALUE]) }, PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func existHelper<K, V>(map: Map<K, V>, fn: (K, V) -> Bool, DNEXT: Nat): Bool {
    let (buckets, capacity, edgeEntry) = map.body;

    var entry = edgeEntry;

    loop {
      entry := entry.0[DNEXT];

      let (links, key, value, hash) = entry;

      if (hash == NULL_HASH) return false else if (fn(key, value[VALUE])) return true;
    };
  };

  public func forEach<K, V>(map: Map<K, V>, fn: (K, V) -> ()) {
    ignore existHelper<K, V>(map, func(key, value) { fn(key, value); false }, NEXT);
  };

  public func forEachDesc<K, V>(map: Map<K, V>, fn: (K, V) -> ()) {
    ignore existHelper<K, V>(map, func(key, value) { fn(key, value); false }, PREV);
  };

  public func some<K, V>(map: Map<K, V>, fn: (K, V) -> Bool): Bool {
    return existHelper<K, V>(map, fn, NEXT);
  };

  public func someDesc<K, V>(map: Map<K, V>, fn: (K, V) -> Bool): Bool {
    return existHelper<K, V>(map, fn, PREV);
  };

  public func every<K, V>(map: Map<K, V>, fn: (K, V) -> Bool): Bool {
    return not existHelper<K, V>(map, func(key, value) { not fn(key, value) }, NEXT);
  };

  public func everyDesc<K, V>(map: Map<K, V>, fn: (K, V) -> Bool): Bool {
    return not existHelper<K, V>(map, func(key, value) { not fn(key, value) }, PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func find<K, V>(map: Map<K, V>, fn: (K, V) -> Bool): (?K, ?V) {
    let (buckets, capacity, edgeEntry) = map.body;

    var entry = edgeEntry;

    loop {
      entry := entry.0[NEXT];

      let (links, key, value, hash) = entry;

      if (hash == NULL_HASH) return (null, null) else {
        let valueBody = value[VALUE];

        if (fn(key, valueBody)) return (?key, ?valueBody);
      };
    };
  };

  public func findDesc<K, V>(map: Map<K, V>, fn: (K, V) -> Bool): (?K, ?V) {
    let (buckets, capacity, edgeEntry) = map.body;

    var entry = edgeEntry;

    loop {
      entry := entry.0[PREV];

      let (links, key, value, hash) = entry;

      if (hash == NULL_HASH) return (null, null) else {
        let valueBody = value[VALUE];

        if (fn(key, valueBody)) return (?key, ?valueBody);
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func fromIter<K, V>(iter: Iter.Iter<(K, V)>, hashUtils: HashUtils<K>): Map<K, V> {
    let newMap = new<K, V>(hashUtils);

    for ((key, value) in iter) set(newMap, hashUtils, key, value);

    return newMap;
  };

  public func toArray<K, V, T>(map: Map<K, V>, fn: (K, V) -> ?T): [T] {
    let (buckets, capacity, edgeEntry) = map.body;

    let array = initArray<?T>(nat(map.size), null);
    var entry = edgeEntry.0[NEXT];
    var arraySize = 0:Nat32;

    loop {
      let (links, key, value, hash) = entry;

      if (hash == NULL_HASH) {
        return tabulateArray<T>(nat(arraySize), func(i) {
          switch (array[i]) { case (?item) item; case (_) trap("unreachable") };
        });
      } else switch (fn(key, value[VALUE])) {
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

  func createEdgeEntry<K, V>(nullKey: K): Entry<K, V> {
    let nullEntry: Entry<K, V> = ([var], nullKey, [var], NULL_HASH);
    let newEdgeEntry: Entry<K, V> = ([var nullEntry, nullEntry], nullKey, [var], NULL_HASH);

    newEdgeEntry.0[PREV] := newEdgeEntry;
    newEdgeEntry.0[NEXT] := newEdgeEntry;

    return newEdgeEntry;
  };

  public func new<K, V>((getHash, areEqual, getNullKey): HashUtils<K>): Map<K, V> {
    let newEdgeEntry = createEdgeEntry<K, V>(getNullKey());

    return { var body = ([var newEdgeEntry, newEdgeEntry], 2, newEdgeEntry); var size = 0 };
  };

  public func clear<K, V>(map: Map<K, V>) {
    let (buckets, capacity, edgeEntry) = map.body;

    let newEdgeEntry = createEdgeEntry<K, V>(edgeEntry.1);

    map.body := ([var newEdgeEntry, newEdgeEntry], 2, newEdgeEntry);
    map.size := 0;
  };

  public func size<K, V>(map: Map<K, V>): Nat {
    return nat(map.size);
  };
};
