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
    let body = map.body;

    let newCapacity = 2 **% (33 -% clz(map.size));
    let newBuckets = initArray<Entry<K, V>>(nat(newCapacity), body.2);
    var entry = body.2.0[NEXT];

    loop {
      if (entry.3 == NULL_HASH) return map.body := (newBuckets, newCapacity, body.2) else {
        let bucketIndex = nat(entry.3 % newCapacity);

        entry.0[BUCKET_NEXT] := newBuckets[bucketIndex];
        newBuckets[bucketIndex] := entry;
        entry := entry.0[NEXT];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func get<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): ?V {
    let body = map.body;

    let hashParam = hashUtils.0(keyParam);
    var entry = body.0[nat(hashParam % body.1)];

    loop {
      if (entry.3 == hashParam and hashUtils.1(entry.1, keyParam)) return ?entry.2[VALUE] else if (entry.3 == NULL_HASH) return null else {
        entry := entry.0[BUCKET_NEXT];
      };
    };
  };

  public func has<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    return switch (get(map, hashUtils, keyParam)) { case (null) false; case (_) true };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func putHelper<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V, DPREV: Nat, DNEXT: Nat): ?V {
    let body = map.body;

    let hashParam = hashUtils.0(keyParam);
    let bucketIndex = nat(hashParam % body.1);
    let bucketEntry = body.0[bucketIndex];
    var entry = bucketEntry;

    loop {
      if (entry.3 == NULL_HASH) {
        let newSize = map.size +% 1;
        let prevEntry = body.2.0[DPREV];

        let newLinks = if (DPREV == PREV) [var prevEntry, body.2, bucketEntry] else [var body.2, prevEntry, bucketEntry];
        let newEntry = (newLinks, keyParam, [var valueParam], hashParam);

        prevEntry.0[DNEXT] := newEntry;
        body.2.0[DPREV] := newEntry;
        body.0[bucketIndex] := newEntry;
        map.size := newSize;

        if (newSize == body.1) rehash(map);

        return null;
      } else if (entry.3 == hashParam and hashUtils.1(entry.1, keyParam)) {
        let prevValue = entry.2[VALUE];

        entry.2[VALUE] := valueParam;

        return ?prevValue;
      } else {
        entry := entry.0[BUCKET_NEXT];
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

  public func remove<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): ?V {
    let body = map.body;

    let hashParam = hashUtils.0(keyParam);
    let bucketIndex = nat(hashParam % body.1);
    var entry = body.0[bucketIndex];
    var bucketPrev = body.2;

    loop {
      if (entry.3 == hashParam and hashUtils.1(entry.1, keyParam)) {
        let size = map.size;
        let prevEntry = entry.0[PREV];
        let nextEntry = entry.0[NEXT];

        prevEntry.0[NEXT] := nextEntry;
        nextEntry.0[PREV] := prevEntry;
        map.size := size -% 1;

        if (bucketPrev.3 == NULL_HASH) body.0[bucketIndex] := entry.0[BUCKET_NEXT] else bucketPrev.0[BUCKET_NEXT] := entry.0[BUCKET_NEXT];
        if (size == body.1 / 8) rehash(map);

        return ?entry.2[VALUE];
      } else if (entry.3 == NULL_HASH) {
        return null;
      } else {
        bucketPrev := entry;
        entry := entry.0[BUCKET_NEXT];
      };
    };
  };

  public func delete<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K) {
    ignore remove(map, hashUtils, keyParam);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func updateHelper<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, fn: (K, ?V) -> V, DPREV: Nat, DNEXT: Nat): V {
    let body = map.body;

    let hashParam = hashUtils.0(keyParam);
    let bucketIndex = nat(hashParam % body.1);
    let bucketEntry = body.0[bucketIndex];
    var entry = bucketEntry;

    loop {
      if (entry.3 == NULL_HASH) {
        let newSize = map.size +% 1;
        let prevEntry = body.2.0[DPREV];

        let newValue = fn(keyParam, null);
        let newLinks = if (DPREV == PREV) [var prevEntry, body.2, bucketEntry] else [var body.2, prevEntry, bucketEntry];
        let newEntry = (newLinks, keyParam, [var newValue], hashParam);

        prevEntry.0[DNEXT] := newEntry;
        body.2.0[DPREV] := newEntry;
        body.0[bucketIndex] := newEntry;
        map.size := newSize;

        if (newSize == body.1) rehash(map);

        return newValue;
      } else if (entry.3 == hashParam and hashUtils.1(entry.1, keyParam)) {
        let newValue = fn(keyParam, ?entry.2[VALUE]);

        entry.2[VALUE] := newValue;

        return newValue;
      } else {
        entry := entry.0[BUCKET_NEXT];
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

    let body = map.body;

    let bucketIndex = nat(body.2.0[DPREV].3 % body.1);
    var entry = body.0[bucketIndex];
    var bucketPrev = body.2;

    loop {
      if (entry.0[DNEXT].3 == NULL_HASH) {
        let prevEntry = entry.0[DPREV];

        prevEntry.0[DNEXT] := body.2;
        body.2.0[DPREV] := prevEntry;
        map.size := size -% 1;

        if (bucketPrev.3 == NULL_HASH) body.0[bucketIndex] := entry.0[BUCKET_NEXT] else bucketPrev.0[BUCKET_NEXT] := entry.0[BUCKET_NEXT];
        if (size == body.1 / 8) rehash(map);

        return (?entry.1, ?entry.2[VALUE]);
      } else {
        bucketPrev := entry;
        entry := entry.0[BUCKET_NEXT];
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
    let entry = map.body.2.0[PREV];

    return if (entry.3 == NULL_HASH) (null, null) else (?entry.1, ?entry.2[VALUE]);
  };

  public func peekFront<K, V>(map: Map<K, V>): (?K, ?V) {
    let entry = map.body.2.0[NEXT];

    return if (entry.3 == NULL_HASH) (null, null) else (?entry.1, ?entry.2[VALUE]);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func map<K, V1, V2>(map: Map<K, V1>, fn: (K, V1) -> V2): Map<K, V2> {
    let body = map.body;

    let newEdgeEntry = createEdgeEntry<K, V2>(body.2.1);
    let newBuckets = initArray<Entry<K, V2>>(nat(body.1), newEdgeEntry);
    var entry = body.2.0[NEXT];
    var prevEntry = newEdgeEntry;

    loop {
      if (entry.3 == NULL_HASH) {
        newEdgeEntry.0[PREV] := prevEntry;

        return { var body = (newBuckets, body.1, newEdgeEntry); var size = map.size };
      } else {
        let bucketIndex = nat(entry.3 % body.1);

        let newEntry = ([var prevEntry, newEdgeEntry, newBuckets[bucketIndex]], entry.1, [var fn(entry.1, entry.2[VALUE])], entry.3);

        prevEntry.0[NEXT] := newEntry;
        newBuckets[bucketIndex] := newEntry;
        prevEntry := newEntry;
        entry := entry.0[NEXT];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func mapFilter<K, V1, V2>(map: Map<K, V1>, fn: (K, V1) -> ?V2): Map<K, V2> {
    let body = map.body;

    let newEdgeEntry = createEdgeEntry<K, V2>(body.2.1);
    var entry = body.2.0[NEXT];
    var prevEntry = newEdgeEntry;
    var newSize = 0:Nat32;

    loop {
      if (entry.3 == NULL_HASH) {
        let newMap: Map<K, V2> = { var body = ([var], 0, newEdgeEntry); var size = newSize };

        newEdgeEntry.0[PREV] := prevEntry;
        rehash(newMap);

        return newMap;
      } else switch (fn(entry.1, entry.2[VALUE])) {
        case (?newValue) {
          let newEntry = ([var prevEntry, newEdgeEntry, newEdgeEntry], entry.1, [var newValue], entry.3);

          prevEntry.0[NEXT] := newEntry;
          prevEntry := newEntry;
          entry := entry.0[NEXT];
          newSize +%= 1;
        };

        case (_) entry := entry.0[NEXT];
      };
    };
  };

  public func filter<K, V>(map: Map<K, V>, fn: (K, V) -> Bool): Map<K, V> {
    return mapFilter<K, V, V>(map, func(key, value) { if (fn(key, value)) ?value else null });
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func iterateHelper<K, V, T>(map: Map<K, V>, fn: (K, [var V]) -> T, DNEXT: Nat): Iter.Iter<T> = object {
    var entry = map.body.2;

    public func next(): ?T {
      entry := entry.0[DNEXT];

      return if (entry.3 == NULL_HASH) null else ?fn(entry.1, entry.2);
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
    var entry = map.body.2;

    loop {
      entry := entry.0[DNEXT];

      if (entry.3 == NULL_HASH) return false else if (fn(entry.1, entry.2[VALUE])) return true;
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
    var entry = map.body.2;

    loop {
      entry := entry.0[NEXT];

      if (entry.3 == NULL_HASH) return (null, null) else {
        let valueBody = entry.2[VALUE];

        if (fn(entry.1, valueBody)) return (?entry.1, ?valueBody);
      };
    };
  };

  public func findDesc<K, V>(map: Map<K, V>, fn: (K, V) -> Bool): (?K, ?V) {
    var entry = map.body.2;

    loop {
      entry := entry.0[PREV];

      if (entry.3 == NULL_HASH) return (null, null) else {
        let valueBody = entry.2[VALUE];

        if (fn(entry.1, valueBody)) return (?entry.1, ?valueBody);
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func fromIter<K, V>(iter: Iter.Iter<(K, V)>, hashUtils: HashUtils<K>): Map<K, V> {
    let newMap = new<K, V>(hashUtils);

    for (entry in iter) set(newMap, hashUtils, entry.0, entry.1);

    return newMap;
  };

  public func toArray<K, V, T>(map: Map<K, V>, fn: (K, V) -> ?T): [T] {
    let array = initArray<?T>(nat(map.size), null);
    var entry = map.body.2.0[NEXT];
    var arraySize = 0:Nat32;

    loop {
      if (entry.3 == NULL_HASH) {
        return tabulateArray<T>(nat(arraySize), func(i) {
          switch (array[i]) { case (?item) item; case (_) trap("unreachable") };
        });
      } else switch (fn(entry.1, entry.2[VALUE])) {
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

  func createEdgeEntry<K, V>(nullKey: K): Entry<K, V> {
    let nullEntry: Entry<K, V> = ([var], nullKey, [var], NULL_HASH);
    let newEdgeEntry: Entry<K, V> = ([var nullEntry, nullEntry], nullKey, [var], NULL_HASH);

    newEdgeEntry.0[PREV] := newEdgeEntry;
    newEdgeEntry.0[NEXT] := newEdgeEntry;

    return newEdgeEntry;
  };

  public func new<K, V>(hashUtils: HashUtils<K>): Map<K, V> {
    let newEdgeEntry = createEdgeEntry<K, V>(hashUtils.2());

    return { var body = ([var newEdgeEntry, newEdgeEntry], 2, newEdgeEntry); var size = 0 };
  };

  public func clear<K, V>(map: Map<K, V>) {
    let newEdgeEntry = createEdgeEntry<K, V>(map.body.2.1);

    map.body := ([var newEdgeEntry, newEdgeEntry], 2, newEdgeEntry);
    map.size := 0;
  };

  public func size<K, V>(map: Map<K, V>): Nat {
    return nat(map.size);
  };
};
