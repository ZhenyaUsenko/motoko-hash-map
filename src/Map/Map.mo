import Iter "mo:base/Iter";
import Prim "mo:prim";
import Utils "../utils";

module {
  public type Entry<K, V> = (
    links: [var Entry<K, V>],
    key: ?K,
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

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func rehash<K, V>(map: Map<K, V>) {
    let (_, _, edgeEntry) = map.body;

    let newCapacity = 2 **% (33 -% clz(map.size));
    let newBuckets = initArray<Entry<K, V>>(nat(newCapacity), edgeEntry);
    var entry = edgeEntry.0[NEXT];

    loop switch (entry) {
      case (links, ?_, _, hash) {
        let bucketIndex = nat(hash % newCapacity);

        links[BUCKET_NEXT] := newBuckets[bucketIndex];
        newBuckets[bucketIndex] := entry;
        entry := links[NEXT];
      };

      case (_) return map.body := (newBuckets, newCapacity, edgeEntry);
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func get<K, V>(map: Map<K, V>, (getHash, areEqual): HashUtils<K>, keyParam: K): ?V {
    let (buckets, capacity, _) = map.body;

    let hashParam = getHash(keyParam);
    var entry = buckets[nat(hashParam % capacity)];

    loop switch (entry) {
      case (links, ?key, value, hash) {
        if (hash == hashParam and areEqual(key, keyParam)) return ?value[VALUE];

        entry := links[BUCKET_NEXT];
      };

      case (_) return null;
    };
  };

  public func has<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    return switch (get(map, hashUtils, keyParam)) { case (null) false; case (_) true };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func putHelper<K, V>(map: Map<K, V>, (getHash, areEqual): HashUtils<K>, keyParam: K, valueParam: V, DIR_PREV: Nat, DIR_NEXT: Nat): ?V {
    let (buckets, capacity, edgeEntry) = map.body;

    let hashParam = getHash(keyParam);
    let bucketIndex = nat(hashParam % capacity);
    let bucketEntry = buckets[bucketIndex];
    var entry = bucketEntry;

    loop switch (entry) {
      case (links, ?key, value, hash) {
        if (hash == hashParam and areEqual(key, keyParam)) {
          let prevValue = value[VALUE];

          value[VALUE] := valueParam;

          return ?prevValue;
        };

        entry := links[BUCKET_NEXT];
      };

      case (_) {
        let newSize = map.size +% 1;
        let prevEntry = edgeEntry.0[DIR_PREV];

        let newLinks = if (DIR_PREV == PREV) [var prevEntry, edgeEntry, bucketEntry] else [var edgeEntry, prevEntry, bucketEntry];
        let newEntry: Entry<K, V> = (newLinks, ?keyParam, [var valueParam], hashParam);

        prevEntry.0[DIR_NEXT] := newEntry;
        edgeEntry.0[DIR_PREV] := newEntry;
        buckets[bucketIndex] := newEntry;
        map.size := newSize;

        if (newSize == capacity) rehash(map);

        return null;
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

  public func remove<K, V>(map: Map<K, V>, (getHash, areEqual): HashUtils<K>, keyParam: K): ?V {
    let (buckets, capacity, edgeEntry) = map.body;

    let hashParam = getHash(keyParam);
    let bucketIndex = nat(hashParam % capacity);
    var entry = buckets[bucketIndex];
    var bucketPrevEntry = edgeEntry;

    loop switch (entry) {
      case (links, ?key, value, hash) {
        if (hash == hashParam and areEqual(key, keyParam)) {
          let size = map.size;
          let prevEntry = links[PREV];
          let nextEntry = links[NEXT];

          prevEntry.0[NEXT] := nextEntry;
          nextEntry.0[PREV] := prevEntry;
          map.size := size -% 1;

          switch (bucketPrevEntry) {
            case (_, null, _, _) buckets[bucketIndex] := links[BUCKET_NEXT];
            case (_) bucketPrevEntry.0[BUCKET_NEXT] := links[BUCKET_NEXT];
          };

          if (size == capacity / 8) rehash(map);

          return ?value[VALUE];
        };

        bucketPrevEntry := entry;
        entry := links[BUCKET_NEXT];
      };

      case (_) return null;
    };
  };

  public func delete<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K) {
    ignore remove(map, hashUtils, keyParam);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func updateHelper<K, V>(map: Map<K, V>, (getHash, areEqual): HashUtils<K>, keyParam: K, getValue: (key: K, value: ?V) -> V, DIR_PREV: Nat, DIR_NEXT: Nat): V {
    let (buckets, capacity, edgeEntry) = map.body;

    let hashParam = getHash(keyParam);
    let bucketIndex = nat(hashParam % capacity);
    let bucketEntry = buckets[bucketIndex];
    var entry = bucketEntry;

    loop switch (entry) {
      case (links, ?key, value, hash) {
        if (hash == hashParam and areEqual(key, keyParam)) {
          let newValue = getValue(keyParam, ?value[VALUE]);

          value[VALUE] := newValue;

          return newValue;
        };

        entry := links[BUCKET_NEXT];
      };

      case (_) {
        let newSize = map.size +% 1;
        let prevEntry = edgeEntry.0[DIR_PREV];

        let newValue = getValue(keyParam, null);
        let newLinks = if (DIR_PREV == PREV) [var prevEntry, edgeEntry, bucketEntry] else [var edgeEntry, prevEntry, bucketEntry];
        let newEntry: Entry<K, V> = (newLinks, ?keyParam, [var newValue], hashParam);

        prevEntry.0[DIR_NEXT] := newEntry;
        edgeEntry.0[DIR_PREV] := newEntry;
        buckets[bucketIndex] := newEntry;
        map.size := newSize;

        if (newSize == capacity) rehash(map);

        return newValue;
      };
    };
  };

  public func update<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, getValue: (key: K, value: ?V) -> V): V {
    return updateHelper(map, hashUtils, keyParam, getValue, PREV, NEXT);
  };

  public func updateFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, getValue: (key: K, value: ?V) -> V): V {
    return updateHelper(map, hashUtils, keyParam, getValue, NEXT, PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func popHelper<K, V>(map: Map<K, V>, DIR_PREV: Nat, DIR_NEXT: Nat): (?K, ?V) {
    let size = map.size;

    if (size == 0) return (null, null);

    let (buckets, capacity, edgeEntry) = map.body;

    let bucketIndex = nat(edgeEntry.0[DIR_PREV].3 % capacity);
    var entry = buckets[bucketIndex];
    var bucketPrevEntry = edgeEntry;

    loop switch (entry.0[DIR_NEXT]) {
      case (_, null, _, _) {
        let (links, key, value, _) = entry;
        let prevEntry = links[DIR_PREV];

        prevEntry.0[DIR_NEXT] := edgeEntry;
        edgeEntry.0[DIR_PREV] := prevEntry;
        map.size := size -% 1;

        switch (bucketPrevEntry) {
          case (_, null, _, _) buckets[bucketIndex] := links[BUCKET_NEXT];
          case (_) bucketPrevEntry.0[BUCKET_NEXT] := links[BUCKET_NEXT];
        };

        if (size == capacity / 8) rehash(map);

        return (key, ?value[VALUE]);
      };

      case (_) {
        bucketPrevEntry := entry;
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
    let (_, _, edgeEntry) = map.body;

    let entry = edgeEntry.0[PREV];

    return switch (entry) { case (_, ?key, value, _) (?key, ?value[VALUE]); case (_) (null, null) };
  };

  public func peekFront<K, V>(map: Map<K, V>): (?K, ?V) {
    let (_, _, edgeEntry) = map.body;

    let entry = edgeEntry.0[NEXT];

    return switch (entry) { case (_, ?key, value, _) (?key, ?value[VALUE]); case (_) (null, null) };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func map<K, V1, V2>(map: Map<K, V1>, fn: (key: K, value: V1) -> V2): Map<K, V2> {
    let (_, capacity, edgeEntry) = map.body;
    let (_, _, newEdgeEntry) = new<K, V2>().body;

    let newBuckets = initArray<Entry<K, V2>>(nat(capacity), newEdgeEntry);
    var entry = edgeEntry.0[NEXT];
    var prevEntry = newEdgeEntry;

    loop switch (entry) {
      case (links, ?key, value, hash) {
        let bucketIndex = nat(hash % capacity);

        let newEntry: Entry<K, V2> = ([var prevEntry, newEdgeEntry, newBuckets[bucketIndex]], ?key, [var fn(key, value[VALUE])], hash);

        prevEntry.0[NEXT] := newEntry;
        newBuckets[bucketIndex] := newEntry;
        prevEntry := newEntry;
        entry := links[NEXT];
      };

      case (_) {
        newEdgeEntry.0[PREV] := prevEntry;

        return { var body = (newBuckets, capacity, newEdgeEntry); var size = map.size };
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func mapFilter<K, V1, V2>(map: Map<K, V1>, fn: (key: K, value: V1) -> ?V2): Map<K, V2> {
    let (_, capacity, edgeEntry) = map.body;
    let (_, _, newEdgeEntry) = new<K, V2>().body;

    var entry = edgeEntry.0[NEXT];
    var prevEntry = newEdgeEntry;
    var newSize = 0:Nat32;

    loop switch (entry) {
      case (links, ?key, value, hash) switch (fn(key, value[VALUE])) {
        case (?newValue) {
          let newEntry: Entry<K, V2> = ([var prevEntry, newEdgeEntry, newEdgeEntry], ?key, [var newValue], hash);

          prevEntry.0[NEXT] := newEntry;
          prevEntry := newEntry;
          entry := links[NEXT];
          newSize +%= 1;
        };

        case (_) entry := links[NEXT];
      };

      case (_) {
        let newMap: Map<K, V2> = { var body = ([var], 0, newEdgeEntry); var size = newSize };

        newEdgeEntry.0[PREV] := prevEntry;
        rehash(newMap);

        return newMap;
      };
    };
  };

  public func filter<K, V>(map: Map<K, V>, fn: (key: K, value: V) -> Bool): Map<K, V> {
    return mapFilter<K, V, V>(map, func(key, value) { if (fn(key, value)) ?value else null });
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func iterateHelper<K, V, T>(map: Map<K, V>, DIR: Nat, fn: (key: K, value: [var V]) -> T): Iter.Iter<T> = object {
    let (_, _, edgeEntry) = map.body;

    var entry = edgeEntry;

    public func next(): ?T {
      entry := entry.0[DIR];

      return switch (entry) { case (_, ?key, value, _) ?fn(key, value); case (_) null };
    };
  };

  public func keys<K, V>(map: Map<K, V>): Iter.Iter<K> {
    return iterateHelper<K, V, K>(map, NEXT, func(key, value) { key });
  };

  public func keysDesc<K, V>(map: Map<K, V>): Iter.Iter<K> {
    return iterateHelper<K, V, K>(map, PREV, func(key, value) { key });
  };

  public func vals<K, V>(map: Map<K, V>): Iter.Iter<V> {
    return iterateHelper<K, V, V>(map, NEXT, func(key, value) { value[VALUE] });
  };

  public func valsDesc<K, V>(map: Map<K, V>): Iter.Iter<V> {
    return iterateHelper<K, V, V>(map, PREV, func(key, value) { value[VALUE] });
  };

  public func entries<K, V>(map: Map<K, V>): Iter.Iter<(K, V)> {
    return iterateHelper<K, V, (K, V)>(map, NEXT, func(key, value) { (key, value[VALUE]) });
  };

  public func entriesDesc<K, V>(map: Map<K, V>): Iter.Iter<(K, V)> {
    return iterateHelper<K, V, (K, V)>(map, PREV, func(key, value) { (key, value[VALUE]) });
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func existHelper<K, V>(map: Map<K, V>, DIR: Nat, fn: (key: K, value: V) -> Bool): Bool {
    let (_, _, edgeEntry) = map.body;

    var entry = edgeEntry;

    loop {
      entry := entry.0[DIR];

      switch (entry) { case (_, ?key, value, _) if (fn(key, value[VALUE])) return true; case (_) return false };
    };
  };

  public func forEach<K, V>(map: Map<K, V>, fn: (key: K, value: V) -> ()) {
    ignore existHelper<K, V>(map, NEXT, func(key, value) { fn(key, value); false });
  };

  public func forEachDesc<K, V>(map: Map<K, V>, fn: (key: K, value: V) -> ()) {
    ignore existHelper<K, V>(map, PREV, func(key, value) { fn(key, value); false });
  };

  public func some<K, V>(map: Map<K, V>, fn: (key: K, value: V) -> Bool): Bool {
    return existHelper<K, V>(map, NEXT, fn);
  };

  public func someDesc<K, V>(map: Map<K, V>, fn: (key: K, value: V) -> Bool): Bool {
    return existHelper<K, V>(map, PREV, fn);
  };

  public func every<K, V>(map: Map<K, V>, fn: (key: K, value: V) -> Bool): Bool {
    return not existHelper<K, V>(map, NEXT, func(key, value) { not fn(key, value) });
  };

  public func everyDesc<K, V>(map: Map<K, V>, fn: (key: K, value: V) -> Bool): Bool {
    return not existHelper<K, V>(map, PREV, func(key, value) { not fn(key, value) });
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func findHelper<K, V>(map: Map<K, V>, DIR: Nat, fn: (key: K, value: V) -> Bool): (?K, ?V) {
    let (_, _, edgeEntry) = map.body;

    var entry = edgeEntry.0[DIR];

    loop switch (entry) {
      case (links, ?key, value, _) {
        let valueBody = value[VALUE];

        if (fn(key, valueBody)) return (?key, ?valueBody);

        entry := links[DIR];
      };

      case (_) return (null, null);
    };
  };

  public func find<K, V>(map: Map<K, V>, fn: (key: K, value: V) -> Bool): (?K, ?V) {
    return findHelper<K, V>(map, NEXT, fn);
  };

  public func findDesc<K, V>(map: Map<K, V>, fn: (key: K, value: V) -> Bool): (?K, ?V) {
    return findHelper<K, V>(map, PREV, fn);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func fromIter<K, V>(iter: Iter.Iter<(K, V)>, hashUtils: HashUtils<K>): Map<K, V> {
    let newMap = new<K, V>();

    for ((key, value) in iter) set(newMap, hashUtils, key, value);

    return newMap;
  };

  public func toArray<K, V, T>(map: Map<K, V>, fn: (key: K, value: V) -> ?T): [T] {
    let (_, _, edgeEntry) = map.body;

    var entry = edgeEntry.0[NEXT];
    var arraySize = 0:Nat32;

    let array = tabulateArray<?T>(nat(map.size), func(_) {
      loop switch (entry) {
        case (_, ?key, value, _) switch (fn(key, value[VALUE])) {
          case (null) entry := entry.0[NEXT];

          case (newValue) {
            entry := entry.0[NEXT];
            arraySize +%= 1;

            return newValue;
          };
        };

        case (_) return null;
      };
    });

    return tabulateArray<T>(nat(arraySize), func(i) {
      return switch (array[i]) { case (?item) item; case (_) trap("unreachable") }
    });
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func new<K, V>(): Map<K, V> {
    let nullEntry: Entry<K, V> = ([var], null, [var], 0);
    let newEdgeEntry: Entry<K, V> = ([var nullEntry, nullEntry, nullEntry], null, [var], 0);

    newEdgeEntry.0[PREV] := newEdgeEntry;
    newEdgeEntry.0[NEXT] := newEdgeEntry;

    return { var body = ([var newEdgeEntry, newEdgeEntry], 2, newEdgeEntry); var size = 0 };
  };

  public func clear<K, V>(map: Map<K, V>) {
    map.body := new<K, V>().body;
    map.size := 0;
  };

  public func size<K, V>(map: Map<K, V>): Nat {
    return nat(map.size);
  };
};
