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
    let body = map.body;

    let newCapacity = 2 **% (33 -% clz(map.size));
    let newBuckets = initArray<Entry<K, V>>(nat(newCapacity), body.2);
    var entry = body.2.0[NEXT];

    loop switch (entry.1) {
      case (?_) {
        let bucketIndex = nat(entry.3 % newCapacity);

        entry.0[BUCKET_NEXT] := newBuckets[bucketIndex];
        newBuckets[bucketIndex] := entry;
        entry := entry.0[NEXT];
      };

      case (_) return map.body := (newBuckets, newCapacity, body.2);
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func get<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): ?V {
    let body = map.body;

    let hashParam = hashUtils.0(keyParam);
    var entry = body.0[nat(hashParam % body.1)];

    loop switch (entry.1) {
      case (?key) {
        if (entry.3 == hashParam and hashUtils.1(key, keyParam)) return ?entry.2[VALUE];

        entry := entry.0[BUCKET_NEXT];
      };

      case (_) return null;
    };
  };

  public func has<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    return switch (get(map, hashUtils, keyParam)) { case (null) false; case (_) true };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func putHelper<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V, DIR_PREV: Nat, DIR_NEXT: Nat): ?V {
    let body = map.body;

    let hashParam = hashUtils.0(keyParam);
    let bucketIndex = nat(hashParam % body.1);
    let bucketEntry = body.0[bucketIndex];
    var entry = bucketEntry;

    loop switch (entry.1) {
      case (?key) {
        if (entry.3 == hashParam and hashUtils.1(key, keyParam)) {
          let prevValue = entry.2[VALUE];

          entry.2[VALUE] := valueParam;

          return ?prevValue;
        };

        entry := entry.0[BUCKET_NEXT];
      };

      case (_) {
        let newSize = map.size +% 1;
        let prevEntry = body.2.0[DIR_PREV];

        let newLinks = if (DIR_PREV == PREV) [var prevEntry, body.2, bucketEntry] else [var body.2, prevEntry, bucketEntry];
        let newEntry: Entry<K, V> = (newLinks, ?keyParam, [var valueParam], hashParam);

        prevEntry.0[DIR_NEXT] := newEntry;
        body.2.0[DIR_PREV] := newEntry;
        body.0[bucketIndex] := newEntry;
        map.size := newSize;

        if (newSize == body.1) rehash(map);

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

  public func remove<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): ?V {
    let body = map.body;

    let hashParam = hashUtils.0(keyParam);
    let bucketIndex = nat(hashParam % body.1);
    var entry = body.0[bucketIndex];
    var bucketPrevEntry = body.2;

    loop switch (entry.1) {
      case (?key) {
        if (entry.3 == hashParam and hashUtils.1(key, keyParam)) {
          let size = map.size;
          let prevEntry = entry.0[PREV];
          let nextEntry = entry.0[NEXT];

          prevEntry.0[NEXT] := nextEntry;
          nextEntry.0[PREV] := prevEntry;
          map.size := size -% 1;

          switch (bucketPrevEntry.1) {
            case (null) body.0[bucketIndex] := entry.0[BUCKET_NEXT];
            case (_) bucketPrevEntry.0[BUCKET_NEXT] := entry.0[BUCKET_NEXT];
          };

          if (size == body.1 / 8) rehash(map);

          return ?entry.2[VALUE];
        };

        bucketPrevEntry := entry;
        entry := entry.0[BUCKET_NEXT];
      };

      case (_) return null;
    };
  };

  public func delete<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K) {
    ignore remove(map, hashUtils, keyParam);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func updateHelper<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, getValue: (key: K, value: ?V) -> V, DIR_PREV: Nat, DIR_NEXT: Nat): V {
    let body = map.body;

    let hashParam = hashUtils.0(keyParam);
    let bucketIndex = nat(hashParam % body.1);
    let bucketEntry = body.0[bucketIndex];
    var entry = bucketEntry;

    loop switch (entry.1) {
      case (?key) {
        if (entry.3 == hashParam and hashUtils.1(key, keyParam)) {
          let newValue = getValue(keyParam, ?entry.2[VALUE]);

          entry.2[VALUE] := newValue;

          return newValue;
        };

        entry := entry.0[BUCKET_NEXT];
      };

      case (_) {
        let newSize = map.size +% 1;
        let prevEntry = body.2.0[DIR_PREV];

        let newValue = getValue(keyParam, null);
        let newLinks = if (DIR_PREV == PREV) [var prevEntry, body.2, bucketEntry] else [var body.2, prevEntry, bucketEntry];
        let newEntry: Entry<K, V> = (newLinks, ?keyParam, [var newValue], hashParam);

        prevEntry.0[DIR_NEXT] := newEntry;
        body.2.0[DIR_PREV] := newEntry;
        body.0[bucketIndex] := newEntry;
        map.size := newSize;

        if (newSize == body.1) rehash(map);

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

    let body = map.body;

    let bucketIndex = nat(body.2.0[DIR_PREV].3 % body.1);
    var entry = body.0[bucketIndex];
    var bucketPrevEntry = body.2;

    loop switch (entry.0[DIR_NEXT].1) {
      case (null) {
        let prevEntry = entry.0[DIR_PREV];

        prevEntry.0[DIR_NEXT] := body.2;
        body.2.0[DIR_PREV] := prevEntry;
        map.size := size -% 1;

        switch (bucketPrevEntry.1) {
          case (null) body.0[bucketIndex] := entry.0[BUCKET_NEXT];
          case (_) bucketPrevEntry.0[BUCKET_NEXT] := entry.0[BUCKET_NEXT];
        };

        if (size == body.1 / 8) rehash(map);

        return (entry.1, ?entry.2[VALUE]);
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
    let entry = map.body.2.0[PREV];

    return switch (entry.1) { case (?_) (entry.1, ?entry.2[VALUE]); case (_) (null, null) };
  };

  public func peekFront<K, V>(map: Map<K, V>): (?K, ?V) {
    let entry = map.body.2.0[NEXT];

    return switch (entry.1) { case (?_) (entry.1, ?entry.2[VALUE]); case (_) (null, null) };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func map<K, V1, V2>(map: Map<K, V1>, fn: (key: K, value: V1) -> V2): Map<K, V2> {
    let body = map.body;
    let newEdgeEntry = new<K, V2>().body.2;

    let newBuckets = initArray<Entry<K, V2>>(nat(body.1), newEdgeEntry);
    var entry = body.2.0[NEXT];
    var prevEntry = newEdgeEntry;

    loop switch (entry.1) {
      case (?key) {
        let bucketIndex = nat(entry.3 % body.1);

        let newEntry: Entry<K, V2> = ([var prevEntry, newEdgeEntry, newBuckets[bucketIndex]], entry.1, [var fn(key, entry.2[VALUE])], entry.3);

        prevEntry.0[NEXT] := newEntry;
        newBuckets[bucketIndex] := newEntry;
        prevEntry := newEntry;
        entry := entry.0[NEXT];
      };

      case (_) {
        newEdgeEntry.0[PREV] := prevEntry;

        return { var body = (newBuckets, body.1, newEdgeEntry); var size = map.size };
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func mapFilter<K, V1, V2>(map: Map<K, V1>, fn: (key: K, value: V1) -> ?V2): Map<K, V2> {
    let body = map.body;
    let newEdgeEntry = new<K, V2>().body.2;

    var entry = body.2.0[NEXT];
    var prevEntry = newEdgeEntry;
    var newSize = 0:Nat32;

    loop switch (entry.1) {
      case (?key) switch (fn(key, entry.2[VALUE])) {
        case (?newValue) {
          let newEntry: Entry<K, V2> = ([var prevEntry, newEdgeEntry, newEdgeEntry], entry.1, [var newValue], entry.3);

          prevEntry.0[NEXT] := newEntry;
          prevEntry := newEntry;
          entry := entry.0[NEXT];
          newSize +%= 1;
        };

        case (_) entry := entry.0[NEXT];
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
    var entry = map.body.2;

    public func next(): ?T {
      entry := entry.0[DIR];

      return switch (entry.1) { case (?key) ?fn(key, entry.2); case (_) null };
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
    var entry = map.body.2;

    loop {
      entry := entry.0[DIR];

      switch (entry.1) { case (?key) if (fn(key, entry.2[VALUE])) return true; case (_) return false };
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
    var entry = map.body.2.0[DIR];

    loop switch (entry.1) {
      case (?key) {
        let valueBody = entry.2[VALUE];

        if (fn(key, valueBody)) return (entry.1, ?valueBody);

        entry := entry.0[DIR];
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

    for (entry in iter) set(newMap, hashUtils, entry.0, entry.1);

    return newMap;
  };

  public func toArray<K, V, T>(map: Map<K, V>, fn: (key: K, value: V) -> ?T): [T] {
    var entry = map.body.2.0[NEXT];
    var arraySize = 0:Nat32;

    let array = tabulateArray<?T>(nat(map.size), func(_) {
      loop switch (entry.1) {
        case (?key) switch (fn(key, entry.2[VALUE])) {
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
