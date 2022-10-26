import Iter "mo:base/Iter";
import Prim "mo:prim";
import Utils "../utils";

module {
  public type Entry<K, V> = (
    key: K,
    value: ?V,
    hash: Nat32,
    links: [var Entry<K, V>],
  );

  public type Map<K, V> = (
    edgeEntry: Entry<K, V>,
    size: [var Nat32],
  );

  public type HashUtils<K> = Utils.HashUtils<K>;

  public let { ihash; nhash; thash; phash; bhash; lhash; useHash; calcHash } = Utils;

  let { Array_tabulate = tabulateArray; Array_init = initArray; nat32ToNat = nat; trap } = Prim;

  func unwrap<T>(value: ?T): T { switch (value) { case (?value) value; case (_) trap("unreachable") } };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  let SIZE = 0;
  let VALUE = 0;
  let BRANCH_1 = 0;
  let BRANCH_2 = 1;
  let BRANCH_3 = 2;
  let BRANCH_4 = 3;
  let DEQ_PREV = 4;
  let DEQ_NEXT = 5;
  let NULL_BRANCH = 6;
  let HASH_OFFSET = 2:Nat32;
  let HASH_CHUNK_SIZE = 4:Nat32;
  let NULL_HASH = 0xffffffff:Nat32;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func createEdgeEntry<K, V>(nullKey: K): Entry<K, V> {
    let tempEntry: Entry<K, V> = (nullKey, null, NULL_HASH, [var]);
    let newEdgeEntry: Entry<K, V> = (nullKey, null, NULL_HASH, [var tempEntry, tempEntry, tempEntry, tempEntry, tempEntry, tempEntry]);

    for (index in newEdgeEntry.3.keys()) newEdgeEntry.3[index] := newEdgeEntry;

    return newEdgeEntry;
  };

  public func new<K, V>((getHash, areEqual, getNullKey): HashUtils<K>): Map<K, V> {
    return (createEdgeEntry<K, V>(getNullKey()), [var 0]);
  };

  public func clear<K, V>(map: Map<K, V>) {
    let (edgeEntry, size) = map;

    for (index in edgeEntry.3.keys()) edgeEntry.3[index] := edgeEntry;

    size[SIZE] := 0;
  };

  public func size<K, V>(map: Map<K, V>): Nat {
    return nat(map.1[SIZE]);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func get<K, V>(map: Map<K, V>, (getHash, areEqual, getNullKey): HashUtils<K>, keyParam: K): ?V {
    let (edgeEntry, size) = map;
    let hashParam = getHash(keyParam);
    var shiftingHash = hashParam;
    var entry = edgeEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      let (key, value, hash, links) = entry;

      if (hash == hashParam and areEqual(key, keyParam)) return value else if (hash == NULL_HASH) return null else {
        shiftingHash >>= HASH_OFFSET;
        entry := links[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func has<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    return switch (get(map, hashUtils, keyParam)) { case (null) false; case (_) true };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func putHelper<K, V>(
    map: Map<K, V>,
    (getHash, areEqual, getNullKey): HashUtils<K>,
    keyParam: K,
    valueParam: V,
    DEQ_PREV: Nat,
    DEQ_NEXT: Nat
  ): ?V {
    let (edgeEntry, size) = map;
    let hashParam = getHash(keyParam);
    var shiftingHash = hashParam;
    var entry = edgeEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
    var parentEntry = edgeEntry;

    loop {
      let (key, value, hash, links) = entry;

      if (hash == NULL_HASH) {
        let deqPrev = edgeEntry.3[DEQ_PREV];
        let newEntry = (keyParam, ?valueParam, hashParam, [var edgeEntry, edgeEntry, edgeEntry, edgeEntry, edgeEntry, edgeEntry]);

        newEntry.3[DEQ_PREV] := deqPrev;
        deqPrev.3[DEQ_NEXT] := newEntry;
        edgeEntry.3[DEQ_PREV] := newEntry;
        parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        size[SIZE] +%= 1;

        return null;
      } else if (hash == hashParam and areEqual(key, keyParam)) {
        let newEntry = (key, ?valueParam, hash, links);

        links[DEQ_PREV].3[DEQ_NEXT] := newEntry;
        links[DEQ_NEXT].3[DEQ_PREV] := newEntry;
        parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

        return value;
      } else {
        parentEntry := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := links[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func put<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V): ?V {
    return putHelper(map, hashUtils, keyParam, valueParam, DEQ_PREV, DEQ_NEXT);
  };

  public func putFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V): ?V {
    return putHelper(map, hashUtils, keyParam, valueParam, DEQ_NEXT, DEQ_PREV);
  };

  public func set<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V) {
    ignore putHelper(map, hashUtils, keyParam, valueParam, DEQ_PREV, DEQ_NEXT);
  };

  public func setFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V) {
    ignore putHelper(map, hashUtils, keyParam, valueParam, DEQ_NEXT, DEQ_PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func putInsideHelper<K, V>(
    map: Map<K, V>,
    (getHash, areEqual, getNullKey): HashUtils<K>,
    keyParam: K,
    spotKey: K,
    valueParam: V,
    DEQ_PREV: Nat,
    DEQ_NEXT: Nat
  ): ?V {
    let (edgeEntry, size) = map;
    let hashParam = getHash(keyParam);
    var shiftingHash = hashParam;
    var entry = edgeEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
    var parentEntry = edgeEntry;

    let spotHash = getHash(keyParam);
    var shiftingPositionHash = hashParam;
    var spotEntry = edgeEntry.3[nat(shiftingPositionHash % HASH_CHUNK_SIZE)];

    loop {
      let (key, value, hash, links) = spotEntry;

      if (hash == NULL_HASH or hash == spotHash and areEqual(key, spotKey)) loop {
        let (key, value, hash, links) = entry;

        if (hash == NULL_HASH) {
          let deqPrev = edgeEntry.3[DEQ_PREV];
          let newEntry = (keyParam, ?valueParam, hashParam, [var edgeEntry, edgeEntry, edgeEntry, edgeEntry, edgeEntry, edgeEntry]);

          newEntry.3[DEQ_PREV] := deqPrev;
          deqPrev.3[DEQ_NEXT] := newEntry;
          edgeEntry.3[DEQ_PREV] := newEntry;
          parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
          size[SIZE] +%= 1;

          return null;
        } else if (hash == hashParam and areEqual(key, keyParam)) {
          let newEntry = (key, ?valueParam, hash, links);

          links[DEQ_PREV].3[DEQ_NEXT] := newEntry;
          links[DEQ_NEXT].3[DEQ_PREV] := newEntry;
          parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

          return value;
        } else {
          parentEntry := entry;
          shiftingHash >>= HASH_OFFSET;
          entry := links[nat(shiftingHash % HASH_CHUNK_SIZE)];
        };
      } else {
        shiftingPositionHash >>= HASH_OFFSET;
        entry := links[nat(shiftingPositionHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func remove<K, V>(map: Map<K, V>, (getHash, areEqual, getNullKey): HashUtils<K>, keyParam: K): ?V {
    let (edgeEntry, size) = map;
    let hashParam = getHash(keyParam);
    var shiftingHash = hashParam;
    var entry = edgeEntry.3[nat(hashParam % HASH_CHUNK_SIZE)];
    var parentEntry = edgeEntry;

    loop {
      let (key, value, hash, links) = entry;

      if (hash == hashParam and areEqual(key, keyParam)) {
        let deqPrev = links[DEQ_PREV];
        let deqNext = links[DEQ_NEXT];
        var leaf = entry;
        var leafParent = parentEntry;
        var leafIndex = NULL_BRANCH;

        deqPrev.3[DEQ_NEXT] := deqNext;
        deqNext.3[DEQ_PREV] := deqPrev;
        size[SIZE] -%= 1;

        loop {
          let branch1 = leaf.3[BRANCH_1]; if (branch1.2 != NULL_HASH) { leafIndex := BRANCH_1; leafParent := leaf; leaf := branch1 } else {
          let branch2 = leaf.3[BRANCH_2]; if (branch2.2 != NULL_HASH) { leafIndex := BRANCH_2; leafParent := leaf; leaf := branch2 } else {
          let branch3 = leaf.3[BRANCH_3]; if (branch3.2 != NULL_HASH) { leafIndex := BRANCH_3; leafParent := leaf; leaf := branch3 } else {
          let branch4 = leaf.3[BRANCH_4]; if (branch4.2 != NULL_HASH) { leafIndex := BRANCH_4; leafParent := leaf; leaf := branch4 } else {
            if (leafIndex == NULL_BRANCH) {
              parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := edgeEntry;
            } else {
              parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := leaf;
              leafParent.3[leafIndex] := edgeEntry;

              for (index in leaf.3.keys()) if (index <= BRANCH_4) leaf.3[index] := entry.3[index];
            };

            return value;
          }}}};
        };
      } else if (hash == NULL_HASH) {
        return null;
      } else {
        parentEntry := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := links[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func delete<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K) {
    ignore remove(map, hashUtils, keyParam);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func updateHelper<K, V>(
    map: Map<K, V>,
    (getHash, areEqual, getNullKey): HashUtils<K>,
    keyParam: K,
    fn: (K, ?V) -> V,
    DEQ_PREV: Nat,
    DEQ_NEXT: Nat
  ): V {
    let (edgeEntry, size) = map;
    let hashParam = getHash(keyParam);
    var shiftingHash = hashParam;
    var entry = edgeEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
    var parentEntry = edgeEntry;

    loop {
      let (key, value, hash, links) = entry;

      if (hash == NULL_HASH) {
        let deqPrev = edgeEntry.3[DEQ_PREV];
        let newValue = fn(keyParam, null);
        let newEntry = (keyParam, ?newValue, hashParam, [var edgeEntry, edgeEntry, edgeEntry, edgeEntry, edgeEntry, edgeEntry]);

        newEntry.3[DEQ_PREV] := deqPrev;
        deqPrev.3[DEQ_NEXT] := newEntry;
        edgeEntry.3[DEQ_PREV] := newEntry;
        parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        size[SIZE] +%= 1;

        return newValue;
      } else if (hash == hashParam and areEqual(key, keyParam)) {
        let newValue = fn(key, value);
        let newEntry = (key, ?newValue, hash, links);

        links[DEQ_PREV].3[DEQ_NEXT] := newEntry;
        links[DEQ_NEXT].3[DEQ_PREV] := newEntry;
        parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

        return newValue;
      } else {
        parentEntry := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := links[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func update<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, fn: (K, ?V) -> V): V {
    return updateHelper(map, hashUtils, keyParam, fn, DEQ_PREV, DEQ_NEXT);
  };

  public func updateFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, fn: (K, ?V) -> V): V {
    return updateHelper(map, hashUtils, keyParam, fn, DEQ_NEXT, DEQ_PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func popHelper<K, V>(map: Map<K, V>, DEQ_PREV: Nat, DEQ_NEXT: Nat): (?K, ?V) {
    let (edgeEntry, size) = map;
    var entry = edgeEntry.3[DEQ_PREV];
    var shiftingHash = entry.2;
    var parentEntry = edgeEntry;

    if (shiftingHash == NULL_HASH) return (null, null);

    loop {
      let (key, value, hash, links) = entry;

      if (links[DEQ_NEXT].2 == NULL_HASH) {
        let deqPrev = links[DEQ_PREV];
        var leaf = entry;
        var leafParent = parentEntry;
        var leafIndex = NULL_BRANCH;

        deqPrev.3[DEQ_NEXT] := edgeEntry;
        edgeEntry.3[DEQ_PREV] := deqPrev;
        size[SIZE] -%= 1;

        loop {
          let branch1 = leaf.3[BRANCH_1]; if (branch1.2 != NULL_HASH) { leafIndex := BRANCH_1; leafParent := leaf; leaf := branch1 } else {
          let branch2 = leaf.3[BRANCH_2]; if (branch2.2 != NULL_HASH) { leafIndex := BRANCH_2; leafParent := leaf; leaf := branch2 } else {
          let branch3 = leaf.3[BRANCH_3]; if (branch3.2 != NULL_HASH) { leafIndex := BRANCH_3; leafParent := leaf; leaf := branch3 } else {
          let branch4 = leaf.3[BRANCH_4]; if (branch4.2 != NULL_HASH) { leafIndex := BRANCH_4; leafParent := leaf; leaf := branch4 } else {
            if (leafIndex == NULL_BRANCH) {
              parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := edgeEntry;
            } else {
              parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := leaf;
              leafParent.3[leafIndex] := edgeEntry;

              for (index in leaf.3.keys()) if (index <= BRANCH_4) leaf.3[index] := entry.3[index];
            };

            return (?key, value);
          }}}};
        };
      } else {
        parentEntry := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := links[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func pop<K, V>(map: Map<K, V>): (?K, ?V) {
    return popHelper(map, DEQ_PREV, DEQ_NEXT);
  };

  public func popFront<K, V>(map: Map<K, V>): (?K, ?V) {
    return popHelper(map, DEQ_NEXT, DEQ_PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func peek<K, V>(map: Map<K, V>): (?K, ?V) {
    let (edgeEntry, size) = map;
    let (key, value, hash, links) = edgeEntry.3[DEQ_PREV];

    return if (hash == NULL_HASH) (null, null) else (?key, value);
  };

  public func peekFront<K, V>(map: Map<K, V>): (?K, ?V) {
    let (edgeEntry, size) = map;
    let (key, value, hash, links) = edgeEntry.3[DEQ_NEXT];

    return if (hash == NULL_HASH) (null, null) else (?key, value);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func mapFilter<K, V1, V2>(map: Map<K, V1>, fn: (K, V1) -> ?V2): Map<K, V2> {
    let (edgeEntry, size) = map;
    let newEdgeEntry = createEdgeEntry<K, V2>(edgeEntry.0);
    var entry = edgeEntry.3[DEQ_NEXT];
    var deqPrev = newEdgeEntry;
    var newSize = 0:Nat32;

    loop {
      let (key, value, hash, links) = entry;

      if (hash == NULL_HASH) {
        newEdgeEntry.3[DEQ_PREV] := deqPrev;

        return (newEdgeEntry, [var newSize]);
      } else switch (fn(key, unwrap(value))) {
        case (null) entry := links[DEQ_NEXT];

        case (newValue) {
          var shiftingHash = hash;
          var nextEntry = newEdgeEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
          var parentEntry = newEdgeEntry;

          while (nextEntry.2 != NULL_HASH) {
            shiftingHash >>= HASH_OFFSET;
            parentEntry := nextEntry;
            nextEntry := nextEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
          };

          let newEntry = (key, newValue, hash, [var newEdgeEntry, newEdgeEntry, newEdgeEntry, newEdgeEntry, deqPrev, newEdgeEntry]);

          deqPrev.3[DEQ_NEXT] := newEntry;
          parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
          deqPrev := newEntry;
          entry := links[DEQ_NEXT];
          newSize +%= 1;
        };
      };
    };
  };

  public func map<K, V1, V2>(map: Map<K, V1>, fn: (K, V1) -> V2): Map<K, V2> {
    return mapFilter<K, V1, V2>(map, func(key, value) { ?fn(key, value) });
  };

  public func filter<K, V>(map: Map<K, V>, fn: (K, V) -> Bool): Map<K, V> {
    return mapFilter<K, V, V>(map, func(key, value) { if (fn(key, value)) ?value else null });
  };

  public func clone<K, V>(map: Map<K, V>): Map<K, V> {
    return mapFilter<K, V, V>(map, func(key, value) { ?value });
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func iterateHelper<K, V, T>(map: Map<K, V>, fn: (K, ?V) -> ?T, DEQ_NEXT: Nat): Iter.Iter<T> = object {
    let (edgeEntry, size) = map;
    var entry = edgeEntry;

    public func next(): ?T {
      entry := entry.3[DEQ_NEXT];

      let (key, value, hash, links) = entry;

      return if (hash == NULL_HASH) null else fn(key, value);
    };
  };

  public func keys<K, V>(map: Map<K, V>): Iter.Iter<K> {
    return iterateHelper<K, V, K>(map, func(key, value) { ?key }, DEQ_NEXT);
  };

  public func keysDesc<K, V>(map: Map<K, V>): Iter.Iter<K> {
    return iterateHelper<K, V, K>(map, func(key, value) { ?key }, DEQ_PREV);
  };

  public func vals<K, V>(map: Map<K, V>): Iter.Iter<V> {
    return iterateHelper<K, V, V>(map, func(key, value) { value }, DEQ_NEXT);
  };

  public func valsDesc<K, V>(map: Map<K, V>): Iter.Iter<V> {
    return iterateHelper<K, V, V>(map, func(key, value) { value }, DEQ_PREV);
  };

  public func entries<K, V>(map: Map<K, V>): Iter.Iter<(K, V)> {
    return iterateHelper<K, V, (K, V)>(map, func(key, value) { ?(key, unwrap(value)) }, DEQ_NEXT);
  };

  public func entriesDesc<K, V>(map: Map<K, V>): Iter.Iter<(K, V)> {
    return iterateHelper<K, V, (K, V)>(map, func(key, value) { ?(key, unwrap(value)) }, DEQ_PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func forEachHelper<K, V>(map: Map<K, V>, fn: (K, V) -> Bool, DEQ_NEXT: Nat): Bool {
    let (edgeEntry, size) = map;
    var entry = edgeEntry;

    loop {
      entry := entry.3[DEQ_NEXT];

      let (key, value, hash, links) = entry;

      if (hash == NULL_HASH) return false else if (fn(key, unwrap(value))) return true;
    };
  };

  public func forEach<K, V>(map: Map<K, V>, fn: (K, V) -> ()) {
    ignore forEachHelper<K, V>(map, func(key, value) { fn(key, value); false }, DEQ_NEXT);
  };

  public func forEachDesc<K, V>(map: Map<K, V>, fn: (K, V) -> ()) {
    ignore forEachHelper<K, V>(map, func(key, value) { fn(key, value); false }, DEQ_PREV);
  };

  public func every<K, V>(map: Map<K, V>, fn: (K, V) -> Bool): Bool {
    return not forEachHelper<K, V>(map, func(key, value) { not fn(key, value) }, DEQ_NEXT);
  };

  public func everyDesc<K, V>(map: Map<K, V>, fn: (K, V) -> Bool): Bool {
    return not forEachHelper<K, V>(map, func(key, value) { not fn(key, value) }, DEQ_PREV);
  };

  public func some<K, V>(map: Map<K, V>, fn: (K, V) -> Bool): Bool {
    return forEachHelper<K, V>(map, fn, DEQ_NEXT);
  };

  public func someDesc<K, V>(map: Map<K, V>, fn: (K, V) -> Bool): Bool {
    return forEachHelper<K, V>(map, fn, DEQ_PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func find<K, V>(map: Map<K, V>, fn: (K, V) -> Bool): (?K, ?V) {
    let (edgeEntry, size) = map;
    var entry = edgeEntry;

    loop {
      entry := entry.3[DEQ_NEXT];

      let (key, value, hash, links) = entry;

      if (hash == NULL_HASH) return (null, null) else if (fn(key, unwrap(value))) return (?key, value);
    };
  };

  public func findDesc<K, V>(map: Map<K, V>, fn: (K, V) -> Bool): (?K, ?V) {
    let (edgeEntry, size) = map;
    var entry = edgeEntry;

    loop {
      entry := entry.3[DEQ_PREV];

      let (key, value, hash, links) = entry;

      if (hash == NULL_HASH) return (null, null) else if (fn(key, unwrap(value))) return (?key, value);
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func fromIter<K, V>(iter: Iter.Iter<(K, V)>, hashUtils: HashUtils<K>): Map<K, V> {
    let newMap = new<K, V>(hashUtils);

    for ((key, value) in iter) set(newMap, hashUtils, key, value);

    return newMap;
  };

  public func toArray<K, V, T>(map: Map<K, V>, fn: (K, V) -> ?T): [T] {
    let (edgeEntry, size) = map;
    let array = initArray<?T>(nat(size[SIZE]), null);
    var entry = edgeEntry.3[DEQ_NEXT];
    var arraySize = 0:Nat32;

    loop {
      let (key, value, hash, links) = entry;

      if (hash == NULL_HASH) return tabulateArray<T>(nat(arraySize), func(i) { unwrap(array[i]) }) else switch (fn(key, unwrap(value))) {
        case (null) entry := links[DEQ_NEXT];

        case (newValue) {
          array[nat(arraySize)] := newValue;
          entry := links[DEQ_NEXT];
          arraySize +%= 1;
        };
      };
    };
  };
};
