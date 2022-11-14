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

  public type Iter<T> = {
    prev: () -> ?T;
    next: () -> ?T;
    current: () -> ?T;
    reset: () -> Iter<T>;
    movePrev: () -> Iter<T>;
    moveNext: () -> Iter<T>;
  };

  public type IterNext<T> = {
    next: () -> ?T;
  };

  public type HashUtils<K> = Utils.HashUtils<K>;

  public let { hashInt; hashText; hashPrincipal; hashBlob; hashBool } = Utils;

  public let { ihash; nhash; thash; phash; bhash; lhash; useHash; calcHash } = Utils;

  let { Array_tabulate = tabulateArray; Array_init = initArray; nat32ToNat = nat; trap } = Prim;

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

  func noop<T>(): T = trap("unreachable");

  func isSome<T>(value: ?T): Bool = switch (value) { case (null) false; case (_) true };

  func unwrap<T>(value: ?T): T = switch (value) { case (?value) value; case (_) noop() };

  func tempHashUtils<K>(): HashUtils<K> = (func(_) = NULL_HASH, func(_) = false, func() = noop());

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func createEdgeEntry<K, V>(nullKey: K): Entry<K, V> {
    let tempEntry = (nullKey, null, NULL_HASH, [var]):Entry<K, V>;
    let newEdge = (nullKey, null, NULL_HASH, [var tempEntry, tempEntry, tempEntry, tempEntry, tempEntry, tempEntry]);

    for (index in newEdge.3.keys()) newEdge.3[index] := newEdge;

    return newEdge;
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

  func getHelper<K, V>(map: Map<K, V>, (getHash, areEqual, getNullKey): HashUtils<K>, keyParam: K): ?V {
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

  public func get<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): ?V {
    return getHelper(map, hashUtils, keyParam);
  };

  public func has<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    return isSome(getHelper(map, hashUtils, keyParam));
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func putHelper<K, V>(
    map: Map<K, V>,
    (getHash, areEqual, getNullKey): HashUtils<K>,
    keyParam: K,
    placeKeyParam: K,
    valueParam: ?V,
    getNewValue: (K, ?V) -> V,
    usePlaceKey: Bool,
    moveExisting: Bool,
    isUpdate: Bool,
    DEQ_PREV: Nat,
    DEQ_NEXT: Nat,
  ): ?V {
    let (edgeEntry, size) = map;
    let hashParam = getHash(keyParam);
    var shiftingHash = hashParam;
    var entry = edgeEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
    var parentEntry = edgeEntry;

    let placeHashParam = if (usePlaceKey) getHash(placeKeyParam) else NULL_HASH;

    var shiftingPlaceHash = placeHashParam;

    var placeEntry = if (usePlaceKey) edgeEntry.3[nat(shiftingPlaceHash % HASH_CHUNK_SIZE)] else edgeEntry;

    loop {
      let (placeKey, placeValue, placeHash, placeLinks) = placeEntry;

      if (placeHash == NULL_HASH or placeHash == placeHashParam and areEqual(placeKey, placeKeyParam)) loop {
        let (key, value, hash, links) = entry;

        if (hash == NULL_HASH) {
          let newValue = if (isUpdate) ?getNewValue(keyParam, null) else valueParam;

          switch (newValue) {
            case (null) return null;

            case (_) {
              let deqPrev = placeLinks[DEQ_PREV];
              let newEntry = (keyParam, newValue, hashParam, [var edgeEntry, edgeEntry, edgeEntry, edgeEntry, placeEntry, placeEntry]);

              newEntry.3[DEQ_PREV] := deqPrev;
              deqPrev.3[DEQ_NEXT] := newEntry;
              placeLinks[DEQ_PREV] := newEntry;
              parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
              size[SIZE] +%= 1;

              return if (isUpdate) newValue else null;
            };
          };
        } else if (hash == hashParam and areEqual(key, keyParam)) {
          let newValue = if (isUpdate) ?getNewValue(key, value) else valueParam;

          let newEntry = switch (newValue) { case (null) entry; case (_) (key, newValue, hash, links) };

          if (moveExisting) {
            let deqNextOld = links[DEQ_NEXT];

            if (usePlaceKey or deqNextOld.2 != NULL_HASH) {
              let deqPrevOld = links[DEQ_PREV];
              let deqPrev = placeLinks[DEQ_PREV];

              deqPrevOld.3[DEQ_NEXT] := deqNextOld;
              deqNextOld.3[DEQ_PREV] := deqPrevOld;
              newEntry.3[DEQ_PREV] := deqPrev;
              newEntry.3[DEQ_NEXT] := placeEntry;
              deqPrev.3[DEQ_NEXT] := newEntry;
              placeLinks[DEQ_PREV] := newEntry;
            } else {
              links[DEQ_PREV].3[DEQ_NEXT] := newEntry;
              placeLinks[DEQ_PREV] := newEntry;
            };
          } else {
            links[DEQ_PREV].3[DEQ_NEXT] := newEntry;
            links[DEQ_NEXT].3[DEQ_PREV] := newEntry;
          };

          parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

          return if (isUpdate) newValue else value;
        } else {
          parentEntry := entry;
          shiftingHash >>= HASH_OFFSET;
          entry := links[nat(shiftingHash % HASH_CHUNK_SIZE)];
        };
      } else {
        shiftingPlaceHash >>= HASH_OFFSET;
        placeEntry := placeLinks[nat(shiftingPlaceHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func put<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V): ?V {
    return putHelper<K, V>(map, hashUtils, keyParam, map.0.0, ?valueParam, func(_) = noop(), false, false, false, DEQ_PREV, DEQ_NEXT);
  };

  public func putFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V): ?V {
    return putHelper<K, V>(map, hashUtils, keyParam, map.0.0, ?valueParam, func(_) = noop(), false, false, false, DEQ_NEXT, DEQ_PREV);
  };

  public func set<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V) {
    ignore putHelper<K, V>(map, hashUtils, keyParam, map.0.0, ?valueParam, func(_) = noop(), false, false, false, DEQ_PREV, DEQ_NEXT);
  };

  public func setFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V) {
    ignore putHelper<K, V>(map, hashUtils, keyParam, map.0.0, ?valueParam, func(_) = noop(), false, false, false, DEQ_NEXT, DEQ_PREV);
  };

  public func putMove<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: ?V): ?V {
    return putHelper<K, V>(map, hashUtils, keyParam, map.0.0, valueParam, func(_) = noop(), false, true, false, DEQ_PREV, DEQ_NEXT);
  };

  public func putMoveFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: ?V): ?V {
    return putHelper<K, V>(map, hashUtils, keyParam, map.0.0, valueParam, func(_) = noop(), false, true, false, DEQ_NEXT, DEQ_PREV);
  };

  public func setMove<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: ?V) {
    ignore putHelper<K, V>(map, hashUtils, keyParam, map.0.0, valueParam, func(_) = noop(), false, true, false, DEQ_PREV, DEQ_NEXT);
  };

  public func setMoveFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: ?V) {
    ignore putHelper<K, V>(map, hashUtils, keyParam, map.0.0, valueParam, func(_) = noop(), false, true, false, DEQ_NEXT, DEQ_PREV);
  };

  public func putBefore<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, placeKeyParam: K, valueParam: ?V): ?V {
    return putHelper<K, V>(map, hashUtils, keyParam, placeKeyParam, valueParam, func(_) = noop(), true, true, false, DEQ_PREV, DEQ_NEXT);
  };

  public func putAfter<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, placeKeyParam: K, valueParam: ?V): ?V {
    return putHelper<K, V>(map, hashUtils, keyParam, placeKeyParam, valueParam, func(_) = noop(), true, true, false, DEQ_NEXT, DEQ_PREV);
  };

  public func setBefore<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, placeKeyParam: K, valueParam: ?V) {
    ignore putHelper<K, V>(map, hashUtils, keyParam, placeKeyParam, valueParam, func(_) = noop(), true, true, false, DEQ_PREV, DEQ_NEXT);
  };

  public func setAfter<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, placeKeyParam: K, valueParam: ?V) {
    ignore putHelper<K, V>(map, hashUtils, keyParam, placeKeyParam, valueParam, func(_) = noop(), true, true, false, DEQ_NEXT, DEQ_PREV);
  };

  public func update<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, getNewValue: (K, ?V) -> V): V {
    return unwrap(putHelper<K, V>(map, hashUtils, keyParam, map.0.0, null, getNewValue, false, false, true, DEQ_PREV, DEQ_NEXT));
  };

  public func updateFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, getNewValue: (K, ?V) -> V): V {
    return unwrap(putHelper<K, V>(map, hashUtils, keyParam, map.0.0, null, getNewValue, false, false, true, DEQ_NEXT, DEQ_PREV));
  };

  public func updateMove<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, getNewValue: (K, ?V) -> V): V {
    return unwrap(putHelper<K, V>(map, hashUtils, keyParam, map.0.0, null, getNewValue, false, true, true, DEQ_PREV, DEQ_NEXT));
  };

  public func updateMoveFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, getNewValue: (K, ?V) -> V): V {
    return unwrap(putHelper<K, V>(map, hashUtils, keyParam, map.0.0, null, getNewValue, false, true, true, DEQ_NEXT, DEQ_PREV));
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func removeHelper<K, V, R>(
    map: Map<K, V>,
    (getHash, areEqual, getNullKey): HashUtils<K>,
    keyParam: K,
    getResult: (K, ?V) -> ?R,
    removeLast: Bool,
    DEQ_PREV: Nat,
    DEQ_NEXT: Nat,
  ): ?R {
    let (edgeEntry, size) = map;
    let deqLast = edgeEntry.3[DEQ_PREV];

    if (deqLast.2 == NULL_HASH) return null else {
      let hashParam = if (removeLast) deqLast.2 else getHash(keyParam);

      var shiftingHash = hashParam;
      var entry = edgeEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      var parentEntry = edgeEntry;

      loop {
        let (key, value, hash, links) = entry;

        if (hash == hashParam and (if (removeLast) links[DEQ_NEXT].2 == NULL_HASH else areEqual(key, keyParam))) {
          let deqPrev = links[DEQ_PREV];
          let deqNext = links[DEQ_NEXT];
          var leaf = entry;
          var leafParent = parentEntry;
          var leafIndex = NULL_BRANCH;

          deqPrev.3[DEQ_NEXT] := deqNext;
          deqNext.3[DEQ_PREV] := deqPrev;
          size[SIZE] -%= 1;

          loop {
            let (leafKey, leafValue, leafHash, leafLinks) = leaf;

            if (leafLinks[BRANCH_1].2 != NULL_HASH) {
              leafParent := leaf;
              leaf := leafLinks[BRANCH_1];
              leafIndex := BRANCH_1;
            } else if (leafLinks[BRANCH_2].2 != NULL_HASH) {
              leafParent := leaf;
              leaf := leafLinks[BRANCH_2];
              leafIndex := BRANCH_2;
            } else if (leafLinks[BRANCH_3].2 != NULL_HASH) {
              leafParent := leaf;
              leaf := leafLinks[BRANCH_3];
              leafIndex := BRANCH_3;
            } else if (leafLinks[BRANCH_4].2 != NULL_HASH) {
              leafParent := leaf;
              leaf := leafLinks[BRANCH_4];
              leafIndex := BRANCH_4;
            } else {
              if (leafIndex == NULL_BRANCH) {
                parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := edgeEntry;
              } else {
                parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := leaf;
                leafParent.3[leafIndex] := edgeEntry;

                for (index in leafLinks.keys()) if (index <= BRANCH_4) leafLinks[index] := links[index];
              };

              return getResult(key, value);
            };
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
  };

  public func remove<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): ?V {
    return removeHelper<K, V, V>(map, hashUtils, keyParam, func(key, value) = value, false, DEQ_PREV, DEQ_NEXT);
  };

  public func delete<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K) {
    ignore removeHelper<K, V, V>(map, hashUtils, keyParam, func(key, value) = value, false, DEQ_PREV, DEQ_NEXT);
  };

  public func pop<K, V>(map: Map<K, V>): ?(K, V) {
    return removeHelper<K, V, (K, V)>(map, tempHashUtils<K>(), map.0.0, func(key, value) = ?(key, unwrap(value)), true, DEQ_PREV, DEQ_NEXT);
  };

  public func popFront<K, V>(map: Map<K, V>): ?(K, V) {
    return removeHelper<K, V, (K, V)>(map, tempHashUtils<K>(), map.0.0, func(key, value) = ?(key, unwrap(value)), true, DEQ_NEXT, DEQ_PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func peekHelper<K, V>(map: Map<K, V>, cycleEntry: Bool, DEQ_PREV: Nat, DEQ_NEXT: Nat): ?(K, V) {
    let (edgeEntry, size) = map;
    let entry = edgeEntry.3[DEQ_PREV];
    let (key, value, hash, links) = entry;

    switch (value) {
      case (?someValue) {
        if (cycleEntry) {
          let deqPrev = links[DEQ_PREV];

          if (deqPrev.2 != NULL_HASH) {
            let deqFirst = edgeEntry.3[DEQ_NEXT];

            deqPrev.3[DEQ_NEXT] := edgeEntry;
            edgeEntry.3[DEQ_PREV] := deqPrev;
            deqFirst.3[DEQ_PREV] := entry;
            edgeEntry.3[DEQ_NEXT] := entry;
            links[DEQ_PREV] := edgeEntry;
            links[DEQ_NEXT] := deqFirst;
          };
        };

        return ?(key, someValue);
      };

      case (_) return null;
    };
  };

  public func peek<K, V>(map: Map<K, V>): ?(K, V) {
    return peekHelper(map, false, DEQ_PREV, DEQ_NEXT);
  };

  public func peekFront<K, V>(map: Map<K, V>): ?(K, V) {
    return peekHelper(map, false, DEQ_NEXT, DEQ_PREV);
  };

  public func cycle<K, V>(map: Map<K, V>): ?(K, V) {
    return peekHelper(map, true, DEQ_PREV, DEQ_NEXT);
  };

  public func cycleFront<K, V>(map: Map<K, V>): ?(K, V) {
    return peekHelper(map, true, DEQ_NEXT, DEQ_PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func mapFilterHelper<K, V1, V2, T>(
    map: Map<K, V1>,
    mapEntry: (K, V1) -> T,
    acceptEntry: (K, T) -> Bool,
    getValue: (T) -> ?V2,
    DEQ_PREV: Nat,
    DEQ_NEXT: Nat,
  ): Map<K, V2> {
    let (edgeEntry, size) = map;
    let newEdge = createEdgeEntry<K, V2>(edgeEntry.0);
    var entry = edgeEntry.3[DEQ_NEXT];
    var deqPrev = newEdge;
    var newSize = 0:Nat32;

    loop {
      let (key, value, hash, links) = entry;

      switch (value) {
        case (?someValue) {
          let newValue = mapEntry(key, someValue);

          if (acceptEntry(key, newValue)) {
            var shiftingHash = hash;
            var searchEntry = newEdge.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
            var parentEntry = newEdge;

            while (searchEntry.2 != NULL_HASH) {
              parentEntry := searchEntry;
              shiftingHash >>= HASH_OFFSET;
              searchEntry := searchEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
            };

            let newEntry = (key, getValue(newValue), hash, [var newEdge, newEdge, newEdge, newEdge, newEdge, newEdge]);

            newEntry.3[DEQ_PREV] := deqPrev;
            deqPrev.3[DEQ_NEXT] := newEntry;
            parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
            deqPrev := newEntry;
            entry := links[DEQ_NEXT];
            newSize +%= 1;
          } else {
            entry := links[DEQ_NEXT];
          };
        };

        case (_) {
          newEdge.3[DEQ_PREV] := deqPrev;

          return (newEdge, [var newSize]);
        };
      };
    };
  };

  public func mapFilter<K, V1, V2>(map: Map<K, V1>, mapEntry: (K, V1) -> ?V2): Map<K, V2> {
    return mapFilterHelper<K, V1, V2, ?V2>(map, mapEntry, func(key, value) = isSome(value), func(value) = value, DEQ_PREV, DEQ_NEXT);
  };

  public func mapFilterDesc<K, V1, V2>(map: Map<K, V1>, mapEntry: (K, V1) -> ?V2): Map<K, V2> {
    return mapFilterHelper<K, V1, V2, ?V2>(map, mapEntry, func(key, value) = isSome(value), func(value) = value, DEQ_NEXT, DEQ_PREV);
  };

  public func map<K, V1, V2>(map: Map<K, V1>, mapEntry: (K, V1) -> V2): Map<K, V2> {
    return mapFilterHelper<K, V1, V2, V2>(map, mapEntry, func(_) = true, func(value) = ?value, DEQ_PREV, DEQ_NEXT);
  };

  public func mapDesc<K, V1, V2>(map: Map<K, V1>, mapEntry: (K, V1) -> V2): Map<K, V2> {
    return mapFilterHelper<K, V1, V2, V2>(map, mapEntry, func(_) = true, func(value) = ?value, DEQ_NEXT, DEQ_PREV);
  };

  public func filter<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool): Map<K, V> {
    return mapFilterHelper<K, V, V, V>(map, func(key, value) = value, acceptEntry, func(value) = ?value, DEQ_PREV, DEQ_NEXT);
  };

  public func filterDesc<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool): Map<K, V> {
    return mapFilterHelper<K, V, V, V>(map, func(key, value) = value, acceptEntry, func(value) = ?value, DEQ_NEXT, DEQ_PREV);
  };

  public func clone<K, V>(map: Map<K, V>): Map<K, V> {
    return mapFilterHelper<K, V, V, V>(map, func(key, value) = value, func(_) = true, func(value) = ?value, DEQ_PREV, DEQ_NEXT);
  };

  public func cloneDesc<K, V>(map: Map<K, V>): Map<K, V> {
    return mapFilterHelper<K, V, V, V>(map, func(key, value) = value, func(_) = true, func(value) = ?value, DEQ_NEXT, DEQ_PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func iterateHelper<K, V, T>(
    map: Map<K, V>,
    (getHash, areEqual, getNullKey): HashUtils<K>,
    keyParam: K,
    mapEntry: (K, ?V) -> ?T,
    DEQ_PREV: Nat,
    DEQ_NEXT: Nat,
  ): Iter<T> {
    let (edgeEntry, size) = map;
    let hashParam = getHash(keyParam);
    var shiftingHash = hashParam;

    var entry = if (shiftingHash != NULL_HASH) edgeEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] else edgeEntry;

    loop {
      let (key, value, hash, links) = entry;

      if (hash == NULL_HASH or hash == hashParam and areEqual(key, keyParam)) return let iter = {
        prev = func(): ?T {
          entry := entry.3[DEQ_PREV];

          let (key, value, hash, links) = entry;

          return if (hash == NULL_HASH) null else mapEntry(key, value);
        };

        next = func(): ?T {
          entry := entry.3[DEQ_NEXT];

          let (key, value, hash, links) = entry;

          return if (hash == NULL_HASH) null else mapEntry(key, value);
        };

        current = func(): ?T {
          let (key, value, hash, links) = entry;

          return if (hash == NULL_HASH) null else mapEntry(key, value);
        };

        reset = func(): Iter<T> {
          entry := edgeEntry;

          return iter;
        };

        movePrev = func(): Iter<T> {
          entry := entry.3[DEQ_PREV];

          return iter;
        };

        moveNext = func(): Iter<T> {
          entry := entry.3[DEQ_NEXT];

          return iter;
        };
      } else {
        shiftingHash >>= HASH_OFFSET;
        entry := links[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func keys<K, V>(map: Map<K, V>): Iter<K> {
    return iterateHelper<K, V, K>(map, tempHashUtils<K>(), map.0.0, func(key, value) = ?key, DEQ_PREV, DEQ_NEXT);
  };

  public func keysDesc<K, V>(map: Map<K, V>): Iter<K> {
    return iterateHelper<K, V, K>(map, tempHashUtils<K>(), map.0.0, func(key, value) = ?key, DEQ_NEXT, DEQ_PREV);
  };

  public func vals<K, V>(map: Map<K, V>): Iter<V> {
    return iterateHelper<K, V, V>(map, tempHashUtils<K>(), map.0.0, func(key, value) = value, DEQ_PREV, DEQ_NEXT);
  };

  public func valsDesc<K, V>(map: Map<K, V>): Iter<V> {
    return iterateHelper<K, V, V>(map, tempHashUtils<K>(), map.0.0, func(key, value) = value, DEQ_NEXT, DEQ_PREV);
  };

  public func entries<K, V>(map: Map<K, V>): Iter<(K, V)> {
    return iterateHelper<K, V, (K, V)>(map, tempHashUtils<K>(), map.0.0, func(key, value) = ?(key, unwrap(value)), DEQ_PREV, DEQ_NEXT);
  };

  public func entriesDesc<K, V>(map: Map<K, V>): Iter<(K, V)> {
    return iterateHelper<K, V, (K, V)>(map, tempHashUtils<K>(), map.0.0, func(key, value) = ?(key, unwrap(value)), DEQ_NEXT, DEQ_PREV);
  };

  public func keysFrom<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): Iter<K> {
    return iterateHelper<K, V, K>(map, hashUtils, keyParam, func(key, value) = ?key, DEQ_PREV, DEQ_NEXT);
  };

  public func keysFromDesc<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): Iter<K> {
    return iterateHelper<K, V, K>(map, hashUtils, keyParam, func(key, value) = ?key, DEQ_NEXT, DEQ_PREV);
  };

  public func valsFrom<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): Iter<V> {
    return iterateHelper<K, V, V>(map, hashUtils, keyParam, func(key, value) = value, DEQ_PREV, DEQ_NEXT);
  };

  public func valsFromDesc<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): Iter<V> {
    return iterateHelper<K, V, V>(map, hashUtils, keyParam, func(key, value) = value, DEQ_NEXT, DEQ_PREV);
  };

  public func entriesFrom<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): Iter<(K, V)> {
    return iterateHelper<K, V, (K, V)>(map, hashUtils, keyParam, func(key, value) = ?(key, unwrap(value)), DEQ_PREV, DEQ_NEXT);
  };

  public func entriesFromDesc<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): Iter<(K, V)> {
    return iterateHelper<K, V, (K, V)>(map, hashUtils, keyParam, func(key, value) = ?(key, unwrap(value)), DEQ_NEXT, DEQ_PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func findHelper<K, V, T, R>(
    map: Map<K, V>,
    mapEntry: (K, V) -> T,
    acceptEntry: (T) -> Bool,
    getResult: (K, V) -> R,
    noMatchResult: R,
    DEQ_NEXT: Nat
  ): R {
    let (edgeEntry, size) = map;
    var entry = edgeEntry;

    loop {
      entry := entry.3[DEQ_NEXT];

      let (key, value, hash, links) = entry;

      switch (value) {
        case (?someValue) if (acceptEntry(mapEntry(key, someValue))) return getResult(key, someValue);
        case (_) return noMatchResult;
      };
    };
  };

  public func find<K, V>(map: Map<K, V>, mapEntry: (K, V) -> Bool): ?(K, V) {
    return findHelper<K, V, Bool, ?(K, V)>(map, mapEntry, func(bool) = bool, func(key, value) = ?(key, value), null, DEQ_NEXT);
  };

  public func findDesc<K, V>(map: Map<K, V>, mapEntry: (K, V) -> Bool): ?(K, V) {
    return findHelper<K, V, Bool, ?(K, V)>(map, mapEntry, func(bool) = bool, func(key, value) = ?(key, value), null, DEQ_PREV);
  };

  public func some<K, V>(map: Map<K, V>, mapEntry: (K, V) -> Bool): Bool {
    return findHelper<K, V, Bool, Bool>(map, mapEntry, func(bool) = bool, func(_) = true, false, DEQ_NEXT);
  };

  public func someDesc<K, V>(map: Map<K, V>, mapEntry: (K, V) -> Bool): Bool {
    return findHelper<K, V, Bool, Bool>(map, mapEntry, func(bool) = bool, func(_) = true, false, DEQ_PREV);
  };

  public func every<K, V>(map: Map<K, V>, mapEntry: (K, V) -> Bool): Bool {
    return findHelper<K, V, Bool, Bool>(map, mapEntry, func(bool) = not bool, func(_) = false, true, DEQ_NEXT);
  };

  public func everyDesc<K, V>(map: Map<K, V>, mapEntry: (K, V) -> Bool): Bool {
    return findHelper<K, V, Bool, Bool>(map, mapEntry, func(bool) = not bool, func(_) = false, true, DEQ_PREV);
  };

  public func forEach<K, V>(map: Map<K, V>, mapEntry: (K, V) -> (())) {
    findHelper<K, V, (), ()>(map, mapEntry, func(_) = false, func(_) = (), (), DEQ_NEXT);
  };

  public func forEachDesc<K, V>(map: Map<K, V>, mapEntry: (K, V) -> (())) {
    findHelper<K, V, (), ()>(map, mapEntry, func(_) = false, func(_) = (), (), DEQ_PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func fromIterHelper<K, V, T>(iter: IterNext<T>, hashUtils: HashUtils<K>, mapItem: (T) -> ?(K, V), prependItem: Bool): Map<K, V> {
    let map = new<K, V>(hashUtils);

    let insertItem = if (prependItem) setFront else set;

    for (item in iter) switch (mapItem(item)) { case (?(key, value)) insertItem(map, hashUtils, key, value); case (_) {} };

    return map;
  };

  public func fromIter<K, V>(iter: IterNext<(K, V)>, hashUtils: HashUtils<K>): Map<K, V> {
    return fromIterHelper<K, V, (K, V)>(iter, hashUtils, func(item) = ?item, false);
  };

  public func fromIterDesc<K, V>(iter: IterNext<(K, V)>, hashUtils: HashUtils<K>): Map<K, V> {
    return fromIterHelper<K, V, (K, V)>(iter, hashUtils, func(item) = ?item, true);
  };

  public func fromIterMap<K, V, T>(iter: IterNext<T>, hashUtils: HashUtils<K>, mapItem: (T) -> ?(K, V)): Map<K, V> {
    return fromIterHelper(iter, hashUtils, mapItem, false);
  };

  public func fromIterMapDesc<K, V, T>(iter: IterNext<T>, hashUtils: HashUtils<K>, mapItem: (T) -> ?(K, V)): Map<K, V> {
    return fromIterHelper(iter, hashUtils, mapItem, true);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func toArrayHelper<K, V, T>(map: Map<K, V>, mapEntry: (K, V) -> ?T, partial: Bool, DEQ_NEXT: Nat): [T] {
    let (edgeEntry, size) = map;

    let array = if (partial) initArray<?T>(nat(size[SIZE]), null) else [var]:[var ?T];

    var entry = if (partial) edgeEntry.3[DEQ_NEXT] else edgeEntry;

    var arraySize = if (partial) 0:Nat32 else size[SIZE];

    loop {
      let (key, value, hash, links) = entry;

      switch (value) {
        case (?someValue) switch (mapEntry(key, someValue)) {
          case (null) entry := links[DEQ_NEXT];

          case (item) {
            array[nat(arraySize)] := item;
            entry := links[DEQ_NEXT];
            arraySize +%= 1;
          };
        };

        case (_) return tabulateArray<T>(nat(arraySize), func(i) = if (partial) return unwrap(array[i]) else {
          entry := entry.3[DEQ_NEXT];

          return unwrap(mapEntry(key, unwrap(value)));
        });
      };
    };
  };

  public func toArray<K, V>(map: Map<K, V>): [(K, V)] {
    return toArrayHelper<K, V, (K, V)>(map, func(key, value) = ?(key, value), false, DEQ_NEXT);
  };

  public func toArrayDesc<K, V>(map: Map<K, V>): [(K, V)] {
    return toArrayHelper<K, V, (K, V)>(map, func(key, value) = ?(key, value), false, DEQ_PREV);
  };

  public func toArrayMap<K, V, T>(map: Map<K, V>, mapEntry: (K, V) -> ?T): [T] {
    return toArrayHelper(map, mapEntry, true, DEQ_NEXT);
  };

  public func toArrayMapDesc<K, V, T>(map: Map<K, V>, mapEntry: (K, V) -> ?T): [T] {
    return toArrayHelper(map, mapEntry, true, DEQ_PREV);
  };
};
