import Prim "mo:prim";
import Utils "../utils";

module {
  public type Entry<K> = (
    key: K,
    hash: Nat32,
    links: [var Entry<K>],
  );

  public type Set<K> = (
    edgeEntry: Entry<K>,
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

  public type IterNative<T> = {
    next: () -> ?T;
  };

  public type HashUtils<K> = Utils.HashUtils<K>;

  public let { ihash; nhash; thash; phash; bhash; lhash; useHash; calcHash } = Utils;

  let { Array_tabulate = tabulateArray; Array_init = initArray; nat32ToNat = nat; trap } = Prim;

  func unwrap<T>(value: ?T): T = switch (value) { case (?value) value; case (_) trap("unreachable") };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  let SIZE = 0;
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

  func createEdgeEntry<K>(nullKey: K): Entry<K> {
    let tempEntry: Entry<K> = (nullKey, NULL_HASH, [var]);
    let newEdgeEntry: Entry<K> = (nullKey, NULL_HASH, [var tempEntry, tempEntry, tempEntry, tempEntry, tempEntry, tempEntry]);

    for (index in newEdgeEntry.2.keys()) newEdgeEntry.2[index] := newEdgeEntry;

    return newEdgeEntry;
  };

  public func new<K>((getHash, areEqual, getNullKey): HashUtils<K>): Set<K> {
    return (createEdgeEntry<K>(getNullKey()), [var 0]);
  };

  public func clear<K>(map: Set<K>) {
    let (edgeEntry, size) = map;

    for (index in edgeEntry.2.keys()) edgeEntry.2[index] := edgeEntry;

    size[SIZE] := 0;
  };

  public func size<K>(map: Set<K>): Nat {
    return nat(map.1[SIZE]);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func has<K>(map: Set<K>, (getHash, areEqual, getNullKey): HashUtils<K>, keyParam: K): Bool {
    let (edgeEntry, size) = map;
    let hashParam = getHash(keyParam);
    var shiftingHash = hashParam;
    var entry = edgeEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      let (key, hash, links) = entry;

      if (hash == hashParam and areEqual(key, keyParam)) return true else if (hash == NULL_HASH) return false else {
        shiftingHash >>= HASH_OFFSET;
        entry := links[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func putHelper<K>(
    map: Set<K>,
    (getHash, areEqual, getNullKey): HashUtils<K>,
    keyParam: K,
    moveExisting: Bool,
    DEQ_PREV: Nat,
    DEQ_NEXT: Nat,
  ): Bool {
    let (edgeEntry, size) = map;
    let hashParam = getHash(keyParam);
    var shiftingHash = hashParam;
    var entry = edgeEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
    var parentEntry = edgeEntry;

    loop {
      let (key, hash, links) = entry;

      if (hash == NULL_HASH) {
        let deqPrev = edgeEntry.2[DEQ_PREV];
        let newEntry = (keyParam, hashParam, [var edgeEntry, edgeEntry, edgeEntry, edgeEntry, edgeEntry, edgeEntry]);

        newEntry.2[DEQ_PREV] := deqPrev;
        deqPrev.2[DEQ_NEXT] := newEntry;
        edgeEntry.2[DEQ_PREV] := newEntry;
        parentEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        size[SIZE] +%= 1;

        return false;
      } else if (hash == hashParam and areEqual(key, keyParam)) {
        if (moveExisting) {
          let deqNextOld = links[DEQ_NEXT];

          if (deqNextOld.1 != NULL_HASH) {
            let deqPrevOld = links[DEQ_PREV];
            let deqPrev = edgeEntry.2[DEQ_PREV];

            deqPrevOld.2[DEQ_NEXT] := deqNextOld;
            deqNextOld.2[DEQ_PREV] := deqPrevOld;
            links[DEQ_PREV] := deqPrev;
            links[DEQ_NEXT] := edgeEntry;
            deqPrev.2[DEQ_NEXT] := entry;
            edgeEntry.2[DEQ_PREV] := entry;
          };
        };

        return true;
      } else {
        parentEntry := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := links[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func put<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    return putHelper(map, hashUtils, keyParam, false, DEQ_PREV, DEQ_NEXT);
  };

  public func putFront<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    return putHelper(map, hashUtils, keyParam, false, DEQ_NEXT, DEQ_PREV);
  };

  public func add<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K) {
    ignore putHelper(map, hashUtils, keyParam, false, DEQ_PREV, DEQ_NEXT);
  };

  public func addFront<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K) {
    ignore putHelper(map, hashUtils, keyParam, false, DEQ_NEXT, DEQ_PREV);
  };

  public func putMove<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    return putHelper(map, hashUtils, keyParam, true, DEQ_PREV, DEQ_NEXT);
  };

  public func putMoveFront<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    return putHelper(map, hashUtils, keyParam, true, DEQ_NEXT, DEQ_PREV);
  };

  public func addMove<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K) {
    ignore putHelper(map, hashUtils, keyParam, true, DEQ_PREV, DEQ_NEXT);
  };

  public func addMoveFront<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K) {
    ignore putHelper(map, hashUtils, keyParam, true, DEQ_NEXT, DEQ_PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func putBeforeHelper<K>(
    map: Set<K>,
    (getHash, areEqual, getNullKey): HashUtils<K>,
    keyParam: K,
    placeKeyParam: K,
    DEQ_PREV: Nat,
    DEQ_NEXT: Nat,
  ): Bool {
    let (edgeEntry, size) = map;
    let hashParam = getHash(keyParam);
    var shiftingHash = hashParam;
    var entry = edgeEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
    var parentEntry = edgeEntry;

    let placeHashParam = getHash(placeKeyParam);
    var shiftingPlaceHash = placeHashParam;
    var placeEntry = edgeEntry.2[nat(shiftingPlaceHash % HASH_CHUNK_SIZE)];

    loop if (placeEntry.1 == NULL_HASH or placeEntry.1 == placeHashParam and areEqual(placeEntry.0, placeKeyParam)) loop {
      let (key, hash, links) = entry;

      if (hash == NULL_HASH) {
        let deqPrev = placeEntry.2[DEQ_PREV];
        let newEntry = (keyParam, hashParam, [var edgeEntry, edgeEntry, edgeEntry, edgeEntry, placeEntry, placeEntry]);

        newEntry.2[DEQ_PREV] := deqPrev;
        deqPrev.2[DEQ_NEXT] := newEntry;
        placeEntry.2[DEQ_PREV] := newEntry;
        parentEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        size[SIZE] +%= 1;

        return false;
      } else if (hash == hashParam and areEqual(key, keyParam)) {
        let deqPrevOld = links[DEQ_PREV];
        let deqNextOld = links[DEQ_NEXT];
        let deqPrev = placeEntry.2[DEQ_PREV];

        deqPrevOld.2[DEQ_NEXT] := deqNextOld;
        deqNextOld.2[DEQ_PREV] := deqPrevOld;
        links[DEQ_PREV] := deqPrev;
        links[DEQ_NEXT] := placeEntry;
        deqPrev.2[DEQ_NEXT] := entry;
        placeEntry.2[DEQ_PREV] := entry;

        return true;
      } else {
        parentEntry := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := links[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    } else {
      shiftingPlaceHash >>= HASH_OFFSET;
      placeEntry := placeEntry.2[nat(shiftingPlaceHash % HASH_CHUNK_SIZE)];
    };
  };

  public func putBefore<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K, placeKeyParam: K): Bool {
    return putBeforeHelper(map, hashUtils, keyParam, placeKeyParam, DEQ_PREV, DEQ_NEXT);
  };

  public func putAfter<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K, placeKeyParam: K): Bool {
    return putBeforeHelper(map, hashUtils, keyParam, placeKeyParam, DEQ_NEXT, DEQ_PREV);
  };

  public func addBefore<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K, placeKeyParam: K) {
    ignore putBeforeHelper(map, hashUtils, keyParam, placeKeyParam, DEQ_PREV, DEQ_NEXT);
  };

  public func addAfter<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K, placeKeyParam: K) {
    ignore putBeforeHelper(map, hashUtils, keyParam, placeKeyParam, DEQ_NEXT, DEQ_PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func remove<K>(map: Set<K>, (getHash, areEqual, getNullKey): HashUtils<K>, keyParam: K): Bool {
    let (edgeEntry, size) = map;
    let hashParam = getHash(keyParam);
    var shiftingHash = hashParam;
    var entry = edgeEntry.2[nat(hashParam % HASH_CHUNK_SIZE)];
    var parentEntry = edgeEntry;

    loop {
      let (key, hash, links) = entry;

      if (hash == hashParam and areEqual(key, keyParam)) {
        let deqPrev = links[DEQ_PREV];
        let deqNext = links[DEQ_NEXT];
        var leaf = entry;
        var leafParent = parentEntry;
        var leafIndex = NULL_BRANCH;

        deqPrev.2[DEQ_NEXT] := deqNext;
        deqNext.2[DEQ_PREV] := deqPrev;
        size[SIZE] -%= 1;

        loop {
          let branch1 = leaf.2[BRANCH_1];

          if (branch1.1 != NULL_HASH) {
            leafIndex := BRANCH_1;
            leafParent := leaf;
            leaf := branch1;
          } else {
            let branch2 = leaf.2[BRANCH_2];

            if (branch2.1 != NULL_HASH) {
              leafIndex := BRANCH_2;
              leafParent := leaf;
              leaf := branch2;
            } else {
              let branch3 = leaf.2[BRANCH_3];

              if (branch3.1 != NULL_HASH) {
                leafIndex := BRANCH_3;
                leafParent := leaf;
                leaf := branch3;
              } else {
                let branch4 = leaf.2[BRANCH_4];

                if (branch4.1 != NULL_HASH) {
                  leafIndex := BRANCH_4;
                  leafParent := leaf;
                  leaf := branch4;
                } else if (leafIndex == NULL_BRANCH) {
                  parentEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := edgeEntry;

                  return true;
                } else {
                  parentEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := leaf;
                  leafParent.2[leafIndex] := edgeEntry;

                  for (index in leaf.2.keys()) if (index <= BRANCH_4) leaf.2[index] := entry.2[index];

                  return true;
                };
              };
            };
          };
        };
      } else if (hash == NULL_HASH) {
        return false;
      } else {
        parentEntry := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := links[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func delete<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K) {
    ignore remove(map, hashUtils, keyParam);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func popHelper<K>(map: Set<K>, DEQ_PREV: Nat, DEQ_NEXT: Nat): ?K {
    let (edgeEntry, size) = map;
    var entry = edgeEntry.2[DEQ_PREV];
    var shiftingHash = entry.1;
    var parentEntry = edgeEntry;

    if (shiftingHash == NULL_HASH) return null;

    loop {
      let (key, hash, links) = entry;

      if (links[DEQ_NEXT].1 == NULL_HASH) {
        let deqPrev = links[DEQ_PREV];
        var leaf = entry;
        var leafParent = parentEntry;
        var leafIndex = NULL_BRANCH;

        deqPrev.2[DEQ_NEXT] := edgeEntry;
        edgeEntry.2[DEQ_PREV] := deqPrev;
        size[SIZE] -%= 1;

        loop {
          let branch1 = leaf.2[BRANCH_1];

          if (branch1.1 != NULL_HASH) {
            leafIndex := BRANCH_1;
            leafParent := leaf;
            leaf := branch1;
          } else {
            let branch2 = leaf.2[BRANCH_2];

            if (branch2.1 != NULL_HASH) {
              leafIndex := BRANCH_2;
              leafParent := leaf;
              leaf := branch2;
            } else {
              let branch3 = leaf.2[BRANCH_3];

              if (branch3.1 != NULL_HASH) {
                leafIndex := BRANCH_3;
                leafParent := leaf;
                leaf := branch3;
              } else {
                let branch4 = leaf.2[BRANCH_4];

                if (branch4.1 != NULL_HASH) {
                  leafIndex := BRANCH_4;
                  leafParent := leaf;
                  leaf := branch4;
                } else if (leafIndex == NULL_BRANCH) {
                  parentEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := edgeEntry;

                  return ?key;
                } else {
                  parentEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := leaf;
                  leafParent.2[leafIndex] := edgeEntry;

                  for (index in leaf.2.keys()) if (index <= BRANCH_4) leaf.2[index] := entry.2[index];

                  return ?key;
                };
              };
            };
          };
        };
      } else {
        parentEntry := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := links[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func pop<K>(map: Set<K>): ?K {
    return popHelper(map, DEQ_PREV, DEQ_NEXT);
  };

  public func popFront<K>(map: Set<K>): ?K {
    return popHelper(map, DEQ_NEXT, DEQ_PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func cycleHelper<K>(map: Set<K>, DEQ_PREV: Nat, DEQ_NEXT: Nat): ?K {
    let (edgeEntry, size) = map;
    let entry = edgeEntry.2[DEQ_PREV];
    let (key, hash, links) = entry;

    if (hash == NULL_HASH) return null else {
      let deqPrev = links[DEQ_PREV];

      if (deqPrev.1 != NULL_HASH) {
        let deqFirst = edgeEntry.2[DEQ_NEXT];

        deqPrev.2[DEQ_NEXT] := edgeEntry;
        edgeEntry.2[DEQ_PREV] := deqPrev;
        deqFirst.2[DEQ_PREV] := entry;
        edgeEntry.2[DEQ_NEXT] := entry;
        links[DEQ_PREV] := edgeEntry;
        links[DEQ_NEXT] := deqFirst;
      };

      return ?key;
    };
  };

  public func cycle<K>(map: Set<K>): ?K {
    return cycleHelper(map, DEQ_PREV, DEQ_NEXT);
  };

  public func cycleFront<K>(map: Set<K>): ?K {
    return cycleHelper(map, DEQ_NEXT, DEQ_PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func peek<K>(map: Set<K>): ?K {
    let (edgeEntry, size) = map;
    let (key, hash, links) = edgeEntry.2[DEQ_PREV];

    return if (hash == NULL_HASH) null else ?key;
  };

  public func peekFront<K>(map: Set<K>): ?K {
    let (edgeEntry, size) = map;
    let (key, hash, links) = edgeEntry.2[DEQ_NEXT];

    return if (hash == NULL_HASH) null else ?key;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func filter<K>(map: Set<K>, fn: (K) -> Bool): Set<K> {
    let (edgeEntry, size) = map;
    let newEdgeEntry = createEdgeEntry<K>(edgeEntry.0);
    var entry = edgeEntry.2[DEQ_NEXT];
    var deqPrev = newEdgeEntry;
    var newSize = 0:Nat32;

    loop {
      let (key, hash, links) = entry;

      if (hash == NULL_HASH) {
        newEdgeEntry.2[DEQ_PREV] := deqPrev;

        return (newEdgeEntry, [var newSize]);
      } else if (fn(key)) {
        var shiftingHash = hash;
        var searchEntry = newEdgeEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
        var parentEntry = newEdgeEntry;

        while (searchEntry.1 != NULL_HASH) {
          parentEntry := searchEntry;
          shiftingHash >>= HASH_OFFSET;
          searchEntry := searchEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
        };

        let newEntry = (key, hash, [var newEdgeEntry, newEdgeEntry, newEdgeEntry, newEdgeEntry, deqPrev, newEdgeEntry]);

        deqPrev.2[DEQ_NEXT] := newEntry;
        parentEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        deqPrev := newEntry;
        entry := links[DEQ_NEXT];
        newSize +%= 1;
      } else {
        entry := links[DEQ_NEXT];
      };
    };
  };

  public func clone<K>(map: Set<K>): Set<K> {
    return filter<K>(map, func(key) = true);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func iterateHelper<K>(map: Set<K>, DEQ_PREV: Nat, DEQ_NEXT: Nat): Iter<K> {
    let (edgeEntry, size) = map;
    var entry = edgeEntry;

    return let iter = {
      prev = func(): ?K {
        entry := entry.2[DEQ_PREV];

        let (key, hash, links) = entry;

        return if (hash == NULL_HASH) null else ?key;
      };

      next = func(): ?K {
        entry := entry.2[DEQ_NEXT];

        let (key, hash, links) = entry;

        return if (hash == NULL_HASH) null else ?key;
      };

      current = func(): ?K {
        let (key, hash, links) = entry;

        return if (hash == NULL_HASH) null else ?key;
      };

      reset = func(): Iter<K> {
        entry := edgeEntry;

        return iter;
      };

      movePrev = func(): Iter<K> {
        entry := entry.2[DEQ_PREV];

        return iter;
      };

      moveNext = func(): Iter<K> {
        entry := entry.2[DEQ_NEXT];

        return iter;
      };
    };
  };

  public func keys<K>(map: Set<K>): Iter<K> {
    return iterateHelper<K>(map, DEQ_PREV, DEQ_NEXT);
  };

  public func keysDesc<K>(map: Set<K>): Iter<K> {
    return iterateHelper<K>(map, DEQ_NEXT, DEQ_PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func iterateFromHelper<K>(
    map: Set<K>,
    (getHash, areEqual, getNullKey): HashUtils<K>,
    keyParam: K,
    DEQ_PREV: Nat,
    DEQ_NEXT: Nat,
  ): Iter<K> {
    let (edgeEntry, size) = map;
    let hashParam = getHash(keyParam);
    var shiftingHash = hashParam;
    var entry = edgeEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      let (key, hash, links) = entry;

      if (hash == NULL_HASH or hash == hashParam and areEqual(key, keyParam)) return let iter = {
        prev = func(): ?K {
          entry := entry.2[DEQ_PREV];

          let (key, hash, links) = entry;

          return if (hash == NULL_HASH) null else ?key;
        };

        next = func(): ?K {
          entry := entry.2[DEQ_NEXT];

          let (key, hash, links) = entry;

          return if (hash == NULL_HASH) null else ?key;
        };

        current = func(): ?K {
          let (key, hash, links) = entry;

          return if (hash == NULL_HASH) null else ?key;
        };

        reset = func(): Iter<K> {
          entry := edgeEntry;

          return iter;
        };

        movePrev = func(): Iter<K> {
          entry := entry.2[DEQ_PREV];

          return iter;
        };

        moveNext = func(): Iter<K> {
          entry := entry.2[DEQ_NEXT];

          return iter;
        };
      } else {
        shiftingHash >>= HASH_OFFSET;
        entry := links[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func keysFrom<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Iter<K> {
    return iterateFromHelper<K>(map, hashUtils, keyParam, DEQ_PREV, DEQ_NEXT);
  };

  public func keysFromDesc<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Iter<K> {
    return iterateFromHelper<K>(map, hashUtils, keyParam, DEQ_NEXT, DEQ_PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func forEach<K>(map: Set<K>, fn: (K) -> ()) {
    let (edgeEntry, size) = map;
    var entry = edgeEntry;

    loop {
      entry := entry.2[DEQ_NEXT];

      let (key, hash, links) = entry;

      if (hash == NULL_HASH) return else fn(key);
    };
  };

  public func forEachDesc<K>(map: Set<K>, fn: (K) -> ()) {
    let (edgeEntry, size) = map;
    var entry = edgeEntry;

    loop {
      entry := entry.2[DEQ_PREV];

      let (key, hash, links) = entry;

      if (hash == NULL_HASH) return else fn(key);
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func every<K>(map: Set<K>, fn: (K) -> Bool): Bool {
    let (edgeEntry, size) = map;
    var entry = edgeEntry;

    loop {
      entry := entry.2[DEQ_NEXT];

      let (key, hash, links) = entry;

      if (hash == NULL_HASH) return true else if (not fn(key)) return false;
    };
  };

  public func everyDesc<K>(map: Set<K>, fn: (K) -> Bool): Bool {
    let (edgeEntry, size) = map;
    var entry = edgeEntry;

    loop {
      entry := entry.2[DEQ_PREV];

      let (key, hash, links) = entry;

      if (hash == NULL_HASH) return true else if (not fn(key)) return false;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func some<K>(map: Set<K>, fn: (K) -> Bool): Bool {
    let (edgeEntry, size) = map;
    var entry = edgeEntry;

    loop {
      entry := entry.2[DEQ_NEXT];

      let (key, hash, links) = entry;

      if (hash == NULL_HASH) return false else if (fn(key)) return true;
    };
  };

  public func someDesc<K>(map: Set<K>, fn: (K) -> Bool): Bool {
    let (edgeEntry, size) = map;
    var entry = edgeEntry;

    loop {
      entry := entry.2[DEQ_PREV];

      let (key, hash, links) = entry;

      if (hash == NULL_HASH) return false else if (fn(key)) return true;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func find<K>(map: Set<K>, fn: (K) -> Bool): ?K {
    let (edgeEntry, size) = map;
    var entry = edgeEntry;

    loop {
      entry := entry.2[DEQ_NEXT];

      let (key, hash, links) = entry;

      if (hash == NULL_HASH) return null else if (fn(key)) return ?key;
    };
  };

  public func findDesc<K>(map: Set<K>, fn: (K) -> Bool): ?K {
    let (edgeEntry, size) = map;
    var entry = edgeEntry;

    loop {
      entry := entry.2[DEQ_PREV];

      let (key, hash, links) = entry;

      if (hash == NULL_HASH) return null else if (fn(key)) return ?key;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func fromIter<K>(iter: IterNative<K>, hashUtils: HashUtils<K>): Set<K> {
    let newMap = new<K>(hashUtils);

    for (key in iter) add(newMap, hashUtils, key);

    return newMap;
  };

  public func fromIterDesc<K>(iter: IterNative<K>, hashUtils: HashUtils<K>): Set<K> {
    let newMap = new<K>(hashUtils);

    for (key in iter) addFront(newMap, hashUtils, key);

    return newMap;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func fromIterMap<K, T>(iter: IterNative<T>, hashUtils: HashUtils<K>, fn: (T) -> ?K): Set<K> {
    let newMap = new<K>(hashUtils);

    for (item in iter) switch (fn(item)) { case (?key) add(newMap, hashUtils, key); case (_) {} };

    return newMap;
  };

  public func fromIterMapDesc<K, T>(iter: IterNative<T>, hashUtils: HashUtils<K>, fn: (T) -> ?K): Set<K> {
    let newMap = new<K>(hashUtils);

    for (item in iter) switch (fn(item)) { case (?key) addFront(newMap, hashUtils, key); case (_) {} };

    return newMap;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func toArray<K>(map: Set<K>): [K] {
    let (edgeEntry, size) = map;
    var entry = edgeEntry;

    return tabulateArray<K>(nat(size[SIZE]), func(i) {
      entry := entry.2[DEQ_NEXT];

      let (key, hash, links) = entry;

      return key;
    });
  };

  public func toArrayDesc<K>(map: Set<K>): [K] {
    let (edgeEntry, size) = map;
    var entry = edgeEntry;

    return tabulateArray<K>(nat(size[SIZE]), func(i) {
      entry := entry.2[DEQ_PREV];

      let (key, hash, links) = entry;

      return key;
    });
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func toArrayMapHelper<K, T>(map: Set<K>, fn: (K) -> ?T, DEQ_NEXT: Nat): [T] {
    let (edgeEntry, size) = map;
    let array = initArray<?T>(nat(size[SIZE]), null);
    var entry = edgeEntry.2[DEQ_NEXT];
    var arraySize = 0:Nat32;

    loop {
      let (key, hash, links) = entry;

      if (hash == NULL_HASH) return tabulateArray<T>(nat(arraySize), func(i) = unwrap(array[i])) else switch (fn(key)) {
        case (null) entry := links[DEQ_NEXT];

        case (item) {
          array[nat(arraySize)] := item;
          entry := links[DEQ_NEXT];
          arraySize +%= 1;
        };
      };
    };
  };

  public func toArrayMap<K, T>(map: Set<K>, fn: (K) -> ?T, DEQ_NEXT: Nat): [T] {
    return toArrayMapHelper(map, fn, DEQ_NEXT);
  };

  public func toArrayMapDesc<K, T>(map: Set<K>, fn: (K) -> ?T, DEQ_NEXT: Nat): [T] {
    return toArrayMapHelper(map, fn, DEQ_PREV);
  };
};
