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

  public type IterNext<T> = {
    next: () -> ?T;
  };

  public type HashUtils<K> = Utils.HashUtils<K>;

  public let { hashInt; hashText; hashPrincipal; hashBlob; hashBool } = Utils;

  public let { ihash; nhash; thash; phash; bhash; lhash; useHash; calcHash } = Utils;

  let { Array_tabulate = tabulateArray; Array_init = initArray; nat32ToNat = nat; trap } = Prim;

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

  func noop<T>(): T = trap("unreachable");

  func isSome<T>(value: ?T): Bool = switch (value) { case (null) false; case (_) true };

  func unwrap<T>(value: ?T): T = switch (value) { case (?value) value; case (_) noop() };

  func tempHashUtils<K>(): HashUtils<K> = (func(_) = NULL_HASH, func(_) = false, func() = noop());

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func createEdgeEntry<K>(nullKey: K): Entry<K> {
    let tempEntry = (nullKey, NULL_HASH, [var]):Entry<K>;
    let newEdge = (nullKey, NULL_HASH, [var tempEntry, tempEntry, tempEntry, tempEntry, tempEntry, tempEntry]);

    for (index in newEdge.2.keys()) newEdge.2[index] := newEdge;

    return newEdge;
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

  func getHelper<K>(map: Set<K>, (getHash, areEqual, getNullKey): HashUtils<K>, keyParam: K): Bool {
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

  public func has<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    return getHelper(map, hashUtils, keyParam);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func putHelper<K>(
    map: Set<K>,
    (getHash, areEqual, getNullKey): HashUtils<K>,
    keyParam: K,
    placeKeyParam: K,
    usePlaceKey: Bool,
    moveExisting: Bool,
    DEQ_PREV: Nat,
    DEQ_NEXT: Nat,
  ): Bool {
    let (edgeEntry, size) = map;
    let hashParam = getHash(keyParam);
    var shiftingHash = hashParam;
    var entry = edgeEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
    var parentEntry = edgeEntry;

    let placeHashParam = if (usePlaceKey) getHash(placeKeyParam) else NULL_HASH;

    var shiftingPlaceHash = placeHashParam;

    var placeEntry = if (usePlaceKey) edgeEntry.2[nat(shiftingPlaceHash % HASH_CHUNK_SIZE)] else edgeEntry;

    loop {
      let (placeKey, placeHash, placeLinks) = placeEntry;

      if (placeHash == NULL_HASH or placeHash == placeHashParam and areEqual(placeKey, placeKeyParam)) loop {
        let (key, hash, links) = entry;

        if (hash == NULL_HASH) {
          let deqPrev = placeLinks[DEQ_PREV];
          let newEntry = (keyParam, hashParam, [var edgeEntry, edgeEntry, edgeEntry, edgeEntry, placeEntry, placeEntry]);

          newEntry.2[DEQ_PREV] := deqPrev;
          deqPrev.2[DEQ_NEXT] := newEntry;
          placeLinks[DEQ_PREV] := newEntry;
          parentEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
          size[SIZE] +%= 1;

          return false;
        } else if (hash == hashParam and areEqual(key, keyParam)) {
          if (moveExisting) {
            let deqNextOld = links[DEQ_NEXT];

            if (usePlaceKey or deqNextOld.1 != NULL_HASH) {
              let deqPrevOld = links[DEQ_PREV];
              let deqPrev = placeLinks[DEQ_PREV];

              deqPrevOld.2[DEQ_NEXT] := deqNextOld;
              deqNextOld.2[DEQ_PREV] := deqPrevOld;
              links[DEQ_PREV] := deqPrev;
              links[DEQ_NEXT] := placeEntry;
              deqPrev.2[DEQ_NEXT] := entry;
              placeLinks[DEQ_PREV] := entry;
            };
          };

          return true;
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

  public func put<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    return putHelper(map, hashUtils, keyParam, map.0.0, false, false, DEQ_PREV, DEQ_NEXT);
  };

  public func putFront<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    return putHelper(map, hashUtils, keyParam, map.0.0, false, false, DEQ_NEXT, DEQ_PREV);
  };

  public func add<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K) {
    ignore putHelper(map, hashUtils, keyParam, map.0.0, false, false, DEQ_PREV, DEQ_NEXT);
  };

  public func addFront<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K) {
    ignore putHelper(map, hashUtils, keyParam, map.0.0, false, false, DEQ_NEXT, DEQ_PREV);
  };

  public func putMove<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    return putHelper(map, hashUtils, keyParam, map.0.0, false, true, DEQ_PREV, DEQ_NEXT);
  };

  public func putMoveFront<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    return putHelper(map, hashUtils, keyParam, map.0.0, false, true, DEQ_NEXT, DEQ_PREV);
  };

  public func addMove<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K) {
    ignore putHelper(map, hashUtils, keyParam, map.0.0, false, true, DEQ_PREV, DEQ_NEXT);
  };

  public func addMoveFront<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K) {
    ignore putHelper(map, hashUtils, keyParam, map.0.0, false, true, DEQ_NEXT, DEQ_PREV);
  };

  public func putBefore<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K, placeKeyParam: K): Bool {
    return putHelper(map, hashUtils, keyParam, placeKeyParam, true, true, DEQ_PREV, DEQ_NEXT);
  };

  public func putAfter<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K, placeKeyParam: K): Bool {
    return putHelper(map, hashUtils, keyParam, placeKeyParam, true, true, DEQ_NEXT, DEQ_PREV);
  };

  public func addBefore<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K, placeKeyParam: K) {
    ignore putHelper(map, hashUtils, keyParam, placeKeyParam, true, true, DEQ_PREV, DEQ_NEXT);
  };

  public func addAfter<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K, placeKeyParam: K) {
    ignore putHelper(map, hashUtils, keyParam, placeKeyParam, true, true, DEQ_NEXT, DEQ_PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func removeHelper<K>(
    map: Set<K>,
    (getHash, areEqual, getNullKey): HashUtils<K>,
    keyParam: K,
    removeLast: Bool,
    DEQ_PREV: Nat,
    DEQ_NEXT: Nat,
  ): ?K {
    let (edgeEntry, size) = map;
    let deqLast = edgeEntry.2[DEQ_PREV];

    if (deqLast.1 == NULL_HASH) return null else {
      let hashParam = if (removeLast) deqLast.1 else getHash(keyParam);

      var shiftingHash = hashParam;
      var entry = edgeEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
      var parentEntry = edgeEntry;

      loop {
        let (key, hash, links) = entry;

        if (hash == hashParam and (if (removeLast) links[DEQ_NEXT].1 == NULL_HASH else areEqual(key, keyParam))) {
          let deqPrev = links[DEQ_PREV];
          let deqNext = links[DEQ_NEXT];
          var leaf = entry;
          var leafParent = parentEntry;
          var leafIndex = NULL_BRANCH;

          deqPrev.2[DEQ_NEXT] := deqNext;
          deqNext.2[DEQ_PREV] := deqPrev;
          size[SIZE] -%= 1;

          loop {
            let (leafKey, leafHash, leafLinks) = leaf;

            if (leafLinks[BRANCH_1].1 != NULL_HASH) {
              leafParent := leaf;
              leaf := leafLinks[BRANCH_1];
              leafIndex := BRANCH_1;
            } else if (leafLinks[BRANCH_2].1 != NULL_HASH) {
              leafParent := leaf;
              leaf := leafLinks[BRANCH_2];
              leafIndex := BRANCH_2;
            } else if (leafLinks[BRANCH_3].1 != NULL_HASH) {
              leafParent := leaf;
              leaf := leafLinks[BRANCH_3];
              leafIndex := BRANCH_3;
            } else if (leafLinks[BRANCH_4].1 != NULL_HASH) {
              leafParent := leaf;
              leaf := leafLinks[BRANCH_4];
              leafIndex := BRANCH_4;
            } else {
              if (leafIndex == NULL_BRANCH) {
                parentEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := edgeEntry;
              } else {
                parentEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := leaf;
                leafParent.2[leafIndex] := edgeEntry;

                for (index in leafLinks.keys()) if (index <= BRANCH_4) leafLinks[index] := links[index];
              };

              return ?key;
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

  public func remove<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    return isSome(removeHelper(map, hashUtils, keyParam, false, DEQ_PREV, DEQ_NEXT));
  };

  public func delete<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K) {
    ignore removeHelper(map, hashUtils, keyParam, false, DEQ_PREV, DEQ_NEXT);
  };

  public func pop<K>(map: Set<K>): ?K {
    return removeHelper<K>(map, tempHashUtils<K>(), map.0.0, true, DEQ_PREV, DEQ_NEXT);
  };

  public func popFront<K>(map: Set<K>): ?K {
    return removeHelper<K>(map, tempHashUtils<K>(), map.0.0, true, DEQ_NEXT, DEQ_PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func peekHelper<K>(map: Set<K>, cycleEntry: Bool, DEQ_PREV: Nat, DEQ_NEXT: Nat): ?K {
    let (edgeEntry, size) = map;
    let entry = edgeEntry.2[DEQ_PREV];
    let (key, hash, links) = entry;

    if (hash == NULL_HASH) return null else {
      if (cycleEntry) {
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
      };

      return ?key;
    };
  };

  public func peek<K>(map: Set<K>): ?K {
    return peekHelper(map, false, DEQ_PREV, DEQ_NEXT);
  };

  public func peekFront<K>(map: Set<K>): ?K {
    return peekHelper(map, false, DEQ_NEXT, DEQ_PREV);
  };

  public func cycle<K>(map: Set<K>): ?K {
    return peekHelper(map, true, DEQ_PREV, DEQ_NEXT);
  };

  public func cycleFront<K>(map: Set<K>): ?K {
    return peekHelper(map, true, DEQ_NEXT, DEQ_PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func mapFilterHelper<K>(
    map: Set<K>,
    acceptEntry: (K) -> Bool,
    DEQ_PREV: Nat,
    DEQ_NEXT: Nat,
  ): Set<K> {
    let (edgeEntry, size) = map;
    let newEdge = createEdgeEntry<K>(edgeEntry.0);
    var entry = edgeEntry.2[DEQ_NEXT];
    var deqPrev = newEdge;
    var newSize = 0:Nat32;

    loop {
      let (key, hash, links) = entry;

      if (hash == NULL_HASH) {
        newEdge.2[DEQ_PREV] := deqPrev;

        return (newEdge, [var newSize]);
      } else if (acceptEntry(key)) {
        var shiftingHash = hash;
        var searchEntry = newEdge.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
        var parentEntry = newEdge;

        while (searchEntry.1 != NULL_HASH) {
          parentEntry := searchEntry;
          shiftingHash >>= HASH_OFFSET;
          searchEntry := searchEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
        };

        let newEntry = (key, hash, [var newEdge, newEdge, newEdge, newEdge, newEdge, newEdge]);

        newEntry.2[DEQ_PREV] := deqPrev;
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

  public func filter<K>(map: Set<K>, acceptEntry: (K) -> Bool): Set<K> {
    return mapFilterHelper(map, acceptEntry, DEQ_PREV, DEQ_NEXT);
  };

  public func filterDesc<K>(map: Set<K>, acceptEntry: (K) -> Bool): Set<K> {
    return mapFilterHelper(map, acceptEntry, DEQ_NEXT, DEQ_PREV);
  };

  public func clone<K>(map: Set<K>): Set<K> {
    return mapFilterHelper<K>(map, func(_) = true, DEQ_PREV, DEQ_NEXT);
  };

  public func cloneDesc<K>(map: Set<K>): Set<K> {
    return mapFilterHelper<K>(map, func(_) = true, DEQ_NEXT, DEQ_PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func iterateHelper<K>(
    map: Set<K>,
    (getHash, areEqual, getNullKey): HashUtils<K>,
    keyParam: K,
    DEQ_PREV: Nat,
    DEQ_NEXT: Nat,
  ): Iter<K> {
    let (edgeEntry, size) = map;
    let hashParam = getHash(keyParam);
    var shiftingHash = hashParam;

    var entry = if (shiftingHash != NULL_HASH) edgeEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)] else edgeEntry;

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

  public func keys<K>(map: Set<K>): Iter<K> {
    return iterateHelper<K>(map, tempHashUtils<K>(), map.0.0, DEQ_PREV, DEQ_NEXT);
  };

  public func keysDesc<K>(map: Set<K>): Iter<K> {
    return iterateHelper<K>(map, tempHashUtils<K>(), map.0.0, DEQ_NEXT, DEQ_PREV);
  };

  public func keysFrom<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Iter<K> {
    return iterateHelper<K>(map, hashUtils, keyParam, DEQ_PREV, DEQ_NEXT);
  };

  public func keysFromDesc<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Iter<K> {
    return iterateHelper<K>(map, hashUtils, keyParam, DEQ_NEXT, DEQ_PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func findHelper<K, T, R>(
    map: Set<K>,
    mapEntry: (K) -> T,
    acceptEntry: (T) -> Bool,
    getResult: (K) -> R,
    noMatchResult: R,
    DEQ_NEXT: Nat,
  ): R {
    let (edgeEntry, size) = map;
    var entry = edgeEntry;

    loop {
      entry := entry.2[DEQ_NEXT];

      let (key, hash, links) = entry;

      if (hash == NULL_HASH) return noMatchResult else if (acceptEntry(mapEntry(key))) return getResult(key);
    };
  };

  public func find<K>(map: Set<K>, mapEntry: (K) -> Bool): ?K {
    return findHelper<K, Bool, ?K>(map, mapEntry, func(bool) = bool, func(key) = ?key, null, DEQ_NEXT);
  };

  public func findDesc<K>(map: Set<K>, mapEntry: (K) -> Bool): ?K {
    return findHelper<K, Bool, ?K>(map, mapEntry, func(bool) = bool, func(key) = ?key, null, DEQ_PREV);
  };

  public func some<K>(map: Set<K>, mapEntry: (K) -> Bool): Bool {
    return findHelper<K, Bool, Bool>(map, mapEntry, func(bool) = bool, func(_) = true, false, DEQ_NEXT);
  };

  public func someDesc<K>(map: Set<K>, mapEntry: (K) -> Bool): Bool {
    return findHelper<K, Bool, Bool>(map, mapEntry, func(bool) = bool, func(_) = true, false, DEQ_PREV);
  };

  public func every<K>(map: Set<K>, mapEntry: (K) -> Bool): Bool {
    return findHelper<K, Bool, Bool>(map, mapEntry, func(bool) = not bool, func(_) = false, true, DEQ_NEXT);
  };

  public func everyDesc<K>(map: Set<K>, mapEntry: (K) -> Bool): Bool {
    return findHelper<K, Bool, Bool>(map, mapEntry, func(bool) = not bool, func(_) = false, true, DEQ_PREV);
  };

  public func forEach<K>(map: Set<K>, mapEntry: (K) -> (())) {
    findHelper<K, (), ()>(map, mapEntry, func(_) = false, func(_) = (), (), DEQ_NEXT);
  };

  public func forEachDesc<K>(map: Set<K>, mapEntry: (K) -> (())) {
    findHelper<K, (), ()>(map, mapEntry, func(_) = false, func(_) = (), (), DEQ_NEXT);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func fromIterHelper<K, T>(iter: IterNext<T>, hashUtils: HashUtils<K>, mapItem: (T) -> ?K, prependItem: Bool): Set<K> {
    let map = new<K>(hashUtils);

    let insertItem = if (prependItem) addFront else add;

    for (item in iter) switch (mapItem(item)) { case (?key) insertItem(map, hashUtils, key); case (_) {} };

    return map;
  };

  public func fromIter<K>(iter: IterNext<K>, hashUtils: HashUtils<K>): Set<K> {
    return fromIterHelper<K, K>(iter, hashUtils, func(item) = ?item, false);
  };

  public func fromIterDesc<K>(iter: IterNext<K>, hashUtils: HashUtils<K>): Set<K> {
    return fromIterHelper<K, K>(iter, hashUtils, func(item) = ?item, true);
  };

  public func fromIterMap<K, T>(iter: IterNext<T>, hashUtils: HashUtils<K>, mapItem: (T) -> ?K): Set<K> {
    return fromIterHelper(iter, hashUtils, mapItem, false);
  };

  public func fromIterMapDesc<K, T>(iter: IterNext<T>, hashUtils: HashUtils<K>, mapItem: (T) -> ?K): Set<K> {
    return fromIterHelper(iter, hashUtils, mapItem, true);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func toArrayHelper<K, T>(map: Set<K>, mapEntry: (K) -> ?T, partial: Bool, DEQ_NEXT: Nat): [T] {
    let (edgeEntry, size) = map;

    let array = if (partial) initArray<?T>(nat(size[SIZE]), null) else [var]:[var ?T];

    var entry = if (partial) edgeEntry.2[DEQ_NEXT] else edgeEntry;

    var arraySize = if (partial) 0:Nat32 else size[SIZE];

    loop {
      let (key, hash, links) = entry;

      if (hash == NULL_HASH) {
        return tabulateArray<T>(nat(arraySize), func(i) = if (partial) return unwrap(array[i]) else {
          entry := entry.2[DEQ_NEXT];

          return unwrap(mapEntry(key));
        });
      } else switch (mapEntry(key)) {
        case (null) entry := links[DEQ_NEXT];

        case (item) {
          array[nat(arraySize)] := item;
          entry := links[DEQ_NEXT];
          arraySize +%= 1;
        };
      };
    };
  };

  public func toArray<K>(map: Set<K>): [K] {
    return toArrayHelper<K, K>(map, func(key) = ?key, false, DEQ_NEXT);
  };

  public func toArrayDesc<K>(map: Set<K>): [K] {
    return toArrayHelper<K, K>(map, func(key) = ?key, false, DEQ_PREV);
  };

  public func toArrayMap<K, T>(map: Set<K>, mapEntry: (K) -> ?T): [T] {
    return toArrayHelper(map, mapEntry, true, DEQ_NEXT);
  };

  public func toArrayMapDesc<K, T>(map: Set<K>, mapEntry: (K) -> ?T): [T] {
    return toArrayHelper(map, mapEntry, true, DEQ_PREV);
  };
};
