import Prim "mo:prim";
import Utils "../utils";

module {
  public type Entry<K> = (
    key: K,
    hash: Nat32,
    links: [var Entry<K>],
  );

  public type Set<K> = (
    root: Entry<K>,
    size: [var Nat32],
  );

  public type Iter<T> = {
    prev: () -> ?T;
    next: () -> ?T;
    peekPrev: () -> ?T;
    peekNext: () -> ?T;
    current: () -> ?T;
    started: () -> Bool;
    finished: () -> Bool;
    reset: () -> Iter<T>;
    movePrev: () -> Iter<T>;
    moveNext: () -> Iter<T>;
  };

  public type IterNext<T> = {
    next: () -> ?T;
  };

  public type HashUtils<K> = Utils.HashUtils<K>;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public let { hashInt; hashInt8; hashInt16; hashInt32; hashInt64 } = Utils;

  public let { hashNat; hashNat8; hashNat16; hashNat32; hashNat64 } = Utils;

  public let { hashText; hashPrincipal; hashBlob; hashBool } = Utils;

  public let { ihash; i8hash; i16hash; i32hash; i64hash } = Utils;

  public let { nhash; n8hash; n16hash; n32hash; n64hash } = Utils;

  public let { thash; phash; bhash; lhash } = Utils;

  public let { useHash; calcHash } = Utils;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  let SIZE = 0;

  let BRANCH_1 = 0;
  let BRANCH_2 = 1;
  let BRANCH_3 = 2;
  let BRANCH_4 = 3;
  let DEQ_PREV = 4;
  let DEQ_NEXT = 5;

  let TEMP_LINK = 4;
  let CLONE_NEXT = 5;
  let NULL_BRANCH = 6;

  let PUT = 1:Nat32;
  let MOVE = 16:Nat32;
  let PUT_MOVE = 17:Nat32;

  let REMOVE = 1:Nat32;
  let POP = 2:Nat32;
  let CYCLE = 4:Nat32;

  let HASH_OFFSET = 2:Nat32;
  let HASH_CHUNK_SIZE = 4:Nat32;

  let ROOT = 0xffffffff:Nat32;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  let { Array_tabulate = tabulateArray; Array_init = initArray; nat32ToNat = nat; trap } = Prim;

  func isSome<T>(value: ?T): Bool = switch (value) { case (null) false; case (_) true };

  func negate<T>(fn: (T) -> Bool): (T) -> Bool = func(value) = not fn(value);

  func reject<T>(fn: (T) -> ()): (T) -> Bool = func(value) { fn(value); return false };

  func unwrap<T>(value: ?T): T = switch (value) { case (?value) value; case (_) trap("unreachable") };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  let nullHash = (func(_) = ROOT, func(_) = false, func() = trap("unreachable")):Utils.NullHashUtils;

  func rootKey<K>(map: Set<K>): K = map.0.0;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func createRoot<K>(nullKey: K): Entry<K> {
    let temp = (nullKey, ROOT, [var]):Entry<K>;
    let root = (nullKey, ROOT, [var temp, temp, temp, temp, temp, temp]);

    for (index in root.2.keys()) root.2[index] := root;

    return root;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func cloneEntry<K>(entry: Entry<K>, newRoot: Entry<K>): Entry<K> {
    let (key, hash, links) = entry;

    let newBranch1 = if (links[BRANCH_1].1 != ROOT) cloneEntry(links[BRANCH_1], newRoot) else newRoot;
    let newBranch2 = if (links[BRANCH_2].1 != ROOT) cloneEntry(links[BRANCH_2], newRoot) else newRoot;
    let newBranch3 = if (links[BRANCH_3].1 != ROOT) cloneEntry(links[BRANCH_3], newRoot) else newRoot;
    let newBranch4 = if (links[BRANCH_4].1 != ROOT) cloneEntry(links[BRANCH_4], newRoot) else newRoot;

    let newEntry = (key, hash, [var newBranch1, newBranch2, newBranch3, newBranch4, newRoot, newRoot]);

    links[TEMP_LINK] := newEntry;

    return newEntry;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func getSibling<K>(entryParam: Entry<K>, DEQ_NEXT: Nat): Entry<K> {
    var entry = entryParam.2[DEQ_NEXT];

    loop {
      let (key, hash, links) = entry;

      if (links[BRANCH_1].1 != hash or links[BRANCH_2].1 != hash or hash == ROOT) {
        return entry;
      } else {
        entry := links[DEQ_NEXT];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func new<K>((getHash, areEqual, getNullKey): HashUtils<K>): Set<K> {
    return (createRoot(getNullKey()), [var 0]);
  };

  public func clear<K>(map: Set<K>) {
    let (root, size) = map;

    for (index in root.2.keys()) root.2[index] := root;

    size[SIZE] := 0;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func size<K>(map: Set<K>): Nat {
    return nat(map.1[SIZE]);
  };

  public func empty<K>(map: Set<K>): Bool {
    return map.1[SIZE] == 0;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func peek<K>(map: Set<K>): ?K {
    let (root, size) = map;
    let (key, hash, links) = root.2[DEQ_PREV];

    return if (hash == ROOT) null else ?key;
  };

  public func peekFront<K>(map: Set<K>): ?K {
    let (root, size) = map;
    let (key, hash, links) = root.2[DEQ_NEXT];

    return if (hash == ROOT) null else ?key;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func getHelper<K>(
    map: Set<K>,
    (getHash, areEqual, getNullKey): HashUtils<K>,
    keyParam: K,
  ): Bool {
    let (root, size) = map;
    let hashParam = getHash(keyParam);
    var shiftingHash = hashParam;
    var entry = root.2[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      let (key, hash, links) = entry;

      if (hash == ROOT) {
        return false;
      } else if (hash == hashParam and areEqual(key, keyParam)) {
        return true;
      } else {
        shiftingHash >>= HASH_OFFSET;
        entry := links[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func has<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    return getHelper(map, hashUtils, keyParam);
  };

  public func contains<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): ?Bool {
    return if (empty(map)) null else ?getHelper(map, hashUtils, keyParam);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func putHelper<K>(
    map: Set<K>,
    (getHash, areEqual, getNullKey): HashUtils<K>,
    keyParam: K,
    placeParam: ?K,
    method: Nat32,
    DEQ_PREV: Nat,
    DEQ_NEXT: Nat,
  ): Bool {
    let (root, size) = map;
    let hashParam = getHash(keyParam);
    var shiftingHash = hashParam;
    var parent = root;
    var entry = root.2[nat(shiftingHash % HASH_CHUNK_SIZE)];

    let placeKeyParam = switch (placeParam) { case (?placeKeyParam) placeKeyParam; case (_) root.0 };

    let placeHashParam = switch (placeParam) { case (null) ROOT; case (_) getHash(placeKeyParam) };

    var shiftingPlaceHash = placeHashParam;

    var place = if (shiftingPlaceHash != ROOT) root.2[nat(shiftingPlaceHash % HASH_CHUNK_SIZE)] else root;

    loop {
      let (placeKey, placeHash, placeLinks) = place;

      if (placeHash == ROOT or placeHash == placeHashParam and areEqual(placeKey, placeKeyParam)) loop {
        let (key, hash, links) = entry;

        if (hash == ROOT) {
          let deqPrev = placeLinks[DEQ_PREV];
          let newEntry = (keyParam, hashParam, [var root, root, root, root, place, place]);

          newEntry.2[DEQ_PREV] := deqPrev;
          deqPrev.2[DEQ_NEXT] := newEntry;
          placeLinks[DEQ_PREV] := newEntry;
          parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
          size[SIZE] +%= 1;

          return false;
        } else if (hash == hashParam and areEqual(key, keyParam)) {
          if (method & MOVE > 0) {
            let deqPrev = placeLinks[DEQ_PREV];
            let newEntry = (key, hash, [var links[BRANCH_1], links[BRANCH_2], links[BRANCH_3], links[BRANCH_4], place, place]);

            newEntry.2[DEQ_PREV] := deqPrev;
            deqPrev.2[DEQ_NEXT] := newEntry;
            placeLinks[DEQ_PREV] := newEntry;
            parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

            let deqPrevOld = links[DEQ_PREV];
            let deqNextOld = links[DEQ_NEXT];

            deqNextOld.2[DEQ_PREV] := deqPrevOld;
            deqPrevOld.2[DEQ_NEXT] := deqNextOld;
            links[BRANCH_1] := entry;
            links[BRANCH_2] := entry;
          };

          return true;
        } else {
          parent := entry;
          shiftingHash >>= HASH_OFFSET;
          entry := links[nat(shiftingHash % HASH_CHUNK_SIZE)];
        };
      } else {
        shiftingPlaceHash >>= HASH_OFFSET;
        place := placeLinks[nat(shiftingPlaceHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func put<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    return putHelper(map, hashUtils, keyParam, null, PUT, DEQ_PREV, DEQ_NEXT);
  };

  public func putFront<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    return putHelper(map, hashUtils, keyParam, null, PUT, DEQ_NEXT, DEQ_PREV);
  };

  public func add<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K) {
    ignore putHelper(map, hashUtils, keyParam, null, PUT, DEQ_PREV, DEQ_NEXT);
  };

  public func addFront<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K) {
    ignore putHelper(map, hashUtils, keyParam, null, PUT, DEQ_NEXT, DEQ_PREV);
  };

  public func putMove<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    return putHelper(map, hashUtils, keyParam, null, PUT_MOVE, DEQ_PREV, DEQ_NEXT);
  };

  public func putMoveFront<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    return putHelper(map, hashUtils, keyParam, null, PUT_MOVE, DEQ_NEXT, DEQ_PREV);
  };

  public func putBefore<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K, placeParam: ?K): Bool {
    return putHelper(map, hashUtils, keyParam, placeParam, PUT_MOVE, DEQ_PREV, DEQ_NEXT);
  };

  public func putAfter<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K, placeParam: ?K): Bool {
    return putHelper(map, hashUtils, keyParam, placeParam, PUT_MOVE, DEQ_NEXT, DEQ_PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func removeHelper<K>(
    map: Set<K>,
    (getHash, areEqual, getNullKey): HashUtils<K>,
    keyParam: K,
    method: Nat32,
    DEQ_PREV: Nat,
    DEQ_NEXT: Nat,
  ): ?K {
    let (root, size) = map;

    let hashParam = if (method == REMOVE) getHash(keyParam) else root.2[DEQ_PREV].1;

    if (hashParam == ROOT) return null;

    var shiftingHash = hashParam;
    var parent = root;
    var entry = root.2[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      let (key, hash, links) = entry;

      if (hash == hashParam and (if (method == REMOVE) areEqual(key, keyParam) else links[DEQ_NEXT].1 == ROOT)) {
        if (method == CYCLE) {
          let deqFirst = root.2[DEQ_NEXT];
          let newEntry = (key, hash, [var links[BRANCH_1], links[BRANCH_2], links[BRANCH_3], links[BRANCH_4], root, root]);

          newEntry.2[DEQ_NEXT] := deqFirst;
          deqFirst.2[DEQ_PREV] := newEntry;
          root.2[DEQ_NEXT] := newEntry;
          parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

          let deqPrev = links[DEQ_PREV];

          deqPrev.2[DEQ_NEXT] := root;
          root.2[DEQ_PREV] := deqPrev;
          links[BRANCH_1] := entry;
          links[BRANCH_2] := entry;

          return ?key;
        } else {
          let deqPrev = links[DEQ_PREV];
          let deqNext = links[DEQ_NEXT];

          deqNext.2[DEQ_PREV] := deqPrev;
          deqPrev.2[DEQ_NEXT] := deqNext;
          size[SIZE] -%= 1;

          var leaf = entry;
          var leafParent = parent;
          var leafIndex = NULL_BRANCH;

          loop {
            let (leafKey, leafHash, leafLinks) = leaf;

            if (leafLinks[BRANCH_1].1 != ROOT) {
              leafParent := leaf;
              leaf := leafLinks[BRANCH_1];
              leafIndex := BRANCH_1;
            } else if (leafLinks[BRANCH_2].1 != ROOT) {
              leafParent := leaf;
              leaf := leafLinks[BRANCH_2];
              leafIndex := BRANCH_2;
            } else if (leafLinks[BRANCH_3].1 != ROOT) {
              leafParent := leaf;
              leaf := leafLinks[BRANCH_3];
              leafIndex := BRANCH_3;
            } else if (leafLinks[BRANCH_4].1 != ROOT) {
              leafParent := leaf;
              leaf := leafLinks[BRANCH_4];
              leafIndex := BRANCH_4;
            } else {
              if (leafIndex == NULL_BRANCH) {
                parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := root;
              } else {
                parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := leaf;
                leafParent.2[leafIndex] := root;

                for (index in leafLinks.keys()) if (index <= BRANCH_4) leafLinks[index] := links[index];
              };

              links[BRANCH_1] := entry;
              links[BRANCH_2] := entry;

              return ?key;
            };
          };
        };
      } else if (hash == ROOT) {
        return null;
      } else {
        parent := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := links[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func remove<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    return isSome(removeHelper(map, hashUtils, keyParam, REMOVE, DEQ_PREV, DEQ_NEXT));
  };

  public func delete<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K) {
    ignore removeHelper(map, hashUtils, keyParam, REMOVE, DEQ_PREV, DEQ_NEXT);
  };

  public func pop<K>(map: Set<K>): ?K {
    return removeHelper(map, nullHash, rootKey(map), POP, DEQ_PREV, DEQ_NEXT);
  };

  public func popFront<K>(map: Set<K>): ?K {
    return removeHelper(map, nullHash, rootKey(map), POP, DEQ_NEXT, DEQ_PREV);
  };

  public func cycle<K>(map: Set<K>): ?K {
    return removeHelper(map, nullHash, rootKey(map), CYCLE, DEQ_NEXT, DEQ_PREV);
  };

  public func cycleFront<K>(map: Set<K>): ?K {
    return removeHelper(map, nullHash, rootKey(map), CYCLE, DEQ_NEXT, DEQ_PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func cloneHelper<K>(
    map: Set<K>,
    DEQ_PREV: Nat,
    DEQ_NEXT: Nat,
  ): Set<K> {
    let (root, size) = map;
    let newRoot = createRoot<K>(rootKey(map));
    var newEntry = newRoot;
    var entry = root;

    root.2[TEMP_LINK] := newRoot;

    for (index in newRoot.2.keys()) if (index <= BRANCH_4) {
      let branch = root.2[index];

      newRoot.2[index] := if (branch.1 != ROOT) cloneEntry(branch, newRoot) else newRoot;
    };

    loop {
      let cloneNext = entry.2[CLONE_NEXT];
      let newDeqNext = cloneNext.2[TEMP_LINK];

      newDeqNext.2[DEQ_PREV] := newEntry;
      newEntry.2[DEQ_NEXT] := newDeqNext;
      cloneNext.2[TEMP_LINK] := entry;

      if (newDeqNext.1 == ROOT) return (newRoot, [var size[SIZE]]);

      newEntry := newDeqNext;
      entry := cloneNext;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func clone<K>(map: Set<K>): Set<K> {
    return cloneHelper(map, DEQ_PREV, DEQ_NEXT);
  };

  public func cloneDesc<K>(map: Set<K>): Set<K> {
    return cloneHelper(map, DEQ_NEXT, DEQ_PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func mapFilterHelper<K>(
    map: Set<K>,
    hashUtils: HashUtils<K>,
    acceptEntry: (K) -> Bool,
    DEQ_PREV: Nat,
    DEQ_NEXT: Nat,
  ): Set<K> {
    let (root, size) = map;
    let newMap = new<K>(hashUtils);
    var entry = root;

    loop {
      entry := getSibling(entry, DEQ_NEXT);

      let (key, hash, links) = entry;

      if (hash == ROOT) return newMap;

      if (acceptEntry(key)) ignore putHelper(newMap, hashUtils, key, null, PUT_MOVE, DEQ_PREV, DEQ_NEXT);
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func filter<K>(map: Set<K>, hashUtils: HashUtils<K>, acceptEntry: (K) -> Bool): Set<K> {
    return mapFilterHelper(map, hashUtils, acceptEntry, DEQ_PREV, DEQ_NEXT);
  };

  public func filterDesc<K>(map: Set<K>, hashUtils: HashUtils<K>, acceptEntry: (K) -> Bool): Set<K> {
    return mapFilterHelper(map, hashUtils, acceptEntry, DEQ_NEXT, DEQ_PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func iterateHelper<K>(
    map: Set<K>,
    (getHash, areEqual, getNullKey): HashUtils<K>,
    placeParam: ?K,
    DEQ_PREV: Nat,
    DEQ_NEXT: Nat,
  ): Iter<K> {
    let (root, size) = map;

    let keyParam = switch (placeParam) { case (?keyParam) keyParam; case (_) root.0 };

    let hashParam = switch (placeParam) { case (null) ROOT; case (_) getHash(keyParam) };

    var shiftingHash = hashParam;

    var entry = if (shiftingHash != ROOT) root.2[nat(shiftingHash % HASH_CHUNK_SIZE)] else root;

    loop {
      let (key, hash, links) = entry;

      if (hash == ROOT or hash == hashParam and areEqual(key, keyParam)) {
        var started = hash != ROOT;

        return let iter = {
          prev = func(): ?K {
            started := true;
            entry := getSibling(entry, DEQ_PREV);

            let (key, hash, links) = entry;

            return if (hash == ROOT) null else ?key;
          };

          next = func(): ?K {
            started := true;
            entry := getSibling(entry, DEQ_NEXT);

            let (key, hash, links) = entry;

            return if (hash == ROOT) null else ?key;
          };

          peekPrev = func(): ?K {
            let deqPrev = getSibling(entry, DEQ_PREV);

            entry.2[DEQ_PREV] := deqPrev;

            let (deqPrevKey, deqPrevHash, deqPrevLinks) = deqPrev;

            return if (deqPrevHash == ROOT) null else ?deqPrevKey;
          };

          peekNext = func(): ?K {
            let deqNext = getSibling(entry, DEQ_NEXT);

            entry.2[DEQ_NEXT] := deqNext;

            let (deqNextKey, deqNextHash, deqNextLinks) = deqNext;

            return if (deqNextHash == ROOT) null else ?deqNextKey;
          };

          current = func(): ?K {
            let (key, hash, links) = entry;

            return if (hash == ROOT) null else ?key;
          };

          started = func(): Bool {
            return started;
          };

          finished = func(): Bool {
            let (key, hash, links) = entry;

            return started and hash == ROOT;
          };

          reset = func(): Iter<K> {
            started := false;
            entry := root;

            return iter;
          };

          movePrev = func(): Iter<K> {
            started := true;
            entry := getSibling(entry, DEQ_PREV);

            return iter;
          };

          moveNext = func(): Iter<K> {
            started := true;
            entry := getSibling(entry, DEQ_NEXT);

            return iter;
          };
        };
      } else {
        shiftingHash >>= HASH_OFFSET;
        entry := links[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func keys<K>(map: Set<K>): Iter<K> {
    return iterateHelper(map, nullHash, null, DEQ_PREV, DEQ_NEXT);
  };

  public func keysDesc<K>(map: Set<K>): Iter<K> {
    return iterateHelper(map, nullHash, null, DEQ_NEXT, DEQ_PREV);
  };

  public func keysFrom<K>(map: Set<K>, hashUtils: HashUtils<K>, placeParam: ?K): Iter<K> {
    return iterateHelper(map, hashUtils, placeParam, DEQ_PREV, DEQ_NEXT);
  };

  public func keysFromDesc<K>(map: Set<K>, hashUtils: HashUtils<K>, placeParam: ?K): Iter<K> {
    return iterateHelper(map, hashUtils, placeParam, DEQ_NEXT, DEQ_PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func findHelper<K>(
    map: Set<K>,
    acceptEntry: (K) -> Bool,
    DEQ_NEXT: Nat,
  ): ?K {
    let (root, size) = map;
    var entry = root;

    loop {
      entry := getSibling(entry, DEQ_NEXT);

      let (key, hash, links) = entry;

      if (hash == ROOT) return null else if (acceptEntry(key)) return ?key;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func find<K>(map: Set<K>, acceptEntry: (K) -> Bool): ?K {
    return findHelper(map, acceptEntry, DEQ_NEXT);
  };

  public func findDesc<K>(map: Set<K>, acceptEntry: (K) -> Bool): ?K {
    return findHelper(map, acceptEntry, DEQ_PREV);
  };

  public func some<K>(map: Set<K>, acceptEntry: (K) -> Bool): Bool {
    return isSome(findHelper(map, acceptEntry, DEQ_NEXT));
  };

  public func someDesc<K>(map: Set<K>, acceptEntry: (K) -> Bool): Bool {
    return isSome(findHelper(map, acceptEntry, DEQ_PREV));
  };

  public func every<K>(map: Set<K>, acceptEntry: (K) -> Bool): Bool {
    return not isSome(findHelper(map, negate(acceptEntry), DEQ_NEXT));
  };

  public func everyDesc<K>(map: Set<K>, acceptEntry: (K) -> Bool): Bool {
    return not isSome(findHelper(map, negate(acceptEntry), DEQ_PREV));
  };

  public func forEach<K>(map: Set<K>, mapEntry: (K) -> ()) {
    ignore findHelper(map, reject(mapEntry), DEQ_NEXT);
  };

  public func forEachDesc<K>(map: Set<K>, mapEntry: (K) -> ()) {
    ignore findHelper(map, reject(mapEntry), DEQ_PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func fromIterHelper<K, T>(
    iter: IterNext<T>,
    hashUtils: HashUtils<K>,
    mapItem: (T) -> ?K,
    DEQ_PREV: Nat,
    DEQ_NEXT: Nat,
  ): Set<K> {
    let map = new(hashUtils);

    for (item in iter) switch (mapItem(item)) {
      case (?key) ignore putHelper(map, hashUtils, key, null, PUT_MOVE, DEQ_PREV, DEQ_NEXT);
      case (_) {};
    };

    return map;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func fromIter<K>(iter: IterNext<K>, hashUtils: HashUtils<K>): Set<K> {
    return fromIterHelper<K, K>(iter, hashUtils, func(item) = ?item, DEQ_PREV, DEQ_NEXT);
  };

  public func fromIterDesc<K>(iter: IterNext<K>, hashUtils: HashUtils<K>): Set<K> {
    return fromIterHelper<K, K>(iter, hashUtils, func(item) = ?item, DEQ_NEXT, DEQ_PREV);
  };

  public func fromIterMap<K, T>(iter: IterNext<T>, hashUtils: HashUtils<K>, mapItem: (T) -> ?K): Set<K> {
    return fromIterHelper(iter, hashUtils, mapItem, DEQ_PREV, DEQ_NEXT);
  };

  public func fromIterMapDesc<K, T>(iter: IterNext<T>, hashUtils: HashUtils<K>, mapItem: (T) -> ?K): Set<K> {
    return fromIterHelper(iter, hashUtils, mapItem, DEQ_NEXT, DEQ_PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func toArrayHelper<K>(
    map: Set<K>,
    DEQ_NEXT: Nat,
  ): [K] {
    let (root, size) = map;
    var entry = root;

    return tabulateArray<K>(nat(size[SIZE]), func(i) {
      entry := entry.2[DEQ_NEXT];

      let (key, hash, links) = entry;

      return key;
    });
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func toArray<K>(map: Set<K>): [K] {
    return toArrayHelper(map, DEQ_NEXT);
  };

  public func toArrayDesc<K>(map: Set<K>): [K] {
    return toArrayHelper(map, DEQ_PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func toArrayMapHelper<K, T>(
    map: Set<K>,
    mapEntry: (K) -> ?T,
    DEQ_NEXT: Nat,
  ): [T] {
    let (root, size) = map;
    var entry = root;
    var maxSize = size[SIZE];
    var array = initArray<?T>(nat(maxSize), null);
    var arraySize = 0:Nat32;

    loop {
      entry := getSibling(entry, DEQ_NEXT);

      let (key, hash, links) = entry;

      if (hash == ROOT) return tabulateArray<T>(nat(arraySize), func(i) = unwrap(array[i]));

      switch (mapEntry(key)) {
        case (null) {};

        case (item) {
          if (arraySize == maxSize) {
            let prevArray = array;

            maxSize *%= 2;
            array := initArray<?T>(nat(maxSize), null);
            arraySize := 0;

            for (item in prevArray.vals()) {
              array[nat(arraySize)] := item;
              arraySize +%= 1;
            };
          };

          array[nat(arraySize)] := item;
          arraySize +%= 1;
        };
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func toArrayMap<K, T>(map: Set<K>, mapEntry: (K) -> ?T): [T] {
    return toArrayMapHelper(map, mapEntry, DEQ_NEXT);
  };

  public func toArrayMapDesc<K, T>(map: Set<K>, mapEntry: (K) -> ?T): [T] {
    return toArrayMapHelper(map, mapEntry, DEQ_PREV);
  };
};
