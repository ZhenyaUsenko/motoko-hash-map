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
    root: Entry<K, V>,
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
  let ADD = 2:Nat32;
  let UPDATE = 4:Nat32;
  let REPLACE = 8:Nat32;
  let MOVE = 16:Nat32;
  let PUT_MOVE = 17:Nat32;
  let ADD_MOVE = 18:Nat32;
  let UPDATE_MOVE = 20:Nat32;
  let REPLACE_MOVE = 24:Nat32;

  let REMOVE = 1:Nat32;
  let POP = 2:Nat32;
  let CYCLE = 4:Nat32;

  let HASH_OFFSET = 2:Nat32;
  let HASH_CHUNK_SIZE = 4:Nat32;

  let ROOT = 0xffffffff:Nat32;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  let { Array_tabulate = tabulateArray; Array_init = initArray; nat32ToNat = nat; trap } = Prim;

  func constant<T>(value: T): (Any, Any) -> T = func(_) = value;

  func isSome<T>(value: ?T): Bool = switch (value) { case (null) false; case (_) true };

  func negate<A, B>(fn: (A, B) -> Bool): (A, B) -> Bool = func(a, b) = not fn(a, b);

  func reject<A, B>(fn: (A, B) -> ()): (A, B) -> Bool = func(a, b) { fn(a, b); return false };

  func makeOption<A, B, R>(fn: (A, B) -> R): (A, B) -> ?R = func(a, b) = ?fn(a, b);

  func boolToOption<A, B>(fn: (A, B) -> Bool): (A, B) -> ?B = func(a, b) = if (fn(a, b)) ?b else null;

  func unwrap<T>(value: ?T): T = switch (value) { case (?value) value; case (_) trap("unreachable") };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  let nullHash = (func(_) = ROOT, func(_) = false, func() = trap("unreachable")):Utils.NullHashUtils;

  func rootKey<K, V>(map: Map<K, V>): K = map.0.0;

  func entryValue<K, V>(entry: ?(K, V)): ?V = switch (entry) { case (?(key, value)) ?value; case (_) null };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func createRoot<K, V>(nullKey: K): Entry<K, V> {
    let temp = (nullKey, null, ROOT, [var]):Entry<K, V>;
    let root = (nullKey, null, ROOT, [var temp, temp, temp, temp, temp, temp]);

    for (index in root.3.keys()) root.3[index] := root;

    return root;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func cloneEntry<K, V>(entry: Entry<K, V>, newRoot: Entry<K, V>): Entry<K, V> {
    let (key, value, hash, links) = entry;

    let newBranch1 = if (links[BRANCH_1].2 != ROOT) cloneEntry(links[BRANCH_1], newRoot) else newRoot;
    let newBranch2 = if (links[BRANCH_2].2 != ROOT) cloneEntry(links[BRANCH_2], newRoot) else newRoot;
    let newBranch3 = if (links[BRANCH_3].2 != ROOT) cloneEntry(links[BRANCH_3], newRoot) else newRoot;
    let newBranch4 = if (links[BRANCH_4].2 != ROOT) cloneEntry(links[BRANCH_4], newRoot) else newRoot;

    let newEntry = (key, value, hash, [var newBranch1, newBranch2, newBranch3, newBranch4, newRoot, newRoot]);

    links[TEMP_LINK] := newEntry;

    return newEntry;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func getSibling<K, V>(entryParam: Entry<K, V>, DEQ_NEXT: Nat): Entry<K, V> {
    var entry = entryParam.3[DEQ_NEXT];

    loop {
      let (key, value, hash, links) = entry;

      if (links[BRANCH_1].2 != hash or links[BRANCH_2].2 != hash or hash == ROOT) {
        return entry;
      } else {
        entry := links[DEQ_NEXT];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func new<K, V>((getHash, areEqual, getNullKey): HashUtils<K>): Map<K, V> {
    return (createRoot(getNullKey()), [var 0]);
  };

  public func clear<K, V>(map: Map<K, V>) {
    let (root, size) = map;

    for (index in root.3.keys()) root.3[index] := root;

    size[SIZE] := 0;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func size<K, V>(map: Map<K, V>): Nat {
    return nat(map.1[SIZE]);
  };

  public func empty<K, V>(map: Map<K, V>): Bool {
    return map.1[SIZE] == 0;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func peek<K, V>(map: Map<K, V>): ?(K, V) {
    let (root, size) = map;
    let (key, value, hash, links) = root.3[DEQ_PREV];

    return if (hash == ROOT) null else ?(key, unwrap(value));
  };

  public func peekFront<K, V>(map: Map<K, V>): ?(K, V) {
    let (root, size) = map;
    let (key, value, hash, links) = root.3[DEQ_NEXT];

    return if (hash == ROOT) null else ?(key, unwrap(value));
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func getHelper<K, V>(
    map: Map<K, V>,
    (getHash, areEqual, getNullKey): HashUtils<K>,
    keyParam: K,
  ): ?V {
    let (root, size) = map;
    let hashParam = getHash(keyParam);
    var shiftingHash = hashParam;
    var entry = root.3[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      let (key, value, hash, links) = entry;

      if (hash == ROOT) {
        return null;
      } else if (hash == hashParam and areEqual(key, keyParam)) {
        return value;
      } else {
        shiftingHash >>= HASH_OFFSET;
        entry := links[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func get<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): ?V {
    return getHelper(map, hashUtils, keyParam);
  };

  public func has<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    return isSome(getHelper(map, hashUtils, keyParam));
  };

  public func contains<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): ?Bool {
    return if (empty(map)) null else ?isSome(getHelper(map, hashUtils, keyParam));
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func putHelper<K, V>(
    map: Map<K, V>,
    (getHash, areEqual, getNullKey): HashUtils<K>,
    keyParam: K,
    placeParam: ?K,
    getNewValue: (K, ?V) -> ?V,
    method: Nat32,
    DEQ_PREV: Nat,
    DEQ_NEXT: Nat,
  ): ?V {
    let (root, size) = map;
    let hashParam = getHash(keyParam);
    var shiftingHash = hashParam;
    var parent = root;
    var entry = root.3[nat(shiftingHash % HASH_CHUNK_SIZE)];

    let placeKeyParam = switch (placeParam) { case (?placeKeyParam) placeKeyParam; case (_) root.0 };

    let placeHashParam = switch (placeParam) { case (null) ROOT; case (_) getHash(placeKeyParam) };

    var shiftingPlaceHash = placeHashParam;

    var place = if (shiftingPlaceHash != ROOT) root.3[nat(shiftingPlaceHash % HASH_CHUNK_SIZE)] else root;

    loop {
      let (placeKey, placeValue, placeHash, placeLinks) = place;

      if (placeHash == ROOT or placeHash == placeHashParam and areEqual(placeKey, placeKeyParam)) loop {
        let (key, value, hash, links) = entry;

        if (hash == ROOT) {
          if (method & REPLACE > 0) return null;

          switch (getNewValue(keyParam, null)) {
            case (null) return null;

            case (newValue) {
              let deqPrev = placeLinks[DEQ_PREV];
              let newEntry = (keyParam, newValue, hashParam, [var root, root, root, root, place, place]);

              newEntry.3[DEQ_PREV] := deqPrev;
              deqPrev.3[DEQ_NEXT] := newEntry;
              placeLinks[DEQ_PREV] := newEntry;
              parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
              size[SIZE] +%= 1;

              return if (method & UPDATE > 0) newValue else null;
            };
          };
        } else if (hash == hashParam and areEqual(key, keyParam)) {
          if (method & ADD > 0) return value;

          if (method & MOVE > 0) {
            let deqPrev = placeLinks[DEQ_PREV];

            let newValue = switch (getNewValue(keyParam, value)) { case (null) value; case (newValue) newValue };

            let newEntry = (key, newValue, hash, [var links[BRANCH_1], links[BRANCH_2], links[BRANCH_3], links[BRANCH_4], place, place]);

            newEntry.3[DEQ_PREV] := deqPrev;
            deqPrev.3[DEQ_NEXT] := newEntry;
            placeLinks[DEQ_PREV] := newEntry;
            parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

            let deqPrevOld = links[DEQ_PREV];
            let deqNextOld = links[DEQ_NEXT];

            deqNextOld.3[DEQ_PREV] := deqPrevOld;
            deqPrevOld.3[DEQ_NEXT] := deqNextOld;
            links[BRANCH_1] := entry;
            links[BRANCH_2] := entry;

            return if (method & UPDATE > 0) newValue else value;
          } else switch (getNewValue(keyParam, value)) {
            case (null) return value;

            case (newValue) {
              let newEntry = (key, newValue, hash, links);

              links[DEQ_PREV].3[DEQ_NEXT] := newEntry;
              links[DEQ_NEXT].3[DEQ_PREV] := newEntry;
              parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

              return if (method & UPDATE > 0) newValue else value;
            };
          };
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

  public func put<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V): ?V {
    return putHelper(map, hashUtils, keyParam, null, constant(?valueParam), PUT, DEQ_PREV, DEQ_NEXT);
  };

  public func putFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V): ?V {
    return putHelper(map, hashUtils, keyParam, null, constant(?valueParam), PUT, DEQ_NEXT, DEQ_PREV);
  };

  public func set<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V) {
    ignore putHelper(map, hashUtils, keyParam, null, constant(?valueParam), PUT, DEQ_PREV, DEQ_NEXT);
  };

  public func setFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V) {
    ignore putHelper(map, hashUtils, keyParam, null, constant(?valueParam), PUT, DEQ_NEXT, DEQ_PREV);
  };

  public func add<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V): ?V {
    return putHelper(map, hashUtils, keyParam, null, constant(?valueParam), ADD, DEQ_PREV, DEQ_NEXT);
  };

  public func addFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V): ?V {
    return putHelper(map, hashUtils, keyParam, null, constant(?valueParam), ADD, DEQ_NEXT, DEQ_PREV);
  };

  public func replace<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V): ?V {
    return putHelper(map, hashUtils, keyParam, null, constant(?valueParam), REPLACE, DEQ_PREV, DEQ_NEXT);
  };

  public func update<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, getNewValue: (K, ?V) -> ?V): ?V {
    return putHelper(map, hashUtils, keyParam, null, getNewValue, UPDATE, DEQ_PREV, DEQ_NEXT);
  };

  public func updateFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, getNewValue: (K, ?V) -> ?V): ?V {
    return putHelper(map, hashUtils, keyParam, null, getNewValue, UPDATE, DEQ_NEXT, DEQ_PREV);
  };

  public func putMove<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: ?V): ?V {
    return putHelper(map, hashUtils, keyParam, null, constant(valueParam), PUT_MOVE, DEQ_PREV, DEQ_NEXT);
  };

  public func putMoveFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: ?V): ?V {
    return putHelper(map, hashUtils, keyParam, null, constant(valueParam), PUT_MOVE, DEQ_NEXT, DEQ_PREV);
  };

  public func replaceMove<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: ?V): ?V {
    return putHelper(map, hashUtils, keyParam, null, constant(valueParam), REPLACE_MOVE, DEQ_PREV, DEQ_NEXT);
  };

  public func replaceMoveFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: ?V): ?V {
    return putHelper(map, hashUtils, keyParam, null, constant(valueParam), REPLACE_MOVE, DEQ_NEXT, DEQ_PREV);
  };

  public func updateMove<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, getNewValue: (K, ?V) -> ?V): ?V {
    return putHelper(map, hashUtils, keyParam, null, getNewValue, UPDATE_MOVE, DEQ_PREV, DEQ_NEXT);
  };

  public func updateMoveFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, getNewValue: (K, ?V) -> ?V): ?V {
    return putHelper(map, hashUtils, keyParam, null, getNewValue, UPDATE_MOVE, DEQ_NEXT, DEQ_PREV);
  };

  public func putBefore<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, placeParam: ?K, valueParam: ?V): ?V {
    return putHelper(map, hashUtils, keyParam, placeParam, constant(valueParam), PUT_MOVE, DEQ_PREV, DEQ_NEXT);
  };

  public func putAfter<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, placeParam: ?K, valueParam: ?V): ?V {
    return putHelper(map, hashUtils, keyParam, placeParam, constant(valueParam), PUT_MOVE, DEQ_NEXT, DEQ_PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func removeHelper<K, V>(
    map: Map<K, V>,
    (getHash, areEqual, getNullKey): HashUtils<K>,
    keyParam: K,
    method: Nat32,
    DEQ_PREV: Nat,
    DEQ_NEXT: Nat,
  ): ?(K, V) {
    let (root, size) = map;

    let hashParam = if (method == REMOVE) getHash(keyParam) else root.3[DEQ_PREV].2;

    if (hashParam == ROOT) return null;

    var shiftingHash = hashParam;
    var parent = root;
    var entry = root.3[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      let (key, value, hash, links) = entry;

      if (hash == hashParam and (if (method == REMOVE) areEqual(key, keyParam) else links[DEQ_NEXT].2 == ROOT)) {
        if (method == CYCLE) {
          let deqFirst = root.3[DEQ_NEXT];
          let newEntry = (key, value, hash, [var links[BRANCH_1], links[BRANCH_2], links[BRANCH_3], links[BRANCH_4], root, root]);

          newEntry.3[DEQ_NEXT] := deqFirst;
          deqFirst.3[DEQ_PREV] := newEntry;
          root.3[DEQ_NEXT] := newEntry;
          parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

          let deqPrev = links[DEQ_PREV];

          deqPrev.3[DEQ_NEXT] := root;
          root.3[DEQ_PREV] := deqPrev;
          links[BRANCH_1] := entry;
          links[BRANCH_2] := entry;

          return ?(key, unwrap(value));
        } else {
          let deqPrev = links[DEQ_PREV];
          let deqNext = links[DEQ_NEXT];

          deqNext.3[DEQ_PREV] := deqPrev;
          deqPrev.3[DEQ_NEXT] := deqNext;
          size[SIZE] -%= 1;

          var leaf = entry;
          var leafParent = parent;
          var leafIndex = NULL_BRANCH;

          loop {
            let (leafKey, leafValue, leafHash, leafLinks) = leaf;

            if (leafLinks[BRANCH_1].2 != ROOT) {
              leafParent := leaf;
              leaf := leafLinks[BRANCH_1];
              leafIndex := BRANCH_1;
            } else if (leafLinks[BRANCH_2].2 != ROOT) {
              leafParent := leaf;
              leaf := leafLinks[BRANCH_2];
              leafIndex := BRANCH_2;
            } else if (leafLinks[BRANCH_3].2 != ROOT) {
              leafParent := leaf;
              leaf := leafLinks[BRANCH_3];
              leafIndex := BRANCH_3;
            } else if (leafLinks[BRANCH_4].2 != ROOT) {
              leafParent := leaf;
              leaf := leafLinks[BRANCH_4];
              leafIndex := BRANCH_4;
            } else {
              if (leafIndex == NULL_BRANCH) {
                parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := root;
              } else {
                parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := leaf;
                leafParent.3[leafIndex] := root;

                for (index in leafLinks.keys()) if (index <= BRANCH_4) leafLinks[index] := links[index];
              };

              links[BRANCH_1] := entry;
              links[BRANCH_2] := entry;

              return ?(key, unwrap(value));
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

  public func remove<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): ?V {
    return entryValue(removeHelper(map, hashUtils, keyParam, REMOVE, DEQ_PREV, DEQ_NEXT));
  };

  public func delete<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K) {
    ignore removeHelper(map, hashUtils, keyParam, REMOVE, DEQ_PREV, DEQ_NEXT);
  };

  public func pop<K, V>(map: Map<K, V>): ?(K, V) {
    return removeHelper(map, nullHash, rootKey(map), POP, DEQ_PREV, DEQ_NEXT);
  };

  public func popFront<K, V>(map: Map<K, V>): ?(K, V) {
    return removeHelper(map, nullHash, rootKey(map), POP, DEQ_NEXT, DEQ_PREV);
  };

  public func cycle<K, V>(map: Map<K, V>): ?(K, V) {
    return removeHelper(map, nullHash, rootKey(map), CYCLE, DEQ_NEXT, DEQ_PREV);
  };

  public func cycleFront<K, V>(map: Map<K, V>): ?(K, V) {
    return removeHelper(map, nullHash, rootKey(map), CYCLE, DEQ_NEXT, DEQ_PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func cloneHelper<K, V>(
    map: Map<K, V>,
    DEQ_PREV: Nat,
    DEQ_NEXT: Nat,
  ): Map<K, V> {
    let (root, size) = map;
    let newRoot = createRoot<K, V>(rootKey(map));
    var newEntry = newRoot;
    var entry = root;

    root.3[TEMP_LINK] := newRoot;

    for (index in newRoot.3.keys()) if (index <= BRANCH_4) {
      let branch = root.3[index];

      newRoot.3[index] := if (branch.2 != ROOT) cloneEntry(branch, newRoot) else newRoot;
    };

    loop {
      let cloneNext = entry.3[CLONE_NEXT];
      let newDeqNext = cloneNext.3[TEMP_LINK];

      newDeqNext.3[DEQ_PREV] := newEntry;
      newEntry.3[DEQ_NEXT] := newDeqNext;
      cloneNext.3[TEMP_LINK] := entry;

      if (newDeqNext.2 == ROOT) return (newRoot, [var size[SIZE]]);

      newEntry := newDeqNext;
      entry := cloneNext;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func clone<K, V>(map: Map<K, V>): Map<K, V> {
    return cloneHelper(map, DEQ_PREV, DEQ_NEXT);
  };

  public func cloneDesc<K, V>(map: Map<K, V>): Map<K, V> {
    return cloneHelper(map, DEQ_NEXT, DEQ_PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func mapFilterHelper<K, V1, V2>(
    map: Map<K, V1>,
    hashUtils: HashUtils<K>,
    mapEntry: (K, V1) -> ?V2,
    DEQ_PREV: Nat,
    DEQ_NEXT: Nat,
  ): Map<K, V2> {
    let (root, size) = map;
    let newMap = new<K, V2>(hashUtils);
    var entry = root;

    loop {
      entry := getSibling(entry, DEQ_NEXT);

      let (key, value, hash, links) = entry;

      if (hash == ROOT) return newMap;

      switch (mapEntry(key, unwrap(value))) {
        case (null) {};
        case (newValue) ignore putHelper(newMap, hashUtils, key, null, constant(newValue), PUT_MOVE, DEQ_PREV, DEQ_NEXT);
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func mapFilter<K, V1, V2>(map: Map<K, V1>, hashUtils: HashUtils<K>, mapEntry: (K, V1) -> ?V2): Map<K, V2> {
    return mapFilterHelper(map, hashUtils, mapEntry, DEQ_PREV, DEQ_NEXT);
  };

  public func mapFilterDesc<K, V1, V2>(map: Map<K, V1>, hashUtils: HashUtils<K>, mapEntry: (K, V1) -> ?V2): Map<K, V2> {
    return mapFilterHelper(map, hashUtils, mapEntry, DEQ_NEXT, DEQ_PREV);
  };

  public func map<K, V1, V2>(map: Map<K, V1>, hashUtils: HashUtils<K>, mapEntry: (K, V1) -> V2): Map<K, V2> {
    return mapFilterHelper(map, hashUtils, makeOption(mapEntry), DEQ_PREV, DEQ_NEXT);
  };

  public func mapDesc<K, V1, V2>(map: Map<K, V1>, hashUtils: HashUtils<K>, mapEntry: (K, V1) -> V2): Map<K, V2> {
    return mapFilterHelper(map, hashUtils, makeOption(mapEntry), DEQ_NEXT, DEQ_PREV);
  };

  public func filter<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, acceptEntry: (K, V) -> Bool): Map<K, V> {
    return mapFilterHelper<K, V, V>(map, hashUtils, boolToOption(acceptEntry), DEQ_PREV, DEQ_NEXT);
  };

  public func filterDesc<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, acceptEntry: (K, V) -> Bool): Map<K, V> {
    return mapFilterHelper<K, V, V>(map, hashUtils, boolToOption(acceptEntry), DEQ_NEXT, DEQ_PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func iterateHelper<K, V, T>(
    map: Map<K, V>,
    (getHash, areEqual, getNullKey): HashUtils<K>,
    placeParam: ?K,
    mapEntry: (K, V) -> ?T,
    DEQ_PREV: Nat,
    DEQ_NEXT: Nat,
  ): Iter<T> {
    let (root, size) = map;

    let keyParam = switch (placeParam) { case (?keyParam) keyParam; case (_) root.0 };

    let hashParam = switch (placeParam) { case (null) ROOT; case (_) getHash(keyParam) };

    var shiftingHash = hashParam;

    var entry = if (shiftingHash != ROOT) root.3[nat(shiftingHash % HASH_CHUNK_SIZE)] else root;

    loop {
      let (key, value, hash, links) = entry;

      if (hash == ROOT or hash == hashParam and areEqual(key, keyParam)) {
        var started = hash != ROOT;

        return let iter = {
          prev = func(): ?T {
            started := true;
            entry := getSibling(entry, DEQ_PREV);

            let (key, value, hash, links) = entry;

            return if (hash == ROOT) null else mapEntry(key, unwrap(value));
          };

          next = func(): ?T {
            started := true;
            entry := getSibling(entry, DEQ_NEXT);

            let (key, value, hash, links) = entry;

            return if (hash == ROOT) null else mapEntry(key, unwrap(value));
          };

          peekPrev = func(): ?T {
            let deqPrev = getSibling(entry, DEQ_PREV);

            entry.3[DEQ_PREV] := deqPrev;

            let (deqPrevKey, deqPrevValue, deqPrevHash, deqPrevLinks) = deqPrev;

            return if (deqPrevHash == ROOT) null else mapEntry(deqPrevKey, unwrap(deqPrevValue));
          };

          peekNext = func(): ?T {
            let deqNext = getSibling(entry, DEQ_NEXT);

            entry.3[DEQ_NEXT] := deqNext;

            let (deqNextKey, deqNextValue, deqNextHash, deqNextLinks) = deqNext;

            return if (deqNextHash == ROOT) null else mapEntry(deqNextKey, unwrap(deqNextValue));
          };

          current = func(): ?T {
            let (key, value, hash, links) = entry;

            return if (hash == ROOT) null else mapEntry(key, unwrap(value));
          };

          started = func(): Bool {
            return started;
          };

          finished = func(): Bool {
            let (key, value, hash, links) = entry;

            return started and hash == ROOT;
          };

          reset = func(): Iter<T> {
            started := false;
            entry := root;

            return iter;
          };

          movePrev = func(): Iter<T> {
            started := true;
            entry := getSibling(entry, DEQ_PREV);

            return iter;
          };

          moveNext = func(): Iter<T> {
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

  public func keys<K, V>(map: Map<K, V>): Iter<K> {
    return iterateHelper<K, V, K>(map, nullHash, null, func(key, value) = ?key, DEQ_PREV, DEQ_NEXT);
  };

  public func keysDesc<K, V>(map: Map<K, V>): Iter<K> {
    return iterateHelper<K, V, K>(map, nullHash, null, func(key, value) = ?key, DEQ_NEXT, DEQ_PREV);
  };

  public func vals<K, V>(map: Map<K, V>): Iter<V> {
    return iterateHelper<K, V, V>(map, nullHash, null, func(key, value) = ?value, DEQ_PREV, DEQ_NEXT);
  };

  public func valsDesc<K, V>(map: Map<K, V>): Iter<V> {
    return iterateHelper<K, V, V>(map, nullHash, null, func(key, value) = ?value, DEQ_NEXT, DEQ_PREV);
  };

  public func entries<K, V>(map: Map<K, V>): Iter<(K, V)> {
    return iterateHelper<K, V, (K, V)>(map, nullHash, null, func(key, value) = ?(key, value), DEQ_PREV, DEQ_NEXT);
  };

  public func entriesDesc<K, V>(map: Map<K, V>): Iter<(K, V)> {
    return iterateHelper<K, V, (K, V)>(map, nullHash, null, func(key, value) = ?(key, value), DEQ_NEXT, DEQ_PREV);
  };

  public func keysFrom<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, placeParam: ?K): Iter<K> {
    return iterateHelper<K, V, K>(map, hashUtils, placeParam, func(key, value) = ?key, DEQ_PREV, DEQ_NEXT);
  };

  public func keysFromDesc<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, placeParam: ?K): Iter<K> {
    return iterateHelper<K, V, K>(map, hashUtils, placeParam, func(key, value) = ?key, DEQ_NEXT, DEQ_PREV);
  };

  public func valsFrom<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, placeParam: ?K): Iter<V> {
    return iterateHelper<K, V, V>(map, hashUtils, placeParam, func(key, value) = ?value, DEQ_PREV, DEQ_NEXT);
  };

  public func valsFromDesc<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, placeParam: ?K): Iter<V> {
    return iterateHelper<K, V, V>(map, hashUtils, placeParam, func(key, value) = ?value, DEQ_NEXT, DEQ_PREV);
  };

  public func entriesFrom<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, placeParam: ?K): Iter<(K, V)> {
    return iterateHelper<K, V, (K, V)>(map, hashUtils, placeParam, func(key, value) = ?(key, value), DEQ_PREV, DEQ_NEXT);
  };

  public func entriesFromDesc<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, placeParam: ?K): Iter<(K, V)> {
    return iterateHelper<K, V, (K, V)>(map, hashUtils, placeParam, func(key, value) = ?(key, value), DEQ_NEXT, DEQ_PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func findHelper<K, V>(
    map: Map<K, V>,
    acceptEntry: (K, V) -> Bool,
    DEQ_NEXT: Nat,
  ): ?(K, V) {
    let (root, size) = map;
    var entry = root;

    loop {
      entry := getSibling(entry, DEQ_NEXT);

      let (key, value, hash, links) = entry;

      if (hash == ROOT) return null else if (acceptEntry(key, unwrap(value))) return ?(key, unwrap(value));
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func find<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool): ?(K, V) {
    return findHelper(map, acceptEntry, DEQ_NEXT);
  };

  public func findDesc<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool): ?(K, V) {
    return findHelper(map, acceptEntry, DEQ_PREV);
  };

  public func some<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool): Bool {
    return isSome(findHelper(map, acceptEntry, DEQ_NEXT));
  };

  public func someDesc<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool): Bool {
    return isSome(findHelper(map, acceptEntry, DEQ_PREV));
  };

  public func every<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool): Bool {
    return not isSome(findHelper(map, negate(acceptEntry), DEQ_NEXT));
  };

  public func everyDesc<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool): Bool {
    return not isSome(findHelper(map, negate(acceptEntry), DEQ_PREV));
  };

  public func forEach<K, V>(map: Map<K, V>, mapEntry: (K, V) -> ()) {
    ignore findHelper(map, reject(mapEntry), DEQ_NEXT);
  };

  public func forEachDesc<K, V>(map: Map<K, V>, mapEntry: (K, V) -> ()) {
    ignore findHelper(map, reject(mapEntry), DEQ_PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func fromIterHelper<K, V, T>(
    iter: IterNext<T>,
    hashUtils: HashUtils<K>,
    mapItem: (T) -> ?(K, V),
    DEQ_PREV: Nat,
    DEQ_NEXT: Nat,
  ): Map<K, V> {
    let map = new<K, V>(hashUtils);

    for (item in iter) switch (mapItem(item)) {
      case (?(key, value)) ignore putHelper(map, hashUtils, key, null, constant(?value), PUT_MOVE, DEQ_PREV, DEQ_NEXT);
      case (_) {};
    };

    return map;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func fromIter<K, V>(iter: IterNext<(K, V)>, hashUtils: HashUtils<K>): Map<K, V> {
    return fromIterHelper<K, V, (K, V)>(iter, hashUtils, func(item) = ?item, DEQ_PREV, DEQ_NEXT);
  };

  public func fromIterDesc<K, V>(iter: IterNext<(K, V)>, hashUtils: HashUtils<K>): Map<K, V> {
    return fromIterHelper<K, V, (K, V)>(iter, hashUtils, func(item) = ?item, DEQ_NEXT, DEQ_PREV);
  };

  public func fromIterMap<K, V, T>(iter: IterNext<T>, hashUtils: HashUtils<K>, mapItem: (T) -> ?(K, V)): Map<K, V> {
    return fromIterHelper(iter, hashUtils, mapItem, DEQ_PREV, DEQ_NEXT);
  };

  public func fromIterMapDesc<K, V, T>(iter: IterNext<T>, hashUtils: HashUtils<K>, mapItem: (T) -> ?(K, V)): Map<K, V> {
    return fromIterHelper(iter, hashUtils, mapItem, DEQ_NEXT, DEQ_PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func toArrayHelper<K, V>(
    map: Map<K, V>,
    DEQ_NEXT: Nat,
  ): [(K, V)] {
    let (root, size) = map;
    var entry = root;

    return tabulateArray<(K, V)>(nat(size[SIZE]), func(i) {
      entry := entry.3[DEQ_NEXT];

      let (key, value, hash, links) = entry;

      return (key, unwrap(value));
    });
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func toArray<K, V>(map: Map<K, V>): [(K, V)] {
    return toArrayHelper(map, DEQ_NEXT);
  };

  public func toArrayDesc<K, V>(map: Map<K, V>): [(K, V)] {
    return toArrayHelper(map, DEQ_PREV);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func toArrayMapHelper<K, V, T>(
    map: Map<K, V>,
    mapEntry: (K, V) -> ?T,
    DEQ_NEXT: Nat,
  ): [T] {
    let (root, size) = map;
    var entry = root;
    var maxSize = size[SIZE];
    var array = initArray<?T>(nat(maxSize), null);
    var arraySize = 0:Nat32;

    loop {
      entry := getSibling(entry, DEQ_NEXT);

      let (key, value, hash, links) = entry;

      if (hash == ROOT) return tabulateArray<T>(nat(arraySize), func(i) = unwrap(array[i]));

      switch (mapEntry(key, unwrap(value))) {
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

  public func toArrayMap<K, V, T>(map: Map<K, V>, mapEntry: (K, V) -> ?T): [T] {
    return toArrayMapHelper(map, mapEntry, DEQ_NEXT);
  };

  public func toArrayMapDesc<K, V, T>(map: Map<K, V>, mapEntry: (K, V) -> ?T): [T] {
    return toArrayMapHelper(map, mapEntry, DEQ_PREV);
  };
};
