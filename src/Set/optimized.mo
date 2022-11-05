import Prim "mo:prim";

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

  public type HashUtils<K> = (
    getHash: (K) -> Nat32,
    areEqual: (K, K) -> Bool,
    getNullKey: () -> K,
  );

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

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func createEdgeEntry<K>(nullKey: K): Entry<K> {
    let tempEntry: Entry<K> = (nullKey, NULL_HASH, [var]);
    let newEdgeEntry: Entry<K> = (nullKey, NULL_HASH, [var tempEntry, tempEntry, tempEntry, tempEntry, tempEntry, tempEntry]);

    newEdgeEntry.2[BRANCH_1] := newEdgeEntry;
    newEdgeEntry.2[BRANCH_2] := newEdgeEntry;
    newEdgeEntry.2[BRANCH_3] := newEdgeEntry;
    newEdgeEntry.2[BRANCH_4] := newEdgeEntry;
    newEdgeEntry.2[DEQ_PREV] := newEdgeEntry;
    newEdgeEntry.2[DEQ_NEXT] := newEdgeEntry;

    newEdgeEntry;
  };

  public func new<K>(hashUtils: HashUtils<K>): Set<K> {
    (createEdgeEntry<K>(hashUtils.2()), [var 0]);
  };

  public func clear<K>(map: Set<K>) {
    map.0.2[BRANCH_1] := map.0;
    map.0.2[BRANCH_2] := map.0;
    map.0.2[BRANCH_3] := map.0;
    map.0.2[BRANCH_4] := map.0;
    map.0.2[DEQ_PREV] := map.0;
    map.0.2[DEQ_NEXT] := map.0;

    map.1[SIZE] := 0;
  };

  public func size<K>(map: Set<K>): Nat {
    nat(map.1[SIZE]);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func has<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;
    var entry = map.0.2[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      if (entry.1 == hashParam and hashUtils.1(entry.0, keyParam)) return true else if (entry.1 == NULL_HASH) return false else {
        shiftingHash >>= HASH_OFFSET;
        entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func put<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;
    var entry = map.0.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
    var parentEntry = map.0;

    loop {
      if (entry.1 == NULL_HASH) {
        let deqPrev = map.0.2[DEQ_PREV];
        let newEntry = (keyParam, hashParam, [var map.0, map.0, map.0, map.0, deqPrev, map.0]);

        deqPrev.2[DEQ_NEXT] := newEntry;
        map.0.2[DEQ_PREV] := newEntry;
        parentEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        map.1[SIZE] +%= 1;

        return false;
      } else if (entry.1 == hashParam and hashUtils.1(entry.0, keyParam)) {
        return true;
      } else {
        parentEntry := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func putFront<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;
    var entry = map.0.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
    var parentEntry = map.0;

    loop {
      if (entry.1 == NULL_HASH) {
        let deqNext = map.0.2[DEQ_NEXT];
        let newEntry = (keyParam, hashParam, [var map.0, map.0, map.0, map.0, map.0, deqNext]);

        deqNext.2[DEQ_PREV] := newEntry;
        map.0.2[DEQ_NEXT] := newEntry;
        parentEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        map.1[SIZE] +%= 1;

        return false;
      } else if (entry.1 == hashParam and hashUtils.1(entry.0, keyParam)) {
        return true;
      } else {
        parentEntry := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func add<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K) {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;
    var entry = map.0.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
    var parentEntry = map.0;

    loop {
      if (entry.1 == NULL_HASH) {
        let deqPrev = map.0.2[DEQ_PREV];
        let newEntry = (keyParam, hashParam, [var map.0, map.0, map.0, map.0, deqPrev, map.0]);

        deqPrev.2[DEQ_NEXT] := newEntry;
        map.0.2[DEQ_PREV] := newEntry;
        parentEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        map.1[SIZE] +%= 1;

        return;
      } else if (entry.1 == hashParam and hashUtils.1(entry.0, keyParam)) {
        return;
      } else {
        parentEntry := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func addFront<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K) {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;
    var entry = map.0.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
    var parentEntry = map.0;

    loop {
      if (entry.1 == NULL_HASH) {
        let deqNext = map.0.2[DEQ_NEXT];
        let newEntry = (keyParam, hashParam, [var map.0, map.0, map.0, map.0, map.0, deqNext]);

        deqNext.2[DEQ_PREV] := newEntry;
        map.0.2[DEQ_NEXT] := newEntry;
        parentEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        map.1[SIZE] +%= 1;

        return;
      } else if (entry.1 == hashParam and hashUtils.1(entry.0, keyParam)) {
        return;
      } else {
        parentEntry := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func putMove<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;
    var entry = map.0.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
    var parentEntry = map.0;

    loop {
      if (entry.1 == NULL_HASH) {
        let deqPrev = map.0.2[DEQ_PREV];
        let newEntry = (keyParam, hashParam, [var map.0, map.0, map.0, map.0, deqPrev, map.0]);

        deqPrev.2[DEQ_NEXT] := newEntry;
        map.0.2[DEQ_PREV] := newEntry;
        parentEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        map.1[SIZE] +%= 1;

        return false;
      } else if (entry.1 == hashParam and hashUtils.1(entry.0, keyParam)) {
        let deqNextOld = entry.2[DEQ_NEXT];

        if (deqNextOld.1 != NULL_HASH) {
          let deqPrevOld = entry.2[DEQ_PREV];
          let deqPrev = map.0.2[DEQ_PREV];

          deqPrevOld.2[DEQ_NEXT] := deqNextOld;
          deqNextOld.2[DEQ_PREV] := deqPrevOld;
          entry.2[DEQ_PREV] := deqPrev;
          entry.2[DEQ_NEXT] := map.0;
          deqPrev.2[DEQ_NEXT] := entry;
          map.0.2[DEQ_PREV] := entry;
        };

        return true;
      } else {
        parentEntry := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func putMoveFront<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;
    var entry = map.0.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
    var parentEntry = map.0;

    loop {
      if (entry.1 == NULL_HASH) {
        let deqNext = map.0.2[DEQ_NEXT];
        let newEntry = (keyParam, hashParam, [var map.0, map.0, map.0, map.0, map.0, deqNext]);

        deqNext.2[DEQ_PREV] := newEntry;
        map.0.2[DEQ_NEXT] := newEntry;
        parentEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        map.1[SIZE] +%= 1;

        return false;
      } else if (entry.1 == hashParam and hashUtils.1(entry.0, keyParam)) {
        let deqPrevOld = entry.2[DEQ_PREV];

        if (deqPrevOld.1 != NULL_HASH) {
          let deqNextOld = entry.2[DEQ_NEXT];
          let deqNext = map.0.2[DEQ_NEXT];

          deqNextOld.2[DEQ_PREV] := deqPrevOld;
          deqPrevOld.2[DEQ_NEXT] := deqNextOld;
          entry.2[DEQ_NEXT] := deqNext;
          entry.2[DEQ_PREV] := map.0;
          deqNext.2[DEQ_PREV] := entry;
          map.0.2[DEQ_NEXT] := entry;
        };

        return true;
      } else {
        parentEntry := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func addMove<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K) {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;
    var entry = map.0.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
    var parentEntry = map.0;

    loop {
      if (entry.1 == NULL_HASH) {
        let deqPrev = map.0.2[DEQ_PREV];
        let newEntry = (keyParam, hashParam, [var map.0, map.0, map.0, map.0, deqPrev, map.0]);

        deqPrev.2[DEQ_NEXT] := newEntry;
        map.0.2[DEQ_PREV] := newEntry;
        parentEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        map.1[SIZE] +%= 1;

        return;
      } else if (entry.1 == hashParam and hashUtils.1(entry.0, keyParam)) {
        let deqNextOld = entry.2[DEQ_NEXT];

        if (deqNextOld.1 != NULL_HASH) {
          let deqPrevOld = entry.2[DEQ_PREV];
          let deqPrev = map.0.2[DEQ_PREV];

          deqPrevOld.2[DEQ_NEXT] := deqNextOld;
          deqNextOld.2[DEQ_PREV] := deqPrevOld;
          entry.2[DEQ_PREV] := deqPrev;
          entry.2[DEQ_NEXT] := map.0;
          deqPrev.2[DEQ_NEXT] := entry;
          map.0.2[DEQ_PREV] := entry;
        };

        return;
      } else {
        parentEntry := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func addMoveFront<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K) {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;
    var entry = map.0.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
    var parentEntry = map.0;

    loop {
      if (entry.1 == NULL_HASH) {
        let deqNext = map.0.2[DEQ_NEXT];
        let newEntry = (keyParam, hashParam, [var map.0, map.0, map.0, map.0, map.0, deqNext]);

        deqNext.2[DEQ_PREV] := newEntry;
        map.0.2[DEQ_NEXT] := newEntry;
        parentEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        map.1[SIZE] +%= 1;

        return;
      } else if (entry.1 == hashParam and hashUtils.1(entry.0, keyParam)) {
        let deqPrevOld = entry.2[DEQ_PREV];

        if (deqPrevOld.1 != NULL_HASH) {
          let deqNextOld = entry.2[DEQ_NEXT];
          let deqNext = map.0.2[DEQ_NEXT];

          deqNextOld.2[DEQ_PREV] := deqPrevOld;
          deqPrevOld.2[DEQ_NEXT] := deqNextOld;
          entry.2[DEQ_NEXT] := deqNext;
          entry.2[DEQ_PREV] := map.0;
          deqNext.2[DEQ_PREV] := entry;
          map.0.2[DEQ_NEXT] := entry;
        };

        return;
      } else {
        parentEntry := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func putBefore<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K, placeKeyParam: K): Bool {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;
    var entry = map.0.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
    var parentEntry = map.0;

    let placeHashParam = hashUtils.0(placeKeyParam);
    var shiftingPlaceHash = placeHashParam;
    var placeEntry = map.0.2[nat(shiftingPlaceHash % HASH_CHUNK_SIZE)];

    loop if (placeEntry.1 == NULL_HASH or placeEntry.1 == placeHashParam and hashUtils.1(placeEntry.0, placeKeyParam)) loop {
      if (entry.1 == NULL_HASH) {
        let deqPrev = placeEntry.2[DEQ_PREV];
        let newEntry = (keyParam, hashParam, [var map.0, map.0, map.0, map.0, deqPrev, placeEntry]);

        deqPrev.2[DEQ_NEXT] := newEntry;
        placeEntry.2[DEQ_PREV] := newEntry;
        parentEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        map.1[SIZE] +%= 1;

        return false;
      } else if (entry.1 == hashParam and hashUtils.1(entry.0, keyParam)) {
        let deqPrevOld = entry.2[DEQ_PREV];
        let deqNextOld = entry.2[DEQ_NEXT];
        let deqPrev = placeEntry.2[DEQ_PREV];

        deqPrevOld.2[DEQ_NEXT] := deqNextOld;
        deqNextOld.2[DEQ_PREV] := deqPrevOld;
        entry.2[DEQ_PREV] := deqPrev;
        entry.2[DEQ_NEXT] := placeEntry;
        deqPrev.2[DEQ_NEXT] := entry;
        placeEntry.2[DEQ_PREV] := entry;

        return true;
      } else {
        parentEntry := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    } else {
      shiftingPlaceHash >>= HASH_OFFSET;
      placeEntry := placeEntry.2[nat(shiftingPlaceHash % HASH_CHUNK_SIZE)];
    };
  };

  public func putAfter<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K, placeKeyParam: K): Bool {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;
    var entry = map.0.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
    var parentEntry = map.0;

    let placeHashParam = hashUtils.0(placeKeyParam);
    var shiftingPlaceHash = placeHashParam;
    var placeEntry = map.0.2[nat(shiftingPlaceHash % HASH_CHUNK_SIZE)];

    loop if (placeEntry.1 == NULL_HASH or placeEntry.1 == placeHashParam and hashUtils.1(placeEntry.0, placeKeyParam)) loop {
      if (entry.1 == NULL_HASH) {
        let deqNext = placeEntry.2[DEQ_NEXT];
        let newEntry = (keyParam, hashParam, [var map.0, map.0, map.0, map.0, placeEntry, deqNext]);

        deqNext.2[DEQ_PREV] := newEntry;
        placeEntry.2[DEQ_NEXT] := newEntry;
        parentEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        map.1[SIZE] +%= 1;

        return false;
      } else if (entry.1 == hashParam and hashUtils.1(entry.0, keyParam)) {
        let deqNextOld = entry.2[DEQ_NEXT];
        let deqPrevOld = entry.2[DEQ_PREV];
        let deqNext = placeEntry.2[DEQ_NEXT];

        deqNextOld.2[DEQ_PREV] := deqPrevOld;
        deqPrevOld.2[DEQ_NEXT] := deqNextOld;
        entry.2[DEQ_NEXT] := deqNext;
        entry.2[DEQ_PREV] := placeEntry;
        deqNext.2[DEQ_PREV] := entry;
        placeEntry.2[DEQ_NEXT] := entry;

        return true;
      } else {
        parentEntry := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    } else {
      shiftingPlaceHash >>= HASH_OFFSET;
      placeEntry := placeEntry.2[nat(shiftingPlaceHash % HASH_CHUNK_SIZE)];
    };
  };

  public func addBefore<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K, placeKeyParam: K) {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;
    var entry = map.0.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
    var parentEntry = map.0;

    let placeHashParam = hashUtils.0(placeKeyParam);
    var shiftingPlaceHash = placeHashParam;
    var placeEntry = map.0.2[nat(shiftingPlaceHash % HASH_CHUNK_SIZE)];

    loop if (placeEntry.1 == NULL_HASH or placeEntry.1 == placeHashParam and hashUtils.1(placeEntry.0, placeKeyParam)) loop {
      if (entry.1 == NULL_HASH) {
        let deqPrev = placeEntry.2[DEQ_PREV];
        let newEntry = (keyParam, hashParam, [var map.0, map.0, map.0, map.0, deqPrev, placeEntry]);

        deqPrev.2[DEQ_NEXT] := newEntry;
        placeEntry.2[DEQ_PREV] := newEntry;
        parentEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        map.1[SIZE] +%= 1;

        return;
      } else if (entry.1 == hashParam and hashUtils.1(entry.0, keyParam)) {
        let deqPrevOld = entry.2[DEQ_PREV];
        let deqNextOld = entry.2[DEQ_NEXT];
        let deqPrev = placeEntry.2[DEQ_PREV];

        deqPrevOld.2[DEQ_NEXT] := deqNextOld;
        deqNextOld.2[DEQ_PREV] := deqPrevOld;
        entry.2[DEQ_PREV] := deqPrev;
        entry.2[DEQ_NEXT] := placeEntry;
        deqPrev.2[DEQ_NEXT] := entry;
        placeEntry.2[DEQ_PREV] := entry;

        return;
      } else {
        parentEntry := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    } else {
      shiftingPlaceHash >>= HASH_OFFSET;
      placeEntry := placeEntry.2[nat(shiftingPlaceHash % HASH_CHUNK_SIZE)];
    };
  };

  public func addAfter<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K, placeKeyParam: K) {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;
    var entry = map.0.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
    var parentEntry = map.0;

    let placeHashParam = hashUtils.0(placeKeyParam);
    var shiftingPlaceHash = placeHashParam;
    var placeEntry = map.0.2[nat(shiftingPlaceHash % HASH_CHUNK_SIZE)];

    loop if (placeEntry.1 == NULL_HASH or placeEntry.1 == placeHashParam and hashUtils.1(placeEntry.0, placeKeyParam)) loop {
      if (entry.1 == NULL_HASH) {
        let deqNext = placeEntry.2[DEQ_NEXT];
        let newEntry = (keyParam, hashParam, [var map.0, map.0, map.0, map.0, placeEntry, deqNext]);

        deqNext.2[DEQ_PREV] := newEntry;
        placeEntry.2[DEQ_NEXT] := newEntry;
        parentEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        map.1[SIZE] +%= 1;

        return;
      } else if (entry.1 == hashParam and hashUtils.1(entry.0, keyParam)) {
        let deqNextOld = entry.2[DEQ_NEXT];
        let deqPrevOld = entry.2[DEQ_PREV];
        let deqNext = placeEntry.2[DEQ_NEXT];

        deqNextOld.2[DEQ_PREV] := deqPrevOld;
        deqPrevOld.2[DEQ_NEXT] := deqNextOld;
        entry.2[DEQ_NEXT] := deqNext;
        entry.2[DEQ_PREV] := placeEntry;
        deqNext.2[DEQ_PREV] := entry;
        placeEntry.2[DEQ_NEXT] := entry;

        return;
      } else {
        parentEntry := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    } else {
      shiftingPlaceHash >>= HASH_OFFSET;
      placeEntry := placeEntry.2[nat(shiftingPlaceHash % HASH_CHUNK_SIZE)];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func remove<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;
    var entry = map.0.2[nat(hashParam % HASH_CHUNK_SIZE)];
    var parentEntry = map.0;

    loop {
      if (entry.1 == hashParam and hashUtils.1(entry.0, keyParam)) {
        let deqPrev = entry.2[DEQ_PREV];
        let deqNext = entry.2[DEQ_NEXT];
        var leaf = entry;
        var leafParent = parentEntry;
        var leafIndex = NULL_BRANCH;

        deqPrev.2[DEQ_NEXT] := deqNext;
        deqNext.2[DEQ_PREV] := deqPrev;
        map.1[SIZE] -%= 1;

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
                  parentEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := map.0;

                  return true;
                } else {
                  parentEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := leaf;
                  leafParent.2[leafIndex] := map.0;

                  leaf.2[BRANCH_1] := entry.2[BRANCH_1];
                  leaf.2[BRANCH_2] := entry.2[BRANCH_2];
                  leaf.2[BRANCH_3] := entry.2[BRANCH_3];
                  leaf.2[BRANCH_4] := entry.2[BRANCH_4];

                  return true;
                };
              };
            };
          };
        };
      } else if (entry.1 == NULL_HASH) {
        return false;
      } else {
        parentEntry := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func delete<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K) {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;
    var entry = map.0.2[nat(hashParam % HASH_CHUNK_SIZE)];
    var parentEntry = map.0;

    loop {
      if (entry.1 == hashParam and hashUtils.1(entry.0, keyParam)) {
        let deqPrev = entry.2[DEQ_PREV];
        let deqNext = entry.2[DEQ_NEXT];
        var leaf = entry;
        var leafParent = parentEntry;
        var leafIndex = NULL_BRANCH;

        deqPrev.2[DEQ_NEXT] := deqNext;
        deqNext.2[DEQ_PREV] := deqPrev;
        map.1[SIZE] -%= 1;

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
                  parentEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := map.0;

                  return;
                } else {
                  parentEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := leaf;
                  leafParent.2[leafIndex] := map.0;

                  leaf.2[BRANCH_1] := entry.2[BRANCH_1];
                  leaf.2[BRANCH_2] := entry.2[BRANCH_2];
                  leaf.2[BRANCH_3] := entry.2[BRANCH_3];
                  leaf.2[BRANCH_4] := entry.2[BRANCH_4];

                  return;
                };
              };
            };
          };
        };
      } else if (entry.1 == NULL_HASH) {
        return;
      } else {
        parentEntry := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func pop<K>(map: Set<K>): ?K {
    var entry = map.0.2[DEQ_PREV];
    var shiftingHash = entry.1;
    var parentEntry = map.0;

    if (shiftingHash == NULL_HASH) return null;

    loop {
      if (entry.2[DEQ_NEXT].1 == NULL_HASH) {
        let deqPrev = entry.2[DEQ_PREV];
        var leaf = entry;
        var leafParent = parentEntry;
        var leafIndex = NULL_BRANCH;

        deqPrev.2[DEQ_NEXT] := map.0;
        map.0.2[DEQ_PREV] := deqPrev;
        map.1[SIZE] -%= 1;

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
                  parentEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := map.0;

                  return ?entry.0;
                } else {
                  parentEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := leaf;
                  leafParent.2[leafIndex] := map.0;

                  leaf.2[BRANCH_1] := entry.2[BRANCH_1];
                  leaf.2[BRANCH_2] := entry.2[BRANCH_2];
                  leaf.2[BRANCH_3] := entry.2[BRANCH_3];
                  leaf.2[BRANCH_4] := entry.2[BRANCH_4];

                  return ?entry.0;
                };
              };
            };
          };
        };
      } else {
        parentEntry := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func popFront<K>(map: Set<K>): ?K {
    var entry = map.0.2[DEQ_NEXT];
    var shiftingHash = entry.1;
    var parentEntry = map.0;

    if (shiftingHash == NULL_HASH) return null;

    loop {
      if (entry.2[DEQ_PREV].1 == NULL_HASH) {
        let deqNext = entry.2[DEQ_NEXT];
        var leaf = entry;
        var leafParent = parentEntry;
        var leafIndex = NULL_BRANCH;

        deqNext.2[DEQ_PREV] := map.0;
        map.0.2[DEQ_NEXT] := deqNext;
        map.1[SIZE] -%= 1;

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
                  parentEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := map.0;

                  return ?entry.0;
                } else {
                  parentEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := leaf;
                  leafParent.2[leafIndex] := map.0;

                  leaf.2[BRANCH_1] := entry.2[BRANCH_1];
                  leaf.2[BRANCH_2] := entry.2[BRANCH_2];
                  leaf.2[BRANCH_3] := entry.2[BRANCH_3];
                  leaf.2[BRANCH_4] := entry.2[BRANCH_4];

                  return ?entry.0;
                };
              };
            };
          };
        };
      } else {
        parentEntry := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func cycle<K>(map: Set<K>): ?K {
    let entry = map.0.2[DEQ_PREV];

    if (entry.1 == NULL_HASH) null else {
      let deqPrev = entry.2[DEQ_PREV];

      if (deqPrev.1 != NULL_HASH) {
        let deqFirst = map.0.2[DEQ_NEXT];

        deqPrev.2[DEQ_NEXT] := map.0;
        map.0.2[DEQ_PREV] := deqPrev;
        deqFirst.2[DEQ_PREV] := entry;
        map.0.2[DEQ_NEXT] := entry;
        entry.2[DEQ_PREV] := map.0;
        entry.2[DEQ_NEXT] := deqFirst;
      };

      ?entry.0;
    };
  };

  public func cycleFront<K>(map: Set<K>): ?K {
    let entry = map.0.2[DEQ_NEXT];

    if (entry.1 == NULL_HASH) null else {
      let deqNext = entry.2[DEQ_NEXT];

      if (deqNext.1 != NULL_HASH) {
        let deqLast = map.0.2[DEQ_PREV];

        deqNext.2[DEQ_PREV] := map.0;
        map.0.2[DEQ_NEXT] := deqNext;
        deqLast.2[DEQ_NEXT] := entry;
        map.0.2[DEQ_PREV] := entry;
        entry.2[DEQ_NEXT] := map.0;
        entry.2[DEQ_PREV] := deqLast;
      };

      ?entry.0;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func peek<K>(map: Set<K>): ?K {
    let entry = map.0.2[DEQ_PREV];

    if (entry.1 == NULL_HASH) null else ?entry.0;
  };

  public func peekFront<K>(map: Set<K>): ?K {
    let entry = map.0.2[DEQ_NEXT];

    if (entry.1 == NULL_HASH) null else ?entry.0;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func filter<K>(map: Set<K>, fn: (K) -> Bool): Set<K> {
    let newEdgeEntry = createEdgeEntry<K>(map.0.0);
    var entry = map.0.2[DEQ_NEXT];
    var deqPrev = newEdgeEntry;
    var newSize = 0:Nat32;

    loop {
      if (entry.1 == NULL_HASH) {
        newEdgeEntry.2[DEQ_PREV] := deqPrev;

        return (newEdgeEntry, [var newSize]);
      } else if (fn(entry.0)) {
        var shiftingHash = entry.1;
        var searchEntry = newEdgeEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
        var parentEntry = newEdgeEntry;

        while (searchEntry.1 != NULL_HASH) {
          parentEntry := searchEntry;
          shiftingHash >>= HASH_OFFSET;
          searchEntry := searchEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
        };

        let newEntry = (entry.0, entry.1, [var newEdgeEntry, newEdgeEntry, newEdgeEntry, newEdgeEntry, deqPrev, newEdgeEntry]);

        deqPrev.2[DEQ_NEXT] := newEntry;
        parentEntry.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        deqPrev := newEntry;
        entry := entry.2[DEQ_NEXT];
        newSize +%= 1;
      } else {
        entry := entry.2[DEQ_NEXT];
      };
    };
  };

  public func clone<K>(map: Set<K>): Set<K> {
    filter<K>(map, func(key) = true);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func keys<K>(map: Set<K>): Iter<K> {
    var entry = map.0;

    let iter = {
      prev = func(): ?K {
        entry := entry.2[DEQ_PREV];

        if (entry.1 == NULL_HASH) null else ?entry.0;
      };

      next = func(): ?K {
        entry := entry.2[DEQ_NEXT];

        if (entry.1 == NULL_HASH) null else ?entry.0;
      };

      current = func(): ?K {
        if (entry.1 == NULL_HASH) null else ?entry.0;
      };

      reset = func(): Iter<K> {
        entry := map.0;

        iter;
      };

      movePrev = func(): Iter<K> {
        entry := entry.2[DEQ_PREV];

        iter;
      };

      moveNext = func(): Iter<K> {
        entry := entry.2[DEQ_NEXT];

        iter;
      };
    };
  };

  public func keysDesc<K>(map: Set<K>): Iter<K> {
    var entry = map.0;

    let iter = {
      prev = func(): ?K {
        entry := entry.2[DEQ_NEXT];

        if (entry.1 == NULL_HASH) null else ?entry.0;
      };

      next = func(): ?K {
        entry := entry.2[DEQ_PREV];

        if (entry.1 == NULL_HASH) null else ?entry.0;
      };

      current = func(): ?K {
        if (entry.1 == NULL_HASH) null else ?entry.0;
      };

      reset = func(): Iter<K> {
        entry := map.0;

        iter;
      };

      movePrev = func(): Iter<K> {
        entry := entry.2[DEQ_NEXT];

        iter;
      };

      moveNext = func(): Iter<K> {
        entry := entry.2[DEQ_PREV];

        iter;
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func keysFrom<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Iter<K> {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;
    var entry = map.0.2[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      if (entry.1 == NULL_HASH or entry.1 == hashParam and hashUtils.1(entry.0, keyParam)) return let iter = {
        prev = func(): ?K {
          entry := entry.2[DEQ_PREV];

          if (entry.1 == NULL_HASH) null else ?entry.0;
        };

        next = func(): ?K {
          entry := entry.2[DEQ_NEXT];

          if (entry.1 == NULL_HASH) null else ?entry.0;
        };

        current = func(): ?K {
          if (entry.1 == NULL_HASH) null else ?entry.0;
        };

        reset = func(): Iter<K> {
          entry := map.0;

          iter;
        };

        movePrev = func(): Iter<K> {
          entry := entry.2[DEQ_PREV];

          iter;
        };

        moveNext = func(): Iter<K> {
          entry := entry.2[DEQ_NEXT];

          iter;
        };
      } else {
        shiftingHash >>= HASH_OFFSET;
        entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func keysFromDesc<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Iter<K> {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;
    var entry = map.0.2[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      if (entry.1 == NULL_HASH or entry.1 == hashParam and hashUtils.1(entry.0, keyParam)) return let iter = {
        prev = func(): ?K {
          entry := entry.2[DEQ_NEXT];

          if (entry.1 == NULL_HASH) null else ?entry.0;
        };

        next = func(): ?K {
          entry := entry.2[DEQ_PREV];

          if (entry.1 == NULL_HASH) null else ?entry.0;
        };

        current = func(): ?K {
          if (entry.1 == NULL_HASH) null else ?entry.0;
        };

        reset = func(): Iter<K> {
          entry := map.0;

          iter;
        };

        movePrev = func(): Iter<K> {
          entry := entry.2[DEQ_NEXT];

          iter;
        };

        moveNext = func(): Iter<K> {
          entry := entry.2[DEQ_PREV];

          iter;
        };
      } else {
        shiftingHash >>= HASH_OFFSET;
        entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func forEach<K>(map: Set<K>, fn: (K) -> ()) {
    var entry = map.0;

    loop {
      entry := entry.2[DEQ_NEXT];

      if (entry.1 == NULL_HASH) return else fn(entry.0);
    };
  };

  public func forEachDesc<K>(map: Set<K>, fn: (K) -> ()) {
    var entry = map.0;

    loop {
      entry := entry.2[DEQ_PREV];

      if (entry.1 == NULL_HASH) return else fn(entry.0);
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func every<K>(map: Set<K>, fn: (K) -> Bool): Bool {
    var entry = map.0;

    loop {
      entry := entry.2[DEQ_NEXT];

      if (entry.1 == NULL_HASH) return true else if (not fn(entry.0)) return false;
    };
  };

  public func everyDesc<K>(map: Set<K>, fn: (K) -> Bool): Bool {
    var entry = map.0;

    loop {
      entry := entry.2[DEQ_PREV];

      if (entry.1 == NULL_HASH) return true else if (not fn(entry.0)) return false;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func some<K>(map: Set<K>, fn: (K) -> Bool): Bool {
    var entry = map.0;

    loop {
      entry := entry.2[DEQ_NEXT];

      if (entry.1 == NULL_HASH) return false else if (fn(entry.0)) return true;
    };
  };

  public func someDesc<K>(map: Set<K>, fn: (K) -> Bool): Bool {
    var entry = map.0;

    loop {
      entry := entry.2[DEQ_PREV];

      if (entry.1 == NULL_HASH) return false else if (fn(entry.0)) return true;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func find<K>(map: Set<K>, fn: (K) -> Bool): ?K {
    var entry = map.0;

    loop {
      entry := entry.2[DEQ_NEXT];

      if (entry.1 == NULL_HASH) return null else if (fn(entry.0)) return ?entry.0;
    };
  };

  public func findDesc<K>(map: Set<K>, fn: (K) -> Bool): ?K {
    var entry = map.0;

    loop {
      entry := entry.2[DEQ_PREV];

      if (entry.1 == NULL_HASH) return null else if (fn(entry.0)) return ?entry.0;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func fromIter<K>(iter: IterNative<K>, hashUtils: HashUtils<K>): Set<K> {
    let newMap = new<K>(hashUtils);

    for (key in iter) add(newMap, hashUtils, key);

    newMap;
  };

  public func fromIterDesc<K>(iter: IterNative<K>, hashUtils: HashUtils<K>): Set<K> {
    let newMap = new<K>(hashUtils);

    for (key in iter) addFront(newMap, hashUtils, key);

    newMap;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func fromIterMap<K, T>(iter: IterNative<T>, hashUtils: HashUtils<K>, fn: (T) -> ?K): Set<K> {
    let newMap = new<K>(hashUtils);

    for (item in iter) switch (fn(item)) { case (?key) add(newMap, hashUtils, key); case (_) {} };

    newMap;
  };

  public func fromIterMapDesc<K, T>(iter: IterNative<T>, hashUtils: HashUtils<K>, fn: (T) -> ?K): Set<K> {
    let newMap = new<K>(hashUtils);

    for (item in iter) switch (fn(item)) { case (?key) addFront(newMap, hashUtils, key); case (_) {} };

    newMap;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func toArray<K>(map: Set<K>): [K] {
    var entry = map.0;

    tabulateArray<K>(nat(map.1[SIZE]), func(i) {
      entry := entry.2[DEQ_NEXT];

      entry.0;
    });
  };

  public func toArrayDesc<K>(map: Set<K>): [K] {
    var entry = map.0;

    tabulateArray<K>(nat(map.1[SIZE]), func(i) {
      entry := entry.2[DEQ_PREV];

      entry.0;
    });
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func toArrayMap<K, T>(map: Set<K>, fn: (K) -> ?T, DEQ_NEXT: Nat): [T] {
    let array = initArray<?T>(nat(map.1[SIZE]), null);
    var entry = map.0.2[DEQ_NEXT];
    var arraySize = 0:Nat32;

    loop {
      if (entry.1 == NULL_HASH)
      return tabulateArray<T>(nat(arraySize), func(i) = switch (array[i]) { case (?value) value; case (_) trap("unreachable") })
      else switch (fn(entry.0)) {
        case (null) entry := entry.2[DEQ_NEXT];

        case (item) {
          array[nat(arraySize)] := item;
          entry := entry.2[DEQ_NEXT];
          arraySize +%= 1;
        };
      };
    };
  };

  public func toArrayMapDesc<K, T>(map: Set<K>, fn: (K) -> ?T, DEQ_PREV: Nat): [T] {
    let array = initArray<?T>(nat(map.1[SIZE]), null);
    var entry = map.0.2[DEQ_PREV];
    var arraySize = 0:Nat32;

    loop {
      if (entry.1 == NULL_HASH)
      return tabulateArray<T>(nat(arraySize), func(i) = switch (array[i]) { case (?value) value; case (_) trap("unreachable") })
      else switch (fn(entry.0)) {
        case (null) entry := entry.2[DEQ_PREV];

        case (item) {
          array[nat(arraySize)] := item;
          entry := entry.2[DEQ_PREV];
          arraySize +%= 1;
        };
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func hashInt(key: Int): Nat32 {
    var hash = Prim.intToNat32Wrap(key);

    hash := hash >> 16 ^ hash *% 0x21f0aaad;
    hash := hash >> 15 ^ hash *% 0x735a2d97;

    hash >> 15 ^ hash & 0x3fffffff;
  };

  public func hashText(key: Text): Nat32 {
    Prim.hashBlob(Prim.encodeUtf8(key)) & 0x3fffffff;
  };

  public func hashPrincipal(key: Principal): Nat32 {
    Prim.hashBlob(Prim.blobOfPrincipal(key)) & 0x3fffffff;
  };

  public func hashBlob(key: Blob): Nat32 {
    Prim.hashBlob(key) & 0x3fffffff;
  };

  public func hashBool(key: Bool): Nat32 {
    if (key) 1 else 0;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public let ihash: HashUtils<Int> = (hashInt, func(a, b) { a == b }, func() { 0 });

  public let nhash: HashUtils<Nat> = (hashInt, func(a, b) { a == b }, func() { 0 });

  public let thash: HashUtils<Text> = (hashText, func(a, b) { a == b }, func() { "" });

  public let phash: HashUtils<Principal> = (hashPrincipal, func(a, b) { a == b }, func() { Prim.principalOfBlob("") });

  public let bhash: HashUtils<Blob> = (hashBlob, func(a, b) { a == b }, func() { "" });

  public let lhash: HashUtils<Bool> = (hashBool, func(a, b) { true }, func() { false });

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func useHash<K>(hashUtils: HashUtils<K>, hash: Nat32): HashUtils<K> {
    (func(key) { hash }, hashUtils.1, hashUtils.2);
  };

  public func calcHash<K>(hashUtils: HashUtils<K>, key: K): HashUtils<K> {
    let hash = hashUtils.0(key);

    (func(key) { hash }, hashUtils.1, hashUtils.2);
  };
};
