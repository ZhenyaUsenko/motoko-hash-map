import Prim "mo:prim";

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

  public type HashUtils<K> = (
    getHash: (K) -> Nat32,
    areEqual: (K, K) -> Bool,
    getNullKey: () -> K,
  );

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

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func createEdgeEntry<K, V>(nullKey: K): Entry<K, V> {
    let tempEntry = (nullKey, null, NULL_HASH, [var]):Entry<K, V>;
    let newEdge = (nullKey, null, NULL_HASH, [var tempEntry, tempEntry, tempEntry, tempEntry, tempEntry, tempEntry]);

    newEdge.3[BRANCH_1] := newEdge;
    newEdge.3[BRANCH_2] := newEdge;
    newEdge.3[BRANCH_3] := newEdge;
    newEdge.3[BRANCH_4] := newEdge;
    newEdge.3[DEQ_PREV] := newEdge;
    newEdge.3[DEQ_NEXT] := newEdge;

    newEdge;
  };

  public func new<K, V>(hashUtils: HashUtils<K>): Map<K, V> {
    (createEdgeEntry<K, V>(hashUtils.2()), [var 0]);
  };

  public func clear<K, V>(map: Map<K, V>) {
    map.0.3[BRANCH_1] := map.0;
    map.0.3[BRANCH_2] := map.0;
    map.0.3[BRANCH_3] := map.0;
    map.0.3[BRANCH_4] := map.0;
    map.0.3[DEQ_PREV] := map.0;
    map.0.3[DEQ_NEXT] := map.0;

    map.1[SIZE] := 0;
  };

  public func size<K, V>(map: Map<K, V>): Nat {
    nat(map.1[SIZE]);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func get<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): ?V {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;
    var entry = map.0.3[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      if (entry.2 == hashParam and hashUtils.1(entry.0, keyParam)) return entry.1 else if (entry.2 == NULL_HASH) return null else {
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func has<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;
    var entry = map.0.3[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      if (entry.2 == hashParam and hashUtils.1(entry.0, keyParam)) return true else if (entry.2 == NULL_HASH) return false else {
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func put<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V): ?V {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;
    var entry = map.0.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
    var parentEntry = map.0;

    loop {
      if (entry.2 == NULL_HASH) {
        let deqPrev = map.0.3[DEQ_PREV];
        let newEntry = (keyParam, ?valueParam, hashParam, [var map.0, map.0, map.0, map.0, deqPrev, map.0]);

        deqPrev.3[DEQ_NEXT] := newEntry;
        map.0.3[DEQ_PREV] := newEntry;
        parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        map.1[SIZE] +%= 1;

        return null;
      } else if (entry.2 == hashParam and hashUtils.1(entry.0, keyParam)) {
        let newEntry = (entry.0, ?valueParam, entry.2, entry.3);

        entry.3[DEQ_PREV].3[DEQ_NEXT] := newEntry;
        entry.3[DEQ_NEXT].3[DEQ_PREV] := newEntry;

        parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

        return entry.1;
      } else {
        parentEntry := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func putFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V): ?V {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;
    var entry = map.0.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
    var parentEntry = map.0;

    loop {
      if (entry.2 == NULL_HASH) {
        let deqNext = map.0.3[DEQ_NEXT];
        let newEntry = (keyParam, ?valueParam, hashParam, [var map.0, map.0, map.0, map.0, map.0, deqNext]);

        deqNext.3[DEQ_PREV] := newEntry;
        map.0.3[DEQ_NEXT] := newEntry;
        parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        map.1[SIZE] +%= 1;

        return null;
      } else if (entry.2 == hashParam and hashUtils.1(entry.0, keyParam)) {
        let newEntry = (entry.0, ?valueParam, entry.2, entry.3);

        entry.3[DEQ_NEXT].3[DEQ_PREV] := newEntry;
        entry.3[DEQ_PREV].3[DEQ_NEXT] := newEntry;

        parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

        return entry.1;
      } else {
        parentEntry := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func set<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V) {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;
    var entry = map.0.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
    var parentEntry = map.0;

    loop {
      if (entry.2 == NULL_HASH) {
        let deqPrev = map.0.3[DEQ_PREV];
        let newEntry = (keyParam, ?valueParam, hashParam, [var map.0, map.0, map.0, map.0, deqPrev, map.0]);

        deqPrev.3[DEQ_NEXT] := newEntry;
        map.0.3[DEQ_PREV] := newEntry;
        parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        map.1[SIZE] +%= 1;

        return;
      } else if (entry.2 == hashParam and hashUtils.1(entry.0, keyParam)) {
        let newEntry = (entry.0, ?valueParam, entry.2, entry.3);

        entry.3[DEQ_PREV].3[DEQ_NEXT] := newEntry;
        entry.3[DEQ_NEXT].3[DEQ_PREV] := newEntry;

        parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

        return;
      } else {
        parentEntry := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func setFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V) {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;
    var entry = map.0.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
    var parentEntry = map.0;

    loop {
      if (entry.2 == NULL_HASH) {
        let deqNext = map.0.3[DEQ_NEXT];
        let newEntry = (keyParam, ?valueParam, hashParam, [var map.0, map.0, map.0, map.0, map.0, deqNext]);

        deqNext.3[DEQ_PREV] := newEntry;
        map.0.3[DEQ_NEXT] := newEntry;
        parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        map.1[SIZE] +%= 1;

        return;
      } else if (entry.2 == hashParam and hashUtils.1(entry.0, keyParam)) {
        let newEntry = (entry.0, ?valueParam, entry.2, entry.3);

        entry.3[DEQ_NEXT].3[DEQ_PREV] := newEntry;
        entry.3[DEQ_PREV].3[DEQ_NEXT] := newEntry;

        parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

        return;
      } else {
        parentEntry := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func putMove<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: ?V): ?V {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;
    var entry = map.0.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
    var parentEntry = map.0;

    loop {
      if (entry.2 == NULL_HASH) {
        switch (valueParam) {
          case (null) return null;

          case (_) {
            let deqPrev = map.0.3[DEQ_PREV];
            let newEntry = (keyParam, valueParam, hashParam, [var map.0, map.0, map.0, map.0, deqPrev, map.0]);

            deqPrev.3[DEQ_NEXT] := newEntry;
            map.0.3[DEQ_PREV] := newEntry;
            parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
            map.1[SIZE] +%= 1;

            return null;
          };
        };
      } else if (entry.2 == hashParam and hashUtils.1(entry.0, keyParam)) {
        let newEntry = switch (valueParam) { case (null) entry; case (_) (entry.0, valueParam, entry.2, entry.3) };

        let deqNextOld = entry.3[DEQ_NEXT];

        if (deqNextOld.2 != NULL_HASH) {
          let deqPrevOld = entry.3[DEQ_PREV];
          let deqPrev = map.0.3[DEQ_PREV];

          deqPrevOld.3[DEQ_NEXT] := deqNextOld;
          deqNextOld.3[DEQ_PREV] := deqPrevOld;
          newEntry.3[DEQ_PREV] := deqPrev;
          newEntry.3[DEQ_NEXT] := map.0;
          deqPrev.3[DEQ_NEXT] := newEntry;
          map.0.3[DEQ_PREV] := newEntry;
        } else {
          entry.3[DEQ_PREV].3[DEQ_NEXT] := newEntry;
          map.0.3[DEQ_PREV] := newEntry;
        };

        parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

        return entry.1;
      } else {
        parentEntry := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func putMoveFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: ?V): ?V {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;
    var entry = map.0.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
    var parentEntry = map.0;

    loop {
      if (entry.2 == NULL_HASH) {
        switch (valueParam) {
          case (null) return null;

          case (_) {
            let deqNext = map.0.3[DEQ_NEXT];
            let newEntry = (keyParam, valueParam, hashParam, [var map.0, map.0, map.0, map.0, map.0, deqNext]);

            deqNext.3[DEQ_PREV] := newEntry;
            map.0.3[DEQ_NEXT] := newEntry;
            parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
            map.1[SIZE] +%= 1;

            return null;
          };
        };
      } else if (entry.2 == hashParam and hashUtils.1(entry.0, keyParam)) {
        let newEntry = switch (valueParam) { case (null) entry; case (_) (entry.0, valueParam, entry.2, entry.3) };

        let deqPrevOld = entry.3[DEQ_PREV];

        if (deqPrevOld.2 != NULL_HASH) {
          let deqNextOld = entry.3[DEQ_NEXT];
          let deqNext = map.0.3[DEQ_NEXT];

          deqNextOld.3[DEQ_PREV] := deqPrevOld;
          deqPrevOld.3[DEQ_NEXT] := deqNextOld;
          newEntry.3[DEQ_NEXT] := deqNext;
          newEntry.3[DEQ_PREV] := map.0;
          deqNext.3[DEQ_PREV] := newEntry;
          map.0.3[DEQ_NEXT] := newEntry;
        } else {
          entry.3[DEQ_NEXT].3[DEQ_PREV] := newEntry;
          map.0.3[DEQ_NEXT] := newEntry;
        };

        parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

        return entry.1;
      } else {
        parentEntry := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func setMove<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: ?V) {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;
    var entry = map.0.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
    var parentEntry = map.0;

    loop {
      if (entry.2 == NULL_HASH) {
        switch (valueParam) {
          case (null) return;

          case (_) {
            let deqPrev = map.0.3[DEQ_PREV];
            let newEntry = (keyParam, valueParam, hashParam, [var map.0, map.0, map.0, map.0, deqPrev, map.0]);

            deqPrev.3[DEQ_NEXT] := newEntry;
            map.0.3[DEQ_PREV] := newEntry;
            parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
            map.1[SIZE] +%= 1;

            return;
          };
        };
      } else if (entry.2 == hashParam and hashUtils.1(entry.0, keyParam)) {
        let newEntry = switch (valueParam) { case (null) entry; case (_) (entry.0, valueParam, entry.2, entry.3) };

        let deqNextOld = entry.3[DEQ_NEXT];

        if (deqNextOld.2 != NULL_HASH) {
          let deqPrevOld = entry.3[DEQ_PREV];
          let deqPrev = map.0.3[DEQ_PREV];

          deqPrevOld.3[DEQ_NEXT] := deqNextOld;
          deqNextOld.3[DEQ_PREV] := deqPrevOld;
          newEntry.3[DEQ_PREV] := deqPrev;
          newEntry.3[DEQ_NEXT] := map.0;
          deqPrev.3[DEQ_NEXT] := newEntry;
          map.0.3[DEQ_PREV] := newEntry;
        } else {
          entry.3[DEQ_PREV].3[DEQ_NEXT] := newEntry;
          map.0.3[DEQ_PREV] := newEntry;
        };

        parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

        return;
      } else {
        parentEntry := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func setMoveFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: ?V) {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;
    var entry = map.0.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
    var parentEntry = map.0;

    loop {
      if (entry.2 == NULL_HASH) {
        switch (valueParam) {
          case (null) return;

          case (_) {
            let deqNext = map.0.3[DEQ_NEXT];
            let newEntry = (keyParam, valueParam, hashParam, [var map.0, map.0, map.0, map.0, map.0, deqNext]);

            deqNext.3[DEQ_PREV] := newEntry;
            map.0.3[DEQ_NEXT] := newEntry;
            parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
            map.1[SIZE] +%= 1;

            return;
          };
        };
      } else if (entry.2 == hashParam and hashUtils.1(entry.0, keyParam)) {
        let newEntry = switch (valueParam) { case (null) entry; case (_) (entry.0, valueParam, entry.2, entry.3) };

        let deqPrevOld = entry.3[DEQ_PREV];

        if (deqPrevOld.2 != NULL_HASH) {
          let deqNextOld = entry.3[DEQ_NEXT];
          let deqNext = map.0.3[DEQ_NEXT];

          deqNextOld.3[DEQ_PREV] := deqPrevOld;
          deqPrevOld.3[DEQ_NEXT] := deqNextOld;
          newEntry.3[DEQ_NEXT] := deqNext;
          newEntry.3[DEQ_PREV] := map.0;
          deqNext.3[DEQ_PREV] := newEntry;
          map.0.3[DEQ_NEXT] := newEntry;
        } else {
          entry.3[DEQ_NEXT].3[DEQ_PREV] := newEntry;
          map.0.3[DEQ_NEXT] := newEntry;
        };

        parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

        return;
      } else {
        parentEntry := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func putBefore<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, placeKeyParam: K, valueParam: ?V): ?V {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;
    var entry = map.0.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
    var parentEntry = map.0;

    let placeHashParam = hashUtils.0(placeKeyParam);

    var shiftingPlaceHash = placeHashParam;

    var placeEntry = map.0.3[nat(shiftingPlaceHash % HASH_CHUNK_SIZE)];

    loop {
      if (placeEntry.2 == NULL_HASH or placeEntry.2 == placeHashParam and hashUtils.1(placeEntry.0, placeKeyParam)) loop {
        if (entry.2 == NULL_HASH) {
          switch (valueParam) {
            case (null) return null;

            case (_) {
              let deqPrev = placeEntry.3[DEQ_PREV];
              let newEntry = (keyParam, valueParam, hashParam, [var map.0, map.0, map.0, map.0, deqPrev, placeEntry]);

              deqPrev.3[DEQ_NEXT] := newEntry;
              placeEntry.3[DEQ_PREV] := newEntry;
              parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
              map.1[SIZE] +%= 1;

              return null;
            };
          };
        } else if (entry.2 == hashParam and hashUtils.1(entry.0, keyParam)) {
          let newEntry = switch (valueParam) { case (null) entry; case (_) (entry.0, valueParam, entry.2, entry.3) };

          let deqNextOld = entry.3[DEQ_NEXT];

          let deqPrevOld = entry.3[DEQ_PREV];
          let deqPrev = placeEntry.3[DEQ_PREV];

          deqPrevOld.3[DEQ_NEXT] := deqNextOld;
          deqNextOld.3[DEQ_PREV] := deqPrevOld;
          newEntry.3[DEQ_PREV] := deqPrev;
          newEntry.3[DEQ_NEXT] := placeEntry;
          deqPrev.3[DEQ_NEXT] := newEntry;
          placeEntry.3[DEQ_PREV] := newEntry;

          parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

          return entry.1;
        } else {
          parentEntry := entry;
          shiftingHash >>= HASH_OFFSET;
          entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
        };
      } else {
        shiftingPlaceHash >>= HASH_OFFSET;
        placeEntry := placeEntry.3[nat(shiftingPlaceHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func putAfter<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, placeKeyParam: K, valueParam: ?V): ?V {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;
    var entry = map.0.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
    var parentEntry = map.0;

    let placeHashParam = hashUtils.0(placeKeyParam);

    var shiftingPlaceHash = placeHashParam;

    var placeEntry = map.0.3[nat(shiftingPlaceHash % HASH_CHUNK_SIZE)];

    loop {
      if (placeEntry.2 == NULL_HASH or placeEntry.2 == placeHashParam and hashUtils.1(placeEntry.0, placeKeyParam)) loop {
        if (entry.2 == NULL_HASH) {
          switch (valueParam) {
            case (null) return null;

            case (_) {
              let deqNext = placeEntry.3[DEQ_NEXT];
              let newEntry = (keyParam, valueParam, hashParam, [var map.0, map.0, map.0, map.0, placeEntry, deqNext]);

              deqNext.3[DEQ_PREV] := newEntry;
              placeEntry.3[DEQ_NEXT] := newEntry;
              parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
              map.1[SIZE] +%= 1;

              return null;
            };
          };
        } else if (entry.2 == hashParam and hashUtils.1(entry.0, keyParam)) {
          let newEntry = switch (valueParam) { case (null) entry; case (_) (entry.0, valueParam, entry.2, entry.3) };

          let deqPrevOld = entry.3[DEQ_PREV];

          let deqNextOld = entry.3[DEQ_NEXT];
          let deqNext = placeEntry.3[DEQ_NEXT];

          deqNextOld.3[DEQ_PREV] := deqPrevOld;
          deqPrevOld.3[DEQ_NEXT] := deqNextOld;
          newEntry.3[DEQ_NEXT] := deqNext;
          newEntry.3[DEQ_PREV] := placeEntry;
          deqNext.3[DEQ_PREV] := newEntry;
          placeEntry.3[DEQ_NEXT] := newEntry;

          parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

          return entry.1;
        } else {
          parentEntry := entry;
          shiftingHash >>= HASH_OFFSET;
          entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
        };
      } else {
        shiftingPlaceHash >>= HASH_OFFSET;
        placeEntry := placeEntry.3[nat(shiftingPlaceHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func setBefore<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, placeKeyParam: K, valueParam: ?V) {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;
    var entry = map.0.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
    var parentEntry = map.0;

    let placeHashParam = hashUtils.0(placeKeyParam);

    var shiftingPlaceHash = placeHashParam;

    var placeEntry = map.0.3[nat(shiftingPlaceHash % HASH_CHUNK_SIZE)];

    loop {
      if (placeEntry.2 == NULL_HASH or placeEntry.2 == placeHashParam and hashUtils.1(placeEntry.0, placeKeyParam)) loop {
        if (entry.2 == NULL_HASH) {
          switch (valueParam) {
            case (null) return;

            case (_) {
              let deqPrev = placeEntry.3[DEQ_PREV];
              let newEntry = (keyParam, valueParam, hashParam, [var map.0, map.0, map.0, map.0, deqPrev, placeEntry]);

              deqPrev.3[DEQ_NEXT] := newEntry;
              placeEntry.3[DEQ_PREV] := newEntry;
              parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
              map.1[SIZE] +%= 1;

              return;
            };
          };
        } else if (entry.2 == hashParam and hashUtils.1(entry.0, keyParam)) {
          let newEntry = switch (valueParam) { case (null) entry; case (_) (entry.0, valueParam, entry.2, entry.3) };

          let deqNextOld = entry.3[DEQ_NEXT];

          let deqPrevOld = entry.3[DEQ_PREV];
          let deqPrev = placeEntry.3[DEQ_PREV];

          deqPrevOld.3[DEQ_NEXT] := deqNextOld;
          deqNextOld.3[DEQ_PREV] := deqPrevOld;
          newEntry.3[DEQ_PREV] := deqPrev;
          newEntry.3[DEQ_NEXT] := placeEntry;
          deqPrev.3[DEQ_NEXT] := newEntry;
          placeEntry.3[DEQ_PREV] := newEntry;

          parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

          return;
        } else {
          parentEntry := entry;
          shiftingHash >>= HASH_OFFSET;
          entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
        };
      } else {
        shiftingPlaceHash >>= HASH_OFFSET;
        placeEntry := placeEntry.3[nat(shiftingPlaceHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func setAfter<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, placeKeyParam: K, valueParam: ?V) {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;
    var entry = map.0.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
    var parentEntry = map.0;

    let placeHashParam = hashUtils.0(placeKeyParam);

    var shiftingPlaceHash = placeHashParam;

    var placeEntry = map.0.3[nat(shiftingPlaceHash % HASH_CHUNK_SIZE)];

    loop {
      if (placeEntry.2 == NULL_HASH or placeEntry.2 == placeHashParam and hashUtils.1(placeEntry.0, placeKeyParam)) loop {
        if (entry.2 == NULL_HASH) {
          switch (valueParam) {
            case (null) return;

            case (_) {
              let deqNext = placeEntry.3[DEQ_NEXT];
              let newEntry = (keyParam, valueParam, hashParam, [var map.0, map.0, map.0, map.0, placeEntry, deqNext]);

              deqNext.3[DEQ_PREV] := newEntry;
              placeEntry.3[DEQ_NEXT] := newEntry;
              parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
              map.1[SIZE] +%= 1;

              return;
            };
          };
        } else if (entry.2 == hashParam and hashUtils.1(entry.0, keyParam)) {
          let newEntry = switch (valueParam) { case (null) entry; case (_) (entry.0, valueParam, entry.2, entry.3) };

          let deqPrevOld = entry.3[DEQ_PREV];

          let deqNextOld = entry.3[DEQ_NEXT];
          let deqNext = placeEntry.3[DEQ_NEXT];

          deqNextOld.3[DEQ_PREV] := deqPrevOld;
          deqPrevOld.3[DEQ_NEXT] := deqNextOld;
          newEntry.3[DEQ_NEXT] := deqNext;
          newEntry.3[DEQ_PREV] := placeEntry;
          deqNext.3[DEQ_PREV] := newEntry;
          placeEntry.3[DEQ_NEXT] := newEntry;

          parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

          return;
        } else {
          parentEntry := entry;
          shiftingHash >>= HASH_OFFSET;
          entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
        };
      } else {
        shiftingPlaceHash >>= HASH_OFFSET;
        placeEntry := placeEntry.3[nat(shiftingPlaceHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func update<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, getNewValue: (K, ?V) -> V): V {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;
    var entry = map.0.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
    var parentEntry = map.0;

    loop {
      if (entry.2 == NULL_HASH) {
        let newValue = getNewValue(keyParam, null);

        let deqPrev = map.0.3[DEQ_PREV];
        let newEntry = (keyParam, ?newValue, hashParam, [var map.0, map.0, map.0, map.0, deqPrev, map.0]);

        deqPrev.3[DEQ_NEXT] := newEntry;
        map.0.3[DEQ_PREV] := newEntry;
        parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        map.1[SIZE] +%= 1;

        return newValue;
      } else if (entry.2 == hashParam and hashUtils.1(entry.0, keyParam)) {
        let newValue = getNewValue(entry.0, entry.1);

        let newEntry = (entry.0, ?newValue, entry.2, entry.3);

        entry.3[DEQ_PREV].3[DEQ_NEXT] := newEntry;
        entry.3[DEQ_NEXT].3[DEQ_PREV] := newEntry;

        parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

        return newValue;
      } else {
        parentEntry := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func updateFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, getNewValue: (K, ?V) -> V): V {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;
    var entry = map.0.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
    var parentEntry = map.0;

    loop {
      if (entry.2 == NULL_HASH) {
        let newValue = getNewValue(keyParam, null);

        let deqNext = map.0.3[DEQ_NEXT];
        let newEntry = (keyParam, ?newValue, hashParam, [var map.0, map.0, map.0, map.0, map.0, deqNext]);

        deqNext.3[DEQ_PREV] := newEntry;
        map.0.3[DEQ_NEXT] := newEntry;
        parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        map.1[SIZE] +%= 1;

        return newValue;
      } else if (entry.2 == hashParam and hashUtils.1(entry.0, keyParam)) {
        let newValue = getNewValue(entry.0, entry.1);

        let newEntry = (entry.0, ?newValue, entry.2, entry.3);

        entry.3[DEQ_NEXT].3[DEQ_PREV] := newEntry;
        entry.3[DEQ_PREV].3[DEQ_NEXT] := newEntry;

        parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

        return newValue;
      } else {
        parentEntry := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func updateMove<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, getNewValue: (K, ?V) -> V): V {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;
    var entry = map.0.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
    var parentEntry = map.0;

    loop {
      if (entry.2 == NULL_HASH) {
        let newValue = getNewValue(keyParam, null);

        let deqPrev = map.0.3[DEQ_PREV];
        let newEntry = (keyParam, ?newValue, hashParam, [var map.0, map.0, map.0, map.0, deqPrev, map.0]);

        deqPrev.3[DEQ_NEXT] := newEntry;
        map.0.3[DEQ_PREV] := newEntry;
        parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        map.1[SIZE] +%= 1;

        return newValue;
      } else if (entry.2 == hashParam and hashUtils.1(entry.0, keyParam)) {
        let newValue = getNewValue(entry.0, entry.1);

        let newEntry = (entry.0, ?newValue, entry.2, entry.3);

        let deqNextOld = entry.3[DEQ_NEXT];

        if (deqNextOld.2 != NULL_HASH) {
          let deqPrevOld = entry.3[DEQ_PREV];
          let deqPrev = map.0.3[DEQ_PREV];

          deqPrevOld.3[DEQ_NEXT] := deqNextOld;
          deqNextOld.3[DEQ_PREV] := deqPrevOld;
          newEntry.3[DEQ_PREV] := deqPrev;
          newEntry.3[DEQ_NEXT] := map.0;
          deqPrev.3[DEQ_NEXT] := newEntry;
          map.0.3[DEQ_PREV] := newEntry;
        } else {
          entry.3[DEQ_PREV].3[DEQ_NEXT] := newEntry;
          map.0.3[DEQ_PREV] := newEntry;
        };

        parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

        return newValue;
      } else {
        parentEntry := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func updateMoveFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, getNewValue: (K, ?V) -> V): V {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;
    var entry = map.0.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
    var parentEntry = map.0;

    loop {
      if (entry.2 == NULL_HASH) {
        let newValue = getNewValue(keyParam, null);

        let deqNext = map.0.3[DEQ_NEXT];
        let newEntry = (keyParam, ?newValue, hashParam, [var map.0, map.0, map.0, map.0, map.0, deqNext]);

        deqNext.3[DEQ_PREV] := newEntry;
        map.0.3[DEQ_NEXT] := newEntry;
        parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        map.1[SIZE] +%= 1;

        return newValue;
      } else if (entry.2 == hashParam and hashUtils.1(entry.0, keyParam)) {
        let newValue = getNewValue(entry.0, entry.1);

        let newEntry = (entry.0, ?newValue, entry.2, entry.3);

        let deqPrevOld = entry.3[DEQ_PREV];

        if (deqPrevOld.2 != NULL_HASH) {
          let deqNextOld = entry.3[DEQ_NEXT];
          let deqNext = map.0.3[DEQ_NEXT];

          deqNextOld.3[DEQ_PREV] := deqPrevOld;
          deqPrevOld.3[DEQ_NEXT] := deqNextOld;
          newEntry.3[DEQ_NEXT] := deqNext;
          newEntry.3[DEQ_PREV] := map.0;
          deqNext.3[DEQ_PREV] := newEntry;
          map.0.3[DEQ_NEXT] := newEntry;
        } else {
          entry.3[DEQ_NEXT].3[DEQ_PREV] := newEntry;
          map.0.3[DEQ_NEXT] := newEntry;
        };

        parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

        return newValue;
      } else {
        parentEntry := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func remove<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): ?V {
    let hashParam = hashUtils.0(keyParam);

    var shiftingHash = hashParam;
    var entry = map.0.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
    var parentEntry = map.0;

    loop {
      if (entry.2 == hashParam and (hashUtils.1(entry.0, keyParam))) {
        let deqPrev = entry.3[DEQ_PREV];
        let deqNext = entry.3[DEQ_NEXT];
        var leaf = entry;
        var leafParent = parentEntry;
        var leafIndex = NULL_BRANCH;

        deqPrev.3[DEQ_NEXT] := deqNext;
        deqNext.3[DEQ_PREV] := deqPrev;
        map.1[SIZE] -%= 1;

        loop {
          let branch1 = leaf.3[BRANCH_1];

          if (branch1.2 != NULL_HASH) {
            leafParent := leaf;
            leaf := branch1;
            leafIndex := BRANCH_1;
          } else {
            let branch2 = leaf.3[BRANCH_2];

            if (branch2.2 != NULL_HASH) {
              leafParent := leaf;
              leaf := branch2;
              leafIndex := BRANCH_2;
            } else {
              let branch3 = leaf.3[BRANCH_3];

              if (branch3.2 != NULL_HASH) {
                leafParent := leaf;
                leaf := branch3;
                leafIndex := BRANCH_3;
              } else {
                let branch4 = leaf.3[BRANCH_4];

                if (branch4.2 != NULL_HASH) {
                  leafParent := leaf;
                  leaf := branch4;
                  leafIndex := BRANCH_4;
                } else {
                  if (leafIndex == NULL_BRANCH) {
                    parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := map.0;
                  } else {
                    parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := leaf;
                    leafParent.3[leafIndex] := map.0;

                    leaf.3[BRANCH_1] := entry.3[BRANCH_1];
                    leaf.3[BRANCH_2] := entry.3[BRANCH_2];
                    leaf.3[BRANCH_3] := entry.3[BRANCH_3];
                    leaf.3[BRANCH_4] := entry.3[BRANCH_4];
                  };

                  return entry.1;
                };
              };
            };
          };
        };
      } else if (entry.2 == NULL_HASH) {
        return null;
      } else {
        parentEntry := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func delete<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K) {
    let hashParam = hashUtils.0(keyParam);

    var shiftingHash = hashParam;
    var entry = map.0.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
    var parentEntry = map.0;

    loop {
      if (entry.2 == hashParam and (hashUtils.1(entry.0, keyParam))) {
        let deqPrev = entry.3[DEQ_PREV];
        let deqNext = entry.3[DEQ_NEXT];
        var leaf = entry;
        var leafParent = parentEntry;
        var leafIndex = NULL_BRANCH;

        deqPrev.3[DEQ_NEXT] := deqNext;
        deqNext.3[DEQ_PREV] := deqPrev;
        map.1[SIZE] -%= 1;

        loop {
          let branch1 = leaf.3[BRANCH_1];

          if (branch1.2 != NULL_HASH) {
            leafParent := leaf;
            leaf := branch1;
            leafIndex := BRANCH_1;
          } else {
            let branch2 = leaf.3[BRANCH_2];

            if (branch2.2 != NULL_HASH) {
              leafParent := leaf;
              leaf := branch2;
              leafIndex := BRANCH_2;
            } else {
              let branch3 = leaf.3[BRANCH_3];

              if (branch3.2 != NULL_HASH) {
                leafParent := leaf;
                leaf := branch3;
                leafIndex := BRANCH_3;
              } else {
                let branch4 = leaf.3[BRANCH_4];

                if (branch4.2 != NULL_HASH) {
                  leafParent := leaf;
                  leaf := branch4;
                  leafIndex := BRANCH_4;
                } else {
                  if (leafIndex == NULL_BRANCH) {
                    parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := map.0;
                  } else {
                    parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := leaf;
                    leafParent.3[leafIndex] := map.0;

                    leaf.3[BRANCH_1] := entry.3[BRANCH_1];
                    leaf.3[BRANCH_2] := entry.3[BRANCH_2];
                    leaf.3[BRANCH_3] := entry.3[BRANCH_3];
                    leaf.3[BRANCH_4] := entry.3[BRANCH_4];
                  };

                  return;
                };
              };
            };
          };
        };
      } else if (entry.2 == NULL_HASH) {
        return;
      } else {
        parentEntry := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func pop<K, V>(map: Map<K, V>): ?(K, V) {
    let deqLast = map.0.3[DEQ_PREV];

    switch (deqLast.1) {
      case (?someValue) {
        var shiftingHash = deqLast.2;
        var entry = map.0.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
        var parentEntry = map.0;

        loop {
          if (entry.2 == deqLast.2 and (entry.3[DEQ_NEXT].2 == NULL_HASH)) {
            let deqPrev = entry.3[DEQ_PREV];

            var leaf = entry;
            var leafParent = parentEntry;
            var leafIndex = NULL_BRANCH;

            deqPrev.3[DEQ_NEXT] := map.0;
            map.0.3[DEQ_PREV] := deqPrev;
            map.1[SIZE] -%= 1;

            loop {
              let branch1 = leaf.3[BRANCH_1];

              if (branch1.2 != NULL_HASH) {
                leafParent := leaf;
                leaf := branch1;
                leafIndex := BRANCH_1;
              } else {
                let branch2 = leaf.3[BRANCH_2];

                if (branch2.2 != NULL_HASH) {
                  leafParent := leaf;
                  leaf := branch2;
                  leafIndex := BRANCH_2;
                } else {
                  let branch3 = leaf.3[BRANCH_3];

                  if (branch3.2 != NULL_HASH) {
                    leafParent := leaf;
                    leaf := branch3;
                    leafIndex := BRANCH_3;
                  } else {
                    let branch4 = leaf.3[BRANCH_4];

                    if (branch4.2 != NULL_HASH) {
                      leafParent := leaf;
                      leaf := branch4;
                      leafIndex := BRANCH_4;
                    } else {
                      if (leafIndex == NULL_BRANCH) {
                        parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := map.0;
                      } else {
                        parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := leaf;
                        leafParent.3[leafIndex] := map.0;

                        leaf.3[BRANCH_1] := entry.3[BRANCH_1];
                        leaf.3[BRANCH_2] := entry.3[BRANCH_2];
                        leaf.3[BRANCH_3] := entry.3[BRANCH_3];
                        leaf.3[BRANCH_4] := entry.3[BRANCH_4];
                      };

                      return ?(entry.0, someValue);
                    };
                  };
                };
              };
            };
          } else {
            parentEntry := entry;
            shiftingHash >>= HASH_OFFSET;
            entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
          };
        };
      };

      case (_) null;
    };
  };

  public func popFront<K, V>(map: Map<K, V>): ?(K, V) {
    let deqFirst = map.0.3[DEQ_NEXT];

    switch (deqFirst.1) {
      case (?someValue) {
        var shiftingHash = deqFirst.2;
        var entry = map.0.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
        var parentEntry = map.0;

        loop {
          if (entry.2 == deqFirst.2 and (entry.3[DEQ_PREV].2 == NULL_HASH)) {
            let deqNext = entry.3[DEQ_NEXT];

            var leaf = entry;
            var leafParent = parentEntry;
            var leafIndex = NULL_BRANCH;

            deqNext.3[DEQ_PREV] := map.0;
            map.0.3[DEQ_NEXT] := deqNext;
            map.1[SIZE] -%= 1;

            loop {
              let branch1 = leaf.3[BRANCH_1];

              if (branch1.2 != NULL_HASH) {
                leafParent := leaf;
                leaf := branch1;
                leafIndex := BRANCH_1;
              } else {
                let branch2 = leaf.3[BRANCH_2];

                if (branch2.2 != NULL_HASH) {
                  leafParent := leaf;
                  leaf := branch2;
                  leafIndex := BRANCH_2;
                } else {
                  let branch3 = leaf.3[BRANCH_3];

                  if (branch3.2 != NULL_HASH) {
                    leafParent := leaf;
                    leaf := branch3;
                    leafIndex := BRANCH_3;
                  } else {
                    let branch4 = leaf.3[BRANCH_4];

                    if (branch4.2 != NULL_HASH) {
                      leafParent := leaf;
                      leaf := branch4;
                      leafIndex := BRANCH_4;
                    } else {
                      if (leafIndex == NULL_BRANCH) {
                        parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := map.0;
                      } else {
                        parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := leaf;
                        leafParent.3[leafIndex] := map.0;

                        leaf.3[BRANCH_1] := entry.3[BRANCH_1];
                        leaf.3[BRANCH_2] := entry.3[BRANCH_2];
                        leaf.3[BRANCH_3] := entry.3[BRANCH_3];
                        leaf.3[BRANCH_4] := entry.3[BRANCH_4];
                      };

                      return ?(entry.0, someValue);
                    };
                  };
                };
              };
            };
          } else {
            parentEntry := entry;
            shiftingHash >>= HASH_OFFSET;
            entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
          };
        };
      };

      case (_) null;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func peek<K, V>(map: Map<K, V>): ?(K, V) {
    let entry = map.0.3[DEQ_PREV];

    switch (entry.1) { case (?someValue) ?(entry.0, someValue); case (_) null };
  };

  public func peekFront<K, V>(map: Map<K, V>): ?(K, V) {
    let entry = map.0.3[DEQ_NEXT];

    switch (entry.1) { case (?someValue) ?(entry.0, someValue); case (_) null };
  };

  public func cycle<K, V>(map: Map<K, V>): ?(K, V) {
    let entry = map.0.3[DEQ_PREV];

    switch (entry.1) {
      case (?someValue) {
        let deqPrev = entry.3[DEQ_PREV];

        if (deqPrev.2 != NULL_HASH) {
          let deqFirst = map.0.3[DEQ_NEXT];

          deqPrev.3[DEQ_NEXT] := map.0;
          map.0.3[DEQ_PREV] := deqPrev;
          deqFirst.3[DEQ_PREV] := entry;
          map.0.3[DEQ_NEXT] := entry;
          entry.3[DEQ_PREV] := map.0;
          entry.3[DEQ_NEXT] := deqFirst;
        };

        ?(entry.0, someValue);
      };

      case (_) null;
    };
  };

  public func cycleFront<K, V>(map: Map<K, V>): ?(K, V) {
    let entry = map.0.3[DEQ_NEXT];

    switch (entry.1) {
      case (?someValue) {
        let deqNext = entry.3[DEQ_NEXT];

        if (deqNext.2 != NULL_HASH) {
          let deqLast = map.0.3[DEQ_PREV];

          deqNext.3[DEQ_PREV] := map.0;
          map.0.3[DEQ_NEXT] := deqNext;
          deqLast.3[DEQ_NEXT] := entry;
          map.0.3[DEQ_PREV] := entry;
          entry.3[DEQ_NEXT] := map.0;
          entry.3[DEQ_PREV] := deqLast;
        };

        ?(entry.0, someValue);
      };

      case (_) null;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func mapFilter<K, V1, V2>(map: Map<K, V1>, mapEntry: (K, V1) -> ?V2): Map<K, V2> {
    let newEdge = createEdgeEntry<K, V2>(map.0.0);
    var entry = map.0.3[DEQ_NEXT];
    var deqPrev = newEdge;
    var newSize = 0:Nat32;

    loop {
      switch (entry.1) {
        case (?someValue) switch (mapEntry(entry.0, someValue)) {
          case (null) entry := entry.3[DEQ_NEXT];

          case (newValue) {
            var shiftingHash = entry.2;
            var searchEntry = newEdge.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
            var parentEntry = newEdge;

            while (searchEntry.2 != NULL_HASH) {
              parentEntry := searchEntry;
              shiftingHash >>= HASH_OFFSET;
              searchEntry := searchEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
            };

            let newEntry = (entry.0, newValue, entry.2, [var newEdge, newEdge, newEdge, newEdge, deqPrev, newEdge]);

            deqPrev.3[DEQ_NEXT] := newEntry;
            parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
            deqPrev := newEntry;
            entry := entry.3[DEQ_NEXT];
            newSize +%= 1;
          };
        };

        case (_) {
          newEdge.3[DEQ_PREV] := deqPrev;

          return (newEdge, [var newSize]);
        };
      };
    };
  };

  public func mapFilterDesc<K, V1, V2>(map: Map<K, V1>, mapEntry: (K, V1) -> ?V2): Map<K, V2> {
    let newEdge = createEdgeEntry<K, V2>(map.0.0);
    var entry = map.0.3[DEQ_PREV];
    var deqNext = newEdge;
    var newSize = 0:Nat32;

    loop {
      switch (entry.1) {
        case (?someValue) switch (mapEntry(entry.0, someValue)) {
          case (null) entry := entry.3[DEQ_PREV];

          case (newValue) {
            var shiftingHash = entry.2;
            var searchEntry = newEdge.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
            var parentEntry = newEdge;

            while (searchEntry.2 != NULL_HASH) {
              parentEntry := searchEntry;
              shiftingHash >>= HASH_OFFSET;
              searchEntry := searchEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
            };

            let newEntry = (entry.0, newValue, entry.2, [var newEdge, newEdge, newEdge, newEdge, newEdge, deqNext]);

            deqNext.3[DEQ_PREV] := newEntry;
            parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
            deqNext := newEntry;
            entry := entry.3[DEQ_PREV];
            newSize +%= 1;
          };
        };

        case (_) {
          newEdge.3[DEQ_NEXT] := deqNext;

          return (newEdge, [var newSize]);
        };
      };
    };
  };

  public func map<K, V1, V2>(map: Map<K, V1>, mapEntry: (K, V1) -> V2): Map<K, V2> {
    let newEdge = createEdgeEntry<K, V2>(map.0.0);
    var entry = map.0.3[DEQ_NEXT];
    var deqPrev = newEdge;
    var newSize = 0:Nat32;

    loop {
      switch (entry.1) {
        case (?someValue) {
          var shiftingHash = entry.2;
          var searchEntry = newEdge.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
          var parentEntry = newEdge;

          while (searchEntry.2 != NULL_HASH) {
            parentEntry := searchEntry;
            shiftingHash >>= HASH_OFFSET;
            searchEntry := searchEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
          };

          let newEntry = (entry.0, ?mapEntry(entry.0, someValue), entry.2, [var newEdge, newEdge, newEdge, newEdge, deqPrev, newEdge]);

          deqPrev.3[DEQ_NEXT] := newEntry;
          parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
          deqPrev := newEntry;
          entry := entry.3[DEQ_NEXT];
          newSize +%= 1;
        };

        case (_) {
          newEdge.3[DEQ_PREV] := deqPrev;

          return (newEdge, [var newSize]);
        };
      };
    };
  };

  public func mapDesc<K, V1, V2>(map: Map<K, V1>, mapEntry: (K, V1) -> V2): Map<K, V2> {
    let newEdge = createEdgeEntry<K, V2>(map.0.0);
    var entry = map.0.3[DEQ_PREV];
    var deqNext = newEdge;
    var newSize = 0:Nat32;

    loop {
      switch (entry.1) {
        case (?someValue) {
          var shiftingHash = entry.2;
          var searchEntry = newEdge.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
          var parentEntry = newEdge;

          while (searchEntry.2 != NULL_HASH) {
            parentEntry := searchEntry;
            shiftingHash >>= HASH_OFFSET;
            searchEntry := searchEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
          };

          let newEntry = (entry.0, ?mapEntry(entry.0, someValue), entry.2, [var newEdge, newEdge, newEdge, newEdge, newEdge, deqNext]);

          deqNext.3[DEQ_PREV] := newEntry;
          parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
          deqNext := newEntry;
          entry := entry.3[DEQ_PREV];
          newSize +%= 1;
        };

        case (_) {
          newEdge.3[DEQ_NEXT] := deqNext;

          return (newEdge, [var newSize]);
        };
      };
    };
  };

  public func filter<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool): Map<K, V> {
    let newEdge = createEdgeEntry<K, V>(map.0.0);
    var entry = map.0.3[DEQ_NEXT];
    var deqPrev = newEdge;
    var newSize = 0:Nat32;

    loop {
      switch (entry.1) {
        case (?someValue) {
          if (acceptEntry(entry.0, someValue)) {
            var shiftingHash = entry.2;
            var searchEntry = newEdge.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
            var parentEntry = newEdge;

            while (searchEntry.2 != NULL_HASH) {
              parentEntry := searchEntry;
              shiftingHash >>= HASH_OFFSET;
              searchEntry := searchEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
            };

            let newEntry = (entry.0, entry.1, entry.2, [var newEdge, newEdge, newEdge, newEdge, deqPrev, newEdge]);

            deqPrev.3[DEQ_NEXT] := newEntry;
            parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
            deqPrev := newEntry;
            entry := entry.3[DEQ_NEXT];
            newSize +%= 1;
          } else {
            entry := entry.3[DEQ_NEXT];
          };
        };

        case (_) {
          newEdge.3[DEQ_PREV] := deqPrev;

          return (newEdge, [var newSize]);
        };
      };
    };
  };

  public func filterDesc<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool): Map<K, V> {
    let newEdge = createEdgeEntry<K, V>(map.0.0);
    var entry = map.0.3[DEQ_PREV];
    var deqNext = newEdge;
    var newSize = 0:Nat32;

    loop {
      switch (entry.1) {
        case (?someValue) {
          if (acceptEntry(entry.0, someValue)) {
            var shiftingHash = entry.2;
            var searchEntry = newEdge.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
            var parentEntry = newEdge;

            while (searchEntry.2 != NULL_HASH) {
              parentEntry := searchEntry;
              shiftingHash >>= HASH_OFFSET;
              searchEntry := searchEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
            };

            let newEntry = (entry.0, entry.1, entry.2, [var newEdge, newEdge, newEdge, newEdge, newEdge, deqNext]);

            deqNext.3[DEQ_PREV] := newEntry;
            parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
            deqNext := newEntry;
            entry := entry.3[DEQ_PREV];
            newSize +%= 1;
          } else {
            entry := entry.3[DEQ_PREV];
          };
        };

        case (_) {
          newEdge.3[DEQ_NEXT] := deqNext;

          return (newEdge, [var newSize]);
        };
      };
    };
  };

  public func clone<K, V>(map: Map<K, V>): Map<K, V> {
    let newEdge = createEdgeEntry<K, V>(map.0.0);
    var entry = map.0.3[DEQ_NEXT];
    var deqPrev = newEdge;
    var newSize = 0:Nat32;

    loop {
      if (entry.2 == NULL_HASH) {
        newEdge.3[DEQ_PREV] := deqPrev;

        return (newEdge, [var newSize]);
      } else {
        var shiftingHash = entry.2;
        var searchEntry = newEdge.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
        var parentEntry = newEdge;

        while (searchEntry.2 != NULL_HASH) {
          parentEntry := searchEntry;
          shiftingHash >>= HASH_OFFSET;
          searchEntry := searchEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
        };

        let newEntry = (entry.0, entry.1, entry.2, [var newEdge, newEdge, newEdge, newEdge, deqPrev, newEdge]);

        deqPrev.3[DEQ_NEXT] := newEntry;
        parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        deqPrev := newEntry;
        entry := entry.3[DEQ_NEXT];
        newSize +%= 1;
      };
    };
  };

  public func cloneDesc<K, V>(map: Map<K, V>): Map<K, V> {
    let newEdge = createEdgeEntry<K, V>(map.0.0);
    var entry = map.0.3[DEQ_PREV];
    var deqNext = newEdge;
    var newSize = 0:Nat32;

    loop {
      if (entry.2 == NULL_HASH) {
        newEdge.3[DEQ_NEXT] := deqNext;

        return (newEdge, [var newSize]);
      } else {
        var shiftingHash = entry.2;
        var searchEntry = newEdge.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
        var parentEntry = newEdge;

        while (searchEntry.2 != NULL_HASH) {
          parentEntry := searchEntry;
          shiftingHash >>= HASH_OFFSET;
          searchEntry := searchEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
        };

        let newEntry = (entry.0, entry.1, entry.2, [var newEdge, newEdge, newEdge, newEdge, newEdge, deqNext]);

        deqNext.3[DEQ_PREV] := newEntry;
        parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        deqNext := newEntry;
        entry := entry.3[DEQ_PREV];
        newSize +%= 1;
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func keys<K, V>(map: Map<K, V>): Iter<K> {
    var entry = map.0;

    let iter = {
      prev = func(): ?K {
        entry := entry.3[DEQ_PREV];

        if (entry.2 == NULL_HASH) null else ?entry.0;
      };

      next = func(): ?K {
        entry := entry.3[DEQ_NEXT];

        if (entry.2 == NULL_HASH) null else ?entry.0;
      };

      current = func(): ?K {
        if (entry.2 == NULL_HASH) null else ?entry.0;
      };

      reset = func(): Iter<K> {
        entry := map.0;

        iter;
      };

      movePrev = func(): Iter<K> {
        entry := entry.3[DEQ_PREV];

        iter;
      };

      moveNext = func(): Iter<K> {
        entry := entry.3[DEQ_NEXT];

        iter;
      };
    };
  };

  public func keysDesc<K, V>(map: Map<K, V>): Iter<K> {
    var entry = map.0;

    let iter = {
      prev = func(): ?K {
        entry := entry.3[DEQ_NEXT];

        if (entry.2 == NULL_HASH) null else ?entry.0;
      };

      next = func(): ?K {
        entry := entry.3[DEQ_PREV];

        if (entry.2 == NULL_HASH) null else ?entry.0;
      };

      current = func(): ?K {
        if (entry.2 == NULL_HASH) null else ?entry.0;
      };

      reset = func(): Iter<K> {
        entry := map.0;

        iter;
      };

      movePrev = func(): Iter<K> {
        entry := entry.3[DEQ_NEXT];

        iter;
      };

      moveNext = func(): Iter<K> {
        entry := entry.3[DEQ_PREV];

        iter;
      };
    };
  };

  public func vals<K, V>(map: Map<K, V>): Iter<V> {
    var entry = map.0;

    let iter = {
      prev = func(): ?V {
        entry := entry.3[DEQ_PREV];

        if (entry.2 == NULL_HASH) null else entry.1;
      };

      next = func(): ?V {
        entry := entry.3[DEQ_NEXT];

        if (entry.2 == NULL_HASH) null else entry.1;
      };

      current = func(): ?V {
        if (entry.2 == NULL_HASH) null else entry.1;
      };

      reset = func(): Iter<V> {
        entry := map.0;

        iter;
      };

      movePrev = func(): Iter<V> {
        entry := entry.3[DEQ_PREV];

        iter;
      };

      moveNext = func(): Iter<V> {
        entry := entry.3[DEQ_NEXT];

        iter;
      };
    };
  };

  public func valsDesc<K, V>(map: Map<K, V>): Iter<V> {
    var entry = map.0;

    let iter = {
      prev = func(): ?V {
        entry := entry.3[DEQ_NEXT];

        if (entry.2 == NULL_HASH) null else entry.1;
      };

      next = func(): ?V {
        entry := entry.3[DEQ_PREV];

        if (entry.2 == NULL_HASH) null else entry.1;
      };

      current = func(): ?V {
        if (entry.2 == NULL_HASH) null else entry.1;
      };

      reset = func(): Iter<V> {
        entry := map.0;

        iter;
      };

      movePrev = func(): Iter<V> {
        entry := entry.3[DEQ_NEXT];

        iter;
      };

      moveNext = func(): Iter<V> {
        entry := entry.3[DEQ_PREV];

        iter;
      };
    };
  };

  public func entries<K, V>(map: Map<K, V>): Iter<(K, V)> {
    var entry = map.0;

    let iter = {
      prev = func(): ?(K, V) {
        entry := entry.3[DEQ_PREV];

        switch (entry.1) { case (?someValue) ?(entry.0, someValue); case (_) null };
      };

      next = func(): ?(K, V) {
        entry := entry.3[DEQ_NEXT];

        switch (entry.1) { case (?someValue) ?(entry.0, someValue); case (_) null };
      };

      current = func(): ?(K, V) {
        switch (entry.1) { case (?someValue) ?(entry.0, someValue); case (_) null };
      };

      reset = func(): Iter<(K, V)> {
        entry := map.0;

        iter;
      };

      movePrev = func(): Iter<(K, V)> {
        entry := entry.3[DEQ_PREV];

        iter;
      };

      moveNext = func(): Iter<(K, V)> {
        entry := entry.3[DEQ_NEXT];

        iter;
      };
    };
  };

  public func entriesDesc<K, V>(map: Map<K, V>): Iter<(K, V)> {
    var entry = map.0;

    let iter = {
      prev = func(): ?(K, V) {
        entry := entry.3[DEQ_NEXT];

        switch (entry.1) { case (?someValue) ?(entry.0, someValue); case (_) null };
      };

      next = func(): ?(K, V) {
        entry := entry.3[DEQ_PREV];

        switch (entry.1) { case (?someValue) ?(entry.0, someValue); case (_) null };
      };

      current = func(): ?(K, V) {
        switch (entry.1) { case (?someValue) ?(entry.0, someValue); case (_) null };
      };

      reset = func(): Iter<(K, V)> {
        entry := map.0;

        iter;
      };

      movePrev = func(): Iter<(K, V)> {
        entry := entry.3[DEQ_NEXT];

        iter;
      };

      moveNext = func(): Iter<(K, V)> {
        entry := entry.3[DEQ_PREV];

        iter;
      };
    };
  };

  public func keysFrom<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): Iter<K> {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;

    var entry = map.0.3[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      if (entry.2 == NULL_HASH or entry.2 == hashParam and hashUtils.1(entry.0, keyParam)) return let iter = {
        prev = func(): ?K {
          entry := entry.3[DEQ_PREV];

          if (entry.2 == NULL_HASH) null else ?entry.0;
        };

        next = func(): ?K {
          entry := entry.3[DEQ_NEXT];

          if (entry.2 == NULL_HASH) null else ?entry.0;
        };

        current = func(): ?K {
          if (entry.2 == NULL_HASH) null else ?entry.0;
        };

        reset = func(): Iter<K> {
          entry := map.0;

          iter;
        };

        movePrev = func(): Iter<K> {
          entry := entry.3[DEQ_PREV];

          iter;
        };

        moveNext = func(): Iter<K> {
          entry := entry.3[DEQ_NEXT];

          iter;
        };
      } else {
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func keysFromDesc<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): Iter<K> {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;

    var entry = map.0.3[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      if (entry.2 == NULL_HASH or entry.2 == hashParam and hashUtils.1(entry.0, keyParam)) return let iter = {
        prev = func(): ?K {
          entry := entry.3[DEQ_NEXT];

          if (entry.2 == NULL_HASH) null else ?entry.0;
        };

        next = func(): ?K {
          entry := entry.3[DEQ_PREV];

          if (entry.2 == NULL_HASH) null else ?entry.0;
        };

        current = func(): ?K {
          if (entry.2 == NULL_HASH) null else ?entry.0;
        };

        reset = func(): Iter<K> {
          entry := map.0;

          iter;
        };

        movePrev = func(): Iter<K> {
          entry := entry.3[DEQ_NEXT];

          iter;
        };

        moveNext = func(): Iter<K> {
          entry := entry.3[DEQ_PREV];

          iter;
        };
      } else {
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func valsFrom<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): Iter<V> {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;

    var entry = map.0.3[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      if (entry.2 == NULL_HASH or entry.2 == hashParam and hashUtils.1(entry.0, keyParam)) return let iter = {
        prev = func(): ?V {
          entry := entry.3[DEQ_PREV];

          if (entry.2 == NULL_HASH) null else entry.1;
        };

        next = func(): ?V {
          entry := entry.3[DEQ_NEXT];

          if (entry.2 == NULL_HASH) null else entry.1;
        };

        current = func(): ?V {
          if (entry.2 == NULL_HASH) null else entry.1;
        };

        reset = func(): Iter<V> {
          entry := map.0;

          iter;
        };

        movePrev = func(): Iter<V> {
          entry := entry.3[DEQ_PREV];

          iter;
        };

        moveNext = func(): Iter<V> {
          entry := entry.3[DEQ_NEXT];

          iter;
        };
      } else {
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func valsFromDesc<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): Iter<V> {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;

    var entry = map.0.3[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      if (entry.2 == NULL_HASH or entry.2 == hashParam and hashUtils.1(entry.0, keyParam)) return let iter = {
        prev = func(): ?V {
          entry := entry.3[DEQ_NEXT];

          if (entry.2 == NULL_HASH) null else entry.1;
        };

        next = func(): ?V {
          entry := entry.3[DEQ_PREV];

          if (entry.2 == NULL_HASH) null else entry.1;
        };

        current = func(): ?V {
          if (entry.2 == NULL_HASH) null else entry.1;
        };

        reset = func(): Iter<V> {
          entry := map.0;

          iter;
        };

        movePrev = func(): Iter<V> {
          entry := entry.3[DEQ_NEXT];

          iter;
        };

        moveNext = func(): Iter<V> {
          entry := entry.3[DEQ_PREV];

          iter;
        };
      } else {
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func entriesFrom<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): Iter<(K, V)> {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;

    var entry = map.0.3[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      if (entry.2 == NULL_HASH or entry.2 == hashParam and hashUtils.1(entry.0, keyParam)) return let iter = {
        prev = func(): ?(K, V) {
          entry := entry.3[DEQ_PREV];

          switch (entry.1) { case (?someValue) ?(entry.0, someValue); case (_) null };
        };

        next = func(): ?(K, V) {
          entry := entry.3[DEQ_NEXT];

          switch (entry.1) { case (?someValue) ?(entry.0, someValue); case (_) null };
        };

        current = func(): ?(K, V) {
          switch (entry.1) { case (?someValue) ?(entry.0, someValue); case (_) null };
        };

        reset = func(): Iter<(K, V)> {
          entry := map.0;

          iter;
        };

        movePrev = func(): Iter<(K, V)> {
          entry := entry.3[DEQ_PREV];

          iter;
        };

        moveNext = func(): Iter<(K, V)> {
          entry := entry.3[DEQ_NEXT];

          iter;
        };
      } else {
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func entriesFromDesc<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): Iter<(K, V)> {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;

    var entry = map.0.3[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      if (entry.2 == NULL_HASH or entry.2 == hashParam and hashUtils.1(entry.0, keyParam)) return let iter = {
        prev = func(): ?(K, V) {
          entry := entry.3[DEQ_NEXT];

          switch (entry.1) { case (?someValue) ?(entry.0, someValue); case (_) null };
        };

        next = func(): ?(K, V) {
          entry := entry.3[DEQ_PREV];

          switch (entry.1) { case (?someValue) ?(entry.0, someValue); case (_) null };
        };

        current = func(): ?(K, V) {
          switch (entry.1) { case (?someValue) ?(entry.0, someValue); case (_) null };
        };

        reset = func(): Iter<(K, V)> {
          entry := map.0;

          iter;
        };

        movePrev = func(): Iter<(K, V)> {
          entry := entry.3[DEQ_NEXT];

          iter;
        };

        moveNext = func(): Iter<(K, V)> {
          entry := entry.3[DEQ_PREV];

          iter;
        };
      } else {
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func find<K, V>(map: Map<K, V>, mapEntry: (K, V) -> Bool): ?(K, V) {
    var entry = map.0;

    loop {
      entry := entry.3[DEQ_NEXT];

      switch (entry.1) {
        case (?someValue) if (mapEntry(entry.0, someValue)) return ?(entry.0, someValue);
        case (_) return null;
      };
    };
  };

  public func findDesc<K, V>(map: Map<K, V>, mapEntry: (K, V) -> Bool): ?(K, V) {
    var entry = map.0;

    loop {
      entry := entry.3[DEQ_PREV];

      switch (entry.1) {
        case (?someValue) if (mapEntry(entry.0, someValue)) return ?(entry.0, someValue);
        case (_) return null;
      };
    };
  };

  public func some<K, V>(map: Map<K, V>, mapEntry: (K, V) -> Bool): Bool {
    var entry = map.0;

    loop {
      entry := entry.3[DEQ_NEXT];

      switch (entry.1) {
        case (?someValue) if (mapEntry(entry.0, someValue)) return true;
        case (_) return false;
      };
    };
  };

  public func someDesc<K, V>(map: Map<K, V>, mapEntry: (K, V) -> Bool): Bool {
    var entry = map.0;

    loop {
      entry := entry.3[DEQ_PREV];

      switch (entry.1) {
        case (?someValue) if (mapEntry(entry.0, someValue)) return true;
        case (_) return false;
      };
    };
  };

  public func every<K, V>(map: Map<K, V>, mapEntry: (K, V) -> Bool): Bool {
    var entry = map.0;

    loop {
      entry := entry.3[DEQ_NEXT];

      switch (entry.1) {
        case (?someValue) if (not mapEntry(entry.0, someValue)) return false;
        case (_) return true;
      };
    };
  };

  public func everyDesc<K, V>(map: Map<K, V>, mapEntry: (K, V) -> Bool): Bool {
    var entry = map.0;

    loop {
      entry := entry.3[DEQ_PREV];

      switch (entry.1) {
        case (?someValue) if (not mapEntry(entry.0, someValue)) return false;
        case (_) return true;
      };
    };
  };

  public func forEach<K, V>(map: Map<K, V>, mapEntry: (K, V) -> ()) {
    var entry = map.0;

    loop {
      entry := entry.3[DEQ_NEXT];

      switch (entry.1) {
        case (?someValue) mapEntry(entry.0, someValue);
        case (_) return;
      };
    };
  };

  public func forEachDesc<K, V>(map: Map<K, V>, mapEntry: (K, V) -> ()) {
    var entry = map.0;

    loop {
      entry := entry.3[DEQ_PREV];

      switch (entry.1) {
        case (?someValue) mapEntry(entry.0, someValue);
        case (_) return;
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func fromIter<K, V>(iter: IterNext<(K, V)>, hashUtils: HashUtils<K>): Map<K, V> {
    let map = (createEdgeEntry<K, V>(hashUtils.2()), [var 0:Nat32]);

    label fromIterLoop for (item in iter) {
      let hashParam = hashUtils.0(item.0);
      var shiftingHash = hashParam;
      var entry = map.0.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      var parentEntry = map.0;

      loop {
        if (entry.2 == NULL_HASH) {
          let deqPrev = map.0.3[DEQ_PREV];
          let newEntry = (item.0, ?item.1, hashParam, [var map.0, map.0, map.0, map.0, deqPrev, map.0]);

          deqPrev.3[DEQ_NEXT] := newEntry;
          map.0.3[DEQ_PREV] := newEntry;
          parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
          map.1[SIZE] +%= 1;

          continue fromIterLoop;
        } else if (entry.2 == hashParam and hashUtils.1(entry.0, item.0)) {
          let newEntry = (entry.0, ?item.1, entry.2, entry.3);

          entry.3[DEQ_PREV].3[DEQ_NEXT] := newEntry;
          entry.3[DEQ_NEXT].3[DEQ_PREV] := newEntry;

          parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

          continue fromIterLoop;
        } else {
          parentEntry := entry;
          shiftingHash >>= HASH_OFFSET;
          entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
        };
      };
    };

    map;
  };

  public func fromIterDesc<K, V>(iter: IterNext<(K, V)>, hashUtils: HashUtils<K>): Map<K, V> {
    let map = (createEdgeEntry<K, V>(hashUtils.2()), [var 0:Nat32]);

    label fromIterLoop for (item in iter) {
      let hashParam = hashUtils.0(item.0);
      var shiftingHash = hashParam;
      var entry = map.0.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      var parentEntry = map.0;

      loop {
        if (entry.2 == NULL_HASH) {
          let deqNext = map.0.3[DEQ_NEXT];
          let newEntry = (item.0, ?item.1, hashParam, [var map.0, map.0, map.0, map.0, map.0, deqNext]);

          deqNext.3[DEQ_PREV] := newEntry;
          map.0.3[DEQ_NEXT] := newEntry;
          parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
          map.1[SIZE] +%= 1;

          continue fromIterLoop;
        } else if (entry.2 == hashParam and hashUtils.1(entry.0, item.0)) {
          let newEntry = (entry.0, ?item.1, entry.2, entry.3);

          entry.3[DEQ_NEXT].3[DEQ_PREV] := newEntry;
          entry.3[DEQ_PREV].3[DEQ_NEXT] := newEntry;

          parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

          continue fromIterLoop;
        } else {
          parentEntry := entry;
          shiftingHash >>= HASH_OFFSET;
          entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
        };
      };
    };

    map;
  };

  public func fromIterMap<K, V, T>(iter: IterNext<T>, hashUtils: HashUtils<K>, mapItem: (T) -> ?(K, V)): Map<K, V> {
    let map = (createEdgeEntry<K, V>(hashUtils.2()), [var 0:Nat32]);

    label fromIterLoop for (item in iter) switch (mapItem(item)) {
      case (?item) {
        let hashParam = hashUtils.0(item.0);
        var shiftingHash = hashParam;
        var entry = map.0.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
        var parentEntry = map.0;

        loop {
          if (entry.2 == NULL_HASH) {
            let deqPrev = map.0.3[DEQ_PREV];
            let newEntry = (item.0, ?item.1, hashParam, [var map.0, map.0, map.0, map.0, deqPrev, map.0]);

            deqPrev.3[DEQ_NEXT] := newEntry;
            map.0.3[DEQ_PREV] := newEntry;
            parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
            map.1[SIZE] +%= 1;

            continue fromIterLoop;
          } else if (entry.2 == hashParam and hashUtils.1(entry.0, item.0)) {
            let newEntry = (entry.0, ?item.1, entry.2, entry.3);

            entry.3[DEQ_PREV].3[DEQ_NEXT] := newEntry;
            entry.3[DEQ_NEXT].3[DEQ_PREV] := newEntry;

            parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

            continue fromIterLoop;
          } else {
            parentEntry := entry;
            shiftingHash >>= HASH_OFFSET;
            entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
          };
        };
      };

      case (_) {};
    };

    map;
  };

  public func fromIterMapDesc<K, V, T>(iter: IterNext<T>, hashUtils: HashUtils<K>, mapItem: (T) -> ?(K, V)): Map<K, V> {
    let map = (createEdgeEntry<K, V>(hashUtils.2()), [var 0:Nat32]);

    label fromIterLoop for (item in iter) switch (mapItem(item)) {
      case (?item) {
        let hashParam = hashUtils.0(item.0);
        var shiftingHash = hashParam;
        var entry = map.0.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
        var parentEntry = map.0;

        loop {
          if (entry.2 == NULL_HASH) {
            let deqNext = map.0.3[DEQ_NEXT];
            let newEntry = (item.0, ?item.1, hashParam, [var map.0, map.0, map.0, map.0, map.0, deqNext]);

            deqNext.3[DEQ_PREV] := newEntry;
            map.0.3[DEQ_NEXT] := newEntry;
            parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
            map.1[SIZE] +%= 1;

            continue fromIterLoop;
          } else if (entry.2 == hashParam and hashUtils.1(entry.0, item.0)) {
            let newEntry = (entry.0, ?item.1, entry.2, entry.3);

            entry.3[DEQ_NEXT].3[DEQ_PREV] := newEntry;
            entry.3[DEQ_PREV].3[DEQ_NEXT] := newEntry;

            parentEntry.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

            continue fromIterLoop;
          } else {
            parentEntry := entry;
            shiftingHash >>= HASH_OFFSET;
            entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
          };
        };
      };

      case (_) {};
    };

    map;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func toArray<K, V>(map: Map<K, V>): [(K, V)] {
    var entry = map.0;

    tabulateArray<(K, V)>(nat(map.1[SIZE]), func(i) {
      entry := entry.3[DEQ_NEXT];

      (entry.0, switch (entry.1) { case (?value) value; case (_) noop() });
    });
  };

  public func toArrayDesc<K, V>(map: Map<K, V>): [(K, V)] {
    var entry = map.0;

    tabulateArray<(K, V)>(nat(map.1[SIZE]), func(i) {
      entry := entry.3[DEQ_PREV];

      (entry.0, switch (entry.1) { case (?value) value; case (_) noop() });
    });
  };

  public func toArrayMap<K, V, T>(map: Map<K, V>, mapEntry: (K, V) -> ?T): [T] {
    let array = initArray<?T>(nat(map.1[SIZE]), null);

    var entry = map.0.3[DEQ_NEXT];

    var arraySize = 0:Nat32;

    loop {
      switch (entry.1) {
        case (?someValue) switch (mapEntry(entry.0, someValue)) {
          case (null) entry := entry.3[DEQ_NEXT];

          case (item) {
            array[nat(arraySize)] := item;
            entry := entry.3[DEQ_NEXT];
            arraySize +%= 1;
          };
        };

        case (_) return tabulateArray<T>(nat(arraySize), func(i) = switch (array[i]) { case (?value) value; case (_) noop() });
      };
    };
  };

  public func toArrayMapDesc<K, V, T>(map: Map<K, V>, mapEntry: (K, V) -> ?T): [T] {
    let array = initArray<?T>(nat(map.1[SIZE]), null);

    var entry = map.0.3[DEQ_PREV];

    var arraySize = 0:Nat32;

    loop {
      switch (entry.1) {
        case (?someValue) switch (mapEntry(entry.0, someValue)) {
          case (null) entry := entry.3[DEQ_PREV];

          case (item) {
            array[nat(arraySize)] := item;
            entry := entry.3[DEQ_PREV];
            arraySize +%= 1;
          };
        };

        case (_) return tabulateArray<T>(nat(arraySize), func(i) = switch (array[i]) { case (?value) value; case (_) noop() });
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

  public let ihash: HashUtils<Int> = (hashInt, func(a, b) = a == b, func() = 0);

  public let nhash: HashUtils<Nat> = (hashInt, func(a, b) = a == b, func() = 0);

  public let thash: HashUtils<Text> = (hashText, func(a, b) = a == b, func() = "");

  public let phash: HashUtils<Principal> = (hashPrincipal, func(a, b) = a == b, func() = Prim.principalOfBlob(""));

  public let bhash: HashUtils<Blob> = (hashBlob, func(a, b) = a == b, func() = "");

  public let lhash: HashUtils<Bool> = (hashBool, func(a, b) = true, func() = false);

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func useHash<K>(hashUtils: HashUtils<K>, hash: Nat32): HashUtils<K> {
    (func(key) = hash, hashUtils.1, hashUtils.2);
  };

  public func calcHash<K>(hashUtils: HashUtils<K>, key: K): HashUtils<K> {
    let hash = hashUtils.0(key);

    (func(key) = hash, hashUtils.1, hashUtils.2);
  };
};
