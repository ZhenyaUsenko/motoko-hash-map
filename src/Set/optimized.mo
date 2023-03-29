import Prim "mo:prim";

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

  public type HashUtils<K> = (
    getHash: (K) -> Nat32,
    areEqual: (K, K) -> Bool,
    getNullKey: () -> K,
  );

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

  let HASH_OFFSET = 2:Nat32;
  let HASH_CHUNK_SIZE = 4:Nat32;

  let ROOT = 0xffffffff:Nat32;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  let { Array_tabulate = tabulateArray; Array_init = initArray; nat32ToNat = nat; trap } = Prim;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func createRoot<K>(nullKey: K): Entry<K> {
    let temp = (nullKey, ROOT, [var]):Entry<K>;
    let rootLinks = [var temp, temp, temp, temp, temp, temp];
    let root = (nullKey, ROOT, rootLinks);

    rootLinks[BRANCH_1] := root;
    rootLinks[BRANCH_2] := root;
    rootLinks[BRANCH_3] := root;
    rootLinks[BRANCH_4] := root;
    rootLinks[DEQ_PREV] := root;
    rootLinks[DEQ_NEXT] := root;

    root;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func cloneEntry<K>(entry: Entry<K>, newRoot: Entry<K>): Entry<K> {
    let links = entry.2;
    let branch1 = links[BRANCH_1];
    let branch2 = links[BRANCH_2];
    let branch3 = links[BRANCH_3];
    let branch4 = links[BRANCH_4];

    let newEntry = (entry.0, entry.1, [
      var if (branch1.1 != ROOT) cloneEntry(branch1, newRoot) else newRoot,
      if (branch2.1 != ROOT) cloneEntry(branch2, newRoot) else newRoot,
      if (branch3.1 != ROOT) cloneEntry(branch3, newRoot) else newRoot,
      if (branch4.1 != ROOT) cloneEntry(branch4, newRoot) else newRoot,
      newRoot,
      newRoot,
    ]);

    links[TEMP_LINK] := newEntry;

    newEntry;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func new<K>(hashUtils: HashUtils<K>): Set<K> {
    (createRoot(hashUtils.2()), [var 0]);
  };

  public func clear<K>(map: Set<K>) {
    let root = map.0;
    let rootLinks = root.2;

    rootLinks[BRANCH_1] := root;
    rootLinks[BRANCH_2] := root;
    rootLinks[BRANCH_3] := root;
    rootLinks[BRANCH_4] := root;
    rootLinks[DEQ_PREV] := root;
    rootLinks[DEQ_NEXT] := root;

    map.1[SIZE] := 0;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func size<K>(map: Set<K>): Nat {
    nat(map.1[SIZE]);
  };

  public func empty<K>(map: Set<K>): Bool {
    map.1[SIZE] == 0;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func peek<K>(map: Set<K>): ?K {
    let entry = map.0.2[DEQ_PREV];

    if (entry.1 == ROOT) null else ?entry.0;
  };

  public func peekFront<K>(map: Set<K>): ?K {
    let entry = map.0.2[DEQ_NEXT];

    if (entry.1 == ROOT) null else ?entry.0;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func has<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;
    var entry = map.0.2[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      let hash = entry.1;

      if (hash == ROOT) {
        return false;
      } else if (hash == hashParam and hashUtils.1(entry.0, keyParam)) {
        return true;
      } else {
        shiftingHash >>= HASH_OFFSET;
        entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func contains<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): ?Bool {
    if (map.1[SIZE] == 0) return null;

    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;
    var entry = map.0.2[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      let hash = entry.1;

      if (hash == ROOT) {
        return ?false;
      } else if (hash == hashParam and hashUtils.1(entry.0, keyParam)) {
        return ?true;
      } else {
        shiftingHash >>= HASH_OFFSET;
        entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func put<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;

    let root = map.0;
    var parent = root;

    let rootLinks = parent.2;
    var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      let hash = entry.1;

      if (hash == ROOT) {
        let deqPrev = rootLinks[DEQ_PREV];
        let newEntry = (keyParam, hashParam, [var root, root, root, root, deqPrev, root]);

        deqPrev.2[DEQ_NEXT] := newEntry;
        rootLinks[DEQ_PREV] := newEntry;
        parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        map.1[SIZE] +%= 1;

        return false;
      } else if (hash == hashParam and hashUtils.1(entry.0, keyParam)) {
        return true;
      } else {
        parent := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func putFront<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;

    let root = map.0;
    var parent = root;

    let rootLinks = parent.2;
    var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      let hash = entry.1;

      if (hash == ROOT) {
        let deqNext = rootLinks[DEQ_NEXT];
        let newEntry = (keyParam, hashParam, [var root, root, root, root, root, deqNext]);

        deqNext.2[DEQ_PREV] := newEntry;
        rootLinks[DEQ_NEXT] := newEntry;
        parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        map.1[SIZE] +%= 1;

        return false;
      } else if (hash == hashParam and hashUtils.1(entry.0, keyParam)) {
        return true;
      } else {
        parent := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func add<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K) {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;

    let root = map.0;
    var parent = root;

    let rootLinks = parent.2;
    var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      let hash = entry.1;

      if (hash == ROOT) {
        let deqPrev = rootLinks[DEQ_PREV];
        let newEntry = (keyParam, hashParam, [var root, root, root, root, deqPrev, root]);

        deqPrev.2[DEQ_NEXT] := newEntry;
        rootLinks[DEQ_PREV] := newEntry;
        parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        map.1[SIZE] +%= 1;

        return;
      } else if (hash == hashParam and hashUtils.1(entry.0, keyParam)) {
        return;
      } else {
        parent := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func addFront<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K) {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;

    let root = map.0;
    var parent = root;

    let rootLinks = parent.2;
    var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      let hash = entry.1;

      if (hash == ROOT) {
        let deqNext = rootLinks[DEQ_NEXT];
        let newEntry = (keyParam, hashParam, [var root, root, root, root, root, deqNext]);

        deqNext.2[DEQ_PREV] := newEntry;
        rootLinks[DEQ_NEXT] := newEntry;
        parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        map.1[SIZE] +%= 1;

        return;
      } else if (hash == hashParam and hashUtils.1(entry.0, keyParam)) {
        return;
      } else {
        parent := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func putMove<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;

    let root = map.0;
    var parent = root;

    let rootLinks = parent.2;
    var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      let hash = entry.1;

      if (hash == ROOT) {
        let deqPrev = rootLinks[DEQ_PREV];
        let newEntry = (keyParam, hashParam, [var root, root, root, root, deqPrev, root]);

        deqPrev.2[DEQ_NEXT] := newEntry;
        rootLinks[DEQ_PREV] := newEntry;
        parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        map.1[SIZE] +%= 1;

        return false;
      } else if (hash == hashParam and hashUtils.1(entry.0, keyParam)) {
        let links = entry.2;
        let deqPrev = rootLinks[DEQ_PREV];
        let newEntry = (entry.0, hash, [var links[BRANCH_1], links[BRANCH_2], links[BRANCH_3], links[BRANCH_4], deqPrev, root]);

        deqPrev.2[DEQ_NEXT] := newEntry;
        rootLinks[DEQ_PREV] := newEntry;
        parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

        let deqPrevOld = links[DEQ_PREV];
        let deqNextOld = links[DEQ_NEXT];

        deqNextOld.2[DEQ_PREV] := deqPrevOld;
        deqPrevOld.2[DEQ_NEXT] := deqNextOld;
        links[BRANCH_1] := entry;
        links[BRANCH_2] := entry;

        return true;
      } else {
        parent := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func putMoveFront<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;

    let root = map.0;
    var parent = root;

    let rootLinks = parent.2;
    var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      let hash = entry.1;

      if (hash == ROOT) {
        let deqNext = rootLinks[DEQ_NEXT];
        let newEntry = (keyParam, hashParam, [var root, root, root, root, root, deqNext]);

        deqNext.2[DEQ_PREV] := newEntry;
        rootLinks[DEQ_NEXT] := newEntry;
        parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        map.1[SIZE] +%= 1;

        return false;
      } else if (hash == hashParam and hashUtils.1(entry.0, keyParam)) {
        let links = entry.2;
        let deqNext = rootLinks[DEQ_NEXT];
        let newEntry = (entry.0, hash, [var links[BRANCH_1], links[BRANCH_2], links[BRANCH_3], links[BRANCH_4], root, deqNext]);

        deqNext.2[DEQ_PREV] := newEntry;
        rootLinks[DEQ_NEXT] := newEntry;
        parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

        let deqNextOld = links[DEQ_NEXT];
        let deqPrevOld = links[DEQ_PREV];

        deqPrevOld.2[DEQ_NEXT] := deqNextOld;
        deqNextOld.2[DEQ_PREV] := deqPrevOld;
        links[BRANCH_1] := entry;
        links[BRANCH_2] := entry;

        return true;
      } else {
        parent := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func putBefore<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K, placeParam: ?K): Bool {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;

    let root = map.0;
    var parent = root;
    var entry = parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)];

    let placeKeyParam = switch (placeParam) { case (?placeKeyParam) placeKeyParam; case (_) root.0 };

    let placeHashParam = switch (placeParam) { case (null) ROOT; case (_) hashUtils.0(placeKeyParam) };

    var shiftingPlaceHash = placeHashParam;

    var place = if (shiftingPlaceHash != ROOT) root.2[nat(shiftingPlaceHash % HASH_CHUNK_SIZE)] else root;

    loop {
      let placeHash = place.1;

      if (placeHash == ROOT or placeHash == placeHashParam and hashUtils.1(place.0, placeKeyParam)) loop {
        let hash = entry.1;

        if (hash == ROOT) {
          let deqPrev = place.2[DEQ_PREV];
          let newEntry = (keyParam, hashParam, [var root, root, root, root, deqPrev, place]);

          deqPrev.2[DEQ_NEXT] := newEntry;
          place.2[DEQ_PREV] := newEntry;
          parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
          map.1[SIZE] +%= 1;

          return false;
        } else if (hash == hashParam and hashUtils.1(entry.0, keyParam)) {
          let links = entry.2;
          let deqPrev = place.2[DEQ_PREV];
          let newEntry = (entry.0, hash, [var links[BRANCH_1], links[BRANCH_2], links[BRANCH_3], links[BRANCH_4], deqPrev, place]);

          deqPrev.2[DEQ_NEXT] := newEntry;
          place.2[DEQ_PREV] := newEntry;
          parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

          let deqPrevOld = links[DEQ_PREV];
          let deqNextOld = links[DEQ_NEXT];

          deqNextOld.2[DEQ_PREV] := deqPrevOld;
          deqPrevOld.2[DEQ_NEXT] := deqNextOld;
          links[BRANCH_1] := entry;
          links[BRANCH_2] := entry;

          return true;
        } else {
          parent := entry;
          shiftingHash >>= HASH_OFFSET;
          entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
        };
      } else {
        shiftingPlaceHash >>= HASH_OFFSET;
        place := place.2[nat(shiftingPlaceHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func putAfter<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K, placeParam: ?K): Bool {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;

    let root = map.0;
    var parent = root;
    var entry = parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)];

    let placeKeyParam = switch (placeParam) { case (?placeKeyParam) placeKeyParam; case (_) root.0 };

    let placeHashParam = switch (placeParam) { case (null) ROOT; case (_) hashUtils.0(placeKeyParam) };

    var shiftingPlaceHash = placeHashParam;

    var place = if (shiftingPlaceHash != ROOT) root.2[nat(shiftingPlaceHash % HASH_CHUNK_SIZE)] else root;

    loop {
      let placeHash = place.1;

      if (placeHash == ROOT or placeHash == placeHashParam and hashUtils.1(place.0, placeKeyParam)) loop {
        let hash = entry.1;

        if (hash == ROOT) {
          let deqNext = place.2[DEQ_NEXT];
          let newEntry = (keyParam, hashParam, [var root, root, root, root, place, deqNext]);

          deqNext.2[DEQ_PREV] := newEntry;
          place.2[DEQ_NEXT] := newEntry;
          parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
          map.1[SIZE] +%= 1;

          return false;
        } else if (hash == hashParam and hashUtils.1(entry.0, keyParam)) {
          let links = entry.2;
          let deqNext = place.2[DEQ_NEXT];
          let newEntry = (entry.0, hash, [var links[BRANCH_1], links[BRANCH_2], links[BRANCH_3], links[BRANCH_4], place, deqNext]);

          deqNext.2[DEQ_PREV] := newEntry;
          place.2[DEQ_NEXT] := newEntry;
          parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

          let deqNextOld = links[DEQ_NEXT];
          let deqPrevOld = links[DEQ_PREV];

          deqPrevOld.2[DEQ_NEXT] := deqNextOld;
          deqNextOld.2[DEQ_PREV] := deqPrevOld;
          links[BRANCH_1] := entry;
          links[BRANCH_2] := entry;

          return true;
        } else {
          parent := entry;
          shiftingHash >>= HASH_OFFSET;
          entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
        };
      } else {
        shiftingPlaceHash >>= HASH_OFFSET;
        place := place.2[nat(shiftingPlaceHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func remove<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;

    let root = map.0;
    var parent = root;
    var entry = parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      let hash = entry.1;

      if (hash == hashParam and hashUtils.1(entry.0, keyParam)) {
        let links = entry.2;
        let deqPrev = links[DEQ_PREV];
        let deqNext = links[DEQ_NEXT];

        deqNext.2[DEQ_PREV] := deqPrev;
        deqPrev.2[DEQ_NEXT] := deqNext;
        map.1[SIZE] -%= 1;

        var leaf = entry;
        var leafParent = leaf;
        var leafIndex = NULL_BRANCH;

        loop {
          let leafLinks = leaf.2;
          let branch1 = leafLinks[BRANCH_1];

          if (branch1.1 != ROOT) {
            leafParent := leaf;
            leaf := branch1;
            leafIndex := BRANCH_1;
          } else {
            let branch2 = leafLinks[BRANCH_2];

            if (branch2.1 != ROOT) {
              leafParent := leaf;
              leaf := branch2;
              leafIndex := BRANCH_2;
            } else {
              let branch3 = leafLinks[BRANCH_3];

              if (branch3.1 != ROOT) {
                leafParent := leaf;
                leaf := branch3;
                leafIndex := BRANCH_3;
              } else {
                let branch4 = leafLinks[BRANCH_4];

                if (branch4.1 != ROOT) {
                  leafParent := leaf;
                  leaf := branch4;
                  leafIndex := BRANCH_4;
                } else {
                  if (leafIndex == NULL_BRANCH) {
                    parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := root;
                  } else {
                    parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := leaf;
                    leafParent.2[leafIndex] := root;

                    leafLinks[BRANCH_1] := links[BRANCH_1];
                    leafLinks[BRANCH_2] := links[BRANCH_2];
                    leafLinks[BRANCH_3] := links[BRANCH_3];
                    leafLinks[BRANCH_4] := links[BRANCH_4];
                  };

                  links[BRANCH_1] := entry;
                  links[BRANCH_2] := entry;

                  return true;
                };
              };
            };
          };
        };
      } else if (hash == ROOT) {
        return false;
      } else {
        parent := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func delete<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K) {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;

    let root = map.0;
    var parent = root;
    var entry = parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      let hash = entry.1;

      if (hash == hashParam and hashUtils.1(entry.0, keyParam)) {
        let links = entry.2;
        let deqPrev = links[DEQ_PREV];
        let deqNext = links[DEQ_NEXT];

        deqNext.2[DEQ_PREV] := deqPrev;
        deqPrev.2[DEQ_NEXT] := deqNext;
        map.1[SIZE] -%= 1;

        var leaf = entry;
        var leafParent = leaf;
        var leafIndex = NULL_BRANCH;

        loop {
          let leafLinks = leaf.2;
          let branch1 = leafLinks[BRANCH_1];

          if (branch1.1 != ROOT) {
            leafParent := leaf;
            leaf := branch1;
            leafIndex := BRANCH_1;
          } else {
            let branch2 = leafLinks[BRANCH_2];

            if (branch2.1 != ROOT) {
              leafParent := leaf;
              leaf := branch2;
              leafIndex := BRANCH_2;
            } else {
              let branch3 = leafLinks[BRANCH_3];

              if (branch3.1 != ROOT) {
                leafParent := leaf;
                leaf := branch3;
                leafIndex := BRANCH_3;
              } else {
                let branch4 = leafLinks[BRANCH_4];

                if (branch4.1 != ROOT) {
                  leafParent := leaf;
                  leaf := branch4;
                  leafIndex := BRANCH_4;
                } else {
                  if (leafIndex == NULL_BRANCH) {
                    parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := root;
                  } else {
                    parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := leaf;
                    leafParent.2[leafIndex] := root;

                    leafLinks[BRANCH_1] := links[BRANCH_1];
                    leafLinks[BRANCH_2] := links[BRANCH_2];
                    leafLinks[BRANCH_3] := links[BRANCH_3];
                    leafLinks[BRANCH_4] := links[BRANCH_4];
                  };

                  links[BRANCH_1] := entry;
                  links[BRANCH_2] := entry;

                  return;
                };
              };
            };
          };
        };
      } else if (hash == ROOT) {
        return;
      } else {
        parent := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func pop<K>(map: Set<K>): ?K {
    let root = map.0;
    let rootLinks = root.2;
    let hashParam = rootLinks[DEQ_PREV].1;

    if (hashParam == ROOT) return null;

    var shiftingHash = hashParam;
    var parent = root;
    var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      if (entry.1 == hashParam and entry.2[DEQ_NEXT].1 == ROOT) {
        let links = entry.2;
        let deqPrev = links[DEQ_PREV];

        deqPrev.2[DEQ_NEXT] := root;
        rootLinks[DEQ_PREV] := deqPrev;
        map.1[SIZE] -%= 1;

        var leaf = entry;
        var leafParent = leaf;
        var leafIndex = NULL_BRANCH;

        loop {
          let leafLinks = leaf.2;
          let branch1 = leafLinks[BRANCH_1];

          if (branch1.1 != ROOT) {
            leafParent := leaf;
            leaf := branch1;
            leafIndex := BRANCH_1;
          } else {
            let branch2 = leafLinks[BRANCH_2];

            if (branch2.1 != ROOT) {
              leafParent := leaf;
              leaf := branch2;
              leafIndex := BRANCH_2;
            } else {
              let branch3 = leafLinks[BRANCH_3];

              if (branch3.1 != ROOT) {
                leafParent := leaf;
                leaf := branch3;
                leafIndex := BRANCH_3;
              } else {
                let branch4 = leafLinks[BRANCH_4];

                if (branch4.1 != ROOT) {
                  leafParent := leaf;
                  leaf := branch4;
                  leafIndex := BRANCH_4;
                } else {
                  if (leafIndex == NULL_BRANCH) {
                    parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := root;
                  } else {
                    parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := leaf;
                    leafParent.2[leafIndex] := root;

                    leafLinks[BRANCH_1] := links[BRANCH_1];
                    leafLinks[BRANCH_2] := links[BRANCH_2];
                    leafLinks[BRANCH_3] := links[BRANCH_3];
                    leafLinks[BRANCH_4] := links[BRANCH_4];
                  };

                  links[BRANCH_1] := entry;
                  links[BRANCH_2] := entry;

                  return ?entry.0;
                };
              };
            };
          };
        };
      } else {
        parent := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func popFront<K>(map: Set<K>): ?K {
    let root = map.0;
    let rootLinks = root.2;
    let hashParam = rootLinks[DEQ_NEXT].1;

    if (hashParam == ROOT) return null;

    var shiftingHash = hashParam;
    var parent = root;
    var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      if (entry.1 == hashParam and entry.2[DEQ_PREV].1 == ROOT) {
        let links = entry.2;
        let deqNext = links[DEQ_NEXT];

        deqNext.2[DEQ_PREV] := root;
        rootLinks[DEQ_NEXT] := deqNext;
        map.1[SIZE] -%= 1;

        var leaf = entry;
        var leafParent = leaf;
        var leafIndex = NULL_BRANCH;

        loop {
          let leafLinks = leaf.2;
          let branch1 = leafLinks[BRANCH_1];

          if (branch1.1 != ROOT) {
            leafParent := leaf;
            leaf := branch1;
            leafIndex := BRANCH_1;
          } else {
            let branch2 = leafLinks[BRANCH_2];

            if (branch2.1 != ROOT) {
              leafParent := leaf;
              leaf := branch2;
              leafIndex := BRANCH_2;
            } else {
              let branch3 = leafLinks[BRANCH_3];

              if (branch3.1 != ROOT) {
                leafParent := leaf;
                leaf := branch3;
                leafIndex := BRANCH_3;
              } else {
                let branch4 = leafLinks[BRANCH_4];

                if (branch4.1 != ROOT) {
                  leafParent := leaf;
                  leaf := branch4;
                  leafIndex := BRANCH_4;
                } else {
                  if (leafIndex == NULL_BRANCH) {
                    parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := root;
                  } else {
                    parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := leaf;
                    leafParent.2[leafIndex] := root;

                    leafLinks[BRANCH_1] := links[BRANCH_1];
                    leafLinks[BRANCH_2] := links[BRANCH_2];
                    leafLinks[BRANCH_3] := links[BRANCH_3];
                    leafLinks[BRANCH_4] := links[BRANCH_4];
                  };

                  links[BRANCH_1] := entry;
                  links[BRANCH_2] := entry;

                  return ?entry.0;
                };
              };
            };
          };
        };
      } else {
        parent := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func cycle<K>(map: Set<K>): ?K {
    let root = map.0;
    let rootLinks = root.2;
    let hashParam = rootLinks[DEQ_PREV].1;

    if (hashParam == ROOT) return null;

    var shiftingHash = hashParam;
    var parent = root;
    var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      if (entry.1 == hashParam and entry.2[DEQ_NEXT].1 == ROOT) {
        let links = entry.2;
        let deqFirst = rootLinks[DEQ_NEXT];
        let newEntry = (entry.0, entry.1, [var links[BRANCH_1], links[BRANCH_2], links[BRANCH_3], links[BRANCH_4], root, deqFirst]);

        deqFirst.2[DEQ_PREV] := newEntry;
        rootLinks[DEQ_NEXT] := newEntry;
        parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

        let deqPrev = links[DEQ_PREV];

        deqPrev.2[DEQ_NEXT] := root;
        rootLinks[DEQ_PREV] := deqPrev;
        links[BRANCH_1] := entry;
        links[BRANCH_2] := entry;

        return ?entry.0;
      } else {
        parent := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func cycleFront<K>(map: Set<K>): ?K {
    let root = map.0;
    let rootLinks = root.2;
    let hashParam = rootLinks[DEQ_NEXT].1;

    if (hashParam == ROOT) return null;

    var shiftingHash = hashParam;
    var parent = root;
    var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      if (entry.1 == hashParam and entry.2[DEQ_PREV].1 == ROOT) {
        let links = entry.2;
        let deqLast = rootLinks[DEQ_PREV];
        let newEntry = (entry.0, entry.1, [var links[BRANCH_1], links[BRANCH_2], links[BRANCH_3], links[BRANCH_4], deqLast, root]);

        deqLast.2[DEQ_NEXT] := newEntry;
        rootLinks[DEQ_PREV] := newEntry;
        parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

        let deqNext = links[DEQ_NEXT];

        deqNext.2[DEQ_PREV] := root;
        rootLinks[DEQ_NEXT] := deqNext;
        links[BRANCH_1] := entry;
        links[BRANCH_2] := entry;

        return ?entry.0;
      } else {
        parent := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func clone<K>(map: Set<K>): Set<K> {
    let root = map.0;
    var entry = root;

    let rootLinks = entry.2;
    let branch1 = rootLinks[BRANCH_1];
    let branch2 = rootLinks[BRANCH_2];
    let branch3 = rootLinks[BRANCH_3];
    let branch4 = rootLinks[BRANCH_4];

    let newRootLinks = [var root, root, root, root, root, root];
    let newRoot = (root.0, ROOT, newRootLinks);
    var newEntry = newRoot;

    rootLinks[TEMP_LINK] := newRoot;

    newRootLinks[BRANCH_1] := if (branch1.1 != ROOT) cloneEntry(branch1, newRoot) else newRoot;
    newRootLinks[BRANCH_2] := if (branch2.1 != ROOT) cloneEntry(branch2, newRoot) else newRoot;
    newRootLinks[BRANCH_3] := if (branch3.1 != ROOT) cloneEntry(branch3, newRoot) else newRoot;
    newRootLinks[BRANCH_4] := if (branch4.1 != ROOT) cloneEntry(branch4, newRoot) else newRoot;

    loop {
      let cloneNext = entry.2[CLONE_NEXT];
      let newDeqNext = cloneNext.2[TEMP_LINK];

      newDeqNext.2[DEQ_PREV] := newEntry;
      newEntry.2[DEQ_NEXT] := newDeqNext;
      cloneNext.2[TEMP_LINK] := entry;

      if (newDeqNext.1 == ROOT) return (newRoot, [var map.1[SIZE]]);

      newEntry := newDeqNext;
      entry := cloneNext;
    };
  };

  public func cloneDesc<K>(map: Set<K>): Set<K> {
    let root = map.0;
    var entry = root;

    let rootLinks = entry.2;
    let branch1 = rootLinks[BRANCH_1];
    let branch2 = rootLinks[BRANCH_2];
    let branch3 = rootLinks[BRANCH_3];
    let branch4 = rootLinks[BRANCH_4];

    let newRootLinks = [var root, root, root, root, root, root];
    let newRoot = (root.0, ROOT, newRootLinks);
    var newEntry = newRoot;

    rootLinks[TEMP_LINK] := newRoot;

    newRootLinks[BRANCH_1] := if (branch1.1 != ROOT) cloneEntry(branch1, newRoot) else newRoot;
    newRootLinks[BRANCH_2] := if (branch2.1 != ROOT) cloneEntry(branch2, newRoot) else newRoot;
    newRootLinks[BRANCH_3] := if (branch3.1 != ROOT) cloneEntry(branch3, newRoot) else newRoot;
    newRootLinks[BRANCH_4] := if (branch4.1 != ROOT) cloneEntry(branch4, newRoot) else newRoot;

    loop {
      let cloneNext = entry.2[CLONE_NEXT];
      let newDeqPrev = cloneNext.2[TEMP_LINK];

      newDeqPrev.2[DEQ_NEXT] := newEntry;
      newEntry.2[DEQ_PREV] := newDeqPrev;
      cloneNext.2[TEMP_LINK] := entry;

      if (newDeqPrev.1 == ROOT) return (newRoot, [var map.1[SIZE]]);

      newEntry := newDeqPrev;
      entry := cloneNext;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func filter<K>(map: Set<K>, hashUtils: HashUtils<K>, acceptEntry: (K) -> Bool): Set<K> {
    let root = createRoot<K>(hashUtils.2());
    let rootLinks = root.2;
    let getHash = hashUtils.0;
    let areEqual = hashUtils.1;
    var size = 0:Nat32;
    var item = map.0;

    loop label loopBody {
      item := item.2[DEQ_NEXT];

      let hash = item.1;

      if (hash == ROOT) {
        return (root, [var size]);
      } else if ((item.2[BRANCH_1].1 != hash or item.2[BRANCH_2].1 != hash) and acceptEntry(item.0)) {
        let hashParam = getHash(item.0);
        var shiftingHash = hashParam;
        var parent = root;
        var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

        loop {
          let hash = entry.1;

          if (hash == ROOT) {
            let deqPrev = rootLinks[DEQ_PREV];
            let newEntry = (item.0, hashParam, [var root, root, root, root, deqPrev, root]);

            deqPrev.2[DEQ_NEXT] := newEntry;
            rootLinks[DEQ_PREV] := newEntry;
            parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
            size +%= 1;

            break loopBody;
          } else if (hash == hashParam and areEqual(entry.0, item.0)) {
            let links = entry.2;
            let deqPrev = rootLinks[DEQ_PREV];
            let newEntry = (entry.0, hash, [var links[BRANCH_1], links[BRANCH_2], links[BRANCH_3], links[BRANCH_4], deqPrev, root]);

            deqPrev.2[DEQ_NEXT] := newEntry;
            rootLinks[DEQ_PREV] := newEntry;
            parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

            let deqPrevOld = links[DEQ_PREV];
            let deqNextOld = links[DEQ_NEXT];

            deqNextOld.2[DEQ_PREV] := deqPrevOld;
            deqPrevOld.2[DEQ_NEXT] := deqNextOld;
            links[BRANCH_1] := entry;
            links[BRANCH_2] := entry;

            break loopBody;
          } else {
            parent := entry;
            shiftingHash >>= HASH_OFFSET;
            entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
          };
        };
      };
    };
  };

  public func filterDesc<K>(map: Set<K>, hashUtils: HashUtils<K>, acceptEntry: (K) -> Bool): Set<K> {
    let root = createRoot<K>(hashUtils.2());
    let rootLinks = root.2;
    let getHash = hashUtils.0;
    let areEqual = hashUtils.1;
    var size = 0:Nat32;
    var item = map.0;

    loop label loopBody {
      item := item.2[DEQ_PREV];

      let hash = item.1;

      if (hash == ROOT) {
        return (root, [var size]);
      } else if ((item.2[BRANCH_1].1 != hash or item.2[BRANCH_2].1 != hash) and acceptEntry(item.0)) {
        let hashParam = getHash(item.0);
        var shiftingHash = hashParam;
        var parent = root;
        var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

        loop {
          let hash = entry.1;

          if (hash == ROOT) {
            let deqPrev = rootLinks[DEQ_PREV];
            let newEntry = (item.0, hashParam, [var root, root, root, root, deqPrev, root]);

            deqPrev.2[DEQ_NEXT] := newEntry;
            rootLinks[DEQ_PREV] := newEntry;
            parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
            size +%= 1;

            break loopBody;
          } else if (hash == hashParam and areEqual(entry.0, item.0)) {
            let links = entry.2;
            let deqPrev = rootLinks[DEQ_PREV];
            let newEntry = (entry.0, hash, [var links[BRANCH_1], links[BRANCH_2], links[BRANCH_3], links[BRANCH_4], deqPrev, root]);

            deqPrev.2[DEQ_NEXT] := newEntry;
            rootLinks[DEQ_PREV] := newEntry;
            parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

            let deqPrevOld = links[DEQ_PREV];
            let deqNextOld = links[DEQ_NEXT];

            deqNextOld.2[DEQ_PREV] := deqPrevOld;
            deqPrevOld.2[DEQ_NEXT] := deqNextOld;
            links[BRANCH_1] := entry;
            links[BRANCH_2] := entry;

            break loopBody;
          } else {
            parent := entry;
            shiftingHash >>= HASH_OFFSET;
            entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
          };
        };
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func keys<K>(map: Set<K>): Iter<K> {
    var started = false;
    var entry = map.0;

    let iter = {
      prev = func(): ?K {
        started := true;
        entry := entry.2[DEQ_PREV];

        loop {
          let hash = entry.1;

          if (hash == ROOT) return null else if (entry.2[BRANCH_1].1 != hash or entry.2[BRANCH_2].1 != hash) return ?entry.0 else {
            entry := entry.2[DEQ_PREV];
          };
        };
      };

      next = func(): ?K {
        started := true;
        entry := entry.2[DEQ_NEXT];

        loop {
          let hash = entry.1;

          if (hash == ROOT) return null else if (entry.2[BRANCH_1].1 != hash or entry.2[BRANCH_2].1 != hash) return ?entry.0 else {
            entry := entry.2[DEQ_NEXT];
          };
        };
      };

      peekPrev = func(): ?K {
        var deqPrev = entry.2[DEQ_PREV];

        loop {
          let hash = deqPrev.1;

          if (hash == ROOT) return null else if (deqPrev.2[BRANCH_1].1 != hash or deqPrev.2[BRANCH_2].1 != hash) {
            entry.2[DEQ_PREV] := deqPrev;

            return ?deqPrev.0;
          } else {
            deqPrev := deqPrev.2[DEQ_PREV];
          };
        };
      };

      peekNext = func(): ?K {
        var deqNext = entry.2[DEQ_NEXT];

        loop {
          let hash = deqNext.1;

          if (hash == ROOT) return null else if (deqNext.2[BRANCH_1].1 != hash or deqNext.2[BRANCH_2].1 != hash) {
            entry.2[DEQ_NEXT] := deqNext;

            return ?deqNext.0;
          } else {
            deqNext := deqNext.2[DEQ_NEXT];
          };
        };
      };

      current = func(): ?K {
        if (entry.1 == ROOT) null else ?entry.0;
      };

      started = func(): Bool {
        started;
      };

      finished = func(): Bool {
        started and entry.1 == ROOT;
      };

      reset = func(): Iter<K> {
        started := false;
        entry := map.0;

        iter;
      };

      movePrev = func(): Iter<K> {
        started := true;
        entry := entry.2[DEQ_PREV];

        loop if (entry.2[BRANCH_1].1 != entry.1 or entry.2[BRANCH_2].1 != entry.1 or entry.1 == ROOT) return iter else {
          entry := entry.2[DEQ_PREV];
        };
      };

      moveNext = func(): Iter<K> {
        started := true;
        entry := entry.2[DEQ_NEXT];

        loop if (entry.2[BRANCH_1].1 != entry.1 or entry.2[BRANCH_2].1 != entry.1 or entry.1 == ROOT) return iter else {
          entry := entry.2[DEQ_NEXT];
        };
      };
    };
  };

  public func keysDesc<K>(map: Set<K>): Iter<K> {
    var started = false;
    var entry = map.0;

    let iter = {
      prev = func(): ?K {
        started := true;
        entry := entry.2[DEQ_NEXT];

        loop {
          let hash = entry.1;

          if (hash == ROOT) return null else if (entry.2[BRANCH_1].1 != hash or entry.2[BRANCH_2].1 != hash) return ?entry.0 else {
            entry := entry.2[DEQ_NEXT];
          };
        };
      };

      next = func(): ?K {
        started := true;
        entry := entry.2[DEQ_PREV];

        loop {
          let hash = entry.1;

          if (hash == ROOT) return null else if (entry.2[BRANCH_1].1 != hash or entry.2[BRANCH_2].1 != hash) return ?entry.0 else {
            entry := entry.2[DEQ_PREV];
          };
        };
      };

      peekPrev = func(): ?K {
        var deqNext = entry.2[DEQ_NEXT];

        loop {
          let hash = deqNext.1;

          if (hash == ROOT) return null else if (deqNext.2[BRANCH_1].1 != hash or deqNext.2[BRANCH_2].1 != hash) {
            entry.2[DEQ_NEXT] := deqNext;

            return ?deqNext.0;
          } else {
            deqNext := deqNext.2[DEQ_NEXT];
          };
        };
      };

      peekNext = func(): ?K {
        var deqPrev = entry.2[DEQ_PREV];

        loop {
          let hash = deqPrev.1;

          if (hash == ROOT) return null else if (deqPrev.2[BRANCH_1].1 != hash or deqPrev.2[BRANCH_2].1 != hash) {
            entry.2[DEQ_PREV] := deqPrev;

            return ?deqPrev.0;
          } else {
            deqPrev := deqPrev.2[DEQ_PREV];
          };
        };
      };

      current = func(): ?K {
        if (entry.1 == ROOT) null else ?entry.0;
      };

      started = func(): Bool {
        started;
      };

      finished = func(): Bool {
        started and entry.1 == ROOT;
      };

      reset = func(): Iter<K> {
        started := false;
        entry := map.0;

        iter;
      };

      movePrev = func(): Iter<K> {
        started := true;
        entry := entry.2[DEQ_NEXT];

        loop if (entry.2[BRANCH_1].1 != entry.1 or entry.2[BRANCH_2].1 != entry.1 or entry.1 == ROOT) return iter else {
          entry := entry.2[DEQ_NEXT];
        };
      };

      moveNext = func(): Iter<K> {
        started := true;
        entry := entry.2[DEQ_PREV];

        loop if (entry.2[BRANCH_1].1 != entry.1 or entry.2[BRANCH_2].1 != entry.1 or entry.1 == ROOT) return iter else {
          entry := entry.2[DEQ_PREV];
        };
      };
    };
  };

  public func keysFrom<K>(map: Set<K>, hashUtils: HashUtils<K>, placeParam: ?K): Iter<K> {
    let keyParam = switch (placeParam) { case (?keyParam) keyParam; case (_) map.0.0 };

    let hashParam = switch (placeParam) { case (null) ROOT; case (_) hashUtils.0(keyParam) };

    var shiftingHash = hashParam;

    var entry = if (shiftingHash != ROOT) map.0.2[nat(shiftingHash % HASH_CHUNK_SIZE)] else map.0;

    loop {
      let hash = entry.1;

      if (hash == ROOT or hash == hashParam and hashUtils.1(entry.0, keyParam)) {
        var started = hash != ROOT;

        return let iter = {
          prev = func(): ?K {
            started := true;
            entry := entry.2[DEQ_PREV];

            loop {
              let hash = entry.1;

              if (hash == ROOT) return null else if (entry.2[BRANCH_1].1 != hash or entry.2[BRANCH_2].1 != hash) return ?entry.0 else {
                entry := entry.2[DEQ_PREV];
              };
            };
          };

          next = func(): ?K {
            started := true;
            entry := entry.2[DEQ_NEXT];

            loop {
              let hash = entry.1;

              if (hash == ROOT) return null else if (entry.2[BRANCH_1].1 != hash or entry.2[BRANCH_2].1 != hash) return ?entry.0 else {
                entry := entry.2[DEQ_NEXT];
              };
            };
          };

          peekPrev = func(): ?K {
            var deqPrev = entry.2[DEQ_PREV];

            loop {
              let hash = deqPrev.1;

              if (hash == ROOT) return null else if (deqPrev.2[BRANCH_1].1 != hash or deqPrev.2[BRANCH_2].1 != hash) {
                entry.2[DEQ_PREV] := deqPrev;

                return ?deqPrev.0;
              } else {
                deqPrev := deqPrev.2[DEQ_PREV];
              };
            };
          };

          peekNext = func(): ?K {
            var deqNext = entry.2[DEQ_NEXT];

            loop {
              let hash = deqNext.1;

              if (hash == ROOT) return null else if (deqNext.2[BRANCH_1].1 != hash or deqNext.2[BRANCH_2].1 != hash) {
                entry.2[DEQ_NEXT] := deqNext;

                return ?deqNext.0;
              } else {
                deqNext := deqNext.2[DEQ_NEXT];
              };
            };
          };

          current = func(): ?K {
            if (entry.1 == ROOT) null else ?entry.0;
          };

          started = func(): Bool {
            started;
          };

          finished = func(): Bool {
            started and entry.1 == ROOT;
          };

          reset = func(): Iter<K> {
            started := false;
            entry := map.0;

            iter;
          };

          movePrev = func(): Iter<K> {
            started := true;
            entry := entry.2[DEQ_PREV];

            loop if (entry.2[BRANCH_1].1 != entry.1 or entry.2[BRANCH_2].1 != entry.1 or entry.1 == ROOT) return iter else {
              entry := entry.2[DEQ_PREV];
            };
          };

          moveNext = func(): Iter<K> {
            started := true;
            entry := entry.2[DEQ_NEXT];

            loop if (entry.2[BRANCH_1].1 != entry.1 or entry.2[BRANCH_2].1 != entry.1 or entry.1 == ROOT) return iter else {
              entry := entry.2[DEQ_NEXT];
            };
          };
        };
      } else {
        shiftingHash >>= HASH_OFFSET;
        entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func keysFromDesc<K>(map: Set<K>, hashUtils: HashUtils<K>, placeParam: ?K): Iter<K> {
    let keyParam = switch (placeParam) { case (?keyParam) keyParam; case (_) map.0.0 };

    let hashParam = switch (placeParam) { case (null) ROOT; case (_) hashUtils.0(keyParam) };

    var shiftingHash = hashParam;

    var entry = if (shiftingHash != ROOT) map.0.2[nat(shiftingHash % HASH_CHUNK_SIZE)] else map.0;

    loop {
      let hash = entry.1;

      if (hash == ROOT or hash == hashParam and hashUtils.1(entry.0, keyParam)) {
        var started = hash != ROOT;

        return let iter = {
          prev = func(): ?K {
            started := true;
            entry := entry.2[DEQ_NEXT];

            loop {
              let hash = entry.1;

              if (hash == ROOT) return null else if (entry.2[BRANCH_1].1 != hash or entry.2[BRANCH_2].1 != hash) return ?entry.0 else {
                entry := entry.2[DEQ_NEXT];
              };
            };
          };

          next = func(): ?K {
            started := true;
            entry := entry.2[DEQ_PREV];

            loop {
              let hash = entry.1;

              if (hash == ROOT) return null else if (entry.2[BRANCH_1].1 != hash or entry.2[BRANCH_2].1 != hash) return ?entry.0 else {
                entry := entry.2[DEQ_PREV];
              };
            };
          };

          peekPrev = func(): ?K {
            var deqNext = entry.2[DEQ_NEXT];

            loop {
              let hash = deqNext.1;

              if (hash == ROOT) return null else if (deqNext.2[BRANCH_1].1 != hash or deqNext.2[BRANCH_2].1 != hash) {
                entry.2[DEQ_NEXT] := deqNext;

                return ?deqNext.0;
              } else {
                deqNext := deqNext.2[DEQ_NEXT];
              };
            };
          };

          peekNext = func(): ?K {
            var deqPrev = entry.2[DEQ_PREV];

            loop {
              let hash = deqPrev.1;

              if (hash == ROOT) return null else if (deqPrev.2[BRANCH_1].1 != hash or deqPrev.2[BRANCH_2].1 != hash) {
                entry.2[DEQ_PREV] := deqPrev;

                return ?deqPrev.0;
              } else {
                deqPrev := deqPrev.2[DEQ_PREV];
              };
            };
          };

          current = func(): ?K {
            if (entry.1 == ROOT) null else ?entry.0;
          };

          started = func(): Bool {
            started;
          };

          finished = func(): Bool {
            started and entry.1 == ROOT;
          };

          reset = func(): Iter<K> {
            started := false;
            entry := map.0;

            iter;
          };

          movePrev = func(): Iter<K> {
            started := true;
            entry := entry.2[DEQ_NEXT];

            loop if (entry.2[BRANCH_1].1 != entry.1 or entry.2[BRANCH_2].1 != entry.1 or entry.1 == ROOT) return iter else {
              entry := entry.2[DEQ_NEXT];
            };
          };

          moveNext = func(): Iter<K> {
            started := true;
            entry := entry.2[DEQ_PREV];

            loop if (entry.2[BRANCH_1].1 != entry.1 or entry.2[BRANCH_2].1 != entry.1 or entry.1 == ROOT) return iter else {
              entry := entry.2[DEQ_PREV];
            };
          };
        };
      } else {
        shiftingHash >>= HASH_OFFSET;
        entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func find<K>(map: Set<K>, acceptEntry: (K) -> Bool): ?K {
    var entry = map.0;

    loop {
      entry := entry.2[DEQ_NEXT];

      let hash = entry.1;

      if (hash == ROOT) return null else if ((entry.2[BRANCH_1].1 != hash or entry.2[BRANCH_2].1 != hash) and acceptEntry(entry.0)) {
        return ?entry.0;
      };
    };
  };

  public func findDesc<K>(map: Set<K>, acceptEntry: (K) -> Bool): ?K {
    var entry = map.0;

    loop {
      entry := entry.2[DEQ_PREV];

      let hash = entry.1;

      if (hash == ROOT) return null else if ((entry.2[BRANCH_1].1 != hash or entry.2[BRANCH_2].1 != hash) and acceptEntry(entry.0)) {
        return ?entry.0;
      };
    };
  };

  public func some<K>(map: Set<K>, acceptEntry: (K) -> Bool): Bool {
    var entry = map.0;

    loop {
      entry := entry.2[DEQ_NEXT];

      let hash = entry.1;

      if (hash == ROOT) return false else if ((entry.2[BRANCH_1].1 != hash or entry.2[BRANCH_2].1 != hash) and acceptEntry(entry.0)) {
        return true;
      };
    };
  };

  public func someDesc<K>(map: Set<K>, acceptEntry: (K) -> Bool): Bool {
    var entry = map.0;

    loop {
      entry := entry.2[DEQ_PREV];

      let hash = entry.1;

      if (hash == ROOT) return false else if ((entry.2[BRANCH_1].1 != hash or entry.2[BRANCH_2].1 != hash) and acceptEntry(entry.0)) {
        return true;
      };
    };
  };

  public func every<K>(map: Set<K>, acceptEntry: (K) -> Bool): Bool {
    var entry = map.0;

    loop {
      entry := entry.2[DEQ_NEXT];

      let hash = entry.1;

      if (hash == ROOT) return true else if ((entry.2[BRANCH_1].1 != hash or entry.2[BRANCH_2].1 != hash) and not acceptEntry(entry.0)) {
        return false;
      };
    };
  };

  public func everyDesc<K>(map: Set<K>, acceptEntry: (K) -> Bool): Bool {
    var entry = map.0;

    loop {
      entry := entry.2[DEQ_PREV];

      let hash = entry.1;

      if (hash == ROOT) return true else if ((entry.2[BRANCH_1].1 != hash or entry.2[BRANCH_2].1 != hash) and not acceptEntry(entry.0)) {
        return false;
      };
    };
  };

  public func forEach<K>(map: Set<K>, mapEntry: (K) -> ()) {
    var entry = map.0;

    loop {
      entry := entry.2[DEQ_NEXT];

      let hash = entry.1;

      if (hash == ROOT) return else if (entry.2[BRANCH_1].1 != hash or entry.2[BRANCH_2].1 != hash) mapEntry(entry.0);
    };
  };

  public func forEachDesc<K>(map: Set<K>, mapEntry: (K) -> ()) {
    var entry = map.0;

    loop {
      entry := entry.2[DEQ_PREV];

      let hash = entry.1;

      if (hash == ROOT) return else if (entry.2[BRANCH_1].1 != hash or entry.2[BRANCH_2].1 != hash) mapEntry(entry.0);
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func fromIter<K>(iter: IterNext<K>, hashUtils: HashUtils<K>): Set<K> {
    let root = createRoot<K>(hashUtils.2());
    let rootLinks = root.2;
    let getHash = hashUtils.0;
    let areEqual = hashUtils.1;
    var size = 0:Nat32;

    for (item in iter) label loopBody {
      let hashParam = getHash(item);
      var shiftingHash = hashParam;
      var parent = root;
      var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

      loop {
        let hash = entry.1;

        if (hash == ROOT) {
          let deqPrev = rootLinks[DEQ_PREV];
          let newEntry = (item, hashParam, [var root, root, root, root, deqPrev, root]);

          deqPrev.2[DEQ_NEXT] := newEntry;
          rootLinks[DEQ_PREV] := newEntry;
          parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
          size +%= 1;

          break loopBody;
        } else if (hash == hashParam and areEqual(entry.0, item)) {
          let links = entry.2;
          let deqPrev = rootLinks[DEQ_PREV];
          let newEntry = (entry.0, hash, [var links[BRANCH_1], links[BRANCH_2], links[BRANCH_3], links[BRANCH_4], deqPrev, root]);

          deqPrev.2[DEQ_NEXT] := newEntry;
          rootLinks[DEQ_PREV] := newEntry;
          parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

          let deqPrevOld = links[DEQ_PREV];
          let deqNextOld = links[DEQ_NEXT];

          deqNextOld.2[DEQ_PREV] := deqPrevOld;
          deqPrevOld.2[DEQ_NEXT] := deqNextOld;
          links[BRANCH_1] := entry;
          links[BRANCH_2] := entry;

          break loopBody;
        } else {
          parent := entry;
          shiftingHash >>= HASH_OFFSET;
          entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
        };
      };
    };

    (root, [var size]);
  };

  public func fromIterDesc<K>(iter: IterNext<K>, hashUtils: HashUtils<K>): Set<K> {
    let root = createRoot<K>(hashUtils.2());
    let rootLinks = root.2;
    let getHash = hashUtils.0;
    let areEqual = hashUtils.1;
    var size = 0:Nat32;

    for (item in iter) label loopBody {
      let hashParam = getHash(item);
      var shiftingHash = hashParam;
      var parent = root;
      var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

      loop {
        let hash = entry.1;

        if (hash == ROOT) {
          let deqNext = rootLinks[DEQ_NEXT];
          let newEntry = (item, hashParam, [var root, root, root, root, root, deqNext]);

          deqNext.2[DEQ_PREV] := newEntry;
          rootLinks[DEQ_NEXT] := newEntry;
          parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
          size +%= 1;

          break loopBody;
        } else if (hash == hashParam and areEqual(entry.0, item)) {
          let links = entry.2;
          let deqNext = rootLinks[DEQ_NEXT];
          let newEntry = (entry.0, hash, [var links[BRANCH_1], links[BRANCH_2], links[BRANCH_3], links[BRANCH_4], root, deqNext]);

          deqNext.2[DEQ_PREV] := newEntry;
          rootLinks[DEQ_NEXT] := newEntry;
          parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

          let deqNextOld = links[DEQ_NEXT];
          let deqPrevOld = links[DEQ_PREV];

          deqPrevOld.2[DEQ_NEXT] := deqNextOld;
          deqNextOld.2[DEQ_PREV] := deqPrevOld;
          links[BRANCH_1] := entry;
          links[BRANCH_2] := entry;

          break loopBody;
        } else {
          parent := entry;
          shiftingHash >>= HASH_OFFSET;
          entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
        };
      };
    };

    (root, [var size]);
  };

  public func fromIterMap<K, T>(iter: IterNext<T>, hashUtils: HashUtils<K>, mapItem: (T) -> ?K): Set<K> {
    let root = createRoot<K>(hashUtils.2());
    let rootLinks = root.2;
    let getHash = hashUtils.0;
    let areEqual = hashUtils.1;
    var size = 0:Nat32;

    for (item in iter) label loopBody switch (mapItem(item)) {
      case (?item) {
        let hashParam = getHash(item);
        var shiftingHash = hashParam;
        var parent = root;
        var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

        loop {
          let hash = entry.1;

          if (hash == ROOT) {
            let deqPrev = rootLinks[DEQ_PREV];
            let newEntry = (item, hashParam, [var root, root, root, root, deqPrev, root]);

            deqPrev.2[DEQ_NEXT] := newEntry;
            rootLinks[DEQ_PREV] := newEntry;
            parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
            size +%= 1;

            break loopBody;
          } else if (hash == hashParam and areEqual(entry.0, item)) {
            let links = entry.2;
            let deqPrev = rootLinks[DEQ_PREV];
            let newEntry = (entry.0, hash, [var links[BRANCH_1], links[BRANCH_2], links[BRANCH_3], links[BRANCH_4], deqPrev, root]);

            deqPrev.2[DEQ_NEXT] := newEntry;
            rootLinks[DEQ_PREV] := newEntry;
            parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

            let deqPrevOld = links[DEQ_PREV];
            let deqNextOld = links[DEQ_NEXT];

            deqNextOld.2[DEQ_PREV] := deqPrevOld;
            deqPrevOld.2[DEQ_NEXT] := deqNextOld;
            links[BRANCH_1] := entry;
            links[BRANCH_2] := entry;

            break loopBody;
          } else {
            parent := entry;
            shiftingHash >>= HASH_OFFSET;
            entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
          };
        };
      };

      case (_) {};
    };

    (root, [var size]);
  };

  public func fromIterMapDesc<K, T>(iter: IterNext<T>, hashUtils: HashUtils<K>, mapItem: (T) -> ?K): Set<K> {
    let root = createRoot<K>(hashUtils.2());
    let rootLinks = root.2;
    let getHash = hashUtils.0;
    let areEqual = hashUtils.1;
    var size = 0:Nat32;

    for (item in iter) label loopBody switch (mapItem(item)) {
      case (?item) {
        let hashParam = getHash(item);
        var shiftingHash = hashParam;
        var parent = root;
        var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

        loop {
          let hash = entry.1;

          if (hash == ROOT) {
            let deqNext = rootLinks[DEQ_NEXT];
            let newEntry = (item, hashParam, [var root, root, root, root, root, deqNext]);

            deqNext.2[DEQ_PREV] := newEntry;
            rootLinks[DEQ_NEXT] := newEntry;
            parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
            size +%= 1;

            break loopBody;
          } else if (hash == hashParam and areEqual(entry.0, item)) {
            let links = entry.2;
            let deqNext = rootLinks[DEQ_NEXT];
            let newEntry = (entry.0, hash, [var links[BRANCH_1], links[BRANCH_2], links[BRANCH_3], links[BRANCH_4], root, deqNext]);

            deqNext.2[DEQ_PREV] := newEntry;
            rootLinks[DEQ_NEXT] := newEntry;
            parent.2[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

            let deqNextOld = links[DEQ_NEXT];
            let deqPrevOld = links[DEQ_PREV];

            deqPrevOld.2[DEQ_NEXT] := deqNextOld;
            deqNextOld.2[DEQ_PREV] := deqPrevOld;
            links[BRANCH_1] := entry;
            links[BRANCH_2] := entry;

            break loopBody;
          } else {
            parent := entry;
            shiftingHash >>= HASH_OFFSET;
            entry := entry.2[nat(shiftingHash % HASH_CHUNK_SIZE)];
          };
        };
      };

      case (_) {};
    };

    (root, [var size]);
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

  public func toArrayMap<K, T>(map: Set<K>, mapEntry: (K) -> ?T): [T] {
    var entry = map.0;
    var maxSize = map.1[SIZE];
    var array = initArray<?T>(nat(maxSize), null);
    var arraySize = 0:Nat32;

    loop {
      entry := entry.2[DEQ_NEXT];

      let hash = entry.1;

      if (hash == ROOT) {
        return tabulateArray<T>(nat(arraySize), func(i) = switch (array[i]) { case (?value) value; case (_) trap("unreachable") })
      } else if (entry.2[BRANCH_1].1 != hash or entry.2[BRANCH_2].1 != hash) switch (mapEntry(entry.0)) {
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

  public func toArrayMapDesc<K, T>(map: Set<K>, mapEntry: (K) -> ?T): [T] {
    var entry = map.0;
    var maxSize = map.1[SIZE];
    var array = initArray<?T>(nat(maxSize), null);
    var arraySize = 0:Nat32;

    loop {
      entry := entry.2[DEQ_PREV];

      let hash = entry.1;

      if (hash == ROOT) {
        return tabulateArray<T>(nat(arraySize), func(i) = switch (array[i]) { case (?value) value; case (_) trap("unreachable") })
      } else if (entry.2[BRANCH_1].1 != hash or entry.2[BRANCH_2].1 != hash) switch (mapEntry(entry.0)) {
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

  public func hashInt(key: Int): Nat32 {
    var hash = Prim.intToNat64Wrap(key);

    hash := hash >> 30 ^ hash *% 0xbf58476d1ce4e5b9;
    hash := hash >> 27 ^ hash *% 0x94d049bb133111eb;

    Prim.intToNat32Wrap(Prim.nat64ToNat(hash >> 31 ^ hash & 0x3fffffff));
  };

  public func hashInt8(key: Int8): Nat32 {
    var hash = Prim.intToNat32Wrap(Prim.int8ToInt(key));

    hash := hash >> 16 ^ hash *% 0x21f0aaad;
    hash := hash >> 15 ^ hash *% 0x735a2d97;

    hash >> 15 ^ hash & 0x3fffffff;
  };

  public func hashInt16(key: Int16): Nat32 {
    var hash = Prim.intToNat32Wrap(Prim.int16ToInt(key));

    hash := hash >> 16 ^ hash *% 0x21f0aaad;
    hash := hash >> 15 ^ hash *% 0x735a2d97;

    hash >> 15 ^ hash & 0x3fffffff;
  };

  public func hashInt32(key: Int32): Nat32 {
    var hash = Prim.int32ToNat32(key);

    hash := hash >> 16 ^ hash *% 0x21f0aaad;
    hash := hash >> 15 ^ hash *% 0x735a2d97;

    hash >> 15 ^ hash & 0x3fffffff;
  };

  public func hashInt64(key: Int64): Nat32 {
    var hash = Prim.int64ToNat64(key);

    hash := hash >> 30 ^ hash *% 0xbf58476d1ce4e5b9;
    hash := hash >> 27 ^ hash *% 0x94d049bb133111eb;

    Prim.intToNat32Wrap(Prim.nat64ToNat(hash >> 31 ^ hash & 0x3fffffff));
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func hashNat(key: Nat): Nat32 {
    var hash = Prim.intToNat64Wrap(key);

    hash := hash >> 30 ^ hash *% 0xbf58476d1ce4e5b9;
    hash := hash >> 27 ^ hash *% 0x94d049bb133111eb;

    Prim.intToNat32Wrap(Prim.nat64ToNat(hash >> 31 ^ hash & 0x3fffffff));
  };

  public func hashNat8(key: Nat8): Nat32 {
    var hash = Prim.intToNat32Wrap(Prim.nat8ToNat(key));

    hash := hash >> 16 ^ hash *% 0x21f0aaad;
    hash := hash >> 15 ^ hash *% 0x735a2d97;

    hash >> 15 ^ hash & 0x3fffffff;
  };

  public func hashNat16(key: Nat16): Nat32 {
    var hash = Prim.intToNat32Wrap(Prim.nat16ToNat(key));

    hash := hash >> 16 ^ hash *% 0x21f0aaad;
    hash := hash >> 15 ^ hash *% 0x735a2d97;

    hash >> 15 ^ hash & 0x3fffffff;
  };

  public func hashNat32(key: Nat32): Nat32 {
    var hash = key;

    hash := hash >> 16 ^ hash *% 0x21f0aaad;
    hash := hash >> 15 ^ hash *% 0x735a2d97;

    hash >> 15 ^ hash & 0x3fffffff;
  };

  public func hashNat64(key: Nat64): Nat32 {
    var hash = key;

    hash := hash >> 30 ^ hash *% 0xbf58476d1ce4e5b9;
    hash := hash >> 27 ^ hash *% 0x94d049bb133111eb;

    Prim.intToNat32Wrap(Prim.nat64ToNat(hash >> 31 ^ hash & 0x3fffffff));
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

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
    if (key) 114489971 else 0;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public let ihash = (hashInt, func(a, b) = a == b, func() = 0):HashUtils<Int>;

  public let i8hash = (hashInt8, func(a, b) = a == b, func() = 0):HashUtils<Int8>;

  public let i16hash = (hashInt16, func(a, b) = a == b, func() = 0):HashUtils<Int16>;

  public let i32hash = (hashInt32, func(a, b) = a == b, func() = 0):HashUtils<Int32>;

  public let i64hash = (hashInt64, func(a, b) = a == b, func() = 0):HashUtils<Int64>;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public let nhash = (hashNat, func(a, b) = a == b, func() = 0):HashUtils<Nat>;

  public let n8hash = (hashNat8, func(a, b) = a == b, func() = 0):HashUtils<Nat8>;

  public let n16hash = (hashNat16, func(a, b) = a == b, func() = 0):HashUtils<Nat16>;

  public let n32hash = (hashNat32, func(a, b) = a == b, func() = 0):HashUtils<Nat32>;

  public let n64hash = (hashNat64, func(a, b) = a == b, func() = 0):HashUtils<Nat64>;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public let thash = (hashText, func(a, b) = a == b, func() = ""):HashUtils<Text>;

  public let phash = (hashPrincipal, func(a, b) = a == b, func() = Prim.principalOfBlob("")):HashUtils<Principal>;

  public let bhash = (hashBlob, func(a, b) = a == b, func() = ""):HashUtils<Blob>;

  public let lhash = (hashBool, func(a, b) = a == b, func() = false):HashUtils<Bool>;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func useHash<K>(hashUtils: HashUtils<K>, hash: Nat32): HashUtils<K> {
    (func(key) = hash, hashUtils.1, hashUtils.2);
  };

  public func calcHash<K>(hashUtils: HashUtils<K>, key: K): HashUtils<K> {
    let hash = hashUtils.0(key);

    (func(key) = hash, hashUtils.1, hashUtils.2);
  };
};
