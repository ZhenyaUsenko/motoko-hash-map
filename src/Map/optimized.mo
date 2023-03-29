import Prim "mo:prim";

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

  func createRoot<K, V>(nullKey: K): Entry<K, V> {
    let temp = (nullKey, null, ROOT, [var]):Entry<K, V>;
    let rootLinks = [var temp, temp, temp, temp, temp, temp];
    let root = (nullKey, null, ROOT, rootLinks);

    rootLinks[BRANCH_1] := root;
    rootLinks[BRANCH_2] := root;
    rootLinks[BRANCH_3] := root;
    rootLinks[BRANCH_4] := root;
    rootLinks[DEQ_PREV] := root;
    rootLinks[DEQ_NEXT] := root;

    root;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func cloneEntry<K, V>(entry: Entry<K, V>, newRoot: Entry<K, V>): Entry<K, V> {
    let links = entry.3;
    let branch1 = links[BRANCH_1];
    let branch2 = links[BRANCH_2];
    let branch3 = links[BRANCH_3];
    let branch4 = links[BRANCH_4];

    let newEntry = (entry.0, entry.1, entry.2, [
      var if (branch1.2 != ROOT) cloneEntry(branch1, newRoot) else newRoot,
      if (branch2.2 != ROOT) cloneEntry(branch2, newRoot) else newRoot,
      if (branch3.2 != ROOT) cloneEntry(branch3, newRoot) else newRoot,
      if (branch4.2 != ROOT) cloneEntry(branch4, newRoot) else newRoot,
      newRoot,
      newRoot,
    ]);

    links[TEMP_LINK] := newEntry;

    newEntry;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func new<K, V>(hashUtils: HashUtils<K>): Map<K, V> {
    (createRoot(hashUtils.2()), [var 0]);
  };

  public func clear<K, V>(map: Map<K, V>) {
    let root = map.0;
    let rootLinks = root.3;

    rootLinks[BRANCH_1] := root;
    rootLinks[BRANCH_2] := root;
    rootLinks[BRANCH_3] := root;
    rootLinks[BRANCH_4] := root;
    rootLinks[DEQ_PREV] := root;
    rootLinks[DEQ_NEXT] := root;

    map.1[SIZE] := 0;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func size<K, V>(map: Map<K, V>): Nat {
    nat(map.1[SIZE]);
  };

  public func empty<K, V>(map: Map<K, V>): Bool {
    map.1[SIZE] == 0;
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

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func get<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): ?V {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;
    var entry = map.0.3[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      let hash = entry.2;

      if (hash == ROOT) {
        return null;
      } else if (hash == hashParam and hashUtils.1(entry.0, keyParam)) {
        return entry.1;
      } else {
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
      let hash = entry.2;

      if (hash == ROOT) {
        return false;
      } else if (hash == hashParam and hashUtils.1(entry.0, keyParam)) {
        return true;
      } else {
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func contains<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): ?Bool {
    if (map.1[SIZE] == 0) return null;

    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;
    var entry = map.0.3[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      let hash = entry.2;

      if (hash == ROOT) {
        return ?false;
      } else if (hash == hashParam and hashUtils.1(entry.0, keyParam)) {
        return ?true;
      } else {
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func put<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V): ?V {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;

    let root = map.0;
    var parent = root;

    let rootLinks = parent.3;
    var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      let hash = entry.2;

      if (hash == ROOT) {
        let deqPrev = rootLinks[DEQ_PREV];
        let newEntry = (keyParam, ?valueParam, hashParam, [var root, root, root, root, deqPrev, root]);

        deqPrev.3[DEQ_NEXT] := newEntry;
        rootLinks[DEQ_PREV] := newEntry;
        parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        map.1[SIZE] +%= 1;

        return null;
      } else if (hash == hashParam and hashUtils.1(entry.0, keyParam)) {
        let newEntry = (entry.0, ?valueParam, hash, entry.3);

        entry.3[DEQ_PREV].3[DEQ_NEXT] := newEntry;
        entry.3[DEQ_NEXT].3[DEQ_PREV] := newEntry;
        parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

        return entry.1;
      } else {
        parent := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func putFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V): ?V {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;

    let root = map.0;
    var parent = root;

    let rootLinks = parent.3;
    var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      let hash = entry.2;

      if (hash == ROOT) {
        let deqNext = rootLinks[DEQ_NEXT];
        let newEntry = (keyParam, ?valueParam, hashParam, [var root, root, root, root, root, deqNext]);

        deqNext.3[DEQ_PREV] := newEntry;
        rootLinks[DEQ_NEXT] := newEntry;
        parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        map.1[SIZE] +%= 1;

        return null;
      } else if (hash == hashParam and hashUtils.1(entry.0, keyParam)) {
        let newEntry = (entry.0, ?valueParam, hash, entry.3);

        entry.3[DEQ_NEXT].3[DEQ_PREV] := newEntry;
        entry.3[DEQ_PREV].3[DEQ_NEXT] := newEntry;
        parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

        return entry.1;
      } else {
        parent := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func set<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V) {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;

    let root = map.0;
    var parent = root;

    let rootLinks = parent.3;
    var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      let hash = entry.2;

      if (hash == ROOT) {
        let deqPrev = rootLinks[DEQ_PREV];
        let newEntry = (keyParam, ?valueParam, hashParam, [var root, root, root, root, deqPrev, root]);

        deqPrev.3[DEQ_NEXT] := newEntry;
        rootLinks[DEQ_PREV] := newEntry;
        parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        map.1[SIZE] +%= 1;

        return;
      } else if (hash == hashParam and hashUtils.1(entry.0, keyParam)) {
        let newEntry = (entry.0, ?valueParam, hash, entry.3);

        entry.3[DEQ_PREV].3[DEQ_NEXT] := newEntry;
        entry.3[DEQ_NEXT].3[DEQ_PREV] := newEntry;
        parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

        return;
      } else {
        parent := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func setFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V) {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;

    let root = map.0;
    var parent = root;

    let rootLinks = parent.3;
    var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      let hash = entry.2;

      if (hash == ROOT) {
        let deqNext = rootLinks[DEQ_NEXT];
        let newEntry = (keyParam, ?valueParam, hashParam, [var root, root, root, root, root, deqNext]);

        deqNext.3[DEQ_PREV] := newEntry;
        rootLinks[DEQ_NEXT] := newEntry;
        parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        map.1[SIZE] +%= 1;

        return;
      } else if (hash == hashParam and hashUtils.1(entry.0, keyParam)) {
        let newEntry = (entry.0, ?valueParam, hash, entry.3);

        entry.3[DEQ_NEXT].3[DEQ_PREV] := newEntry;
        entry.3[DEQ_PREV].3[DEQ_NEXT] := newEntry;
        parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

        return;
      } else {
        parent := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func add<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V): ?V {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;

    let root = map.0;
    var parent = root;

    let rootLinks = parent.3;
    var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      let hash = entry.2;

      if (hash == ROOT) {
        let deqPrev = rootLinks[DEQ_PREV];
        let newEntry = (keyParam, ?valueParam, hashParam, [var root, root, root, root, deqPrev, root]);

        deqPrev.3[DEQ_NEXT] := newEntry;
        rootLinks[DEQ_PREV] := newEntry;
        parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        map.1[SIZE] +%= 1;

        return null;
      } else if (hash == hashParam and hashUtils.1(entry.0, keyParam)) {
        return entry.1;
      } else {
        parent := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func addFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V): ?V {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;

    let root = map.0;
    var parent = root;

    let rootLinks = parent.3;
    var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      let hash = entry.2;

      if (hash == ROOT) {
        let deqNext = rootLinks[DEQ_NEXT];
        let newEntry = (keyParam, ?valueParam, hashParam, [var root, root, root, root, root, deqNext]);

        deqNext.3[DEQ_PREV] := newEntry;
        rootLinks[DEQ_NEXT] := newEntry;
        parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        map.1[SIZE] +%= 1;

        return null;
      } else if (hash == hashParam and hashUtils.1(entry.0, keyParam)) {
        return entry.1;
      } else {
        parent := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func replace<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V): ?V {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;

    let root = map.0;
    var parent = root;
    var entry = parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      let hash = entry.2;

      if (hash == ROOT) {
        return null;
      } else if (hash == hashParam and hashUtils.1(entry.0, keyParam)) {
        let newEntry = (entry.0, ?valueParam, hash, entry.3);

        entry.3[DEQ_PREV].3[DEQ_NEXT] := newEntry;
        entry.3[DEQ_NEXT].3[DEQ_PREV] := newEntry;
        parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

        return entry.1;
      } else {
        parent := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func update<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, getNewValue: (K, ?V) -> ?V): ?V {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;

    let root = map.0;
    var parent = root;

    let rootLinks = parent.3;
    var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      let hash = entry.2;

      if (hash == ROOT) {
        switch (getNewValue(keyParam, null)) {
          case (null) return null;

          case (newValue) {
            let deqPrev = rootLinks[DEQ_PREV];
            let newEntry = (keyParam, newValue, hashParam, [var root, root, root, root, deqPrev, root]);

            deqPrev.3[DEQ_NEXT] := newEntry;
            rootLinks[DEQ_PREV] := newEntry;
            parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
            map.1[SIZE] +%= 1;

            return newValue;
          };
        };
      } else if (hash == hashParam and hashUtils.1(entry.0, keyParam)) {
        switch (getNewValue(keyParam, entry.1)) {
          case (null) return entry.1;

          case (newValue) {
            let newEntry = (entry.0, newValue, hash, entry.3);

            entry.3[DEQ_PREV].3[DEQ_NEXT] := newEntry;
            entry.3[DEQ_NEXT].3[DEQ_PREV] := newEntry;
            parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

            return newValue;
          };
        };
      } else {
        parent := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func updateFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, getNewValue: (K, ?V) -> ?V): ?V {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;

    let root = map.0;
    var parent = root;

    let rootLinks = parent.3;
    var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      let hash = entry.2;

      if (hash == ROOT) {
        switch (getNewValue(keyParam, null)) {
          case (null) return null;

          case (newValue) {
            let deqNext = rootLinks[DEQ_NEXT];
            let newEntry = (keyParam, newValue, hashParam, [var root, root, root, root, root, deqNext]);

            deqNext.3[DEQ_PREV] := newEntry;
            rootLinks[DEQ_NEXT] := newEntry;
            parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
            map.1[SIZE] +%= 1;

            return newValue;
          };
        };
      } else if (hash == hashParam and hashUtils.1(entry.0, keyParam)) {
        switch (getNewValue(keyParam, entry.1)) {
          case (null) return entry.1;

          case (newValue) {
            let newEntry = (entry.0, newValue, hash, entry.3);

            entry.3[DEQ_NEXT].3[DEQ_PREV] := newEntry;
            entry.3[DEQ_PREV].3[DEQ_NEXT] := newEntry;
            parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

            return newValue;
          };
        };
      } else {
        parent := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func putMove<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: ?V): ?V {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;

    let root = map.0;
    var parent = root;

    let rootLinks = parent.3;
    var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      let hash = entry.2;

      if (hash == ROOT) {
        switch (valueParam) {
          case (null) return null;

          case (_) {
            let deqPrev = rootLinks[DEQ_PREV];
            let newEntry = (keyParam, valueParam, hashParam, [var root, root, root, root, deqPrev, root]);

            deqPrev.3[DEQ_NEXT] := newEntry;
            rootLinks[DEQ_PREV] := newEntry;
            parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
            map.1[SIZE] +%= 1;

            return null;
          };
        };
      } else if (hash == hashParam and hashUtils.1(entry.0, keyParam)) {
        let links = entry.3;
        let deqPrev = rootLinks[DEQ_PREV];

        let newEntry = (
          entry.0,
          switch (valueParam) { case (null) entry.1; case (_) valueParam },
          hash,
          [var links[BRANCH_1], links[BRANCH_2], links[BRANCH_3], links[BRANCH_4], deqPrev, root],
        );

        deqPrev.3[DEQ_NEXT] := newEntry;
        rootLinks[DEQ_PREV] := newEntry;
        parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

        let deqPrevOld = links[DEQ_PREV];
        let deqNextOld = links[DEQ_NEXT];

        deqNextOld.3[DEQ_PREV] := deqPrevOld;
        deqPrevOld.3[DEQ_NEXT] := deqNextOld;
        links[BRANCH_1] := entry;
        links[BRANCH_2] := entry;

        return entry.1;
      } else {
        parent := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func putMoveFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: ?V): ?V {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;

    let root = map.0;
    var parent = root;

    let rootLinks = parent.3;
    var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      let hash = entry.2;

      if (hash == ROOT) {
        switch (valueParam) {
          case (null) return null;

          case (_) {
            let deqNext = rootLinks[DEQ_NEXT];
            let newEntry = (keyParam, valueParam, hashParam, [var root, root, root, root, root, deqNext]);

            deqNext.3[DEQ_PREV] := newEntry;
            rootLinks[DEQ_NEXT] := newEntry;
            parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
            map.1[SIZE] +%= 1;

            return null;
          };
        };
      } else if (hash == hashParam and hashUtils.1(entry.0, keyParam)) {
        let links = entry.3;
        let deqNext = rootLinks[DEQ_NEXT];

        let newEntry = (
          entry.0,
          switch (valueParam) { case (null) entry.1; case (_) valueParam },
          hash,
          [var links[BRANCH_1], links[BRANCH_2], links[BRANCH_3], links[BRANCH_4], root, deqNext],
        );

        deqNext.3[DEQ_PREV] := newEntry;
        rootLinks[DEQ_NEXT] := newEntry;
        parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

        let deqNextOld = links[DEQ_NEXT];
        let deqPrevOld = links[DEQ_PREV];

        deqPrevOld.3[DEQ_NEXT] := deqNextOld;
        deqNextOld.3[DEQ_PREV] := deqPrevOld;
        links[BRANCH_1] := entry;
        links[BRANCH_2] := entry;

        return entry.1;
      } else {
        parent := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func replaceMove<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: ?V): ?V {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;

    let root = map.0;
    var parent = root;

    let rootLinks = parent.3;
    var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      let hash = entry.2;

      if (hash == ROOT) {
        return null;
      } else if (hash == hashParam and hashUtils.1(entry.0, keyParam)) {
        let links = entry.3;
        let deqPrev = rootLinks[DEQ_PREV];

        let newEntry = (
          entry.0,
          switch (valueParam) { case (null) entry.1; case (_) valueParam },
          hash,
          [var links[BRANCH_1], links[BRANCH_2], links[BRANCH_3], links[BRANCH_4], deqPrev, root],
        );

        deqPrev.3[DEQ_NEXT] := newEntry;
        rootLinks[DEQ_PREV] := newEntry;
        parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

        let deqPrevOld = links[DEQ_PREV];
        let deqNextOld = links[DEQ_NEXT];

        deqNextOld.3[DEQ_PREV] := deqPrevOld;
        deqPrevOld.3[DEQ_NEXT] := deqNextOld;
        links[BRANCH_1] := entry;
        links[BRANCH_2] := entry;

        return entry.1;
      } else {
        parent := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func replaceMoveFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: ?V): ?V {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;

    let root = map.0;
    var parent = root;

    let rootLinks = parent.3;
    var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      let hash = entry.2;

      if (hash == ROOT) {
        return null;
      } else if (hash == hashParam and hashUtils.1(entry.0, keyParam)) {
        let links = entry.3;
        let deqNext = rootLinks[DEQ_NEXT];

        let newEntry = (
          entry.0,
          switch (valueParam) { case (null) entry.1; case (_) valueParam },
          hash,
          [var links[BRANCH_1], links[BRANCH_2], links[BRANCH_3], links[BRANCH_4], root, deqNext],
        );

        deqNext.3[DEQ_PREV] := newEntry;
        rootLinks[DEQ_NEXT] := newEntry;
        parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

        let deqNextOld = links[DEQ_NEXT];
        let deqPrevOld = links[DEQ_PREV];

        deqPrevOld.3[DEQ_NEXT] := deqNextOld;
        deqNextOld.3[DEQ_PREV] := deqPrevOld;
        links[BRANCH_1] := entry;
        links[BRANCH_2] := entry;

        return entry.1;
      } else {
        parent := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func updateMove<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, getNewValue: (K, ?V) -> ?V): ?V {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;

    let root = map.0;
    var parent = root;

    let rootLinks = parent.3;
    var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      let hash = entry.2;

      if (hash == ROOT) {
        switch (getNewValue(keyParam, null)) {
          case (null) return null;

          case (newValue) {
            let deqPrev = rootLinks[DEQ_PREV];
            let newEntry = (keyParam, newValue, hashParam, [var root, root, root, root, deqPrev, root]);

            deqPrev.3[DEQ_NEXT] := newEntry;
            rootLinks[DEQ_PREV] := newEntry;
            parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
            map.1[SIZE] +%= 1;

            return newValue;
          };
        };
      } else if (hash == hashParam and hashUtils.1(entry.0, keyParam)) {
        let links = entry.3;
        let deqPrev = rootLinks[DEQ_PREV];

        let newValue = switch (getNewValue(keyParam, entry.1)) { case (null) entry.1; case (newValue) newValue };

        let newEntry = (entry.0, newValue, hash, [var links[BRANCH_1], links[BRANCH_2], links[BRANCH_3], links[BRANCH_4], deqPrev, root]);

        deqPrev.3[DEQ_NEXT] := newEntry;
        rootLinks[DEQ_PREV] := newEntry;
        parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

        let deqPrevOld = links[DEQ_PREV];
        let deqNextOld = links[DEQ_NEXT];

        deqNextOld.3[DEQ_PREV] := deqPrevOld;
        deqPrevOld.3[DEQ_NEXT] := deqNextOld;
        links[BRANCH_1] := entry;
        links[BRANCH_2] := entry;

        return newValue;
      } else {
        parent := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func updateMoveFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, getNewValue: (K, ?V) -> ?V): ?V {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;

    let root = map.0;
    var parent = root;

    let rootLinks = parent.3;
    var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      let hash = entry.2;

      if (hash == ROOT) {
        switch (getNewValue(keyParam, null)) {
          case (null) return null;

          case (newValue) {
            let deqNext = rootLinks[DEQ_NEXT];
            let newEntry = (keyParam, newValue, hashParam, [var root, root, root, root, root, deqNext]);

            deqNext.3[DEQ_PREV] := newEntry;
            rootLinks[DEQ_NEXT] := newEntry;
            parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
            map.1[SIZE] +%= 1;

            return newValue;
          };
        };
      } else if (hash == hashParam and hashUtils.1(entry.0, keyParam)) {
        let links = entry.3;
        let deqNext = rootLinks[DEQ_NEXT];

        let newValue = switch (getNewValue(keyParam, entry.1)) { case (null) entry.1; case (newValue) newValue };

        let newEntry = (entry.0, newValue, hash, [var links[BRANCH_1], links[BRANCH_2], links[BRANCH_3], links[BRANCH_4], root, deqNext]);

        deqNext.3[DEQ_PREV] := newEntry;
        rootLinks[DEQ_NEXT] := newEntry;
        parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

        let deqNextOld = links[DEQ_NEXT];
        let deqPrevOld = links[DEQ_PREV];

        deqPrevOld.3[DEQ_NEXT] := deqNextOld;
        deqNextOld.3[DEQ_PREV] := deqPrevOld;
        links[BRANCH_1] := entry;
        links[BRANCH_2] := entry;

        return newValue;
      } else {
        parent := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func putBefore<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, placeParam: ?K, valueParam: ?V): ?V {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;

    let root = map.0;
    var parent = root;
    var entry = parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)];

    let placeKeyParam = switch (placeParam) { case (?placeKeyParam) placeKeyParam; case (_) root.0 };

    let placeHashParam = switch (placeParam) { case (null) ROOT; case (_) hashUtils.0(placeKeyParam) };

    var shiftingPlaceHash = placeHashParam;

    var place = if (shiftingPlaceHash != ROOT) root.3[nat(shiftingPlaceHash % HASH_CHUNK_SIZE)] else root;

    loop {
      let placeHash = place.2;

      if (placeHash == ROOT or placeHash == placeHashParam and hashUtils.1(place.0, placeKeyParam)) loop {
        let hash = entry.2;

        if (hash == ROOT) {
          switch (valueParam) {
            case (null) return null;

            case (_) {
              let deqPrev = place.3[DEQ_PREV];
              let newEntry = (keyParam, valueParam, hashParam, [var root, root, root, root, deqPrev, place]);

              deqPrev.3[DEQ_NEXT] := newEntry;
              place.3[DEQ_PREV] := newEntry;
              parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
              map.1[SIZE] +%= 1;

              return null;
            };
          };
        } else if (hash == hashParam and hashUtils.1(entry.0, keyParam)) {
          let links = entry.3;
          let deqPrev = place.3[DEQ_PREV];

          let newEntry = (
            entry.0,
            switch (valueParam) { case (null) entry.1; case (_) valueParam },
            hash,
            [var links[BRANCH_1], links[BRANCH_2], links[BRANCH_3], links[BRANCH_4], deqPrev, place],
          );

          deqPrev.3[DEQ_NEXT] := newEntry;
          place.3[DEQ_PREV] := newEntry;
          parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

          let deqPrevOld = links[DEQ_PREV];
          let deqNextOld = links[DEQ_NEXT];

          deqNextOld.3[DEQ_PREV] := deqPrevOld;
          deqPrevOld.3[DEQ_NEXT] := deqNextOld;
          links[BRANCH_1] := entry;
          links[BRANCH_2] := entry;

          return entry.1;
        } else {
          parent := entry;
          shiftingHash >>= HASH_OFFSET;
          entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
        };
      } else {
        shiftingPlaceHash >>= HASH_OFFSET;
        place := place.3[nat(shiftingPlaceHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func putAfter<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, placeParam: ?K, valueParam: ?V): ?V {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;

    let root = map.0;
    var parent = root;
    var entry = parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)];

    let placeKeyParam = switch (placeParam) { case (?placeKeyParam) placeKeyParam; case (_) root.0 };

    let placeHashParam = switch (placeParam) { case (null) ROOT; case (_) hashUtils.0(placeKeyParam) };

    var shiftingPlaceHash = placeHashParam;

    var place = if (shiftingPlaceHash != ROOT) root.3[nat(shiftingPlaceHash % HASH_CHUNK_SIZE)] else root;

    loop {
      let placeHash = place.2;

      if (placeHash == ROOT or placeHash == placeHashParam and hashUtils.1(place.0, placeKeyParam)) loop {
        let hash = entry.2;

        if (hash == ROOT) {
          switch (valueParam) {
            case (null) return null;

            case (_) {
              let deqNext = place.3[DEQ_NEXT];
              let newEntry = (keyParam, valueParam, hashParam, [var root, root, root, root, place, deqNext]);

              deqNext.3[DEQ_PREV] := newEntry;
              place.3[DEQ_NEXT] := newEntry;
              parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
              map.1[SIZE] +%= 1;

              return null;
            };
          };
        } else if (hash == hashParam and hashUtils.1(entry.0, keyParam)) {
          let links = entry.3;
          let deqNext = place.3[DEQ_NEXT];

          let newEntry = (
            entry.0,
            switch (valueParam) { case (null) entry.1; case (_) valueParam },
            hash,
            [var links[BRANCH_1], links[BRANCH_2], links[BRANCH_3], links[BRANCH_4], place, deqNext],
          );

          deqNext.3[DEQ_PREV] := newEntry;
          place.3[DEQ_NEXT] := newEntry;
          parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

          let deqNextOld = links[DEQ_NEXT];
          let deqPrevOld = links[DEQ_PREV];

          deqPrevOld.3[DEQ_NEXT] := deqNextOld;
          deqNextOld.3[DEQ_PREV] := deqPrevOld;
          links[BRANCH_1] := entry;
          links[BRANCH_2] := entry;

          return entry.1;
        } else {
          parent := entry;
          shiftingHash >>= HASH_OFFSET;
          entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
        };
      } else {
        shiftingPlaceHash >>= HASH_OFFSET;
        place := place.3[nat(shiftingPlaceHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func remove<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): ?V {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;

    let root = map.0;
    var parent = root;
    var entry = parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      let hash = entry.2;

      if (hash == hashParam and hashUtils.1(entry.0, keyParam)) {
        let links = entry.3;
        let deqPrev = links[DEQ_PREV];
        let deqNext = links[DEQ_NEXT];

        deqNext.3[DEQ_PREV] := deqPrev;
        deqPrev.3[DEQ_NEXT] := deqNext;
        map.1[SIZE] -%= 1;

        var leaf = entry;
        var leafParent = leaf;
        var leafIndex = NULL_BRANCH;

        loop {
          let leafLinks = leaf.3;
          let branch1 = leafLinks[BRANCH_1];

          if (branch1.2 != ROOT) {
            leafParent := leaf;
            leaf := branch1;
            leafIndex := BRANCH_1;
          } else {
            let branch2 = leafLinks[BRANCH_2];

            if (branch2.2 != ROOT) {
              leafParent := leaf;
              leaf := branch2;
              leafIndex := BRANCH_2;
            } else {
              let branch3 = leafLinks[BRANCH_3];

              if (branch3.2 != ROOT) {
                leafParent := leaf;
                leaf := branch3;
                leafIndex := BRANCH_3;
              } else {
                let branch4 = leafLinks[BRANCH_4];

                if (branch4.2 != ROOT) {
                  leafParent := leaf;
                  leaf := branch4;
                  leafIndex := BRANCH_4;
                } else {
                  if (leafIndex == NULL_BRANCH) {
                    parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := root;
                  } else {
                    parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := leaf;
                    leafParent.3[leafIndex] := root;

                    leafLinks[BRANCH_1] := links[BRANCH_1];
                    leafLinks[BRANCH_2] := links[BRANCH_2];
                    leafLinks[BRANCH_3] := links[BRANCH_3];
                    leafLinks[BRANCH_4] := links[BRANCH_4];
                  };

                  links[BRANCH_1] := entry;
                  links[BRANCH_2] := entry;

                  return entry.1;
                };
              };
            };
          };
        };
      } else if (hash == ROOT) {
        return null;
      } else {
        parent := entry;
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func delete<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K) {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;

    let root = map.0;
    var parent = root;
    var entry = parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop {
      let hash = entry.2;

      if (hash == hashParam and hashUtils.1(entry.0, keyParam)) {
        let links = entry.3;
        let deqPrev = links[DEQ_PREV];
        let deqNext = links[DEQ_NEXT];

        deqNext.3[DEQ_PREV] := deqPrev;
        deqPrev.3[DEQ_NEXT] := deqNext;
        map.1[SIZE] -%= 1;

        var leaf = entry;
        var leafParent = leaf;
        var leafIndex = NULL_BRANCH;

        loop {
          let leafLinks = leaf.3;
          let branch1 = leafLinks[BRANCH_1];

          if (branch1.2 != ROOT) {
            leafParent := leaf;
            leaf := branch1;
            leafIndex := BRANCH_1;
          } else {
            let branch2 = leafLinks[BRANCH_2];

            if (branch2.2 != ROOT) {
              leafParent := leaf;
              leaf := branch2;
              leafIndex := BRANCH_2;
            } else {
              let branch3 = leafLinks[BRANCH_3];

              if (branch3.2 != ROOT) {
                leafParent := leaf;
                leaf := branch3;
                leafIndex := BRANCH_3;
              } else {
                let branch4 = leafLinks[BRANCH_4];

                if (branch4.2 != ROOT) {
                  leafParent := leaf;
                  leaf := branch4;
                  leafIndex := BRANCH_4;
                } else {
                  if (leafIndex == NULL_BRANCH) {
                    parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := root;
                  } else {
                    parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := leaf;
                    leafParent.3[leafIndex] := root;

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
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func pop<K, V>(map: Map<K, V>): ?(K, V) {
    let root = map.0;
    let rootLinks = root.3;
    let deqLast = rootLinks[DEQ_PREV];

    switch (deqLast.1) {
      case (?someValue) {
        let hashParam = deqLast.2;
        var shiftingHash = hashParam;
        var parent = root;
        var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

        loop {
          if (entry.2 == hashParam and entry.3[DEQ_NEXT].2 == ROOT) {
            let links = entry.3;
            let deqPrev = links[DEQ_PREV];

            deqPrev.3[DEQ_NEXT] := root;
            rootLinks[DEQ_PREV] := deqPrev;
            map.1[SIZE] -%= 1;

            var leaf = entry;
            var leafParent = leaf;
            var leafIndex = NULL_BRANCH;

            loop {
              let leafLinks = leaf.3;
              let branch1 = leafLinks[BRANCH_1];

              if (branch1.2 != ROOT) {
                leafParent := leaf;
                leaf := branch1;
                leafIndex := BRANCH_1;
              } else {
                let branch2 = leafLinks[BRANCH_2];

                if (branch2.2 != ROOT) {
                  leafParent := leaf;
                  leaf := branch2;
                  leafIndex := BRANCH_2;
                } else {
                  let branch3 = leafLinks[BRANCH_3];

                  if (branch3.2 != ROOT) {
                    leafParent := leaf;
                    leaf := branch3;
                    leafIndex := BRANCH_3;
                  } else {
                    let branch4 = leafLinks[BRANCH_4];

                    if (branch4.2 != ROOT) {
                      leafParent := leaf;
                      leaf := branch4;
                      leafIndex := BRANCH_4;
                    } else {
                      if (leafIndex == NULL_BRANCH) {
                        parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := root;
                      } else {
                        parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := leaf;
                        leafParent.3[leafIndex] := root;

                        leafLinks[BRANCH_1] := links[BRANCH_1];
                        leafLinks[BRANCH_2] := links[BRANCH_2];
                        leafLinks[BRANCH_3] := links[BRANCH_3];
                        leafLinks[BRANCH_4] := links[BRANCH_4];
                      };

                      links[BRANCH_1] := entry;
                      links[BRANCH_2] := entry;

                      return ?(entry.0, someValue);
                    };
                  };
                };
              };
            };
          } else {
            parent := entry;
            shiftingHash >>= HASH_OFFSET;
            entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
          };
        };
      };

      case (_) null;
    };
  };

  public func popFront<K, V>(map: Map<K, V>): ?(K, V) {
    let root = map.0;
    let rootLinks = root.3;
    let deqFirst = rootLinks[DEQ_NEXT];

    switch (deqFirst.1) {
      case (?someValue) {
        let hashParam = deqFirst.2;
        var shiftingHash = hashParam;
        var parent = root;
        var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

        loop {
          if (entry.2 == hashParam and entry.3[DEQ_PREV].2 == ROOT) {
            let links = entry.3;
            let deqNext = links[DEQ_NEXT];

            deqNext.3[DEQ_PREV] := root;
            rootLinks[DEQ_NEXT] := deqNext;
            map.1[SIZE] -%= 1;

            var leaf = entry;
            var leafParent = leaf;
            var leafIndex = NULL_BRANCH;

            loop {
              let leafLinks = leaf.3;
              let branch1 = leafLinks[BRANCH_1];

              if (branch1.2 != ROOT) {
                leafParent := leaf;
                leaf := branch1;
                leafIndex := BRANCH_1;
              } else {
                let branch2 = leafLinks[BRANCH_2];

                if (branch2.2 != ROOT) {
                  leafParent := leaf;
                  leaf := branch2;
                  leafIndex := BRANCH_2;
                } else {
                  let branch3 = leafLinks[BRANCH_3];

                  if (branch3.2 != ROOT) {
                    leafParent := leaf;
                    leaf := branch3;
                    leafIndex := BRANCH_3;
                  } else {
                    let branch4 = leafLinks[BRANCH_4];

                    if (branch4.2 != ROOT) {
                      leafParent := leaf;
                      leaf := branch4;
                      leafIndex := BRANCH_4;
                    } else {
                      if (leafIndex == NULL_BRANCH) {
                        parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := root;
                      } else {
                        parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := leaf;
                        leafParent.3[leafIndex] := root;

                        leafLinks[BRANCH_1] := links[BRANCH_1];
                        leafLinks[BRANCH_2] := links[BRANCH_2];
                        leafLinks[BRANCH_3] := links[BRANCH_3];
                        leafLinks[BRANCH_4] := links[BRANCH_4];
                      };

                      links[BRANCH_1] := entry;
                      links[BRANCH_2] := entry;

                      return ?(entry.0, someValue);
                    };
                  };
                };
              };
            };
          } else {
            parent := entry;
            shiftingHash >>= HASH_OFFSET;
            entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
          };
        };
      };

      case (_) null;
    };
  };

  public func cycle<K, V>(map: Map<K, V>): ?(K, V) {
    let root = map.0;
    let rootLinks = root.3;
    let deqLast = rootLinks[DEQ_PREV];

    switch (deqLast.1) {
      case (?someValue) {
        let hashParam = deqLast.2;
        var shiftingHash = hashParam;
        var parent = root;
        var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

        loop {
          if (entry.2 == hashParam and entry.3[DEQ_NEXT].2 == ROOT) {
            let links = entry.3;
            let deqFirst = rootLinks[DEQ_NEXT];
            let newEntry = (entry.0, entry.1, entry.2, [var links[BRANCH_1], links[BRANCH_2], links[BRANCH_3], links[BRANCH_4], root, deqFirst]);

            deqFirst.3[DEQ_PREV] := newEntry;
            rootLinks[DEQ_NEXT] := newEntry;
            parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

            let deqPrev = links[DEQ_PREV];

            deqPrev.3[DEQ_NEXT] := root;
            rootLinks[DEQ_PREV] := deqPrev;
            links[BRANCH_1] := entry;
            links[BRANCH_2] := entry;

            return ?(entry.0, someValue);
          } else {
            parent := entry;
            shiftingHash >>= HASH_OFFSET;
            entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
          };
        };
      };

      case (_) null;
    };
  };

  public func cycleFront<K, V>(map: Map<K, V>): ?(K, V) {
    let root = map.0;
    let rootLinks = root.3;
    let deqFirst = rootLinks[DEQ_NEXT];

    switch (deqFirst.1) {
      case (?someValue) {
        let hashParam = deqFirst.2;
        var shiftingHash = hashParam;
        var parent = root;
        var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

        loop {
          if (entry.2 == hashParam and entry.3[DEQ_PREV].2 == ROOT) {
            let links = entry.3;
            let deqLast = rootLinks[DEQ_PREV];
            let newEntry = (entry.0, entry.1, entry.2, [var links[BRANCH_1], links[BRANCH_2], links[BRANCH_3], links[BRANCH_4], deqLast, root]);

            deqLast.3[DEQ_NEXT] := newEntry;
            rootLinks[DEQ_PREV] := newEntry;
            parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

            let deqNext = links[DEQ_NEXT];

            deqNext.3[DEQ_PREV] := root;
            rootLinks[DEQ_NEXT] := deqNext;
            links[BRANCH_1] := entry;
            links[BRANCH_2] := entry;

            return ?(entry.0, someValue);
          } else {
            parent := entry;
            shiftingHash >>= HASH_OFFSET;
            entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
          };
        };
      };

      case (_) null;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func clone<K, V>(map: Map<K, V>): Map<K, V> {
    let root = map.0;
    var entry = root;

    let rootLinks = entry.3;
    let branch1 = rootLinks[BRANCH_1];
    let branch2 = rootLinks[BRANCH_2];
    let branch3 = rootLinks[BRANCH_3];
    let branch4 = rootLinks[BRANCH_4];

    let newRootLinks = [var root, root, root, root, root, root];
    let newRoot = (root.0, null, ROOT, newRootLinks):Entry<K, V>;
    var newEntry = newRoot;

    rootLinks[TEMP_LINK] := newRoot;

    newRootLinks[BRANCH_1] := if (branch1.2 != ROOT) cloneEntry(branch1, newRoot) else newRoot;
    newRootLinks[BRANCH_2] := if (branch2.2 != ROOT) cloneEntry(branch2, newRoot) else newRoot;
    newRootLinks[BRANCH_3] := if (branch3.2 != ROOT) cloneEntry(branch3, newRoot) else newRoot;
    newRootLinks[BRANCH_4] := if (branch4.2 != ROOT) cloneEntry(branch4, newRoot) else newRoot;

    loop {
      let cloneNext = entry.3[CLONE_NEXT];
      let newDeqNext = cloneNext.3[TEMP_LINK];

      newDeqNext.3[DEQ_PREV] := newEntry;
      newEntry.3[DEQ_NEXT] := newDeqNext;
      cloneNext.3[TEMP_LINK] := entry;

      if (newDeqNext.2 == ROOT) return (newRoot, [var map.1[SIZE]]);

      newEntry := newDeqNext;
      entry := cloneNext;
    };
  };

  public func cloneDesc<K, V>(map: Map<K, V>): Map<K, V> {
    let root = map.0;
    var entry = root;

    let rootLinks = entry.3;
    let branch1 = rootLinks[BRANCH_1];
    let branch2 = rootLinks[BRANCH_2];
    let branch3 = rootLinks[BRANCH_3];
    let branch4 = rootLinks[BRANCH_4];

    let newRootLinks = [var root, root, root, root, root, root];
    let newRoot = (root.0, null, ROOT, newRootLinks):Entry<K, V>;
    var newEntry = newRoot;

    rootLinks[TEMP_LINK] := newRoot;

    newRootLinks[BRANCH_1] := if (branch1.2 != ROOT) cloneEntry(branch1, newRoot) else newRoot;
    newRootLinks[BRANCH_2] := if (branch2.2 != ROOT) cloneEntry(branch2, newRoot) else newRoot;
    newRootLinks[BRANCH_3] := if (branch3.2 != ROOT) cloneEntry(branch3, newRoot) else newRoot;
    newRootLinks[BRANCH_4] := if (branch4.2 != ROOT) cloneEntry(branch4, newRoot) else newRoot;

    loop {
      let cloneNext = entry.3[CLONE_NEXT];
      let newDeqPrev = cloneNext.3[TEMP_LINK];

      newDeqPrev.3[DEQ_NEXT] := newEntry;
      newEntry.3[DEQ_PREV] := newDeqPrev;
      cloneNext.3[TEMP_LINK] := entry;

      if (newDeqPrev.2 == ROOT) return (newRoot, [var map.1[SIZE]]);

      newEntry := newDeqPrev;
      entry := cloneNext;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func mapFilter<K, V1, V2>(map: Map<K, V1>, hashUtils: HashUtils<K>, mapEntry: (K, V1) -> ?V2): Map<K, V2> {
    let root = createRoot<K, V2>(hashUtils.2());
    let rootLinks = root.3;
    let getHash = hashUtils.0;
    let areEqual = hashUtils.1;
    var size = 0:Nat32;
    var item = map.0;

    loop {
      item := item.3[DEQ_NEXT];

      switch (item.1) {
        case (?someValue) label loopBody if (item.3[BRANCH_1].2 != item.2 or item.3[BRANCH_2].2 != item.2) {
          switch (mapEntry(item.0, someValue)) {
            case (null) {};

            case (newValue) {
              let hashParam = getHash(item.0);
              var shiftingHash = hashParam;
              var parent = root;
              var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

              loop {
                let hash = entry.2;

                if (hash == ROOT) {
                  let deqPrev = rootLinks[DEQ_PREV];
                  let newEntry = (item.0, newValue, hashParam, [var root, root, root, root, deqPrev, root]);

                  deqPrev.3[DEQ_NEXT] := newEntry;
                  rootLinks[DEQ_PREV] := newEntry;
                  parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
                  size +%= 1;

                  break loopBody;
                } else if (hash == hashParam and areEqual(entry.0, item.0)) {
                  let links = entry.3;
                  let deqPrev = rootLinks[DEQ_PREV];

                  let newEntry = (
                    entry.0,
                    newValue,
                    hash,
                    [var links[BRANCH_1], links[BRANCH_2], links[BRANCH_3], links[BRANCH_4], deqPrev, root],
                  );

                  deqPrev.3[DEQ_NEXT] := newEntry;
                  rootLinks[DEQ_PREV] := newEntry;
                  parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

                  let deqPrevOld = links[DEQ_PREV];
                  let deqNextOld = links[DEQ_NEXT];

                  deqNextOld.3[DEQ_PREV] := deqPrevOld;
                  deqPrevOld.3[DEQ_NEXT] := deqNextOld;
                  links[BRANCH_1] := entry;
                  links[BRANCH_2] := entry;

                  break loopBody;
                } else {
                  parent := entry;
                  shiftingHash >>= HASH_OFFSET;
                  entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
                };
              };
            };
          };
        };

        case (_) return (root, [var size]);
      };
    };
  };

  public func mapFilterDesc<K, V1, V2>(map: Map<K, V1>, hashUtils: HashUtils<K>, mapEntry: (K, V1) -> ?V2): Map<K, V2> {
    let root = createRoot<K, V2>(hashUtils.2());
    let rootLinks = root.3;
    let getHash = hashUtils.0;
    let areEqual = hashUtils.1;
    var size = 0:Nat32;
    var item = map.0;

    loop {
      item := item.3[DEQ_PREV];

      switch (item.1) {
        case (?someValue) label loopBody if (item.3[BRANCH_1].2 != item.2 or item.3[BRANCH_2].2 != item.2) {
          switch (mapEntry(item.0, someValue)) {
            case (null) {};

            case (newValue) {
              let hashParam = getHash(item.0);
              var shiftingHash = hashParam;
              var parent = root;
              var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

              loop {
                let hash = entry.2;

                if (hash == ROOT) {
                  let deqPrev = rootLinks[DEQ_PREV];
                  let newEntry = (item.0, newValue, hashParam, [var root, root, root, root, deqPrev, root]);

                  deqPrev.3[DEQ_NEXT] := newEntry;
                  rootLinks[DEQ_PREV] := newEntry;
                  parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
                  size +%= 1;

                  break loopBody;
                } else if (hash == hashParam and areEqual(entry.0, item.0)) {
                  let links = entry.3;
                  let deqPrev = rootLinks[DEQ_PREV];

                  let newEntry = (
                    entry.0,
                    newValue,
                    hash,
                    [var links[BRANCH_1], links[BRANCH_2], links[BRANCH_3], links[BRANCH_4], deqPrev, root],
                  );

                  deqPrev.3[DEQ_NEXT] := newEntry;
                  rootLinks[DEQ_PREV] := newEntry;
                  parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

                  let deqPrevOld = links[DEQ_PREV];
                  let deqNextOld = links[DEQ_NEXT];

                  deqNextOld.3[DEQ_PREV] := deqPrevOld;
                  deqPrevOld.3[DEQ_NEXT] := deqNextOld;
                  links[BRANCH_1] := entry;
                  links[BRANCH_2] := entry;

                  break loopBody;
                } else {
                  parent := entry;
                  shiftingHash >>= HASH_OFFSET;
                  entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
                };
              };
            };
          };
        };

        case (_) return (root, [var size]);
      };
    };
  };

  public func map<K, V1, V2>(map: Map<K, V1>, hashUtils: HashUtils<K>, mapEntry: (K, V1) -> V2): Map<K, V2> {
    let root = createRoot<K, V2>(hashUtils.2());
    let rootLinks = root.3;
    let getHash = hashUtils.0;
    let areEqual = hashUtils.1;
    var size = 0:Nat32;
    var item = map.0;

    loop {
      item := item.3[DEQ_NEXT];

      switch (item.1) {
        case (?someValue) label loopBody if (item.3[BRANCH_1].2 != item.2 or item.3[BRANCH_2].2 != item.2) {
          let hashParam = getHash(item.0);
          var shiftingHash = hashParam;
          var parent = root;
          var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

          loop {
            let hash = entry.2;

            if (hash == ROOT) {
              let deqPrev = rootLinks[DEQ_PREV];
              let newEntry = (item.0, ?mapEntry(entry.0, someValue), hashParam, [var root, root, root, root, deqPrev, root]);

              deqPrev.3[DEQ_NEXT] := newEntry;
              rootLinks[DEQ_PREV] := newEntry;
              parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
              size +%= 1;

              break loopBody;
            } else if (hash == hashParam and areEqual(entry.0, item.0)) {
              let links = entry.3;
              let deqPrev = rootLinks[DEQ_PREV];

              let newEntry = (
                entry.0,
                ?mapEntry(entry.0, someValue),
                hash,
                [var links[BRANCH_1], links[BRANCH_2], links[BRANCH_3], links[BRANCH_4], deqPrev, root],
              );

              deqPrev.3[DEQ_NEXT] := newEntry;
              rootLinks[DEQ_PREV] := newEntry;
              parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

              let deqPrevOld = links[DEQ_PREV];
              let deqNextOld = links[DEQ_NEXT];

              deqNextOld.3[DEQ_PREV] := deqPrevOld;
              deqPrevOld.3[DEQ_NEXT] := deqNextOld;
              links[BRANCH_1] := entry;
              links[BRANCH_2] := entry;

              break loopBody;
            } else {
              parent := entry;
              shiftingHash >>= HASH_OFFSET;
              entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
            };
          };
        };

        case (_) return (root, [var size]);
      };
    };
  };

  public func mapDesc<K, V1, V2>(map: Map<K, V1>, hashUtils: HashUtils<K>, mapEntry: (K, V1) -> V2): Map<K, V2> {
    let root = createRoot<K, V2>(hashUtils.2());
    let rootLinks = root.3;
    let getHash = hashUtils.0;
    let areEqual = hashUtils.1;
    var size = 0:Nat32;
    var item = map.0;

    loop {
      item := item.3[DEQ_PREV];

      switch (item.1) {
        case (?someValue) label loopBody if (item.3[BRANCH_1].2 != item.2 or item.3[BRANCH_2].2 != item.2) {
          let hashParam = getHash(item.0);
          var shiftingHash = hashParam;
          var parent = root;
          var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

          loop {
            let hash = entry.2;

            if (hash == ROOT) {
              let deqPrev = rootLinks[DEQ_PREV];
              let newEntry = (item.0, ?mapEntry(entry.0, someValue), hashParam, [var root, root, root, root, deqPrev, root]);

              deqPrev.3[DEQ_NEXT] := newEntry;
              rootLinks[DEQ_PREV] := newEntry;
              parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
              size +%= 1;

              break loopBody;
            } else if (hash == hashParam and areEqual(entry.0, item.0)) {
              let links = entry.3;
              let deqPrev = rootLinks[DEQ_PREV];

              let newEntry = (
                entry.0,
                ?mapEntry(entry.0, someValue),
                hash,
                [var links[BRANCH_1], links[BRANCH_2], links[BRANCH_3], links[BRANCH_4], deqPrev, root],
              );

              deqPrev.3[DEQ_NEXT] := newEntry;
              rootLinks[DEQ_PREV] := newEntry;
              parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

              let deqPrevOld = links[DEQ_PREV];
              let deqNextOld = links[DEQ_NEXT];

              deqNextOld.3[DEQ_PREV] := deqPrevOld;
              deqPrevOld.3[DEQ_NEXT] := deqNextOld;
              links[BRANCH_1] := entry;
              links[BRANCH_2] := entry;

              break loopBody;
            } else {
              parent := entry;
              shiftingHash >>= HASH_OFFSET;
              entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
            };
          };
        };

        case (_) return (root, [var size]);
      };
    };
  };

  public func filter<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, acceptEntry: (K, V) -> Bool): Map<K, V> {
    let root = createRoot<K, V>(hashUtils.2());
    let rootLinks = root.3;
    let getHash = hashUtils.0;
    let areEqual = hashUtils.1;
    var size = 0:Nat32;
    var item = map.0;

    loop {
      item := item.3[DEQ_NEXT];

      switch (item.1) {
        case (?someValue) label loopBody if ((item.3[BRANCH_1].2 != item.2 or item.3[BRANCH_2].2 != item.2) and acceptEntry(item.0, someValue)) {
          let hashParam = getHash(item.0);
          var shiftingHash = hashParam;
          var parent = root;
          var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

          loop {
            let hash = entry.2;

            if (hash == ROOT) {
              let deqPrev = rootLinks[DEQ_PREV];
              let newEntry = (item.0, item.1, hashParam, [var root, root, root, root, deqPrev, root]);

              deqPrev.3[DEQ_NEXT] := newEntry;
              rootLinks[DEQ_PREV] := newEntry;
              parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
              size +%= 1;

              break loopBody;
            } else if (hash == hashParam and areEqual(entry.0, item.0)) {
              let links = entry.3;
              let deqPrev = rootLinks[DEQ_PREV];

              let newEntry = (
                entry.0,
                item.1,
                hash,
                [var links[BRANCH_1], links[BRANCH_2], links[BRANCH_3], links[BRANCH_4], deqPrev, root],
              );

              deqPrev.3[DEQ_NEXT] := newEntry;
              rootLinks[DEQ_PREV] := newEntry;
              parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

              let deqPrevOld = links[DEQ_PREV];
              let deqNextOld = links[DEQ_NEXT];

              deqNextOld.3[DEQ_PREV] := deqPrevOld;
              deqPrevOld.3[DEQ_NEXT] := deqNextOld;
              links[BRANCH_1] := entry;
              links[BRANCH_2] := entry;

              break loopBody;
            } else {
              parent := entry;
              shiftingHash >>= HASH_OFFSET;
              entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
            };
          };
        };

        case (_) return (root, [var size]);
      };
    };
  };

  public func filterDesc<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, acceptEntry: (K, V) -> Bool): Map<K, V> {
    let root = createRoot<K, V>(hashUtils.2());
    let rootLinks = root.3;
    let getHash = hashUtils.0;
    let areEqual = hashUtils.1;
    var size = 0:Nat32;
    var item = map.0;

    loop {
      item := item.3[DEQ_PREV];

      switch (item.1) {
        case (?someValue) label loopBody if ((item.3[BRANCH_1].2 != item.2 or item.3[BRANCH_2].2 != item.2) and acceptEntry(item.0, someValue)) {
          let hashParam = getHash(item.0);
          var shiftingHash = hashParam;
          var parent = root;
          var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

          loop {
            let hash = entry.2;

            if (hash == ROOT) {
              let deqPrev = rootLinks[DEQ_PREV];
              let newEntry = (item.0, item.1, hashParam, [var root, root, root, root, deqPrev, root]);

              deqPrev.3[DEQ_NEXT] := newEntry;
              rootLinks[DEQ_PREV] := newEntry;
              parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
              size +%= 1;

              break loopBody;
            } else if (hash == hashParam and areEqual(entry.0, item.0)) {
              let links = entry.3;
              let deqPrev = rootLinks[DEQ_PREV];

              let newEntry = (
                entry.0,
                item.1,
                hash,
                [var links[BRANCH_1], links[BRANCH_2], links[BRANCH_3], links[BRANCH_4], deqPrev, root],
              );

              deqPrev.3[DEQ_NEXT] := newEntry;
              rootLinks[DEQ_PREV] := newEntry;
              parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

              let deqPrevOld = links[DEQ_PREV];
              let deqNextOld = links[DEQ_NEXT];

              deqNextOld.3[DEQ_PREV] := deqPrevOld;
              deqPrevOld.3[DEQ_NEXT] := deqNextOld;
              links[BRANCH_1] := entry;
              links[BRANCH_2] := entry;

              break loopBody;
            } else {
              parent := entry;
              shiftingHash >>= HASH_OFFSET;
              entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
            };
          };
        };

        case (_) return (root, [var size]);
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func keys<K, V>(map: Map<K, V>): Iter<K> {
    var started = false;
    var entry = map.0;

    let iter = {
      prev = func(): ?K {
        started := true;
        entry := entry.3[DEQ_PREV];

        loop {
          let hash = entry.2;

          if (hash == ROOT) return null else if (entry.3[BRANCH_1].2 != hash or entry.3[BRANCH_2].2 != hash) return ?entry.0 else {
            entry := entry.3[DEQ_PREV];
          };
        };
      };

      next = func(): ?K {
        started := true;
        entry := entry.3[DEQ_NEXT];

        loop {
          let hash = entry.2;

          if (hash == ROOT) return null else if (entry.3[BRANCH_1].2 != hash or entry.3[BRANCH_2].2 != hash) return ?entry.0 else {
            entry := entry.3[DEQ_NEXT];
          };
        };
      };

      peekPrev = func(): ?K {
        var deqPrev = entry.3[DEQ_PREV];

        loop {
          let hash = deqPrev.2;

          if (hash == ROOT) return null else if (deqPrev.3[BRANCH_1].2 != hash or deqPrev.3[BRANCH_2].2 != hash) {
            entry.3[DEQ_PREV] := deqPrev;

            return ?deqPrev.0;
          } else {
            deqPrev := deqPrev.3[DEQ_PREV];
          };
        };
      };

      peekNext = func(): ?K {
        var deqNext = entry.3[DEQ_NEXT];

        loop {
          let hash = deqNext.2;

          if (hash == ROOT) return null else if (deqNext.3[BRANCH_1].2 != hash or deqNext.3[BRANCH_2].2 != hash) {
            entry.3[DEQ_NEXT] := deqNext;

            return ?deqNext.0;
          } else {
            deqNext := deqNext.3[DEQ_NEXT];
          };
        };
      };

      current = func(): ?K {
        if (entry.2 == ROOT) null else ?entry.0;
      };

      started = func(): Bool {
        started;
      };

      finished = func(): Bool {
        started and entry.2 == ROOT;
      };

      reset = func(): Iter<K> {
        started := false;
        entry := map.0;

        iter;
      };

      movePrev = func(): Iter<K> {
        started := true;
        entry := entry.3[DEQ_PREV];

        loop if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2 or entry.2 == ROOT) return iter else {
          entry := entry.3[DEQ_PREV];
        };
      };

      moveNext = func(): Iter<K> {
        started := true;
        entry := entry.3[DEQ_NEXT];

        loop if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2 or entry.2 == ROOT) return iter else {
          entry := entry.3[DEQ_NEXT];
        };
      };
    };
  };

  public func keysDesc<K, V>(map: Map<K, V>): Iter<K> {
    var started = false;
    var entry = map.0;

    let iter = {
      prev = func(): ?K {
        started := true;
        entry := entry.3[DEQ_NEXT];

        loop {
          let hash = entry.2;

          if (hash == ROOT) return null else if (entry.3[BRANCH_1].2 != hash or entry.3[BRANCH_2].2 != hash) return ?entry.0 else {
            entry := entry.3[DEQ_NEXT];
          };
        };
      };

      next = func(): ?K {
        started := true;
        entry := entry.3[DEQ_PREV];

        loop {
          let hash = entry.2;

          if (hash == ROOT) return null else if (entry.3[BRANCH_1].2 != hash or entry.3[BRANCH_2].2 != hash) return ?entry.0 else {
            entry := entry.3[DEQ_PREV];
          };
        };
      };

      peekPrev = func(): ?K {
        var deqNext = entry.3[DEQ_NEXT];

        loop {
          let hash = deqNext.2;

          if (hash == ROOT) return null else if (deqNext.3[BRANCH_1].2 != hash or deqNext.3[BRANCH_2].2 != hash) {
            entry.3[DEQ_NEXT] := deqNext;

            return ?deqNext.0;
          } else {
            deqNext := deqNext.3[DEQ_NEXT];
          };
        };
      };

      peekNext = func(): ?K {
        var deqPrev = entry.3[DEQ_PREV];

        loop {
          let hash = deqPrev.2;

          if (hash == ROOT) return null else if (deqPrev.3[BRANCH_1].2 != hash or deqPrev.3[BRANCH_2].2 != hash) {
            entry.3[DEQ_PREV] := deqPrev;

            return ?deqPrev.0;
          } else {
            deqPrev := deqPrev.3[DEQ_PREV];
          };
        };
      };

      current = func(): ?K {
        if (entry.2 == ROOT) null else ?entry.0;
      };

      started = func(): Bool {
        started;
      };

      finished = func(): Bool {
        started and entry.2 == ROOT;
      };

      reset = func(): Iter<K> {
        started := false;
        entry := map.0;

        iter;
      };

      movePrev = func(): Iter<K> {
        started := true;
        entry := entry.3[DEQ_NEXT];

        loop if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2 or entry.2 == ROOT) return iter else {
          entry := entry.3[DEQ_NEXT];
        };
      };

      moveNext = func(): Iter<K> {
        started := true;
        entry := entry.3[DEQ_PREV];

        loop if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2 or entry.2 == ROOT) return iter else {
          entry := entry.3[DEQ_PREV];
        };
      };
    };
  };

  public func vals<K, V>(map: Map<K, V>): Iter<V> {
    var started = false;
    var entry = map.0;

    let iter = {
      prev = func(): ?V {
        started := true;
        entry := entry.3[DEQ_PREV];

        loop if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2 or entry.2 == ROOT) return entry.1 else {
          entry := entry.3[DEQ_PREV];
        };
      };

      next = func(): ?V {
        started := true;
        entry := entry.3[DEQ_NEXT];

        loop if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2 or entry.2 == ROOT) return entry.1 else {
          entry := entry.3[DEQ_NEXT];
        };
      };

      peekPrev = func(): ?V {
        var deqPrev = entry.3[DEQ_PREV];

        loop if (deqPrev.3[BRANCH_1].2 != deqPrev.2 or deqPrev.3[BRANCH_2].2 != deqPrev.2 or deqPrev.2 == ROOT) {
          entry.3[DEQ_PREV] := deqPrev;

          return deqPrev.1;
        } else {
          deqPrev := deqPrev.3[DEQ_PREV];
        };
      };

      peekNext = func(): ?V {
        var deqNext = entry.3[DEQ_NEXT];

        loop if (deqNext.3[BRANCH_1].2 != deqNext.2 or deqNext.3[BRANCH_2].2 != deqNext.2 or deqNext.2 == ROOT) {
          entry.3[DEQ_NEXT] := deqNext;

          return deqNext.1;
        } else {
          deqNext := deqNext.3[DEQ_NEXT];
        };
      };

      current = func(): ?V {
        entry.1;
      };

      started = func(): Bool {
        started;
      };

      finished = func(): Bool {
        started and entry.2 == ROOT;
      };

      reset = func(): Iter<V> {
        started := false;
        entry := map.0;

        iter;
      };

      movePrev = func(): Iter<V> {
        started := true;
        entry := entry.3[DEQ_PREV];

        loop if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2 or entry.2 == ROOT) return iter else {
          entry := entry.3[DEQ_PREV];
        };
      };

      moveNext = func(): Iter<V> {
        started := true;
        entry := entry.3[DEQ_NEXT];

        loop if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2 or entry.2 == ROOT) return iter else {
          entry := entry.3[DEQ_NEXT];
        };
      };
    };
  };

  public func valsDesc<K, V>(map: Map<K, V>): Iter<V> {
    var started = false;
    var entry = map.0;

    let iter = {
      prev = func(): ?V {
        started := true;
        entry := entry.3[DEQ_NEXT];

        loop if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2 or entry.2 == ROOT) return entry.1 else {
          entry := entry.3[DEQ_NEXT];
        };
      };

      next = func(): ?V {
        started := true;
        entry := entry.3[DEQ_PREV];

        loop if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2 or entry.2 == ROOT) return entry.1 else {
          entry := entry.3[DEQ_PREV];
        };
      };

      peekPrev = func(): ?V {
        var deqNext = entry.3[DEQ_NEXT];

        loop if (deqNext.3[BRANCH_1].2 != deqNext.2 or deqNext.3[BRANCH_2].2 != deqNext.2 or deqNext.2 == ROOT) {
          entry.3[DEQ_NEXT] := deqNext;

          return deqNext.1;
        } else {
          deqNext := deqNext.3[DEQ_NEXT];
        };
      };

      peekNext = func(): ?V {
        var deqPrev = entry.3[DEQ_PREV];

        loop if (deqPrev.3[BRANCH_1].2 != deqPrev.2 or deqPrev.3[BRANCH_2].2 != deqPrev.2 or deqPrev.2 == ROOT) {
          entry.3[DEQ_PREV] := deqPrev;

          return deqPrev.1;
        } else {
          deqPrev := deqPrev.3[DEQ_PREV];
        };
      };

      current = func(): ?V {
        entry.1;
      };

      started = func(): Bool {
        started;
      };

      finished = func(): Bool {
        started and entry.2 == ROOT;
      };

      reset = func(): Iter<V> {
        started := false;
        entry := map.0;

        iter;
      };

      movePrev = func(): Iter<V> {
        started := true;
        entry := entry.3[DEQ_NEXT];

        loop if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2 or entry.2 == ROOT) return iter else {
          entry := entry.3[DEQ_NEXT];
        };
      };

      moveNext = func(): Iter<V> {
        started := true;
        entry := entry.3[DEQ_PREV];

        loop if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2 or entry.2 == ROOT) return iter else {
          entry := entry.3[DEQ_PREV];
        };
      };
    };
  };

  public func entries<K, V>(map: Map<K, V>): Iter<(K, V)> {
    var started = false;
    var entry = map.0;

    let iter = {
      prev = func(): ?(K, V) {
        started := true;
        entry := entry.3[DEQ_PREV];

        loop switch (entry.1) {
          case (?someValue) if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2) return ?(entry.0, someValue) else {
            entry := entry.3[DEQ_PREV];
          };

          case (_) return null;
        };
      };

      next = func(): ?(K, V) {
        started := true;
        entry := entry.3[DEQ_NEXT];

        loop switch (entry.1) {
          case (?someValue) if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2) return ?(entry.0, someValue) else {
            entry := entry.3[DEQ_NEXT];
          };

          case (_) return null;
        };
      };

      peekPrev = func(): ?(K, V) {
        var deqPrev = entry.3[DEQ_PREV];

        loop switch (deqPrev.1) {
          case (?someValue) if (deqPrev.3[BRANCH_1].2 != deqPrev.2 or deqPrev.3[BRANCH_2].2 != deqPrev.2) {
            entry.3[DEQ_PREV] := deqPrev;

            return ?(deqPrev.0, someValue);
          } else {
            deqPrev := deqPrev.3[DEQ_PREV];
          };

          case (_) return null;
        };
      };

      peekNext = func(): ?(K, V) {
        var deqNext = entry.3[DEQ_NEXT];

        loop switch (deqNext.1) {
          case (?someValue) if (deqNext.3[BRANCH_1].2 != deqNext.2 or deqNext.3[BRANCH_2].2 != deqNext.2) {
            entry.3[DEQ_NEXT] := deqNext;

            return ?(deqNext.0, someValue);
          } else {
            deqNext := deqNext.3[DEQ_NEXT];
          };

          case (_) return null;
        };
      };

      current = func(): ?(K, V) {
        switch (entry.1) { case (?someValue) ?(entry.0, someValue); case (_) null };
      };

      started = func(): Bool {
        started;
      };

      finished = func(): Bool {
        started and entry.2 == ROOT;
      };

      reset = func(): Iter<(K, V)> {
        started := false;
        entry := map.0;

        iter;
      };

      movePrev = func(): Iter<(K, V)> {
        started := true;
        entry := entry.3[DEQ_PREV];

        loop if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2 or entry.2 == ROOT) return iter else {
          entry := entry.3[DEQ_PREV];
        };
      };

      moveNext = func(): Iter<(K, V)> {
        started := true;
        entry := entry.3[DEQ_NEXT];

        loop if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2 or entry.2 == ROOT) return iter else {
          entry := entry.3[DEQ_NEXT];
        };
      };
    };
  };

  public func entriesDesc<K, V>(map: Map<K, V>): Iter<(K, V)> {
    var started = false;
    var entry = map.0;

    let iter = {
      prev = func(): ?(K, V) {
        started := true;
        entry := entry.3[DEQ_NEXT];

        loop switch (entry.1) {
          case (?someValue) if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2) return ?(entry.0, someValue) else {
            entry := entry.3[DEQ_NEXT];
          };

          case (_) return null;
        };
      };

      next = func(): ?(K, V) {
        started := true;
        entry := entry.3[DEQ_PREV];

        loop switch (entry.1) {
          case (?someValue) if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2) return ?(entry.0, someValue) else {
            entry := entry.3[DEQ_PREV];
          };

          case (_) return null;
        };
      };

      peekPrev = func(): ?(K, V) {
        var deqNext = entry.3[DEQ_NEXT];

        loop switch (deqNext.1) {
          case (?someValue) if (deqNext.3[BRANCH_1].2 != deqNext.2 or deqNext.3[BRANCH_2].2 != deqNext.2) {
            entry.3[DEQ_NEXT] := deqNext;

            return ?(deqNext.0, someValue);
          } else {
            deqNext := deqNext.3[DEQ_NEXT];
          };

          case (_) return null;
        };
      };

      peekNext = func(): ?(K, V) {
        var deqPrev = entry.3[DEQ_PREV];

        loop switch (deqPrev.1) {
          case (?someValue) if (deqPrev.3[BRANCH_1].2 != deqPrev.2 or deqPrev.3[BRANCH_2].2 != deqPrev.2) {
            entry.3[DEQ_PREV] := deqPrev;

            return ?(deqPrev.0, someValue);
          } else {
            deqPrev := deqPrev.3[DEQ_PREV];
          };

          case (_) return null;
        };
      };

      current = func(): ?(K, V) {
        switch (entry.1) { case (?someValue) ?(entry.0, someValue); case (_) null };
      };

      started = func(): Bool {
        started;
      };

      finished = func(): Bool {
        started and entry.2 == ROOT;
      };

      reset = func(): Iter<(K, V)> {
        started := false;
        entry := map.0;

        iter;
      };

      movePrev = func(): Iter<(K, V)> {
        started := true;
        entry := entry.3[DEQ_NEXT];

        loop if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2 or entry.2 == ROOT) return iter else {
          entry := entry.3[DEQ_NEXT];
        };
      };

      moveNext = func(): Iter<(K, V)> {
        started := true;
        entry := entry.3[DEQ_PREV];

        loop if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2 or entry.2 == ROOT) return iter else {
          entry := entry.3[DEQ_PREV];
        };
      };
    };
  };

  public func keysFrom<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, placeParam: ?K): Iter<K> {
    let keyParam = switch (placeParam) { case (?keyParam) keyParam; case (_) map.0.0 };

    let hashParam = switch (placeParam) { case (null) ROOT; case (_) hashUtils.0(keyParam) };

    var shiftingHash = hashParam;

    var entry = if (shiftingHash != ROOT) map.0.3[nat(shiftingHash % HASH_CHUNK_SIZE)] else map.0;

    loop {
      let hash = entry.2;

      if (hash == ROOT or hash == hashParam and hashUtils.1(entry.0, keyParam)) {
        var started = hash != ROOT;

        return let iter = {
          prev = func(): ?K {
            started := true;
            entry := entry.3[DEQ_PREV];

            loop {
              let hash = entry.2;

              if (hash == ROOT) return null else if (entry.3[BRANCH_1].2 != hash or entry.3[BRANCH_2].2 != hash) return ?entry.0 else {
                entry := entry.3[DEQ_PREV];
              };
            };
          };

          next = func(): ?K {
            started := true;
            entry := entry.3[DEQ_NEXT];

            loop {
              let hash = entry.2;

              if (hash == ROOT) return null else if (entry.3[BRANCH_1].2 != hash or entry.3[BRANCH_2].2 != hash) return ?entry.0 else {
                entry := entry.3[DEQ_NEXT];
              };
            };
          };

          peekPrev = func(): ?K {
            var deqPrev = entry.3[DEQ_PREV];

            loop {
              let hash = deqPrev.2;

              if (hash == ROOT) return null else if (deqPrev.3[BRANCH_1].2 != hash or deqPrev.3[BRANCH_2].2 != hash) {
                entry.3[DEQ_PREV] := deqPrev;

                return ?deqPrev.0;
              } else {
                deqPrev := deqPrev.3[DEQ_PREV];
              };
            };
          };

          peekNext = func(): ?K {
            var deqNext = entry.3[DEQ_NEXT];

            loop {
              let hash = deqNext.2;

              if (hash == ROOT) return null else if (deqNext.3[BRANCH_1].2 != hash or deqNext.3[BRANCH_2].2 != hash) {
                entry.3[DEQ_NEXT] := deqNext;

                return ?deqNext.0;
              } else {
                deqNext := deqNext.3[DEQ_NEXT];
              };
            };
          };

          current = func(): ?K {
            if (entry.2 == ROOT) null else ?entry.0;
          };

          started = func(): Bool {
            started;
          };

          finished = func(): Bool {
            started and entry.2 == ROOT;
          };

          reset = func(): Iter<K> {
            started := false;
            entry := map.0;

            iter;
          };

          movePrev = func(): Iter<K> {
            started := true;
            entry := entry.3[DEQ_PREV];

            loop if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2 or entry.2 == ROOT) return iter else {
              entry := entry.3[DEQ_PREV];
            };
          };

          moveNext = func(): Iter<K> {
            started := true;
            entry := entry.3[DEQ_NEXT];

            loop if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2 or entry.2 == ROOT) return iter else {
              entry := entry.3[DEQ_NEXT];
            };
          };
        };
      } else {
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func keysFromDesc<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, placeParam: ?K): Iter<K> {
    let keyParam = switch (placeParam) { case (?keyParam) keyParam; case (_) map.0.0 };

    let hashParam = switch (placeParam) { case (null) ROOT; case (_) hashUtils.0(keyParam) };

    var shiftingHash = hashParam;

    var entry = if (shiftingHash != ROOT) map.0.3[nat(shiftingHash % HASH_CHUNK_SIZE)] else map.0;

    loop {
      let hash = entry.2;

      if (hash == ROOT or hash == hashParam and hashUtils.1(entry.0, keyParam)) {
        var started = hash != ROOT;

        return let iter = {
          prev = func(): ?K {
            started := true;
            entry := entry.3[DEQ_NEXT];

            loop {
              let hash = entry.2;

              if (hash == ROOT) return null else if (entry.3[BRANCH_1].2 != hash or entry.3[BRANCH_2].2 != hash) return ?entry.0 else {
                entry := entry.3[DEQ_NEXT];
              };
            };
          };

          next = func(): ?K {
            started := true;
            entry := entry.3[DEQ_PREV];

            loop {
              let hash = entry.2;

              if (hash == ROOT) return null else if (entry.3[BRANCH_1].2 != hash or entry.3[BRANCH_2].2 != hash) return ?entry.0 else {
                entry := entry.3[DEQ_PREV];
              };
            };
          };

          peekPrev = func(): ?K {
            var deqNext = entry.3[DEQ_NEXT];

            loop {
              let hash = deqNext.2;

              if (hash == ROOT) return null else if (deqNext.3[BRANCH_1].2 != hash or deqNext.3[BRANCH_2].2 != hash) {
                entry.3[DEQ_NEXT] := deqNext;

                return ?deqNext.0;
              } else {
                deqNext := deqNext.3[DEQ_NEXT];
              };
            };
          };

          peekNext = func(): ?K {
            var deqPrev = entry.3[DEQ_PREV];

            loop {
              let hash = deqPrev.2;

              if (hash == ROOT) return null else if (deqPrev.3[BRANCH_1].2 != hash or deqPrev.3[BRANCH_2].2 != hash) {
                entry.3[DEQ_PREV] := deqPrev;

                return ?deqPrev.0;
              } else {
                deqPrev := deqPrev.3[DEQ_PREV];
              };
            };
          };

          current = func(): ?K {
            if (entry.2 == ROOT) null else ?entry.0;
          };

          started = func(): Bool {
            started;
          };

          finished = func(): Bool {
            started and entry.2 == ROOT;
          };

          reset = func(): Iter<K> {
            started := false;
            entry := map.0;

            iter;
          };

          movePrev = func(): Iter<K> {
            started := true;
            entry := entry.3[DEQ_NEXT];

            loop if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2 or entry.2 == ROOT) return iter else {
              entry := entry.3[DEQ_NEXT];
            };
          };

          moveNext = func(): Iter<K> {
            started := true;
            entry := entry.3[DEQ_PREV];

            loop if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2 or entry.2 == ROOT) return iter else {
              entry := entry.3[DEQ_PREV];
            };
          };
        };
      } else {
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func valsFrom<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, placeParam: ?K): Iter<V> {
    let keyParam = switch (placeParam) { case (?keyParam) keyParam; case (_) map.0.0 };

    let hashParam = switch (placeParam) { case (null) ROOT; case (_) hashUtils.0(keyParam) };

    var shiftingHash = hashParam;

    var entry = if (shiftingHash != ROOT) map.0.3[nat(shiftingHash % HASH_CHUNK_SIZE)] else map.0;

    loop {
      let hash = entry.2;

      if (hash == ROOT or hash == hashParam and hashUtils.1(entry.0, keyParam)) {
        var started = hash != ROOT;

        return let iter = {
          prev = func(): ?V {
            started := true;
            entry := entry.3[DEQ_PREV];

            loop if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2 or entry.2 == ROOT) return entry.1 else {
              entry := entry.3[DEQ_PREV];
            };
          };

          next = func(): ?V {
            started := true;
            entry := entry.3[DEQ_NEXT];

            loop if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2 or entry.2 == ROOT) return entry.1 else {
              entry := entry.3[DEQ_NEXT];
            };
          };

          peekPrev = func(): ?V {
            var deqPrev = entry.3[DEQ_PREV];

            loop if (deqPrev.3[BRANCH_1].2 != deqPrev.2 or deqPrev.3[BRANCH_2].2 != deqPrev.2 or deqPrev.2 == ROOT) {
              entry.3[DEQ_PREV] := deqPrev;

              return deqPrev.1;
            } else {
              deqPrev := deqPrev.3[DEQ_PREV];
            };
          };

          peekNext = func(): ?V {
            var deqNext = entry.3[DEQ_NEXT];

            loop if (deqNext.3[BRANCH_1].2 != deqNext.2 or deqNext.3[BRANCH_2].2 != deqNext.2 or deqNext.2 == ROOT) {
              entry.3[DEQ_NEXT] := deqNext;

              return deqNext.1;
            } else {
              deqNext := deqNext.3[DEQ_NEXT];
            };
          };

          current = func(): ?V {
            entry.1;
          };

          started = func(): Bool {
            started;
          };

          finished = func(): Bool {
            started and entry.2 == ROOT;
          };

          reset = func(): Iter<V> {
            started := false;
            entry := map.0;

            iter;
          };

          movePrev = func(): Iter<V> {
            started := true;
            entry := entry.3[DEQ_PREV];

            loop if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2 or entry.2 == ROOT) return iter else {
              entry := entry.3[DEQ_PREV];
            };
          };

          moveNext = func(): Iter<V> {
            started := true;
            entry := entry.3[DEQ_NEXT];

            loop if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2 or entry.2 == ROOT) return iter else {
              entry := entry.3[DEQ_NEXT];
            };
          };
        };
      } else {
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func valsFromDesc<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, placeParam: ?K): Iter<V> {
    let keyParam = switch (placeParam) { case (?keyParam) keyParam; case (_) map.0.0 };

    let hashParam = switch (placeParam) { case (null) ROOT; case (_) hashUtils.0(keyParam) };

    var shiftingHash = hashParam;

    var entry = if (shiftingHash != ROOT) map.0.3[nat(shiftingHash % HASH_CHUNK_SIZE)] else map.0;

    loop {
      let hash = entry.2;

      if (hash == ROOT or hash == hashParam and hashUtils.1(entry.0, keyParam)) {
        var started = hash != ROOT;

        return let iter = {
          prev = func(): ?V {
            started := true;
            entry := entry.3[DEQ_NEXT];

            loop if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2 or entry.2 == ROOT) return entry.1 else {
              entry := entry.3[DEQ_NEXT];
            };
          };

          next = func(): ?V {
            started := true;
            entry := entry.3[DEQ_PREV];

            loop if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2 or entry.2 == ROOT) return entry.1 else {
              entry := entry.3[DEQ_PREV];
            };
          };

          peekPrev = func(): ?V {
            var deqNext = entry.3[DEQ_NEXT];

            loop if (deqNext.3[BRANCH_1].2 != deqNext.2 or deqNext.3[BRANCH_2].2 != deqNext.2 or deqNext.2 == ROOT) {
              entry.3[DEQ_NEXT] := deqNext;

              return deqNext.1;
            } else {
              deqNext := deqNext.3[DEQ_NEXT];
            };
          };

          peekNext = func(): ?V {
            var deqPrev = entry.3[DEQ_PREV];

            loop if (deqPrev.3[BRANCH_1].2 != deqPrev.2 or deqPrev.3[BRANCH_2].2 != deqPrev.2 or deqPrev.2 == ROOT) {
              entry.3[DEQ_PREV] := deqPrev;

              return deqPrev.1;
            } else {
              deqPrev := deqPrev.3[DEQ_PREV];
            };
          };

          current = func(): ?V {
            entry.1;
          };

          started = func(): Bool {
            started;
          };

          finished = func(): Bool {
            started and entry.2 == ROOT;
          };

          reset = func(): Iter<V> {
            started := false;
            entry := map.0;

            iter;
          };

          movePrev = func(): Iter<V> {
            started := true;
            entry := entry.3[DEQ_NEXT];

            loop if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2 or entry.2 == ROOT) return iter else {
              entry := entry.3[DEQ_NEXT];
            };
          };

          moveNext = func(): Iter<V> {
            started := true;
            entry := entry.3[DEQ_PREV];

            loop if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2 or entry.2 == ROOT) return iter else {
              entry := entry.3[DEQ_PREV];
            };
          };
        };
      } else {
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func entriesFrom<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, placeParam: ?K): Iter<(K, V)> {
    let keyParam = switch (placeParam) { case (?keyParam) keyParam; case (_) map.0.0 };

    let hashParam = switch (placeParam) { case (null) ROOT; case (_) hashUtils.0(keyParam) };

    var shiftingHash = hashParam;

    var entry = if (shiftingHash != ROOT) map.0.3[nat(shiftingHash % HASH_CHUNK_SIZE)] else map.0;

    loop {
      let hash = entry.2;

      if (hash == ROOT or hash == hashParam and hashUtils.1(entry.0, keyParam)) {
        var started = hash != ROOT;

        return let iter = {
          prev = func(): ?(K, V) {
            started := true;
            entry := entry.3[DEQ_PREV];

            loop switch (entry.1) {
              case (?someValue) if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2) return ?(entry.0, someValue) else {
                entry := entry.3[DEQ_PREV];
              };

              case (_) return null;
            };
          };

          next = func(): ?(K, V) {
            started := true;
            entry := entry.3[DEQ_NEXT];

            loop switch (entry.1) {
              case (?someValue) if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2) return ?(entry.0, someValue) else {
                entry := entry.3[DEQ_NEXT];
              };

              case (_) return null;
            };
          };

          peekPrev = func(): ?(K, V) {
            var deqPrev = entry.3[DEQ_PREV];

            loop switch (deqPrev.1) {
              case (?someValue) if (deqPrev.3[BRANCH_1].2 != deqPrev.2 or deqPrev.3[BRANCH_2].2 != deqPrev.2) {
                entry.3[DEQ_PREV] := deqPrev;

                return ?(deqPrev.0, someValue);
              } else {
                deqPrev := deqPrev.3[DEQ_PREV];
              };

              case (_) return null;
            };
          };

          peekNext = func(): ?(K, V) {
            var deqNext = entry.3[DEQ_NEXT];

            loop switch (deqNext.1) {
              case (?someValue) if (deqNext.3[BRANCH_1].2 != deqNext.2 or deqNext.3[BRANCH_2].2 != deqNext.2) {
                entry.3[DEQ_NEXT] := deqNext;

                return ?(deqNext.0, someValue);
              } else {
                deqNext := deqNext.3[DEQ_NEXT];
              };

              case (_) return null;
            };
          };

          current = func(): ?(K, V) {
            switch (entry.1) { case (?someValue) ?(entry.0, someValue); case (_) null };
          };

          started = func(): Bool {
            started;
          };

          finished = func(): Bool {
            started and entry.2 == ROOT;
          };

          reset = func(): Iter<(K, V)> {
            started := false;
            entry := map.0;

            iter;
          };

          movePrev = func(): Iter<(K, V)> {
            started := true;
            entry := entry.3[DEQ_PREV];

            loop if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2 or entry.2 == ROOT) return iter else {
              entry := entry.3[DEQ_PREV];
            };
          };

          moveNext = func(): Iter<(K, V)> {
            started := true;
            entry := entry.3[DEQ_NEXT];

            loop if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2 or entry.2 == ROOT) return iter else {
              entry := entry.3[DEQ_NEXT];
            };
          };
        };
      } else {
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  public func entriesFromDesc<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, placeParam: ?K): Iter<(K, V)> {
    let keyParam = switch (placeParam) { case (?keyParam) keyParam; case (_) map.0.0 };

    let hashParam = switch (placeParam) { case (null) ROOT; case (_) hashUtils.0(keyParam) };

    var shiftingHash = hashParam;

    var entry = if (shiftingHash != ROOT) map.0.3[nat(shiftingHash % HASH_CHUNK_SIZE)] else map.0;

    loop {
      let hash = entry.2;

      if (hash == ROOT or hash == hashParam and hashUtils.1(entry.0, keyParam)) {
        var started = hash != ROOT;

        return let iter = {
          prev = func(): ?(K, V) {
            started := true;
            entry := entry.3[DEQ_NEXT];

            loop switch (entry.1) {
              case (?someValue) if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2) return ?(entry.0, someValue) else {
                entry := entry.3[DEQ_NEXT];
              };

              case (_) return null;
            };
          };

          next = func(): ?(K, V) {
            started := true;
            entry := entry.3[DEQ_PREV];

            loop switch (entry.1) {
              case (?someValue) if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2) return ?(entry.0, someValue) else {
                entry := entry.3[DEQ_PREV];
              };

              case (_) return null;
            };
          };

          peekPrev = func(): ?(K, V) {
            var deqNext = entry.3[DEQ_NEXT];

            loop switch (deqNext.1) {
              case (?someValue) if (deqNext.3[BRANCH_1].2 != deqNext.2 or deqNext.3[BRANCH_2].2 != deqNext.2) {
                entry.3[DEQ_NEXT] := deqNext;

                return ?(deqNext.0, someValue);
              } else {
                deqNext := deqNext.3[DEQ_NEXT];
              };

              case (_) return null;
            };
          };

          peekNext = func(): ?(K, V) {
            var deqPrev = entry.3[DEQ_PREV];

            loop switch (deqPrev.1) {
              case (?someValue) if (deqPrev.3[BRANCH_1].2 != deqPrev.2 or deqPrev.3[BRANCH_2].2 != deqPrev.2) {
                entry.3[DEQ_PREV] := deqPrev;

                return ?(deqPrev.0, someValue);
              } else {
                deqPrev := deqPrev.3[DEQ_PREV];
              };

              case (_) return null;
            };
          };

          current = func(): ?(K, V) {
            switch (entry.1) { case (?someValue) ?(entry.0, someValue); case (_) null };
          };

          started = func(): Bool {
            started;
          };

          finished = func(): Bool {
            started and entry.2 == ROOT;
          };

          reset = func(): Iter<(K, V)> {
            started := false;
            entry := map.0;

            iter;
          };

          movePrev = func(): Iter<(K, V)> {
            started := true;
            entry := entry.3[DEQ_NEXT];

            loop if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2 or entry.2 == ROOT) return iter else {
              entry := entry.3[DEQ_NEXT];
            };
          };

          moveNext = func(): Iter<(K, V)> {
            started := true;
            entry := entry.3[DEQ_PREV];

            loop if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2 or entry.2 == ROOT) return iter else {
              entry := entry.3[DEQ_PREV];
            };
          };
        };
      } else {
        shiftingHash >>= HASH_OFFSET;
        entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func find<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool): ?(K, V) {
    var entry = map.0;

    loop {
      entry := entry.3[DEQ_NEXT];

      switch (entry.1) {
        case (?someValue) if ((entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2) and acceptEntry(entry.0, someValue)) {
          return ?(entry.0, someValue);
        };

        case (_) return null;
      };
    };
  };

  public func findDesc<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool): ?(K, V) {
    var entry = map.0;

    loop {
      entry := entry.3[DEQ_PREV];

      switch (entry.1) {
        case (?someValue) if ((entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2) and acceptEntry(entry.0, someValue)) {
          return ?(entry.0, someValue);
        };

        case (_) return null;
      };
    };
  };

  public func some<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool): Bool {
    var entry = map.0;

    loop {
      entry := entry.3[DEQ_NEXT];

      switch (entry.1) {
        case (?someValue) if ((entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2) and acceptEntry(entry.0, someValue)) {
          return true;
        };

        case (_) return false;
      };
    };
  };

  public func someDesc<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool): Bool {
    var entry = map.0;

    loop {
      entry := entry.3[DEQ_PREV];

      switch (entry.1) {
        case (?someValue) if ((entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2) and acceptEntry(entry.0, someValue)) {
          return true;
        };

        case (_) return false;
      };
    };
  };

  public func every<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool): Bool {
    var entry = map.0;

    loop {
      entry := entry.3[DEQ_NEXT];

      switch (entry.1) {
        case (?someValue) if ((entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2) and not acceptEntry(entry.0, someValue)) {
          return false;
        };

        case (_) return true;
      };
    };
  };

  public func everyDesc<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool): Bool {
    var entry = map.0;

    loop {
      entry := entry.3[DEQ_PREV];

      switch (entry.1) {
        case (?someValue) if ((entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2) and not acceptEntry(entry.0, someValue)) {
          return false;
        };

        case (_) return true;
      };
    };
  };

  public func forEach<K, V>(map: Map<K, V>, mapEntry: (K, V) -> ()) {
    var entry = map.0;

    loop {
      entry := entry.3[DEQ_NEXT];

      switch (entry.1) {
        case (?someValue) if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2) mapEntry(entry.0, someValue);
        case (_) return;
      };
    };
  };

  public func forEachDesc<K, V>(map: Map<K, V>, mapEntry: (K, V) -> ()) {
    var entry = map.0;

    loop {
      entry := entry.3[DEQ_PREV];

      switch (entry.1) {
        case (?someValue) if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2) mapEntry(entry.0, someValue);
        case (_) return;
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func fromIter<K, V>(iter: IterNext<(K, V)>, hashUtils: HashUtils<K>): Map<K, V> {
    let root = createRoot<K, V>(hashUtils.2());
    let rootLinks = root.3;
    let getHash = hashUtils.0;
    let areEqual = hashUtils.1;
    var size = 0:Nat32;

    for (item in iter) label loopBody {
      let hashParam = getHash(item.0);
      var shiftingHash = hashParam;
      var parent = root;
      var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

      loop {
        let hash = entry.2;

        if (hash == ROOT) {
          let deqPrev = rootLinks[DEQ_PREV];
          let newEntry = (item.0, ?item.1, hashParam, [var root, root, root, root, deqPrev, root]);

          deqPrev.3[DEQ_NEXT] := newEntry;
          rootLinks[DEQ_PREV] := newEntry;
          parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
          size +%= 1;

          break loopBody;
        } else if (hash == hashParam and areEqual(entry.0, item.0)) {
          let links = entry.3;
          let deqPrev = rootLinks[DEQ_PREV];

          let newEntry = (
            entry.0,
            ?item.1,
            hash,
            [var links[BRANCH_1], links[BRANCH_2], links[BRANCH_3], links[BRANCH_4], deqPrev, root],
          );

          deqPrev.3[DEQ_NEXT] := newEntry;
          rootLinks[DEQ_PREV] := newEntry;
          parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

          let deqPrevOld = links[DEQ_PREV];
          let deqNextOld = links[DEQ_NEXT];

          deqNextOld.3[DEQ_PREV] := deqPrevOld;
          deqPrevOld.3[DEQ_NEXT] := deqNextOld;
          links[BRANCH_1] := entry;
          links[BRANCH_2] := entry;

          break loopBody;
        } else {
          parent := entry;
          shiftingHash >>= HASH_OFFSET;
          entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
        };
      };
    };

    (root, [var size]);
  };

  public func fromIterDesc<K, V>(iter: IterNext<(K, V)>, hashUtils: HashUtils<K>): Map<K, V> {
    let root = createRoot<K, V>(hashUtils.2());
    let rootLinks = root.3;
    let getHash = hashUtils.0;
    let areEqual = hashUtils.1;
    var size = 0:Nat32;

    for (item in iter) label loopBody {
      let hashParam = getHash(item.0);
      var shiftingHash = hashParam;
      var parent = root;
      var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

      loop {
        let hash = entry.2;

        if (hash == ROOT) {
          let deqNext = rootLinks[DEQ_NEXT];
          let newEntry = (item.0, ?item.1, hashParam, [var root, root, root, root, root, deqNext]);

          deqNext.3[DEQ_PREV] := newEntry;
          rootLinks[DEQ_NEXT] := newEntry;
          parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
          size +%= 1;

          break loopBody;
        } else if (hash == hashParam and areEqual(entry.0, item.0)) {
          let links = entry.3;
          let deqNext = rootLinks[DEQ_NEXT];

          let newEntry = (
            entry.0,
            ?item.1,
            hash,
            [var links[BRANCH_1], links[BRANCH_2], links[BRANCH_3], links[BRANCH_4], root, deqNext],
          );

          deqNext.3[DEQ_PREV] := newEntry;
          rootLinks[DEQ_NEXT] := newEntry;
          parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

          let deqNextOld = links[DEQ_NEXT];
          let deqPrevOld = links[DEQ_PREV];

          deqPrevOld.3[DEQ_NEXT] := deqNextOld;
          deqNextOld.3[DEQ_PREV] := deqPrevOld;
          links[BRANCH_1] := entry;
          links[BRANCH_2] := entry;

          break loopBody;
        } else {
          parent := entry;
          shiftingHash >>= HASH_OFFSET;
          entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
        };
      };
    };

    (root, [var size]);
  };

  public func fromIterMap<K, V, T>(iter: IterNext<T>, hashUtils: HashUtils<K>, mapItem: (T) -> ?(K, V)): Map<K, V> {
    let root = createRoot<K, V>(hashUtils.2());
    let rootLinks = root.3;
    let getHash = hashUtils.0;
    let areEqual = hashUtils.1;
    var size = 0:Nat32;

    for (item in iter) label loopBody switch (mapItem(item)) {
      case (?item) {
        let hashParam = getHash(item.0);
        var shiftingHash = hashParam;
        var parent = root;
        var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

        loop {
          let hash = entry.2;

          if (hash == ROOT) {
            let deqPrev = rootLinks[DEQ_PREV];
            let newEntry = (item.0, ?item.1, hashParam, [var root, root, root, root, deqPrev, root]);

            deqPrev.3[DEQ_NEXT] := newEntry;
            rootLinks[DEQ_PREV] := newEntry;
            parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
            size +%= 1;

            break loopBody;
          } else if (hash == hashParam and areEqual(entry.0, item.0)) {
            let links = entry.3;
            let deqPrev = rootLinks[DEQ_PREV];

            let newEntry = (
              entry.0,
              ?item.1,
              hash,
              [var links[BRANCH_1], links[BRANCH_2], links[BRANCH_3], links[BRANCH_4], deqPrev, root],
            );

            deqPrev.3[DEQ_NEXT] := newEntry;
            rootLinks[DEQ_PREV] := newEntry;
            parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

            let deqPrevOld = links[DEQ_PREV];
            let deqNextOld = links[DEQ_NEXT];

            deqNextOld.3[DEQ_PREV] := deqPrevOld;
            deqPrevOld.3[DEQ_NEXT] := deqNextOld;
            links[BRANCH_1] := entry;
            links[BRANCH_2] := entry;

            break loopBody;
          } else {
            parent := entry;
            shiftingHash >>= HASH_OFFSET;
            entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
          };
        };
      };

      case (_) {};
    };

    (root, [var size]);
  };

  public func fromIterMapDesc<K, V, T>(iter: IterNext<T>, hashUtils: HashUtils<K>, mapItem: (T) -> ?(K, V)): Map<K, V> {
    let root = createRoot<K, V>(hashUtils.2());
    let rootLinks = root.3;
    let getHash = hashUtils.0;
    let areEqual = hashUtils.1;
    var size = 0:Nat32;

    for (item in iter) label loopBody switch (mapItem(item)) {
      case (?item) {
        let hashParam = getHash(item.0);
        var shiftingHash = hashParam;
        var parent = root;
        var entry = rootLinks[nat(shiftingHash % HASH_CHUNK_SIZE)];

        loop {
          let hash = entry.2;

          if (hash == ROOT) {
            let deqNext = rootLinks[DEQ_NEXT];
            let newEntry = (item.0, ?item.1, hashParam, [var root, root, root, root, root, deqNext]);

            deqNext.3[DEQ_PREV] := newEntry;
            rootLinks[DEQ_NEXT] := newEntry;
            parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
            size +%= 1;

            break loopBody;
          } else if (hash == hashParam and areEqual(entry.0, item.0)) {
            let links = entry.3;
            let deqNext = rootLinks[DEQ_NEXT];

            let newEntry = (
              entry.0,
              ?item.1,
              hash,
              [var links[BRANCH_1], links[BRANCH_2], links[BRANCH_3], links[BRANCH_4], root, deqNext],
            );

            deqNext.3[DEQ_PREV] := newEntry;
            rootLinks[DEQ_NEXT] := newEntry;
            parent.3[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;

            let deqNextOld = links[DEQ_NEXT];
            let deqPrevOld = links[DEQ_PREV];

            deqPrevOld.3[DEQ_NEXT] := deqNextOld;
            deqNextOld.3[DEQ_PREV] := deqPrevOld;
            links[BRANCH_1] := entry;
            links[BRANCH_2] := entry;

            break loopBody;
          } else {
            parent := entry;
            shiftingHash >>= HASH_OFFSET;
            entry := entry.3[nat(shiftingHash % HASH_CHUNK_SIZE)];
          };
        };
      };

      case (_) {};
    };

    (root, [var size]);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func toArray<K, V>(map: Map<K, V>): [(K, V)] {
    var entry = map.0;

    tabulateArray<(K, V)>(nat(map.1[SIZE]), func(i) {
      entry := entry.3[DEQ_NEXT];

      (entry.0, switch (entry.1) { case (?value) value; case (_) trap("unreachable") });
    });
  };

  public func toArrayDesc<K, V>(map: Map<K, V>): [(K, V)] {
    var entry = map.0;

    tabulateArray<(K, V)>(nat(map.1[SIZE]), func(i) {
      entry := entry.3[DEQ_PREV];

      (entry.0, switch (entry.1) { case (?value) value; case (_) trap("unreachable") });
    });
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func toArrayMap<K, V, T>(map: Map<K, V>, mapEntry: (K, V) -> ?T): [T] {
    var entry = map.0;
    var maxSize = map.1[SIZE];
    var array = initArray<?T>(nat(maxSize), null);
    var arraySize = 0:Nat32;

    loop {
      entry := entry.3[DEQ_NEXT];

      switch (entry.1) {
        case (?someValue) if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2) switch (mapEntry(entry.0, someValue)) {
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

        case (_) return tabulateArray<T>(nat(arraySize), func(i) = switch (array[i]) { case (?value) value; case (_) trap("unreachable") });
      };
    };
  };

  public func toArrayMapDesc<K, V, T>(map: Map<K, V>, mapEntry: (K, V) -> ?T): [T] {
    var entry = map.0;
    var maxSize = map.1[SIZE];
    var array = initArray<?T>(nat(maxSize), null);
    var arraySize = 0:Nat32;

    loop {
      entry := entry.3[DEQ_PREV];

      switch (entry.1) {
        case (?someValue) if (entry.3[BRANCH_1].2 != entry.2 or entry.3[BRANCH_2].2 != entry.2) switch (mapEntry(entry.0, someValue)) {
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

        case (_) return tabulateArray<T>(nat(arraySize), func(i) = switch (array[i]) { case (?value) value; case (_) trap("unreachable") });
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
