import Prim "mo:prim";
import Nat32 "mo:base/Nat32";

module {
  public type Entry<K, V> = [var (
    key: K,
    value: V,
    hash: Nat32,
    branch1: ?Entry<K, V>,
    branch2: ?Entry<K, V>,
    branch3: ?Entry<K, V>,
    branch4: ?Entry<K, V>,
    deqPrev: ?Entry<K, V>,
    deqNext: ?Entry<K, V>,
  )];

  public type Map<K, V> = (
    root: [var ?Entry<K, V>],
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
  );

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  let SIZE = 0;
  let ENTRY = 0;

  let BRANCH_1 = 0;
  let BRANCH_2 = 1;
  let BRANCH_3 = 2;
  let BRANCH_4 = 3;
  let BRANCH32_1 = 0:Nat32;
  let BRANCH32_2 = 1:Nat32;
  let BRANCH32_3 = 2:Nat32;
  let BRANCH32_4 = 3:Nat32;

  let DEQ_LAST = 4;
  let DEQ_FIRST = 5;

  let TEMP_LINK = 4;
  let CLONE_NEXT = 5;
  let NULL_BRANCH = 6;
  let NULL_BRANCH32 = 6:Nat32;

  let HASH_OFFSET = 2:Nat32;
  let HASH_CHUNK_SIZE = 4:Nat32;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  let { Array_tabulate = tabulateArray; Array_init = initArray; nat32ToNat = nat; trap } = Prim;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func new<K, V>(hashUtils: HashUtils<K>): Map<K, V> {
    ([var null, null, null, null, null, null], [var 0]);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func size<K, V>(map: Map<K, V>): Nat {
    nat(map.1[SIZE]);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func get<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): ?V {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;
    var entry = map.0[nat(shiftingHash % HASH_CHUNK_SIZE)];

    loop switch (entry) {
      case (?someEntry) {
        let data = someEntry[ENTRY];

        if (data.2 == hashParam and hashUtils.1(data.0, keyParam)) {
          return ?data.1;
        } else {
          shiftingHash >>= HASH_OFFSET;

          let hashChunk = shiftingHash % HASH_CHUNK_SIZE;

          entry := if (hashChunk < BRANCH32_3) {
            if (hashChunk == BRANCH32_1) data.3 else data.4;
          } else {
            if (hashChunk == BRANCH32_3) data.5 else data.6;
          };
        };
      };

      case (_) return null;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func set<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V) {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;

    var entry = map.0[nat(shiftingHash % HASH_CHUNK_SIZE)];
    var parent = null:?Entry<K, V>;

    loop switch (entry) {
      case (?someEntry) {
        let data = someEntry[ENTRY];

        if (data.2 == hashParam and hashUtils.1(data.0, keyParam)) {
          someEntry[ENTRY] := (data.0, valueParam, data.2, data.3, data.4, data.5, data.6, data.7, data.8);

          return;
        } else {
          parent := entry;
          shiftingHash >>= HASH_OFFSET;

          let hashChunk = shiftingHash % HASH_CHUNK_SIZE;

          entry := if (hashChunk < BRANCH32_3) {
            if (hashChunk == BRANCH32_1) data.3 else data.4;
          } else {
            if (hashChunk == BRANCH32_3) data.5 else data.6;
          };
        };
      };

      case (_) {
        let deqLast = map.0[DEQ_LAST];
        let newEntry = ?[var (keyParam, valueParam, hashParam, null, null, null, null, deqLast, null)]:?Entry<K, V>;

        switch (deqLast) {
          case (?someDeqLast) {
            let data = someDeqLast[ENTRY];

            someDeqLast[ENTRY] := (data.0, data.1, data.2, data.3, data.4, data.5, data.6, data.7, newEntry);
          };

          case (_) map.0[DEQ_FIRST] := newEntry;
        };

        switch (parent) {
          case (?someParent) {
            let data = someParent[ENTRY];

            let hashChunk = shiftingHash % HASH_CHUNK_SIZE;

            if (hashChunk < BRANCH32_3) {
              if (hashChunk == BRANCH32_1) {
                someParent[ENTRY] := (data.0, data.1, data.2, newEntry, data.4, data.5, data.6, data.7, data.8);
              } else {
                someParent[ENTRY] := (data.0, data.1, data.2, data.3, newEntry, data.5, data.6, data.7, data.8);
              };
            } else {
              if (hashChunk == BRANCH32_3) {
                someParent[ENTRY] := (data.0, data.1, data.2, data.3, data.4, newEntry, data.6, data.7, data.8);
              } else {
                someParent[ENTRY] := (data.0, data.1, data.2, data.3, data.4, data.5, newEntry, data.7, data.8);
              };
            };
          };

          case (_) map.0[nat(shiftingHash % HASH_CHUNK_SIZE)] := newEntry;
        };

        map.0[DEQ_LAST] := newEntry;
        map.1[SIZE] +%= 1;

        return;
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func delete<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K) {
    let hashParam = hashUtils.0(keyParam);
    var shiftingHash = hashParam;

    let root = map.0;
    var entry = root[nat(shiftingHash % HASH_CHUNK_SIZE)];
    var parent = null:?Entry<K, V>;

    loop {
      switch (entry) {
        case (?someEntry) {
          let data = someEntry[ENTRY];

          if (data.2 == hashParam and hashUtils.1(data.0, keyParam)) {
            switch (data.7) {
              case (?someDeqPrev) {
                let deqPrev = someDeqPrev[ENTRY];

                someDeqPrev[ENTRY] := (deqPrev.0, deqPrev.1, deqPrev.2, deqPrev.3, deqPrev.4, deqPrev.5, deqPrev.6, deqPrev.7, data.8);
              };

              case (_) root[DEQ_FIRST] := data.8;
            };

            switch (data.8) {
              case (?someDeqNext) {
                let deqNext = someDeqNext[ENTRY];

                someDeqNext[ENTRY] := (deqNext.0, deqNext.1, deqNext.2, deqNext.3, deqNext.4, deqNext.5, deqNext.6, data.7, deqNext.8);
              };

              case (_) root[DEQ_LAST] := data.7;
            };

            map.1[SIZE] -%= 1;

            var leaf = someEntry;
            var leafParent = leaf;
            var leafIndex = NULL_BRANCH32;

            loop {
              let leafData = leaf[ENTRY];

              switch (leafData.3) {
                case (?branch1) {
                  leafParent := leaf;
                  leaf := branch1;
                  leafIndex := BRANCH32_1;
                };

                case (_) switch (leafData.4) {
                  case (?branch2) {
                    leafParent := leaf;
                    leaf := branch2;
                    leafIndex := BRANCH32_2;
                  };

                  case (_) switch (leafData.5) {
                    case (?branch3) {
                      leafParent := leaf;
                      leaf := branch3;
                      leafIndex := BRANCH32_3;
                    };

                    case (_) switch (leafData.6) {
                      case (?branch4) {
                        leafParent := leaf;
                        leaf := branch4;
                        leafIndex := BRANCH32_4;
                      };

                      case (_) {
                        if (leafIndex == NULL_BRANCH32) {
                          switch (parent) {
                            case (?someParent) {
                              let data = someParent[ENTRY];

                              let hashChunk = shiftingHash % HASH_CHUNK_SIZE;

                              if (hashChunk < BRANCH32_3) {
                                if (hashChunk == BRANCH32_1) {
                                  someParent[ENTRY] := (data.0, data.1, data.2, null, data.4, data.5, data.6, data.7, data.8);
                                } else {
                                  someParent[ENTRY] := (data.0, data.1, data.2, data.3, null, data.5, data.6, data.7, data.8);
                                };
                              } else {
                                if (hashChunk == BRANCH32_3) {
                                  someParent[ENTRY] := (data.0, data.1, data.2, data.3, data.4, null, data.6, data.7, data.8);
                                } else {
                                  someParent[ENTRY] := (data.0, data.1, data.2, data.3, data.4, data.5, null, data.7, data.8);
                                };
                              };
                            };

                            case (_) root[nat(shiftingHash % HASH_CHUNK_SIZE)] := null;
                          };
                        } else {
                          let parentData = leafParent[ENTRY];

                          if (leafIndex < BRANCH32_3) {
                            if (leafIndex == BRANCH32_1) {
                              leafParent[ENTRY] := (parentData.0, parentData.1, parentData.2, null, parentData.4, parentData.5, parentData.6, parentData.7, parentData.8);
                            } else {
                              leafParent[ENTRY] := (parentData.0, parentData.1, parentData.2, parentData.3, null, parentData.5, parentData.6, parentData.7, parentData.8);
                            };
                          } else {
                            if (leafIndex == BRANCH32_3) {
                              leafParent[ENTRY] := (parentData.0, parentData.1, parentData.2, parentData.3, parentData.4, null, parentData.6, parentData.7, parentData.8);
                            } else {
                              leafParent[ENTRY] := (parentData.0, parentData.1, parentData.2, parentData.3, parentData.4, parentData.5, null, parentData.7, parentData.8);
                            };
                          };

                          someEntry[ENTRY] := (leafData.0, leafData.1, leafData.2, data.3, data.4, data.5, data.6, leafData.7, leafData.8);
                        };

                        return;
                      };
                    };
                  };
                };
              };
            };
          } else {
            parent := entry;
            shiftingHash >>= HASH_OFFSET;

            let hashChunk = shiftingHash % HASH_CHUNK_SIZE;

            entry := if (hashChunk < BRANCH32_3) {
              if (hashChunk == BRANCH32_1) data.3 else data.4;
            } else {
              if (hashChunk == BRANCH32_3) data.5 else data.6;
            };
          };
        };

        case (_) return;
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

  public let ihash = (hashInt, func(a, b) = a == b):HashUtils<Int>;

  public let i8hash = (hashInt8, func(a, b) = a == b):HashUtils<Int8>;

  public let i16hash = (hashInt16, func(a, b) = a == b):HashUtils<Int16>;

  public let i32hash = (hashInt32, func(a, b) = a == b):HashUtils<Int32>;

  public let i64hash = (hashInt64, func(a, b) = a == b):HashUtils<Int64>;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public let nhash = (hashNat, func(a, b) = a == b):HashUtils<Nat>;

  public let n8hash = (hashNat8, func(a, b) = a == b):HashUtils<Nat8>;

  public let n16hash = (hashNat16, func(a, b) = a == b):HashUtils<Nat16>;

  public let n32hash = (hashNat32, func(a, b) = a == b):HashUtils<Nat32>;

  public let n64hash = (hashNat64, func(a, b) = a == b):HashUtils<Nat64>;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public let thash = (hashText, func(a, b) = a == b):HashUtils<Text>;

  public let phash = (hashPrincipal, func(a, b) = a == b):HashUtils<Principal>;

  public let bhash = (hashBlob, func(a, b) = a == b):HashUtils<Blob>;

  public let lhash = (hashBool, func(a, b) = a == b):HashUtils<Bool>;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func useHash<K>(hashUtils: HashUtils<K>, hash: Nat32): HashUtils<K> {
    (func(key) = hash, hashUtils.1);
  };

  public func calcHash<K>(hashUtils: HashUtils<K>, key: K): HashUtils<K> {
    let hash = hashUtils.0(key);

    (func(key) = hash, hashUtils.1);
  };
};
