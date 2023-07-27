import Prim "mo:prim";
import Debug "mo:base/Debug";

module {
  public type Entry<K, V> = (
    key: K,
    value: ?V,
    hash: Nat32,
    links: [var Entry<K, V>],
  );

  public type Map<K, V> = {
    var data: (
      keys: [var ?K],
      values: [var ?V],
      chainTable: [var Nat],
      hashTable: [var Nat],
      capacity: Nat32,
    );
    var frontIndex: Nat32;
    var backIndex: Nat32;
    var size: Nat32;
  };

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

  let NULL = 0x3fffffff;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  let { Array_tabulate = tabulateArray; Array_init = initArray; nat32ToNat = nat; clzNat32 = clz; trap } = Prim;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func rehash<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>) {
    let getHash = hashUtils.0;
    let (keys, values, chainTable, hashTable, capacity) = map.data;
    let size = map.size;

    let newCapacity = 2 **% (32 -% clz((size *% 4 / 3) | 1));
    let newCapacityNat = nat(newCapacity);

    if (newCapacityNat >= NULL) trap("Map capacity limit reached (2 ** 30)");

    let newKeys = initArray<?K>(newCapacityNat, null);
    let newValues = initArray<?V>(newCapacityNat, null);
    let newChainTable = initArray<Nat>(newCapacityNat, NULL);
    let newHashTable = initArray<Nat>(newCapacityNat, NULL);

    let firstIndex = (map.frontIndex +% 1) % capacity;
    var index = map.backIndex;
    var newIndex = size;

    if (size != 0) loop {
      index := (index -% 1) % capacity;

      let indexNat = nat(index);
      let key = keys[indexNat];

      switch (key) {
        case (?someKey) {
          newIndex -%= 1;

          let newIndexNat = nat(newIndex);
          let bucketIndex = nat(getHash(someKey) % newCapacity);

          newKeys[newIndexNat] := key;
          newValues[newIndexNat] := values[indexNat];
          newChainTable[newIndexNat] := newHashTable[bucketIndex];
          newHashTable[bucketIndex] := newIndexNat;
        };

        case (_) {};
      };
    } while (index != firstIndex);

    map.data := (newKeys, newValues, newChainTable, newHashTable, newCapacity);
    map.backIndex := size;
    map.frontIndex := newCapacity -% 1;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func new<K, V>(): Map<K, V> {
    {
      var data = ([var null, null], [var null, null], [var NULL, NULL], [var NULL, NULL], 2);
      var frontIndex = 1;
      var backIndex = 0;
      var size = 0;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func size<K, V>(map: Map<K, V>): Nat {
    nat(map.size);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func get<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): ?V {
    let (keys, values, chainTable, hashTable, capacity) = map.data;

    var index = hashTable[nat(hashUtils.0(keyParam) % capacity)];

    loop if (index == NULL) {
      return null;
    } else if (hashUtils.1(switch (keys[index]) { case (?value) value; case (_) trap("get unreachable") }, keyParam)) {
      return values[index];
    } else {
      index := chainTable[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func set<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V) {
    let (keys, values, chainTable, hashTable, capacity) = map.data;

    let bucketIndex = nat(hashUtils.0(keyParam) % capacity);
    var index = hashTable[bucketIndex];
    var prevIndex = NULL;

    loop if (index == NULL) {
      let backIndex = map.backIndex;
      let backIndexNat = nat(backIndex);

      map.backIndex := (backIndex +% 1) % capacity;
      map.size +%= 1;

      keys[backIndexNat] := ?keyParam;
      values[backIndexNat] := ?valueParam;

      if (prevIndex == NULL) hashTable[bucketIndex] := backIndexNat else chainTable[prevIndex] := backIndexNat;

      if (backIndex == map.frontIndex) rehash(map, hashUtils);

      return;
    } else if (hashUtils.1(switch (keys[index]) { case (?value) value; case (_) trap("set unreachable") }, keyParam)) {
      values[index] := ?valueParam;

      return;
    } else {
      prevIndex := index;
      index := chainTable[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func delete<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K) {
    let (keys, values, chainTable, hashTable, capacity) = map.data;

    let bucketIndex = nat(hashUtils.0(keyParam) % capacity);
    var index = hashTable[bucketIndex];
    var prevIndex = NULL;

    loop if (index == NULL) {
      return;
    } else if (hashUtils.1(switch (keys[index]) { case (?value) value; case (_) trap("delete unreachable") }, keyParam)) {
      let newSize = map.size -% 1;

      map.size := newSize;

      let nextIndex = chainTable[index];

      if (nextIndex != NULL) chainTable[index] := NULL;
      if (prevIndex == NULL) hashTable[bucketIndex] := nextIndex else chainTable[prevIndex] := nextIndex;

      keys[index] := null;
      values[index] := null;

      if (newSize < capacity *% 3 / 8) rehash(map, hashUtils);

      return;
    } else {
      prevIndex := index;
      index := chainTable[index];
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
