import Prim "mo:prim";
import Utils "../utils";

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

  let PUT = 0x001:Nat32;
  let ADD = 0x002:Nat32;
  let UPDATE = 0x004:Nat32;
  let REPLACE = 0x008:Nat32;

  let FRONT = 0x010:Nat32;
  let MOVE = 0x100:Nat32;

  let REMOVE = 0x001:Nat32;
  let POP = 0x002:Nat32;
  let CYCLE = 0x004:Nat32;

  let MAX_CAPACITY = 0x3fffffff:Nat32;
  let NULL = 0x3fffffff;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  let { Array_tabulate = tabulateArray; Array_init = initArray; natToNat32 = nat32; nat32ToNat = nat; clzNat32 = clz; trap } = Prim;

  func constant<T>(value: T): (Any, Any) -> T = func(_) = value;

  func isSome<T>(value: ?T): Bool = switch (value) { case (null) false; case (_) true };

  func negate<A, B>(fn: (A, B) -> Bool): (A, B) -> Bool = func(a, b) = not fn(a, b);

  func reject<A, B>(fn: (A, B) -> ()): (A, B) -> Bool = func(a, b) { fn(a, b); return false };

  func makeOption<A, B, R>(fn: (A, B) -> R): (A, B) -> ?R = func(a, b) = ?fn(a, b);

  func boolToOption<A, B>(fn: (A, B) -> Bool): (A, B) -> ?B = func(a, b) = if (fn(a, b)) ?b else null;

  func is(method: Nat32, param: Nat32): Bool = method & param != 0;

  func unwrap<T>(value: ?T): T = switch (value) { case (?value) value; case (_) trap("unreachable") };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  let nullHash = (func(_) = trap("unreachable"), func(_) = trap("unreachable")):Utils.NullHashUtils;

  func entryValue<K, V>(entry: ?(K, V)): ?V = switch (entry) { case (?(key, value)) ?value; case (_) null };

  func getEntry<K, V>(key: ?K, value: ?V): ?(K, V) = switch (key) { case (?someKey) ?(someKey, unwrap(value)); case (_) null };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func new<K, V>(): Map<K, V> {
    return {
      var data = ([var null, null], [var null, null], [var NULL, NULL], [var NULL, NULL], 2);
      var frontIndex = 1;
      var backIndex = 0;
      var size = 0;
    };
  };

  public func make<K, V>((getHash, areEqual): HashUtils<K>, keyParam: K, valueParam: V): Map<K, V> {
    return {
      var data = (
        [var ?keyParam, null],
        [var ?valueParam, null],
        [var NULL, NULL],
        if (getHash(keyParam) % 2 == 0) [var 0, NULL] else [var NULL, 0],
        2,
      );
      var frontIndex = 1;
      var backIndex = 0;
      var size = 1;
    };
  };

  public func clear<K, V>(map: Map<K, V>) {
    map.data := ([var null, null], [var null, null], [var NULL, NULL], [var NULL, NULL], 2);
    map.frontIndex := 1;
    map.backIndex := 0;
    map.size := 0;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func size<K, V>(map: Map<K, V>): Nat {
    return nat(map.size);
  };

  public func empty<K, V>(map: Map<K, V>): Bool {
    return map.size == 0;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func peek<K, V>(map: Map<K, V>): ?(K, V) {
    let (keys, values, chainTable, hashTable, capacity) = map.data;
    let index = nat((map.backIndex -% 1) % capacity);

    return switch (keys[index]) { case (?key) ?(key, unwrap(values[index])); case (_) null };
  };

  public func peekFront<K, V>(map: Map<K, V>): ?(K, V) {
    let (keys, values, chainTable, hashTable, capacity) = map.data;
    let index = nat((map.frontIndex +% 1) % capacity);

    return switch (keys[index]) { case (?key) ?(key, unwrap(values[index])); case (_) null };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func rehash<K, V>(map: Map<K, V>, (getHash, areEqual): HashUtils<K>) {
    let (keys, values, chainTable, hashTable, capacity) = map.data;

    let newCapacity = 2 **% (32 -% clz((map.size *% 4 / 3) | 1));

    if (newCapacity >= MAX_CAPACITY) trap("Map capacity limit reached (2 ** 30)");

    let newKeys = initArray<?K>(nat(newCapacity), null);
    let newValues = initArray<?V>(nat(newCapacity), null);
    let newChainTable = initArray<Nat>(nat(newCapacity), NULL);
    let newHashTable = initArray<Nat>(nat(newCapacity), NULL);

    let lastIndex = (map.backIndex -% 1) % capacity;
    var index = map.frontIndex;
    var newIndex = 0:Nat32;

    if (map.size != 0) loop {
      index := (index +% 1) % capacity;

      switch (keys[nat(index)]) {
        case (?someKey) {
          let hashIndex = nat(getHash(someKey) % newCapacity);

          newKeys[nat(newIndex)] := ?someKey;
          newValues[nat(newIndex)] := values[nat(index)];
          newChainTable[nat(newIndex)] := newHashTable[hashIndex];
          newHashTable[hashIndex] := nat(newIndex);

          newIndex +%= 1;
        };

        case (_) {};
      };
    } while (index != lastIndex);

    map.data := (newKeys, newValues, newChainTable, newHashTable, newCapacity);
    map.backIndex := newIndex;
    map.frontIndex := newCapacity -% 1;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func getHelper<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): ?V {
    let (getHash, areEqual) = hashUtils;
    let (keys, values, chainTable, hashTable, capacity) = map.data;

    var index = hashTable[nat(getHash(keyParam) % capacity)];

    loop if (index == NULL) {
      return null;
    } else if (areEqual(unwrap(keys[index]), keyParam)) {
      return values[index];
    } else {
      index := chainTable[index];
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
    hashUtils: HashUtils<K>,
    keyParam: K,
    getNewValue: (K, ?V) -> ?V,
    method: Nat32,
  ): ?V {
    let (getHash, areEqual) = hashUtils;
    let (keys, values, chainTable, hashTable, capacity) = map.data;

    let hashIndex = nat(getHash(keyParam) % capacity);
    var index = hashTable[hashIndex];
    var prevIndex = NULL;

    loop if (index == NULL) {
      if (is(method, REPLACE)) return null;

      switch (getNewValue(keyParam, null)) {
        case (null) return null;

        case (newValue) {
          let nextIndex = if (is(method, FRONT)) map.frontIndex else map.backIndex;
          let lastIndex = if (is(method, FRONT)) map.backIndex else map.frontIndex;

          if (is(method, FRONT)) {
            map.backIndex := (nextIndex +% 1) % capacity;
          } else {
            map.frontIndex := (nextIndex -% 1) % capacity;
          };

          if (prevIndex == NULL) hashTable[hashIndex] := nat(nextIndex) else chainTable[prevIndex] := nat(nextIndex);

          map.size +%= 1;
          keys[nat(nextIndex)] := ?keyParam;
          values[nat(nextIndex)] := newValue;

          if (nextIndex == lastIndex) rehash(map, hashUtils);

          return if (is(method, UPDATE)) newValue else null;
        };
      };
    } else if (areEqual(unwrap(keys[index]), keyParam)) {
      let prevValue = values[index];

      if (is(method, ADD)) return prevValue;

      if (is(method, MOVE)) {
        let newValue = switch (getNewValue(keyParam, prevValue)) { case (null) prevValue; case (newValue) newValue };

        let nextIndex = if (is(method, FRONT)) map.frontIndex else map.backIndex;
        let lastIndex = if (is(method, FRONT)) map.backIndex else map.frontIndex;

        if (is(method, FRONT)) {
          map.backIndex := (nextIndex +% 1) % capacity;
        } else {
          map.frontIndex := (nextIndex -% 1) % capacity;
        };

        if (prevIndex == NULL) hashTable[hashIndex] := nat(nextIndex) else chainTable[prevIndex] := nat(nextIndex);

        keys[nat(nextIndex)] := ?keyParam;
        values[nat(nextIndex)] := newValue;
        chainTable[nat(nextIndex)] := chainTable[index];
        keys[index] := null;
        values[index] := null;
        chainTable[index] := NULL;

        if (nextIndex == lastIndex) rehash(map, hashUtils);

        return if (is(method, UPDATE)) newValue else prevValue;
      } else {
        switch (getNewValue(keyParam, prevValue)) {
          case (null) return prevValue;

          case (newValue) {
            values[index] := newValue;

            return if (is(method, UPDATE)) newValue else prevValue;
          };
        };
      };
    } else {
      prevIndex := index;
      index := chainTable[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func put<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V): ?V {
    return putHelper(map, hashUtils, keyParam, constant(?valueParam), PUT);
  };

  public func putFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V): ?V {
    return putHelper(map, hashUtils, keyParam, constant(?valueParam), PUT | FRONT);
  };

  public func set<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V) {
    ignore putHelper(map, hashUtils, keyParam, constant(?valueParam), PUT);
  };

  public func setFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V) {
    ignore putHelper(map, hashUtils, keyParam, constant(?valueParam), PUT | FRONT);
  };

  public func add<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V): ?V {
    return putHelper(map, hashUtils, keyParam, constant(?valueParam), ADD);
  };

  public func addFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V): ?V {
    return putHelper(map, hashUtils, keyParam, constant(?valueParam), ADD | FRONT);
  };

  public func replace<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V): ?V {
    return putHelper(map, hashUtils, keyParam, constant(?valueParam), REPLACE);
  };

  public func update<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, getNewValue: (K, ?V) -> ?V): ?V {
    return putHelper(map, hashUtils, keyParam, getNewValue, UPDATE);
  };

  public func updateFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, getNewValue: (K, ?V) -> ?V): ?V {
    return putHelper(map, hashUtils, keyParam, getNewValue, UPDATE | FRONT);
  };

  public func putMove<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: ?V): ?V {
    return putHelper(map, hashUtils, keyParam, constant(valueParam), PUT | MOVE);
  };

  public func putMoveFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: ?V): ?V {
    return putHelper(map, hashUtils, keyParam, constant(valueParam), PUT | MOVE | FRONT);
  };

  public func replaceMove<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: ?V): ?V {
    return putHelper(map, hashUtils, keyParam, constant(valueParam), REPLACE | MOVE);
  };

  public func replaceMoveFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: ?V): ?V {
    return putHelper(map, hashUtils, keyParam, constant(valueParam), REPLACE | MOVE | FRONT);
  };

  public func updateMove<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, getNewValue: (K, ?V) -> ?V): ?V {
    return putHelper(map, hashUtils, keyParam, getNewValue, UPDATE | MOVE);
  };

  public func updateMoveFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, getNewValue: (K, ?V) -> ?V): ?V {
    return putHelper(map, hashUtils, keyParam, getNewValue, UPDATE | MOVE | FRONT);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func removeHelper<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: ?K, method: Nat32): ?(K, V) {
    let (getHash, areEqual) = hashUtils;
    let (keys, values, chainTable, hashTable, capacity) = map.data;

    let targetKey = if (is(method, REMOVE)) {
      unwrap(keyParam)
    } else if (is(method, FRONT)) {
      switch (keys[nat((map.frontIndex +% 1) % capacity)]) { case (?key) key; case (_) return null };
    } else {
      switch (keys[nat((map.backIndex -% 1) % capacity)]) { case (?key) key; case (_) return null };
    };

    let hashIndex = nat(getHash(targetKey) % capacity);
    var index = hashTable[hashIndex];
    var prevIndex = NULL;

    loop if (index == NULL) {
      return null;
    } else if (areEqual(unwrap(keys[index]), targetKey)) {
      if (is(method, CYCLE)) {
        let value = values[index];
        let nextIndex = if (is(method, FRONT)) map.frontIndex else map.backIndex;
        let lastIndex = if (is(method, FRONT)) map.backIndex else map.frontIndex;

        if (is(method, FRONT)) {
          map.backIndex := (nextIndex +% 1) % capacity;
        } else {
          map.frontIndex := (nextIndex -% 1) % capacity;
        };

        if (prevIndex == NULL) hashTable[hashIndex] := nat(nextIndex) else chainTable[prevIndex] := nat(nextIndex);

        keys[nat(nextIndex)] := ?targetKey;
        values[nat(nextIndex)] := value;
        chainTable[nat(nextIndex)] := chainTable[index];
        keys[index] := null;
        values[index] := null;
        chainTable[index] := NULL;

        if (nextIndex == lastIndex) rehash(map, hashUtils);

        return ?(targetKey, unwrap(value));
      } else {
        let prevValue = values[index];
        let newSize = map.size -% 1;

        map.size := newSize;

        if (prevIndex == NULL) hashTable[hashIndex] := chainTable[index] else chainTable[prevIndex] := chainTable[index];

        keys[index] := null;
        values[index] := null;
        chainTable[index] := NULL;

        if (newSize < capacity *% 3 / 8) rehash(map, hashUtils);

        return ?(targetKey, unwrap(prevValue));
      };
    } else {
      prevIndex := index;
      index := chainTable[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func remove<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): ?V {
    return entryValue(removeHelper(map, hashUtils, ?keyParam, REMOVE));
  };

  public func delete<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K) {
    ignore removeHelper(map, hashUtils, ?keyParam, REMOVE);
  };

  public func pop<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>): ?(K, V) {
    return removeHelper(map, hashUtils, null, POP);
  };

  public func popFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>): ?(K, V) {
    return removeHelper(map, hashUtils, null, POP | FRONT);
  };

  public func cycle<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>): ?(K, V) {
    return removeHelper(map, hashUtils, null, CYCLE);
  };

  public func cycleFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>): ?(K, V) {
    return removeHelper(map, hashUtils, null, CYCLE | FRONT);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func mapFilterHelper<K, V1, V2>(map: Map<K, V1>, hashUtils: HashUtils<K>, mapEntry: (K, V1) -> ?V2, isDesc: Bool): Map<K, V2> {
    let (keys, values, chainTable, hashTable, capacity) = map.data;
    let lastHashIndex = capacity -% 1;

    let newMap = new<K, V2>();

    let lastIndex = if (isDesc) map.frontIndex else map.backIndex;
    var index = if (isDesc) (map.backIndex -% 1) % capacity else (map.frontIndex +% 1) % capacity;

    while (index != lastIndex) {
      switch (keys[nat(index)]) {
        case (?someKey) switch (mapEntry(someKey, unwrap(values[nat(index)]))) {
          case (null) {};
          case (newValue) ignore putHelper(newMap, hashUtils, someKey, constant(newValue), PUT | MOVE);
        };

        case (_) {};
      };

      index := if (isDesc) (index -% 1) % capacity else (index +% 1) % capacity;
    };

    return newMap;

    /*
    let (keys, values, chainTable, hashTable, capacity) = map.data;
    let lastHashIndex = capacity -% 1;

    let newKeys = initArray<?K>(2, null);
    let newValues = initArray<?V2>(2, null);

    let lastIndex = if (isDesc) map.frontIndex else map.backIndex;
    var index = if (isDesc) (map.backIndex -% 1) % capacity else (map.frontIndex +% 1) % capacity;

    while (index != lastIndex) {
      switch (keys[nat(index)]) {
        case (?someKey) {
          let newIndex = if (isDesc) nat(lastHashIndex -% index) else nat(index);

          newKeys[newIndex] := ?someKey;
          newValues[newIndex] := ?mapEntry(someKey, unwrap(values[nat(index)]));

          newChainTable[newIndex] := if (isDesc) nat(lastHashIndex -% nat32(chainTable[nat(index)])) else chainTable[nat(index)];
        };

        case (_) {};
      };

      index := if (isDesc) (index -% 1) % capacity else (index +% 1) % capacity;
    };

    return {
      var data = (newKeys, newValues, newChainTable, newHashTable, capacity);
      var frontIndex = if (isDesc) lastHashIndex -% map.backIndex else map.frontIndex;
      var backIndex = if (isDesc) lastHashIndex -% map.frontIndex else map.backIndex;
      var size = map.size;
    };
    */
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func mapFilter<K, V1, V2>(map: Map<K, V1>, hashUtils: HashUtils<K>, mapEntry: (K, V1) -> ?V2): Map<K, V2> {
    return mapFilterHelper(map, hashUtils, mapEntry, false);
  };

  public func mapFilterDesc<K, V1, V2>(map: Map<K, V1>, hashUtils: HashUtils<K>, mapEntry: (K, V1) -> ?V2): Map<K, V2> {
    return mapFilterHelper(map, hashUtils, mapEntry, true);
  };

  public func filter<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, acceptEntry: (K, V) -> Bool): Map<K, V> {
    return mapFilterHelper<K, V, V>(map, hashUtils, boolToOption(acceptEntry), false);
  };

  public func filterDesc<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, acceptEntry: (K, V) -> Bool): Map<K, V> {
    return mapFilterHelper<K, V, V>(map, hashUtils, boolToOption(acceptEntry), true);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func mapHelper<K, V1, V2>(map: Map<K, V1>, mapEntry: (K, V1) -> V2, isDesc: Bool): Map<K, V2> {
    let (keys, values, chainTable, hashTable, capacity) = map.data;
    let lastHashIndex = capacity -% 1;

    let newKeys = initArray<?K>(nat(capacity), null);
    let newValues = initArray<?V2>(nat(capacity), null);
    let newChainTable = initArray<Nat>(nat(capacity), NULL);
    let newHashTable = initArray<Nat>(nat(capacity), NULL);

    let lastIndex = if (isDesc) map.frontIndex else map.backIndex;
    var index = if (isDesc) (map.backIndex -% 1) % capacity else (map.frontIndex +% 1) % capacity;

    for (index in hashTable.keys()) {
      let hashIndex = hashTable[index];

      if (hashIndex != NULL) {
        newHashTable[index] := if (isDesc) hashIndex else nat(lastHashIndex -% nat32(hashIndex));
      };
    };

    while (index != lastIndex) {
      switch (keys[nat(index)]) {
        case (?someKey) {
          let newIndex = if (isDesc) nat(lastHashIndex -% index) else nat(index);

          newKeys[newIndex] := ?someKey;
          newValues[newIndex] := ?mapEntry(someKey, unwrap(values[nat(index)]));

          newChainTable[newIndex] := if (isDesc) nat(lastHashIndex -% nat32(chainTable[nat(index)])) else chainTable[nat(index)];
        };

        case (_) {};
      };

      index := if (isDesc) (index -% 1) % capacity else (index +% 1) % capacity;
    };

    return {
      var data = (newKeys, newValues, newChainTable, newHashTable, capacity);
      var frontIndex = if (isDesc) lastHashIndex -% map.backIndex else map.frontIndex;
      var backIndex = if (isDesc) lastHashIndex -% map.frontIndex else map.backIndex;
      var size = map.size;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func map<K, V1, V2>(map: Map<K, V1>, hashUtils: HashUtils<K>, mapEntry: (K, V1) -> V2): Map<K, V2> {
    return mapHelper(map, mapEntry, false);
  };

  public func mapDesc<K, V1, V2>(map: Map<K, V1>, hashUtils: HashUtils<K>, mapEntry: (K, V1) -> V2): Map<K, V2> {
    return mapHelper(map, mapEntry, true);
  };

  public func clone<K, V>(map: Map<K, V>): Map<K, V> {
    return mapHelper<K, V, V>(map, func(key, value) = value, false);
  };

  public func cloneDesc<K, V>(map: Map<K, V>): Map<K, V> {
    return mapHelper<K, V, V>(map, func(key, value) = value, true);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func iterateHelper<K, V, T>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: ?K, mapEntry: (K, V) -> ?T, isDesc: Bool): Iter<T> {
    let (getHash, areEqual) = hashUtils;
    let (keys, values, chainTable, hashTable, capacity) = map.data;
    let frontIndex = map.frontIndex;
    let backIndex = map.backIndex;

    var index = switch (keyParam) { case (?someKey) hashTable[nat(getHash(someKey) % capacity)]; case (_) NULL };

    loop if (index == NULL or areEqual(unwrap(keys[index]), unwrap(keyParam))) {
      var started = index != NULL;

      var iterIndex = if (index != NULL) nat32(index) else if (isDesc) backIndex else frontIndex;

      return let iter = {
        prev = func(): ?T {
          started := true;

          loop {
            iterIndex := if (isDesc) {
              if (iterIndex == backIndex) (frontIndex +% 1) % capacity else (iterIndex +% 1) % capacity;
            } else {
              if (iterIndex == frontIndex) (backIndex -% 1) % capacity else (iterIndex -% 1) % capacity;
            };

            switch (keys[nat(iterIndex)]) {
              case (?someKey) return mapEntry(someKey, unwrap(values[nat(iterIndex)]));
              case (_) if (iterIndex == (if (isDesc) backIndex else frontIndex)) return null;
            };
          };
        };

        next = func(): ?T {
          started := true;

          loop {
            iterIndex := if (isDesc) {
              if (iterIndex == frontIndex) (backIndex -% 1) % capacity else (iterIndex -% 1) % capacity;
            } else {
              if (iterIndex == backIndex) (frontIndex +% 1) % capacity else (iterIndex +% 1) % capacity;
            };

            switch (keys[nat(iterIndex)]) {
              case (?someKey) return mapEntry(someKey, unwrap(values[nat(iterIndex)]));
              case (_) if (iterIndex == (if (isDesc) frontIndex else backIndex)) return null;
            };
          };
        };

        peekPrev = func(): ?T {
          var newIndex = iterIndex;

          loop {
            newIndex := if (isDesc) {
              if (newIndex == backIndex) (frontIndex +% 1) % capacity else (newIndex +% 1) % capacity;
            } else {
              if (newIndex == frontIndex) (backIndex -% 1) % capacity else (newIndex -% 1) % capacity;
            };

            switch (keys[nat(newIndex)]) {
              case (?someKey) return mapEntry(someKey, unwrap(values[nat(newIndex)]));
              case (_) if (newIndex == (if (isDesc) backIndex else frontIndex)) return null;
            };
          };
        };

        peekNext = func(): ?T {
          var newIndex = iterIndex;

          loop {
            newIndex := if (isDesc) {
              if (newIndex == frontIndex) (backIndex -% 1) % capacity else (newIndex -% 1) % capacity;
            } else {
              if (newIndex == backIndex) (frontIndex +% 1) % capacity else (newIndex +% 1) % capacity;
            };

            switch (keys[nat(newIndex)]) {
              case (?someKey) return mapEntry(someKey, unwrap(values[nat(newIndex)]));
              case (_) if (newIndex == (if (isDesc) frontIndex else backIndex)) return null;
            };
          };
        };

        current = func(): ?T {
          return switch (keys[nat(iterIndex)]) {
            case (?someKey) mapEntry(someKey, unwrap(values[nat(iterIndex)]));
            case (_) null;
          };
        };

        started = func(): Bool {
          return started;
        };

        finished = func(): Bool {
          return started and (iterIndex == frontIndex or iterIndex == backIndex);
        };

        reset = func(): Iter<T> {
          started := false;

          iterIndex := if (isDesc) backIndex else frontIndex;

          return iter;
        };

        movePrev = func(): Iter<T> {
          started := true;

          loop {
            iterIndex := if (isDesc) {
              if (iterIndex == backIndex) (frontIndex +% 1) % capacity else (iterIndex +% 1) % capacity;
            } else {
              if (iterIndex == frontIndex) (backIndex -% 1) % capacity else (iterIndex -% 1) % capacity;
            };

            switch (keys[nat(iterIndex)]) {
              case (?someKey) return iter;
              case (_) if (iterIndex == (if (isDesc) backIndex else frontIndex)) return iter;
            };
          };
        };

        moveNext = func(): Iter<T> {
          started := true;

          loop {
            iterIndex := if (isDesc) {
              if (iterIndex == frontIndex) (backIndex -% 1) % capacity else (iterIndex -% 1) % capacity;
            } else {
              if (iterIndex == backIndex) (frontIndex +% 1) % capacity else (iterIndex +% 1) % capacity;
            };

            switch (keys[nat(iterIndex)]) {
              case (?someKey) return iter;
              case (_) if (iterIndex == (if (isDesc) frontIndex else backIndex)) return iter;
            };
          };
        };
      };
    } else {
      index := chainTable[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func keys<K, V>(map: Map<K, V>): Iter<K> {
    return iterateHelper<K, V, K>(map, nullHash, null, func(key, value) = ?key, false);
  };

  public func keysDesc<K, V>(map: Map<K, V>): Iter<K> {
    return iterateHelper<K, V, K>(map, nullHash, null, func(key, value) = ?key, true);
  };

  public func vals<K, V>(map: Map<K, V>): Iter<V> {
    return iterateHelper<K, V, V>(map, nullHash, null, func(key, value) = ?value, false);
  };

  public func valsDesc<K, V>(map: Map<K, V>): Iter<V> {
    return iterateHelper<K, V, V>(map, nullHash, null, func(key, value) = ?value, true);
  };

  public func entries<K, V>(map: Map<K, V>): Iter<(K, V)> {
    return iterateHelper<K, V, (K, V)>(map, nullHash, null, func(key, value) = ?(key, value), false);
  };

  public func entriesDesc<K, V>(map: Map<K, V>): Iter<(K, V)> {
    return iterateHelper<K, V, (K, V)>(map, nullHash, null, func(key, value) = ?(key, value), true);
  };

  public func keysFrom<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, placeParam: ?K): Iter<K> {
    return iterateHelper<K, V, K>(map, hashUtils, placeParam, func(key, value) = ?key, false);
  };

  public func keysFromDesc<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, placeParam: ?K): Iter<K> {
    return iterateHelper<K, V, K>(map, hashUtils, placeParam, func(key, value) = ?key, true);
  };

  public func valsFrom<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, placeParam: ?K): Iter<V> {
    return iterateHelper<K, V, V>(map, hashUtils, placeParam, func(key, value) = ?value, false);
  };

  public func valsFromDesc<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, placeParam: ?K): Iter<V> {
    return iterateHelper<K, V, V>(map, hashUtils, placeParam, func(key, value) = ?value, true);
  };

  public func entriesFrom<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, placeParam: ?K): Iter<(K, V)> {
    return iterateHelper<K, V, (K, V)>(map, hashUtils, placeParam, func(key, value) = ?(key, value), false);
  };

  public func entriesFromDesc<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, placeParam: ?K): Iter<(K, V)> {
    return iterateHelper<K, V, (K, V)>(map, hashUtils, placeParam, func(key, value) = ?(key, value), true);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func findHelper<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool, isDesc: Bool): ?(K, V) {
    let (keys, values, chainTable, hashTable, capacity) = map.data;

    let lastIndex = if (isDesc) map.frontIndex else map.backIndex;
    var index = if (isDesc) (map.backIndex -% 1) % capacity else (map.frontIndex +% 1) % capacity;

    while (index != lastIndex) {
      switch (keys[nat(index)]) {
        case (?someKey) {
          let value = unwrap(values[nat(index)]);

          if (acceptEntry(someKey, value)) return ?(someKey, value);
        };

        case (_) {};
      };

      index := if (isDesc) (index -% 1) % capacity else (index +% 1) % capacity;
    };

    return null;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func find<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool): ?(K, V) {
    return findHelper(map, acceptEntry, false);
  };

  public func findDesc<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool): ?(K, V) {
    return findHelper(map, acceptEntry, true);
  };

  public func some<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool): Bool {
    return isSome(findHelper(map, acceptEntry, false));
  };

  public func someDesc<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool): Bool {
    return isSome(findHelper(map, acceptEntry, true));
  };

  public func every<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool): Bool {
    return not isSome(findHelper(map, negate(acceptEntry), false));
  };

  public func everyDesc<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool): Bool {
    return not isSome(findHelper(map, negate(acceptEntry), true));
  };

  public func forEach<K, V>(map: Map<K, V>, mapEntry: (K, V) -> ()) {
    ignore findHelper(map, reject(mapEntry), false);
  };

  public func forEachDesc<K, V>(map: Map<K, V>, mapEntry: (K, V) -> ()) {
    ignore findHelper(map, reject(mapEntry), true);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func fromIterHelper<K, V, T>(iter: IterNext<T>, hashUtils: HashUtils<K>, mapItem: (T) -> ?(K, V), method: Nat32): Map<K, V> {
    let map = new<K, V>();

    for (item in iter) switch (mapItem(item)) {
      case (?(key, value)) ignore putHelper(map, hashUtils, key, constant(?value), method);
      case (_) {};
    };

    return map;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func fromIter<K, V>(iter: IterNext<(K, V)>, hashUtils: HashUtils<K>): Map<K, V> {
    return fromIterHelper<K, V, (K, V)>(iter, hashUtils, func(item) = ?item, PUT | MOVE);
  };

  public func fromIterDesc<K, V>(iter: IterNext<(K, V)>, hashUtils: HashUtils<K>): Map<K, V> {
    return fromIterHelper<K, V, (K, V)>(iter, hashUtils, func(item) = ?item, PUT | MOVE | FRONT);
  };

  public func fromIterMap<K, V, T>(iter: IterNext<T>, hashUtils: HashUtils<K>, mapItem: (T) -> ?(K, V)): Map<K, V> {
    return fromIterHelper(iter, hashUtils, mapItem, PUT | MOVE);
  };

  public func fromIterMapDesc<K, V, T>(iter: IterNext<T>, hashUtils: HashUtils<K>, mapItem: (T) -> ?(K, V)): Map<K, V> {
    return fromIterHelper(iter, hashUtils, mapItem, PUT | MOVE | FRONT);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func toArrayHelper<K, V>(map: Map<K, V>, isDesc: Bool): [(K, V)] {
    let (keys, values, chainTable, hashTable, capacity) = map.data;

    var index = if (isDesc) (map.backIndex -% 1) % capacity else (map.frontIndex +% 1) % capacity;

    return tabulateArray<(K, V)>(nat(map.size), func(i) = loop {
      switch (keys[nat(index)]) { case (?key) return (key, unwrap(values[nat(index)])); case (_) {} };

      index := if (isDesc) (index -% 1) % capacity else (index +% 1) % capacity;
    });
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func toArray<K, V>(map: Map<K, V>): [(K, V)] {
    return toArrayHelper(map, false);
  };

  public func toArrayDesc<K, V>(map: Map<K, V>): [(K, V)] {
    return toArrayHelper(map, true);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func toArrayMapHelper<K, V, T>(map: Map<K, V>, mapEntry: (K, V) -> ?T, isDesc: Bool): [T] {
    let (keys, values, chainTable, hashTable, capacity) = map.data;

    var array = [var null, null]:[var ?T];
    var arraySize = 2:Nat32;
    var arrayIndex = 0:Nat32;

    let lastIndex = if (isDesc) map.frontIndex else map.backIndex;
    var index = if (isDesc) (map.backIndex -% 1) % capacity else (map.frontIndex +% 1) % capacity;

    while (index != lastIndex) {
      switch (keys[nat(index)]) {
        case (?someKey) switch (mapEntry(someKey, unwrap(values[nat(index)]))) {
          case (null) {};

          case (item) {
            if (arrayIndex == arraySize) {
              let prevArray = array;

              arraySize *%= 2;
              array := initArray<?T>(nat(arraySize), null);
              arrayIndex := 0;

              for (item in prevArray.vals()) {
                array[nat(arrayIndex)] := item;
                arrayIndex +%= 1;
              };
            };

            array[nat(arrayIndex)] := item;
            arrayIndex +%= 1;
          };
        };

        case (_) {};
      };

      index := if (isDesc) (index -% 1) % capacity else (index +% 1) % capacity;
    };

    return tabulateArray<T>(nat(arrayIndex), func(i) = unwrap(array[i]));
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func toArrayMap<K, V, T>(map: Map<K, V>, mapEntry: (K, V) -> ?T): [T] {
    return toArrayMapHelper(map, mapEntry, false);
  };

  public func toArrayMapDesc<K, V, T>(map: Map<K, V>, mapEntry: (K, V) -> ?T): [T] {
    return toArrayMapHelper(map, mapEntry, true);
  };
};
