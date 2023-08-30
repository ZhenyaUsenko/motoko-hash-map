import Prim "mo:prim";

module {
  public type Map<K, V> = [var ?(
    keys: [var ?K],
    values: [var ?V],
    indexes: [var Nat],
    bounds: [var Nat32],
  )];

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

  let DATA = 0;
  let FRONT = 0;
  let BACK = 1;
  let SIZE = 2;
  let NULL = 0x3fffffff;
  let MAX_CAPACITY = 0x1fffffff:Nat32;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  let { Array_tabulate = tabulateArray; Array_init = initArray; natToNat32 = nat32; nat32ToNat = nat; clzNat32 = clz; trap } = Prim;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func init<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: ?V): Null {
    switch (valueParam) {
      case (null) {};

      case (_) map[DATA] := ?(
        [var ?keyParam, null],
        [var valueParam, null],
        if (hashUtils.0(keyParam) % 2 == 0) [var NULL, NULL, 0, NULL] else [var NULL, NULL, NULL, 0],
        [var 1, 1, 1],
      );
    };

    null;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func initFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: ?V): Null {
    switch (valueParam) {
      case (null) {};

      case (_) map[DATA] := ?(
        [var null, ?keyParam],
        [var null, valueParam],
        if (hashUtils.0(keyParam) % 2 == 0) [var NULL, NULL, 1, NULL] else [var NULL, NULL, NULL, 1],
        [var 0, 0, 1],
      );
    };

    null;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func make<K, V>(hashUtils: HashUtils<K>, keyParam: K, valueParam: V): Map<K, V> {
    [var ?(
      [var ?keyParam, null],
      [var ?valueParam, null],
      if (hashUtils.0(keyParam) % 2 == 0) [var NULL, NULL, 0, NULL] else [var NULL, NULL, NULL, 0],
      [var 1, 1, 1],
    )];
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func new<K, V>(): Map<K, V> {
    [var null];
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func clear<K, V>(map: Map<K, V>) {
    map[DATA] := null;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func size<K, V>(map: Map<K, V>): Nat {
    switch (map[DATA]) { case (?data) nat(data.3[SIZE]); case (_) 0 };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func empty<K, V>(map: Map<K, V>): Bool {
    switch (map[DATA]) { case (null) true; case (_) false };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func peek<K, V>(map: Map<K, V>): ?(K, V) {
    let data = switch (map[DATA]) { case (?data) data; case (_) return null };

    let index = nat((data.3[BACK] -% 1) % nat32(data.0.size()));

    switch (data.0[index]) {
      case (?key) ?(key, switch (data.1[index]) { case (?value) value; case (_) trap("unreachable") });
      case (_) null;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func peekFront<K, V>(map: Map<K, V>): ?(K, V) {
    let data = switch (map[DATA]) { case (?data) data; case (_) return null };

    let index = nat((data.3[FRONT] +% 1) % nat32(data.0.size()));

    switch (data.0[index]) {
      case (?key) ?(key, switch (data.1[index]) { case (?value) value; case (_) trap("unreachable") });
      case (_) null;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func rehash<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>) {
    let data = switch (map[DATA]) { case (?data) data; case (_) return };

    let bounds = data.3;
    let size = bounds[SIZE];

    if (size == 0) return map[DATA] := null;

    let newCapacity = 2 **% (32 -% clz((size *% 4 / 3) | 1));
    let newCapacityNat = nat(newCapacity);

    if (newCapacity >= MAX_CAPACITY) trap("Map capacity limit reached (2 ** 29)");

    let newKeys = initArray<?K>(newCapacityNat, null);
    let newValues = initArray<?V>(newCapacityNat, null);
    let newIndexes = initArray<Nat>(nat(newCapacity *% 2), NULL);
    var newIndex = 0:Nat32;

    let getHash = hashUtils.0;
    let values = data.1;
    let keys = data.0;
    let capacity = nat32(keys.size());

    let lastIndex = (bounds[BACK] -% 1) % capacity;
    var index = bounds[FRONT];

    loop {
      index := (index +% 1) % capacity;

      let indexNat = nat(index);
      let key = keys[indexNat];

      switch (key) {
        case (?someKey) {
          let newIndexNat = nat(newIndex);
          let hashIndex = nat(getHash(someKey) % newCapacity +% newCapacity);

          newKeys[newIndexNat] := key;
          newValues[newIndexNat] := values[indexNat];
          newIndexes[newIndexNat] := newIndexes[hashIndex];
          newIndexes[hashIndex] := newIndexNat;

          newIndex +%= 1;
        };

        case (_) {};
      };
    } while (index != lastIndex);

    map[DATA] := ?(newKeys, newValues, newIndexes, bounds);
    bounds[BACK] := newIndex;
    bounds[FRONT] := newCapacity -% 1;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func get<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): ?V {
    let data = switch (map[DATA]) { case (?data) data; case (_) return null };

    let keys = data.0;
    let capacity = nat32(keys.size());
    var index = data.2[nat(hashUtils.0(keyParam) % capacity +% capacity)];

    loop if (index == NULL) {
      return null;
    } else if (hashUtils.1(switch (keys[index]) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
      return data.1[index];
    } else {
      index := data.2[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func has<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    let data = switch (map[DATA]) { case (?data) data; case (_) return false };

    let keys = data.0;
    let capacity = nat32(keys.size());
    var index = data.2[nat(hashUtils.0(keyParam) % capacity +% capacity)];

    loop if (index == NULL) {
      return false;
    } else if (hashUtils.1(switch (keys[index]) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
      return true;
    } else {
      index := data.2[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func contains<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): ?Bool {
    let data = switch (map[DATA]) { case (?data) data; case (_) return null };

    let keys = data.0;
    let capacity = nat32(keys.size());
    var index = data.2[nat(hashUtils.0(keyParam) % capacity +% capacity)];

    loop if (index == NULL) {
      return ?false;
    } else if (hashUtils.1(switch (keys[index]) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
      return ?true;
    } else {
      index := data.2[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func put<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V): ?V {
    let data = switch (map[DATA]) { case (?data) data; case (_) return init(map, hashUtils, keyParam, ?valueParam) };

    let keys = data.0;
    let capacity = nat32(keys.size());
    let hashIndex = nat(hashUtils.0(keyParam) % capacity +% capacity);

    let indexes = data.2;
    let firstIndex = indexes[hashIndex];
    var index = firstIndex;

    loop if (index == NULL) {
      let bounds = data.3;
      let back = bounds[BACK];
      let backNat = nat(back);

      bounds[BACK] := (back +% 1) % capacity;
      bounds[SIZE] +%= 1;

      keys[backNat] := ?keyParam;
      data.1[backNat] := ?valueParam;
      indexes[backNat] := firstIndex;
      indexes[hashIndex] := backNat;

      if (back == bounds[FRONT]) rehash(map, hashUtils);

      return null;
    } else if (hashUtils.1(switch (keys[index]) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
      let prevValue = data.1[index];

      data.1[index] := ?valueParam;

      return prevValue;
    } else {
      index := indexes[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func putFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V): ?V {
    let data = switch (map[DATA]) { case (?data) data; case (_) return initFront(map, hashUtils, keyParam, ?valueParam) };

    let keys = data.0;
    let capacity = nat32(keys.size());
    let hashIndex = nat(hashUtils.0(keyParam) % capacity +% capacity);

    let indexes = data.2;
    let firstIndex = indexes[hashIndex];
    var index = firstIndex;

    loop if (index == NULL) {
      let bounds = data.3;
      let front = bounds[FRONT];
      let frontNat = nat(front);

      bounds[FRONT] := (front -% 1) % capacity;
      bounds[SIZE] +%= 1;

      keys[frontNat] := ?keyParam;
      data.1[frontNat] := ?valueParam;
      indexes[frontNat] := firstIndex;
      indexes[hashIndex] := frontNat;

      if (front == bounds[BACK]) rehash(map, hashUtils);

      return null;
    } else if (hashUtils.1(switch (keys[index]) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
      let prevValue = data.1[index];

      data.1[index] := ?valueParam;

      return prevValue;
    } else {
      index := indexes[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func set<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V) {
    let data = switch (map[DATA]) { case (?data) data; case (_) return ignore init(map, hashUtils, keyParam, ?valueParam) };

    let keys = data.0;
    let capacity = nat32(keys.size());
    let hashIndex = nat(hashUtils.0(keyParam) % capacity +% capacity);

    let indexes = data.2;
    let firstIndex = indexes[hashIndex];
    var index = firstIndex;

    loop if (index == NULL) {
      let bounds = data.3;
      let back = bounds[BACK];
      let backNat = nat(back);

      bounds[BACK] := (back +% 1) % capacity;
      bounds[SIZE] +%= 1;

      keys[backNat] := ?keyParam;
      data.1[backNat] := ?valueParam;
      indexes[backNat] := firstIndex;
      indexes[hashIndex] := backNat;

      if (back == bounds[FRONT]) rehash(map, hashUtils);

      return;
    } else if (hashUtils.1(switch (keys[index]) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
      data.1[index] := ?valueParam;

      return;
    } else {
      index := indexes[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func setFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V) {
    let data = switch (map[DATA]) { case (?data) data; case (_) return ignore initFront(map, hashUtils, keyParam, ?valueParam) };

    let keys = data.0;
    let capacity = nat32(keys.size());
    let hashIndex = nat(hashUtils.0(keyParam) % capacity +% capacity);

    let indexes = data.2;
    let firstIndex = indexes[hashIndex];
    var index = firstIndex;

    loop if (index == NULL) {
      let bounds = data.3;
      let front = bounds[FRONT];
      let frontNat = nat(front);

      bounds[FRONT] := (front -% 1) % capacity;
      bounds[SIZE] +%= 1;

      keys[frontNat] := ?keyParam;
      data.1[frontNat] := ?valueParam;
      indexes[frontNat] := firstIndex;
      indexes[hashIndex] := frontNat;

      if (front == bounds[BACK]) rehash(map, hashUtils);

      return;
    } else if (hashUtils.1(switch (keys[index]) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
      data.1[index] := ?valueParam;

      return;
    } else {
      index := indexes[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func add<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V): ?V {
    let data = switch (map[DATA]) { case (?data) data; case (_) return init(map, hashUtils, keyParam, ?valueParam) };

    let keys = data.0;
    let capacity = nat32(keys.size());
    let hashIndex = nat(hashUtils.0(keyParam) % capacity +% capacity);

    let indexes = data.2;
    let firstIndex = indexes[hashIndex];
    var index = firstIndex;

    loop if (index == NULL) {
      let bounds = data.3;
      let back = bounds[BACK];
      let backNat = nat(back);

      bounds[BACK] := (back +% 1) % capacity;
      bounds[SIZE] +%= 1;

      keys[backNat] := ?keyParam;
      data.1[backNat] := ?valueParam;
      indexes[backNat] := firstIndex;
      indexes[hashIndex] := backNat;

      if (back == bounds[FRONT]) rehash(map, hashUtils);

      return null;
    } else if (hashUtils.1(switch (keys[index]) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
      return data.1[index];
    } else {
      index := indexes[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func addFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V): ?V {
    let data = switch (map[DATA]) { case (?data) data; case (_) return initFront(map, hashUtils, keyParam, ?valueParam) };

    let keys = data.0;
    let capacity = nat32(keys.size());
    let hashIndex = nat(hashUtils.0(keyParam) % capacity +% capacity);

    let indexes = data.2;
    let firstIndex = indexes[hashIndex];
    var index = firstIndex;

    loop if (index == NULL) {
      let bounds = data.3;
      let front = bounds[FRONT];
      let frontNat = nat(front);

      bounds[FRONT] := (front -% 1) % capacity;
      bounds[SIZE] +%= 1;

      keys[frontNat] := ?keyParam;
      data.1[frontNat] := ?valueParam;
      indexes[frontNat] := firstIndex;
      indexes[hashIndex] := frontNat;

      if (front == bounds[BACK]) rehash(map, hashUtils);

      return null;
    } else if (hashUtils.1(switch (keys[index]) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
      return data.1[index];
    } else {
      index := indexes[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func replace<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V): ?V {
    let data = switch (map[DATA]) { case (?data) data; case (_) return null };

    let keys = data.0;
    let capacity = nat32(keys.size());
    let hashIndex = nat(hashUtils.0(keyParam) % capacity +% capacity);

    let indexes = data.2;
    let firstIndex = indexes[hashIndex];
    var index = firstIndex;

    loop if (index == NULL) {
      return null;
    } else if (hashUtils.1(switch (keys[index]) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
      let prevValue = data.1[index];

      data.1[index] := ?valueParam;

      return prevValue;
    } else {
      index := indexes[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func update<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, getNewValue: (K, ?V) -> ?V): ?V {
    let data = switch (map[DATA]) { case (?data) data; case (_) return init(map, hashUtils, keyParam, getNewValue(keyParam, null)) };

    let keys = data.0;
    let capacity = nat32(keys.size());
    let hashIndex = nat(hashUtils.0(keyParam) % capacity +% capacity);

    let indexes = data.2;
    let firstIndex = indexes[hashIndex];
    var index = firstIndex;

    loop if (index == NULL) {
      switch (getNewValue(keyParam, null)) {
        case (null) return null;

        case (valueParam) {
          let bounds = data.3;
          let back = bounds[BACK];
          let backNat = nat(back);

          bounds[BACK] := (back +% 1) % capacity;
          bounds[SIZE] +%= 1;

          keys[backNat] := ?keyParam;
          data.1[backNat] := valueParam;
          indexes[backNat] := firstIndex;
          indexes[hashIndex] := backNat;

          if (back == bounds[FRONT]) rehash(map, hashUtils);

          return valueParam;
        };
      };
    } else if (hashUtils.1(switch (keys[index]) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
      let value = data.1[index];

      switch (getNewValue(keyParam, value)) {
        case (null) return value;

        case (valueParam) {
          data.1[index] := valueParam;

          return valueParam;
        };
      };
    } else {
      index := indexes[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func updateFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, getNewValue: (K, ?V) -> ?V): ?V {
    let data = switch (map[DATA]) { case (?data) data; case (_) return initFront(map, hashUtils, keyParam, getNewValue(keyParam, null)) };

    let keys = data.0;
    let capacity = nat32(keys.size());
    let hashIndex = nat(hashUtils.0(keyParam) % capacity +% capacity);

    let indexes = data.2;
    let firstIndex = indexes[hashIndex];
    var index = firstIndex;

    loop if (index == NULL) {
      switch (getNewValue(keyParam, null)) {
        case (null) return null;

        case (valueParam) {
          let bounds = data.3;
          let front = bounds[FRONT];
          let frontNat = nat(front);

          bounds[FRONT] := (front -% 1) % capacity;
          bounds[SIZE] +%= 1;

          keys[frontNat] := ?keyParam;
          data.1[frontNat] := valueParam;
          indexes[frontNat] := firstIndex;
          indexes[hashIndex] := frontNat;

          if (front == bounds[BACK]) rehash(map, hashUtils);

          return valueParam;
        };
      };
    } else if (hashUtils.1(switch (keys[index]) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
      let value = data.1[index];

      switch (getNewValue(keyParam, value)) {
        case (null) return value;

        case (valueParam) {
          data.1[index] := valueParam;

          return valueParam;
        };
      };
    } else {
      index := indexes[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func putMove<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: ?V): ?V {
    let data = switch (map[DATA]) { case (?data) data; case (_) return init(map, hashUtils, keyParam, valueParam) };

    let keys = data.0;
    let capacity = nat32(keys.size());
    let hashIndex = nat(hashUtils.0(keyParam) % capacity +% capacity);

    let indexes = data.2;
    let firstIndex = indexes[hashIndex];
    var index = firstIndex;
    var prevIndex = NULL;

    loop if (index == NULL) {
      switch (valueParam) {
        case (null) return null;

        case (_) {
          let bounds = data.3;
          let back = bounds[BACK];
          let backNat = nat(back);

          bounds[BACK] := (back +% 1) % capacity;
          bounds[SIZE] +%= 1;

          keys[backNat] := ?keyParam;
          data.1[backNat] := valueParam;
          indexes[backNat] := firstIndex;
          indexes[hashIndex] := backNat;

          if (back == bounds[FRONT]) rehash(map, hashUtils);

          return null;
        };
      };
    } else {
      let key = keys[index];

      if (hashUtils.1(switch (key) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
        let values = data.1;
        let value = values[index];

        let bounds = data.3;
        let back = bounds[BACK];
        let index32 = nat32(index);

        if (index32 == (back -% 1) % capacity) {
          switch (valueParam) { case (null) {}; case (_) values[index] := valueParam };
        } else {
          let backNat = nat(back);

          bounds[BACK] := (back +% 1) % capacity;

          keys[backNat] := key;
          values[backNat] := switch (valueParam) { case (null) value; case (_) valueParam };
          indexes[backNat] := indexes[index];
          keys[index] := null;
          values[index] := null;

          indexes[if (prevIndex == NULL) hashIndex else prevIndex] := backNat;

          var front = bounds[FRONT];
          let prevFront = (front +% 1) % capacity;

          if (index32 == prevFront) {
            front := prevFront;

            while (switch (keys[nat((front +% 1) % capacity)]) { case (null) true; case (_) false }) {
              front := (front +% 1) % capacity;
            };

            bounds[FRONT] := front;
          } else if (back == front) {
            rehash(map, hashUtils);
          };
        };

        return value;
      } else {
        prevIndex := index;
        index := indexes[index];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func putMoveFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: ?V): ?V {
    let data = switch (map[DATA]) { case (?data) data; case (_) return initFront(map, hashUtils, keyParam, valueParam) };

    let keys = data.0;
    let capacity = nat32(keys.size());
    let hashIndex = nat(hashUtils.0(keyParam) % capacity +% capacity);

    let indexes = data.2;
    let firstIndex = indexes[hashIndex];
    var index = firstIndex;
    var prevIndex = NULL;

    loop if (index == NULL) {
      switch (valueParam) {
        case (null) return null;

        case (_) {
          let bounds = data.3;
          let front = bounds[FRONT];
          let frontNat = nat(front);

          bounds[FRONT] := (front -% 1) % capacity;
          bounds[SIZE] +%= 1;

          keys[frontNat] := ?keyParam;
          data.1[frontNat] := valueParam;
          indexes[frontNat] := firstIndex;
          indexes[hashIndex] := frontNat;

          if (front == bounds[BACK]) rehash(map, hashUtils);

          return null;
        };
      };
    } else {
      let key = keys[index];

      if (hashUtils.1(switch (key) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
        let values = data.1;
        let value = values[index];

        let bounds = data.3;
        let front = bounds[FRONT];
        let index32 = nat32(index);

        if (index32 == (front +% 1) % capacity) {
          switch (valueParam) { case (null) {}; case (_) values[index] := valueParam };
        } else {
          let frontNat = nat(front);

          bounds[FRONT] := (front -% 1) % capacity;

          keys[frontNat] := key;
          values[frontNat] := switch (valueParam) { case (null) value; case (_) valueParam };
          indexes[frontNat] := indexes[index];
          keys[index] := null;
          values[index] := null;

          indexes[if (prevIndex == NULL) hashIndex else prevIndex] := frontNat;

          var back = bounds[BACK];
          let prevBack = (back -% 1) % capacity;

          if (index32 == prevBack) {
            back := prevBack;

            while (switch (keys[nat((back -% 1) % capacity)]) { case (null) true; case (_) false }) {
              back := (back -% 1) % capacity;
            };

            bounds[BACK] := back;
          } else if (front == back) {
            rehash(map, hashUtils);
          };
        };

        return value;
      } else {
        prevIndex := index;
        index := indexes[index];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func replaceMove<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: ?V): ?V {
    let data = switch (map[DATA]) { case (?data) data; case (_) return null };

    let keys = data.0;
    let capacity = nat32(keys.size());
    let hashIndex = nat(hashUtils.0(keyParam) % capacity +% capacity);

    let indexes = data.2;
    let firstIndex = indexes[hashIndex];
    var index = firstIndex;
    var prevIndex = NULL;

    loop if (index == NULL) {
      return null;
    } else {
      let key = keys[index];

      if (hashUtils.1(switch (key) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
        let values = data.1;
        let value = values[index];

        let bounds = data.3;
        let back = bounds[BACK];
        let index32 = nat32(index);

        if (index32 == (back -% 1) % capacity) {
          switch (valueParam) { case (null) {}; case (_) values[index] := valueParam };
        } else {
          let backNat = nat(back);

          bounds[BACK] := (back +% 1) % capacity;

          keys[backNat] := key;
          values[backNat] := switch (valueParam) { case (null) value; case (_) valueParam };
          indexes[backNat] := indexes[index];
          keys[index] := null;
          values[index] := null;

          indexes[if (prevIndex == NULL) hashIndex else prevIndex] := backNat;

          var front = bounds[FRONT];
          let prevFront = (front +% 1) % capacity;

          if (index32 == prevFront) {
            front := prevFront;

            while (switch (keys[nat((front +% 1) % capacity)]) { case (null) true; case (_) false }) {
              front := (front +% 1) % capacity;
            };

            bounds[FRONT] := front;
          } else if (back == front) {
            rehash(map, hashUtils);
          };
        };

        return value;
      } else {
        prevIndex := index;
        index := indexes[index];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func replaceMoveFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: ?V): ?V {
    let data = switch (map[DATA]) { case (?data) data; case (_) return null };

    let keys = data.0;
    let capacity = nat32(keys.size());
    let hashIndex = nat(hashUtils.0(keyParam) % capacity +% capacity);

    let indexes = data.2;
    let firstIndex = indexes[hashIndex];
    var index = firstIndex;
    var prevIndex = NULL;

    loop if (index == NULL) {
      return null;
    } else {
      let key = keys[index];

      if (hashUtils.1(switch (key) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
        let values = data.1;
        let value = values[index];

        let bounds = data.3;
        let front = bounds[FRONT];
        let index32 = nat32(index);

        if (index32 == (front +% 1) % capacity) {
          switch (valueParam) { case (null) {}; case (_) values[index] := valueParam };
        } else {
          let frontNat = nat(front);

          bounds[FRONT] := (front -% 1) % capacity;

          keys[frontNat] := key;
          values[frontNat] := switch (valueParam) { case (null) value; case (_) valueParam };
          indexes[frontNat] := indexes[index];
          keys[index] := null;
          values[index] := null;

          indexes[if (prevIndex == NULL) hashIndex else prevIndex] := frontNat;

          var back = bounds[BACK];
          let prevBack = (back -% 1) % capacity;

          if (index32 == prevBack) {
            back := prevBack;

            while (switch (keys[nat((back -% 1) % capacity)]) { case (null) true; case (_) false }) {
              back := (back -% 1) % capacity;
            };

            bounds[BACK] := back;
          } else if (front == back) {
            rehash(map, hashUtils);
          };
        };

        return value;
      } else {
        prevIndex := index;
        index := indexes[index];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func updateMove<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, getNewValue: (K, ?V) -> ?V): ?V {
    let data = switch (map[DATA]) { case (?data) data; case (_) return init(map, hashUtils, keyParam, getNewValue(keyParam, null)) };

    let keys = data.0;
    let capacity = nat32(keys.size());
    let hashIndex = nat(hashUtils.0(keyParam) % capacity +% capacity);

    let indexes = data.2;
    let firstIndex = indexes[hashIndex];
    var index = firstIndex;
    var prevIndex = NULL;

    loop if (index == NULL) {
      switch (getNewValue(keyParam, null)) {
        case (null) return null;

        case (valueParam) {
          let bounds = data.3;
          let back = bounds[BACK];
          let backNat = nat(back);

          bounds[BACK] := (back +% 1) % capacity;
          bounds[SIZE] +%= 1;

          keys[backNat] := ?keyParam;
          data.1[backNat] := valueParam;
          indexes[backNat] := firstIndex;
          indexes[hashIndex] := backNat;

          if (back == bounds[FRONT]) rehash(map, hashUtils);

          return valueParam;
        };
      };
    } else {
      let key = keys[index];

      if (hashUtils.1(switch (key) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
        let values = data.1;
        let value = values[index];

        let bounds = data.3;
        let back = bounds[BACK];
        let index32 = nat32(index);

        if (index32 == (back -% 1) % capacity) {
          switch (getNewValue(keyParam, value)) {
            case (null) return value;

            case (valueParam) {
              values[index] := valueParam;

              return valueParam;
            };
          };
        } else {
          let backNat = nat(back);

          let valueParam = switch (getNewValue(keyParam, value)) { case (null) value; case (valueParam) valueParam };

          bounds[BACK] := (back +% 1) % capacity;

          keys[backNat] := key;
          values[backNat] := valueParam;
          indexes[backNat] := indexes[index];
          keys[index] := null;
          values[index] := null;

          indexes[if (prevIndex == NULL) hashIndex else prevIndex] := backNat;

          var front = bounds[FRONT];
          let prevFront = (front +% 1) % capacity;

          if (index32 == prevFront) {
            front := prevFront;

            while (switch (keys[nat((front +% 1) % capacity)]) { case (null) true; case (_) false }) {
              front := (front +% 1) % capacity;
            };

            bounds[FRONT] := front;
          } else if (back == front) {
            rehash(map, hashUtils);
          };

          return valueParam;
        };
      } else {
        prevIndex := index;
        index := indexes[index];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func updateMoveFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, getNewValue: (K, ?V) -> ?V): ?V {
    let data = switch (map[DATA]) { case (?data) data; case (_) return initFront(map, hashUtils, keyParam, getNewValue(keyParam, null)) };

    let keys = data.0;
    let capacity = nat32(keys.size());
    let hashIndex = nat(hashUtils.0(keyParam) % capacity +% capacity);

    let indexes = data.2;
    let firstIndex = indexes[hashIndex];
    var index = firstIndex;
    var prevIndex = NULL;

    loop if (index == NULL) {
      switch (getNewValue(keyParam, null)) {
        case (null) return null;

        case (valueParam) {
          let bounds = data.3;
          let front = bounds[FRONT];
          let frontNat = nat(front);

          bounds[FRONT] := (front -% 1) % capacity;
          bounds[SIZE] +%= 1;

          keys[frontNat] := ?keyParam;
          data.1[frontNat] := valueParam;
          indexes[frontNat] := firstIndex;
          indexes[hashIndex] := frontNat;

          if (front == bounds[BACK]) rehash(map, hashUtils);

          return valueParam;
        };
      };
    } else {
      let key = keys[index];

      if (hashUtils.1(switch (key) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
        let values = data.1;
        let value = values[index];

        let bounds = data.3;
        let front = bounds[FRONT];
        let index32 = nat32(index);

        if (index32 == (front +% 1) % capacity) {
          switch (getNewValue(keyParam, value)) {
            case (null) return value;

            case (valueParam) {
              values[index] := valueParam;

              return valueParam;
            };
          };
        } else {
          let frontNat = nat(front);

          let valueParam = switch (getNewValue(keyParam, value)) { case (null) value; case (valueParam) valueParam };

          bounds[FRONT] := (front -% 1) % capacity;

          keys[frontNat] := key;
          values[frontNat] := valueParam;
          indexes[frontNat] := indexes[index];
          keys[index] := null;
          values[index] := null;

          indexes[if (prevIndex == NULL) hashIndex else prevIndex] := frontNat;

          var back = bounds[BACK];
          let prevBack = (back -% 1) % capacity;

          if (index32 == prevBack) {
            back := prevBack;

            while (switch (keys[nat((back -% 1) % capacity)]) { case (null) true; case (_) false }) {
              back := (back -% 1) % capacity;
            };

            bounds[BACK] := back;
          } else if (front == back) {
            rehash(map, hashUtils);
          };

          return valueParam;
        };
      } else {
        prevIndex := index;
        index := indexes[index];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func remove<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K): ?V {
    let data = switch (map[DATA]) { case (?data) data; case (_) return null };

    let keys = data.0;
    let capacity = nat32(keys.size());
    let hashIndex = nat(hashUtils.0(keyParam) % capacity +% capacity);

    let indexes = data.2;
    var index = indexes[hashIndex];
    var prevIndex = NULL;

    loop if (index == NULL) {
      return null;
    } else if (hashUtils.1(switch (keys[index]) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
      let value = data.1[index];
      let bounds = data.3;
      let newSize = bounds[SIZE] -% 1;

      bounds[SIZE] := newSize;

      keys[index] := null;
      data.1[index] := null;

      indexes[if (prevIndex == NULL) hashIndex else prevIndex] := indexes[index];

      if (newSize < capacity *% 3 / 8) {
        rehash(map, hashUtils);
      } else {
        var front = bounds[FRONT];
        let prevFront = (front +% 1) % capacity;
        let index32 = nat32(index);

        if (index32 == prevFront) {
          front := prevFront;

          if (newSize != 0) {
            while (switch (keys[nat((front +% 1) % capacity)]) { case (null) true; case (_) false }) {
              front := (front +% 1) % capacity;
            };
          };

          bounds[FRONT] := front;
        } else {
          var back = bounds[BACK];
          let prevBack = (back -% 1) % capacity;

          if (index32 == prevBack) {
            back := prevBack;

            if (newSize != 0) {
              while (switch (keys[nat((back -% 1) % capacity)]) { case (null) true; case (_) false }) {
                back := (back -% 1) % capacity;
              };
            };

            bounds[BACK] := back;
          };
        };
      };

      return value;
    } else {
      prevIndex := index;
      index := indexes[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func delete<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K) {
    let data = switch (map[DATA]) { case (?data) data; case (_) return };

    let keys = data.0;
    let capacity = nat32(keys.size());
    let hashIndex = nat(hashUtils.0(keyParam) % capacity +% capacity);

    let indexes = data.2;
    var index = indexes[hashIndex];
    var prevIndex = NULL;

    loop if (index == NULL) {
      return;
    } else if (hashUtils.1(switch (keys[index]) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
      let bounds = data.3;
      let newSize = bounds[SIZE] -% 1;

      bounds[SIZE] := newSize;

      keys[index] := null;
      data.1[index] := null;

      indexes[if (prevIndex == NULL) hashIndex else prevIndex] := indexes[index];

      if (newSize < capacity *% 3 / 8) {
        rehash(map, hashUtils);
      } else {
        var front = bounds[FRONT];
        let prevFront = (front +% 1) % capacity;
        let index32 = nat32(index);

        if (index32 == prevFront) {
          front := prevFront;

          if (newSize != 0) {
            while (switch (keys[nat((front +% 1) % capacity)]) { case (null) true; case (_) false }) {
              front := (front +% 1) % capacity;
            };
          };

          bounds[FRONT] := front;
        } else {
          var back = bounds[BACK];
          let prevBack = (back -% 1) % capacity;

          if (index32 == prevBack) {
            back := prevBack;

            if (newSize != 0) {
              while (switch (keys[nat((back -% 1) % capacity)]) { case (null) true; case (_) false }) {
                back := (back -% 1) % capacity;
              };
            };

            bounds[BACK] := back;
          };
        };
      };

      return;
    } else {
      prevIndex := index;
      index := indexes[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func pop<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>): ?(K, V) {
    let data = switch (map[DATA]) { case (?data) data; case (_) return null };

    let keys = data.0;
    let capacity = nat32(keys.size());

    let targetKey = switch (keys[nat((data.3[BACK] -% 1) % capacity)]) { case (?key) key; case (_) trap("unreachable") };

    let hashIndex = nat(hashUtils.0(targetKey) % capacity +% capacity);
    let indexes = data.2;
    var index = indexes[hashIndex];
    var prevIndex = NULL;

    loop if (index == NULL) {
      return null;
    } else if (hashUtils.1(switch (keys[index]) { case (?key) key; case (_) trap("unreachable") }, targetKey)) {
      let value = data.1[index];
      let bounds = data.3;
      let newSize = bounds[SIZE] -% 1;

      bounds[SIZE] := newSize;

      keys[index] := null;
      data.1[index] := null;

      indexes[if (prevIndex == NULL) hashIndex else prevIndex] := indexes[index];

      if (newSize < capacity *% 3 / 8) {
        rehash(map, hashUtils);
      } else {
        var back = (bounds[BACK] -% 1) % capacity;

        if (newSize != 0) {
          while (switch (keys[nat((back -% 1) % capacity)]) { case (null) true; case (_) false }) {
            back := (back -% 1) % capacity;
          };
        };

        bounds[BACK] := back;
      };

      return ?(targetKey, switch (value) { case (?value) value; case (_) trap("unreachable") });
    } else {
      prevIndex := index;
      index := indexes[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func popFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>): ?(K, V) {
    let data = switch (map[DATA]) { case (?data) data; case (_) return null };

    let keys = data.0;
    let capacity = nat32(keys.size());

    let targetKey = switch (keys[nat((data.3[FRONT] +% 1) % capacity)]) { case (?key) key; case (_) trap("unreachable") };

    let hashIndex = nat(hashUtils.0(targetKey) % capacity +% capacity);
    let indexes = data.2;
    var index = indexes[hashIndex];
    var prevIndex = NULL;

    loop if (index == NULL) {
      return null;
    } else if (hashUtils.1(switch (keys[index]) { case (?key) key; case (_) trap("unreachable") }, targetKey)) {
      let value = data.1[index];
      let bounds = data.3;
      let newSize = bounds[SIZE] -% 1;

      bounds[SIZE] := newSize;

      keys[index] := null;
      data.1[index] := null;

      indexes[if (prevIndex == NULL) hashIndex else prevIndex] := indexes[index];

      if (newSize < capacity *% 3 / 8) {
        rehash(map, hashUtils);
      } else {
        var front = (bounds[FRONT] +% 1) % capacity;

        if (newSize != 0) {
          while (switch (keys[nat((front +% 1) % capacity)]) { case (null) true; case (_) false }) {
            front := (front +% 1) % capacity;
          };
        };

        bounds[FRONT] := front;
      };

      return ?(targetKey, switch (value) { case (?value) value; case (_) trap("unreachable") });
    } else {
      prevIndex := index;
      index := indexes[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func cycle<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>): ?(K, V) {
    let data = switch (map[DATA]) { case (?data) data; case (_) return null };

    let keys = data.0;
    let capacity = nat32(keys.size());

    let targetKey = switch (keys[nat((data.3[BACK] -% 1) % capacity)]) { case (?key) key; case (_) trap("unreachable") };

    let hashIndex = nat(hashUtils.0(targetKey) % capacity +% capacity);
    let indexes = data.2;
    var index = indexes[hashIndex];
    var prevIndex = NULL;

    loop if (index == NULL) {
      return null;
    } else {
      let key = keys[index];

      if (hashUtils.1(switch (key) { case (?key) key; case (_) trap("unreachable") }, targetKey)) {
        let values = data.1;
        let value = values[index];
        let bounds = data.3;
        let back = bounds[BACK];
        let backNat = nat(back);

        bounds[BACK] := (back +% 1) % capacity;

        keys[backNat] := key;
        values[backNat] := value;
        indexes[backNat] := indexes[index];
        keys[index] := null;
        values[index] := null;

        indexes[if (prevIndex == NULL) hashIndex else prevIndex] := backNat;

        var front = (bounds[FRONT] +% 1) % capacity;

        while (switch (keys[nat((front +% 1) % capacity)]) { case (null) true; case (_) false }) {
          front := (front +% 1) % capacity;
        };

        bounds[FRONT] := front;

        return ?(targetKey, switch (value) { case (?value) value; case (_) trap("unreachable") });
      } else {
        prevIndex := index;
        index := indexes[index];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func cycleFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>): ?(K, V) {
    let data = switch (map[DATA]) { case (?data) data; case (_) return null };

    let keys = data.0;
    let capacity = nat32(keys.size());

    let targetKey = switch (keys[nat((data.3[FRONT] +% 1) % capacity)]) { case (?key) key; case (_) trap("unreachable") };

    let hashIndex = nat(hashUtils.0(targetKey) % capacity +% capacity);
    let indexes = data.2;
    var index = indexes[hashIndex];
    var prevIndex = NULL;

    loop if (index == NULL) {
      return null;
    } else {
      let key = keys[index];

      if (hashUtils.1(switch (key) { case (?key) key; case (_) trap("unreachable") }, targetKey)) {
        let values = data.1;
        let value = values[index];
        let bounds = data.3;
        let front = bounds[FRONT];
        let frontNat = nat(front);

        bounds[FRONT] := (front -% 1) % capacity;

        keys[frontNat] := key;
        values[frontNat] := value;
        indexes[frontNat] := indexes[index];
        keys[index] := null;
        values[index] := null;

        indexes[if (prevIndex == NULL) hashIndex else prevIndex] := frontNat;

        var back = (bounds[BACK] -% 1) % capacity;

        while (switch (keys[nat((back -% 1) % capacity)]) { case (null) true; case (_) false }) {
          back := (back -% 1) % capacity;
        };

        bounds[BACK] := back;

        return ?(targetKey, switch (value) { case (?value) value; case (_) trap("unreachable") });
      } else {
        prevIndex := index;
        index := indexes[index];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func mapFilter<K, V1, V2>(map: Map<K, V1>, hashUtils: HashUtils<K>, mapEntry: (K, V1) -> ?V2): Map<K, V2> {
    let data = switch (map[DATA]) { case (?data) data; case (_) return [var null] };

    let capacity = nat32(data.0.size());
    let lastHashIndex = capacity -% 1;

    let newMap = [var null]:Map<K, V2>;

    let lastIndex = data.3[BACK];
    var index = (data.3[FRONT] +% 1) % capacity;

    while (index != lastIndex) {
      switch (data.0[nat(index)]) {
        case (?someKey) switch (mapEntry(someKey, switch (data.1[nat(index)]) { case (?value) value; case (_) trap("unreachable") })) {
          case (null) {};
          case (newValue) ignore putMove(newMap, hashUtils, someKey, newValue);
        };

        case (_) {};
      };

      index := (index +% 1) % capacity;
    };

    newMap;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func mapFilterDesc<K, V1, V2>(map: Map<K, V1>, hashUtils: HashUtils<K>, mapEntry: (K, V1) -> ?V2): Map<K, V2> {
    let data = switch (map[DATA]) { case (?data) data; case (_) return [var null] };

    let capacity = nat32(data.0.size());
    let lastHashIndex = capacity -% 1;

    let newMap = [var null]:Map<K, V2>;

    let lastIndex = data.3[FRONT];
    var index = (data.3[BACK] -% 1) % capacity;

    while (index != lastIndex) {
      switch (data.0[nat(index)]) {
        case (?someKey) switch (mapEntry(someKey, switch (data.1[nat(index)]) { case (?value) value; case (_) trap("unreachable") })) {
          case (null) {};
          case (newValue) ignore putMove(newMap, hashUtils, someKey, newValue);
        };

        case (_) {};
      };

      index := (index -% 1) % capacity;
    };

    newMap;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func filter<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, acceptEntry: (K, V) -> Bool): Map<K, V> {
    let data = switch (map[DATA]) { case (?data) data; case (_) return [var null] };

    let capacity = nat32(data.0.size());
    let lastHashIndex = capacity -% 1;

    let newMap = [var null]:Map<K, V>;

    let lastIndex = data.3[BACK];
    var index = (data.3[FRONT] +% 1) % capacity;

    while (index != lastIndex) {
      switch (data.0[nat(index)]) {
        case (?someKey) {
          let value = data.1[nat(index)];

          if (acceptEntry(someKey, switch (value) { case (?value) value; case (_) trap("unreachable") })) {
            ignore putMove(newMap, hashUtils, someKey, value);
          };
        };

        case (_) {};
      };

      index := (index +% 1) % capacity;
    };

    newMap;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func filterDesc<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, acceptEntry: (K, V) -> Bool): Map<K, V> {
    let data = switch (map[DATA]) { case (?data) data; case (_) return [var null] };

    let capacity = nat32(data.0.size());
    let lastHashIndex = capacity -% 1;

    let newMap = [var null]:Map<K, V>;

    let lastIndex = data.3[FRONT];
    var index = (data.3[BACK] -% 1) % capacity;

    while (index != lastIndex) {
      switch (data.0[nat(index)]) {
        case (?someKey) {
          let value = data.1[nat(index)];

          if (acceptEntry(someKey, switch (value) { case (?value) value; case (_) trap("unreachable") })) {
            ignore putMove(newMap, hashUtils, someKey, value);
          };
        };

        case (_) {};
      };

      index := (index -% 1) % capacity;
    };

    newMap;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func map<K, V1, V2>(map: Map<K, V1>, hashUtils: HashUtils<K>, mapEntry: (K, V1) -> V2): Map<K, V2> {
    let data = switch (map[DATA]) { case (?data) data; case (_) return [var null] };

    let indexes = data.2;
    let values = data.1;
    let keys = data.0;
    let capacity = nat32(keys.size());
    let capacityNat = nat(capacity);

    var lastIndex = capacity *% 2;
    var index = capacity;

    let newKeys = initArray<?K>(capacityNat, null);
    let newValues = initArray<?V2>(capacityNat, null);
    let newIndexes = initArray<Nat>(nat(lastIndex), NULL);

    while (index < lastIndex) {
      let indexNat = nat(index);
      let hashIndex = indexes[indexNat];

      if (hashIndex != NULL) newIndexes[indexNat] := hashIndex;

      index +%= 1;
    };

    let bounds = data.3;
    let back = bounds[BACK];
    let front = bounds[FRONT];

    index := (front +% 1) % capacity;

    while (index != back) {
      let indexNat = nat(index);
      let key = keys[indexNat];

      switch (key) {
        case (?someKey) {
          let nextIndex = indexes[indexNat];

          if (nextIndex != NULL) newIndexes[indexNat] := nextIndex;

          newKeys[indexNat] := key;
          newValues[indexNat] := ?mapEntry(someKey, switch (values[indexNat]) { case (?value) value; case (_) trap("unreachable") });
        };

        case (_) {};
      };

      index := (index +% 1) % capacity;
    };

    return [var ?(newKeys, newValues, newIndexes, [var front, back, bounds[SIZE]])];
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func mapDesc<K, V1, V2>(map: Map<K, V1>, hashUtils: HashUtils<K>, mapEntry: (K, V1) -> V2): Map<K, V2> {
    let data = switch (map[DATA]) { case (?data) data; case (_) return [var null] };

    let indexes = data.2;
    let values = data.1;
    let keys = data.0;
    let capacity = nat32(keys.size());
    let capacityNat = nat(capacity);

    let lastHashIndex = capacity -% 1;
    var lastIndex = capacity *% 2;
    var index = capacity;

    let newKeys = initArray<?K>(capacityNat, null);
    let newValues = initArray<?V2>(capacityNat, null);
    let newIndexes = initArray<Nat>(nat(lastIndex), NULL);

    while (index < lastIndex) {
      let indexNat = nat(index);
      let hashIndex = indexes[indexNat];

      if (hashIndex != NULL) newIndexes[indexNat] := nat(lastHashIndex -% nat32(hashIndex));

      index +%= 1;
    };

    let bounds = data.3;
    let front = bounds[FRONT];
    let back = bounds[BACK];

    index := (back -% 1) % capacity;

    while (index != front) {
      let indexNat = nat(index);
      let key = keys[indexNat];

      switch (key) {
        case (?someKey) {
          let newIndex = nat(lastHashIndex -% index);
          let nextIndex = indexes[indexNat];

          if (nextIndex != NULL) newIndexes[newIndex] := nat(lastHashIndex -% nat32(nextIndex));

          newKeys[newIndex] := key;
          newValues[newIndex] := ?mapEntry(someKey, switch (values[indexNat]) { case (?value) value; case (_) trap("unreachable") });
        };

        case (_) {};
      };

      index := (index -% 1) % capacity;
    };

    return [var ?(newKeys, newValues, newIndexes, [var lastHashIndex -% back, lastHashIndex -% front, bounds[SIZE]])];
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func clone<K, V>(map: Map<K, V>): Map<K, V> {
    let data = switch (map[DATA]) { case (?data) data; case (_) return [var null] };

    let indexes = data.2;
    let values = data.1;
    let keys = data.0;
    let capacity = nat32(keys.size());
    let capacityNat = nat(capacity);

    var lastIndex = capacity *% 2;
    var index = capacity;

    let newKeys = initArray<?K>(capacityNat, null);
    let newValues = initArray<?V>(capacityNat, null);
    let newIndexes = initArray<Nat>(nat(lastIndex), NULL);

    while (index < lastIndex) {
      let indexNat = nat(index);
      let hashIndex = indexes[indexNat];

      if (hashIndex != NULL) newIndexes[indexNat] := hashIndex;

      index +%= 1;
    };

    let bounds = data.3;
    let back = bounds[BACK];
    let front = bounds[FRONT];

    index := (front +% 1) % capacity;

    while (index != back) {
      let indexNat = nat(index);
      let key = keys[indexNat];

      switch (key) {
        case (?someKey) {
          let nextIndex = indexes[indexNat];

          if (nextIndex != NULL) newIndexes[indexNat] := nextIndex;

          newKeys[indexNat] := key;
          newValues[indexNat] := values[indexNat];
        };

        case (_) {};
      };

      index := (index +% 1) % capacity;
    };

    return [var ?(newKeys, newValues, newIndexes, [var front, back, bounds[SIZE]])];
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func cloneDesc<K, V>(map: Map<K, V>): Map<K, V> {
    let data = switch (map[DATA]) { case (?data) data; case (_) return [var null] };

    let indexes = data.2;
    let values = data.1;
    let keys = data.0;
    let capacity = nat32(keys.size());
    let capacityNat = nat(capacity);

    let lastHashIndex = capacity -% 1;
    var lastIndex = capacity *% 2;
    var index = capacity;

    let newKeys = initArray<?K>(capacityNat, null);
    let newValues = initArray<?V>(capacityNat, null);
    let newIndexes = initArray<Nat>(nat(lastIndex), NULL);

    while (index < lastIndex) {
      let indexNat = nat(index);
      let hashIndex = indexes[indexNat];

      if (hashIndex != NULL) newIndexes[indexNat] := nat(lastHashIndex -% nat32(hashIndex));

      index +%= 1;
    };

    let bounds = data.3;
    let front = bounds[FRONT];
    let back = bounds[BACK];

    index := (back -% 1) % capacity;

    while (index != front) {
      let indexNat = nat(index);
      let key = keys[indexNat];

      switch (key) {
        case (null) {};

        case (_) {
          let newIndex = nat(lastHashIndex -% index);
          let nextIndex = indexes[indexNat];

          if (nextIndex != NULL) newIndexes[newIndex] := nat(lastHashIndex -% nat32(nextIndex));

          newKeys[newIndex] := key;
          newValues[newIndex] := values[indexNat];
        };
      };

      index := (index -% 1) % capacity;
    };

    return [var ?(newKeys, newValues, newIndexes, [var lastHashIndex -% back, lastHashIndex -% front, bounds[SIZE]])];
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func keys<K, V>(map: Map<K, V>): Iter<K> {
    let dataOpt = map[DATA];

    let capacity = switch (dataOpt) { case (?data) nat32(data.0.size()); case (_) 0:Nat32 };
    let front = switch (dataOpt) { case (?data) data.3[FRONT]; case (_) 0:Nat32 };
    let back = switch (dataOpt) { case (?data) data.3[BACK]; case (_) 0:Nat32 };

    var started = false;
    var iterIndex = front;

    let iter = {
      prev = func(): ?K {
        started := true;

        switch (dataOpt) {
          case (?data) loop {
            iterIndex := if (iterIndex == front) (back -% 1) % capacity else (iterIndex -% 1) % capacity;

            switch (data.0[nat(iterIndex)]) {
              case (null) if (iterIndex == front) return null;
              case (key) return key;
            };
          };

          case (_) null;
        };
      };

      next = func(): ?K {
        started := true;

        switch (dataOpt) {
          case (?data) loop {
            iterIndex := if (iterIndex == back) (front +% 1) % capacity else (iterIndex +% 1) % capacity;

            switch (data.0[nat(iterIndex)]) {
              case (null) if (iterIndex == back) return null;
              case (key) return key;
            };
          };

          case (_) null;
        };
      };

      peekPrev = func(): ?K {
        var newIndex = iterIndex;

        switch (dataOpt) {
          case (?data) loop {
            newIndex := if (newIndex == front) (back -% 1) % capacity else (newIndex -% 1) % capacity;

            switch (data.0[nat(newIndex)]) {
              case (null) if (newIndex == front) return null;
              case (key) return key;
            };
          };

          case (_) null;
        };
      };

      peekNext = func(): ?K {
        var newIndex = iterIndex;

        switch (dataOpt) {
          case (?data) loop {
            newIndex := if (newIndex == back) (front +% 1) % capacity else (newIndex +% 1) % capacity;

            switch (data.0[nat(newIndex)]) {
              case (null) if (newIndex == back) return null;
              case (key) return key;
            };
          };

          case (_) null;
        };
      };

      current = func(): ?K {
        switch (dataOpt) {
          case (?data) switch (data.0[nat(iterIndex)]) { case (null) null; case (key) key };
          case (_) null;
        };
      };

      started = func(): Bool {
        started;
      };

      finished = func(): Bool {
        started and (iterIndex == front or iterIndex == back);
      };

      reset = func(): Iter<K> {
        started := false;

        iterIndex := front;

        iter;
      };

      movePrev = func(): Iter<K> {
        started := true;

        switch (dataOpt) {
          case (?data) loop {
            iterIndex := if (iterIndex == front) (back -% 1) % capacity else (iterIndex -% 1) % capacity;

            switch (data.0[nat(iterIndex)]) {
              case (null) if (iterIndex == front) return iter;
              case (_) return iter;
            };
          };

          case (_) iter;
        };
      };

      moveNext = func(): Iter<K> {
        started := true;

        switch (dataOpt) {
          case (?data) loop {
            iterIndex := if (iterIndex == back) (front +% 1) % capacity else (iterIndex +% 1) % capacity;

            switch (data.0[nat(iterIndex)]) {
              case (null) if (iterIndex == back) return iter;
              case (_) return iter;
            };
          };

          case (_) iter;
        };
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func keysDesc<K, V>(map: Map<K, V>): Iter<K> {
    let dataOpt = map[DATA];

    let capacity = switch (dataOpt) { case (?data) nat32(data.0.size()); case (_) 0:Nat32 };
    let front = switch (dataOpt) { case (?data) data.3[FRONT]; case (_) 0:Nat32 };
    let back = switch (dataOpt) { case (?data) data.3[BACK]; case (_) 0:Nat32 };

    var started = false;
    var iterIndex = back;

    let iter = {
      prev = func(): ?K {
        started := true;

        switch (dataOpt) {
          case (?data) loop {
            iterIndex := if (iterIndex == back) (front +% 1) % capacity else (iterIndex +% 1) % capacity;

            switch (data.0[nat(iterIndex)]) {
              case (null) if (iterIndex == back) return null;
              case (key) return key;
            };
          };

          case (_) null;
        };
      };

      next = func(): ?K {
        started := true;

        switch (dataOpt) {
          case (?data) loop {
            iterIndex := if (iterIndex == front) (back -% 1) % capacity else (iterIndex -% 1) % capacity;

            switch (data.0[nat(iterIndex)]) {
              case (null) if (iterIndex == front) return null;
              case (key) return key;
            };
          };

          case (_) null;
        };
      };

      peekPrev = func(): ?K {
        var newIndex = iterIndex;

        switch (dataOpt) {
          case (?data) loop {
            newIndex := if (newIndex == back) (front +% 1) % capacity else (newIndex +% 1) % capacity;

            switch (data.0[nat(newIndex)]) {
              case (null) if (newIndex == back) return null;
              case (key) return key;
            };
          };

          case (_) null;
        };
      };

      peekNext = func(): ?K {
        var newIndex = iterIndex;

        switch (dataOpt) {
          case (?data) loop {
            newIndex := if (newIndex == front) (back -% 1) % capacity else (newIndex -% 1) % capacity;

            switch (data.0[nat(newIndex)]) {
              case (null) if (newIndex == front) return null;
              case (key) return key;
            };
          };

          case (_) null;
        };
      };

      current = func(): ?K {
        switch (dataOpt) {
          case (?data) switch (data.0[nat(iterIndex)]) { case (null) null; case (key) key };
          case (_) null;
        };
      };

      started = func(): Bool {
        started;
      };

      finished = func(): Bool {
        started and (iterIndex == front or iterIndex == back);
      };

      reset = func(): Iter<K> {
        started := false;

        iterIndex := back;

        iter;
      };

      movePrev = func(): Iter<K> {
        started := true;

        switch (dataOpt) {
          case (?data) loop {
            iterIndex := if (iterIndex == back) (front +% 1) % capacity else (iterIndex +% 1) % capacity;

            switch (data.0[nat(iterIndex)]) {
              case (null) if (iterIndex == back) return iter;
              case (_) return iter;
            };
          };

          case (_) iter;
        };
      };

      moveNext = func(): Iter<K> {
        started := true;

        switch (dataOpt) {
          case (?data) loop {
            iterIndex := if (iterIndex == front) (back -% 1) % capacity else (iterIndex -% 1) % capacity;

            switch (data.0[nat(iterIndex)]) {
              case (null) if (iterIndex == front) return iter;
              case (_) return iter;
            };
          };

          case (_) iter;
        };
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func vals<K, V>(map: Map<K, V>): Iter<V> {
    let dataOpt = map[DATA];

    let capacity = switch (dataOpt) { case (?data) nat32(data.0.size()); case (_) 0:Nat32 };
    let front = switch (dataOpt) { case (?data) data.3[FRONT]; case (_) 0:Nat32 };
    let back = switch (dataOpt) { case (?data) data.3[BACK]; case (_) 0:Nat32 };

    var started = false;
    var iterIndex = front;

    let iter = {
      prev = func(): ?V {
        started := true;

        switch (dataOpt) {
          case (?data) loop {
            iterIndex := if (iterIndex == front) (back -% 1) % capacity else (iterIndex -% 1) % capacity;

            switch (data.1[nat(iterIndex)]) {
              case (null) if (iterIndex == front) return null;
              case (value) return value;
            };
          };

          case (_) null;
        };
      };

      next = func(): ?V {
        started := true;

        switch (dataOpt) {
          case (?data) loop {
            iterIndex := if (iterIndex == back) (front +% 1) % capacity else (iterIndex +% 1) % capacity;

            switch (data.1[nat(iterIndex)]) {
              case (null) if (iterIndex == back) return null;
              case (value) return value;
            };
          };

          case (_) null;
        };
      };

      peekPrev = func(): ?V {
        var newIndex = iterIndex;

        switch (dataOpt) {
          case (?data) loop {
            newIndex := if (newIndex == front) (back -% 1) % capacity else (newIndex -% 1) % capacity;

            switch (data.1[nat(newIndex)]) {
              case (null) if (newIndex == front) return null;
              case (value) return value;
            };
          };

          case (_) null;
        };
      };

      peekNext = func(): ?V {
        var newIndex = iterIndex;

        switch (dataOpt) {
          case (?data) loop {
            newIndex := if (newIndex == back) (front +% 1) % capacity else (newIndex +% 1) % capacity;

            switch (data.1[nat(newIndex)]) {
              case (null) if (newIndex == back) return null;
              case (value) return value;
            };
          };

          case (_) null;
        };
      };

      current = func(): ?V {
        switch (dataOpt) {
          case (?data) switch (data.1[nat(iterIndex)]) { case (null) null; case (value) value };
          case (_) null;
        };
      };

      started = func(): Bool {
        started;
      };

      finished = func(): Bool {
        started and (iterIndex == front or iterIndex == back);
      };

      reset = func(): Iter<V> {
        started := false;

        iterIndex := front;

        iter;
      };

      movePrev = func(): Iter<V> {
        started := true;

        switch (dataOpt) {
          case (?data) loop {
            iterIndex := if (iterIndex == front) (back -% 1) % capacity else (iterIndex -% 1) % capacity;

            switch (data.1[nat(iterIndex)]) {
              case (null) if (iterIndex == front) return iter;
              case (_) return iter;
            };
          };

          case (_) iter;
        };
      };

      moveNext = func(): Iter<V> {
        started := true;

        switch (dataOpt) {
          case (?data) loop {
            iterIndex := if (iterIndex == back) (front +% 1) % capacity else (iterIndex +% 1) % capacity;

            switch (data.1[nat(iterIndex)]) {
              case (null) if (iterIndex == back) return iter;
              case (_) return iter;
            };
          };

          case (_) iter;
        };
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func valsDesc<K, V>(map: Map<K, V>): Iter<V> {
    let dataOpt = map[DATA];

    let capacity = switch (dataOpt) { case (?data) nat32(data.0.size()); case (_) 0:Nat32 };
    let front = switch (dataOpt) { case (?data) data.3[FRONT]; case (_) 0:Nat32 };
    let back = switch (dataOpt) { case (?data) data.3[BACK]; case (_) 0:Nat32 };

    var started = false;
    var iterIndex = back;

    let iter = {
      prev = func(): ?V {
        started := true;

        switch (dataOpt) {
          case (?data) loop {
            iterIndex := if (iterIndex == back) (front +% 1) % capacity else (iterIndex +% 1) % capacity;

            switch (data.1[nat(iterIndex)]) {
              case (null) if (iterIndex == back) return null;
              case (value) return value;
            };
          };

          case (_) null;
        };
      };

      next = func(): ?V {
        started := true;

        switch (dataOpt) {
          case (?data) loop {
            iterIndex := if (iterIndex == front) (back -% 1) % capacity else (iterIndex -% 1) % capacity;

            switch (data.1[nat(iterIndex)]) {
              case (null) if (iterIndex == front) return null;
              case (value) return value;
            };
          };

          case (_) null;
        };
      };

      peekPrev = func(): ?V {
        var newIndex = iterIndex;

        switch (dataOpt) {
          case (?data) loop {
            newIndex := if (newIndex == back) (front +% 1) % capacity else (newIndex +% 1) % capacity;

            switch (data.1[nat(newIndex)]) {
              case (null) if (newIndex == back) return null;
              case (value) return value;
            };
          };

          case (_) null;
        };
      };

      peekNext = func(): ?V {
        var newIndex = iterIndex;

        switch (dataOpt) {
          case (?data) loop {
            newIndex := if (newIndex == front) (back -% 1) % capacity else (newIndex -% 1) % capacity;

            switch (data.1[nat(newIndex)]) {
              case (null) if (newIndex == front) return null;
              case (value) return value;
            };
          };

          case (_) null;
        };
      };

      current = func(): ?V {
        switch (dataOpt) {
          case (?data) switch (data.1[nat(iterIndex)]) { case (null) null; case (value) value };
          case (_) null;
        };
      };

      started = func(): Bool {
        started;
      };

      finished = func(): Bool {
        started and (iterIndex == front or iterIndex == back);
      };

      reset = func(): Iter<V> {
        started := false;

        iterIndex := back;

        iter;
      };

      movePrev = func(): Iter<V> {
        started := true;

        switch (dataOpt) {
          case (?data) loop {
            iterIndex := if (iterIndex == back) (front +% 1) % capacity else (iterIndex +% 1) % capacity;

            switch (data.1[nat(iterIndex)]) {
              case (null) if (iterIndex == back) return iter;
              case (_) return iter;
            };
          };

          case (_) iter;
        };
      };

      moveNext = func(): Iter<V> {
        started := true;

        switch (dataOpt) {
          case (?data) loop {
            iterIndex := if (iterIndex == front) (back -% 1) % capacity else (iterIndex -% 1) % capacity;

            switch (data.1[nat(iterIndex)]) {
              case (null) if (iterIndex == front) return iter;
              case (_) return iter;
            };
          };

          case (_) iter;
        };
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func entries<K, V>(map: Map<K, V>): Iter<(K, V)> {
    let dataOpt = map[DATA];

    let capacity = switch (dataOpt) { case (?data) nat32(data.0.size()); case (_) 0:Nat32 };
    let front = switch (dataOpt) { case (?data) data.3[FRONT]; case (_) 0:Nat32 };
    let back = switch (dataOpt) { case (?data) data.3[BACK]; case (_) 0:Nat32 };

    var started = false;
    var iterIndex = front;

    let iter = {
      prev = func(): ?(K, V) {
        started := true;

        switch (dataOpt) {
          case (?data) loop {
            iterIndex := if (iterIndex == front) (back -% 1) % capacity else (iterIndex -% 1) % capacity;

            let iterIndexNat = nat(iterIndex);

            switch (data.0[iterIndexNat]) {
              case (?key) return ?(key, switch (data.1[iterIndexNat]) { case (?value) value; case (_) trap("unreachable") });
              case (_) if (iterIndex == front) return null;
            };
          };

          case (_) null;
        };
      };

      next = func(): ?(K, V) {
        started := true;

        switch (dataOpt) {
          case (?data) loop {
            iterIndex := if (iterIndex == back) (front +% 1) % capacity else (iterIndex +% 1) % capacity;

            let iterIndexNat = nat(iterIndex);

            switch (data.0[iterIndexNat]) {
              case (?key) return ?(key, switch (data.1[iterIndexNat]) { case (?value) value; case (_) trap("unreachable") });
              case (_) if (iterIndex == back) return null;
            };
          };

          case (_) null;
        };
      };

      peekPrev = func(): ?(K, V) {
        var newIndex = iterIndex;

        switch (dataOpt) {
          case (?data) loop {
            newIndex := if (newIndex == front) (back -% 1) % capacity else (newIndex -% 1) % capacity;

            let iterIndexNat = nat(iterIndex);

            switch (data.0[iterIndexNat]) {
              case (?key) return ?(key, switch (data.1[iterIndexNat]) { case (?value) value; case (_) trap("unreachable") });
              case (_) if (newIndex == front) return null;
            };
          };

          case (_) null;
        };
      };

      peekNext = func(): ?(K, V) {
        var newIndex = iterIndex;

        switch (dataOpt) {
          case (?data) loop {
            newIndex := if (newIndex == back) (front +% 1) % capacity else (newIndex +% 1) % capacity;

            let iterIndexNat = nat(iterIndex);

            switch (data.0[iterIndexNat]) {
              case (?key) return ?(key, switch (data.1[iterIndexNat]) { case (?value) value; case (_) trap("unreachable") });
              case (_) if (newIndex == back) return null;
            };
          };

          case (_) null;
        };
      };

      current = func(): ?(K, V) {
        switch (dataOpt) {
          case (?data) {
            let iterIndexNat = nat(iterIndex);

            switch (data.0[iterIndexNat]) {
              case (?key) return ?(key, switch (data.1[iterIndexNat]) { case (?value) value; case (_) trap("unreachable") });
              case (_) null;
            };
          };

          case (_) null;
        };
      };

      started = func(): Bool {
        started;
      };

      finished = func(): Bool {
        started and (iterIndex == front or iterIndex == back);
      };

      reset = func(): Iter<(K, V)> {
        started := false;

        iterIndex := front;

        iter;
      };

      movePrev = func(): Iter<(K, V)> {
        started := true;

        switch (dataOpt) {
          case (?data) loop {
            iterIndex := if (iterIndex == front) (back -% 1) % capacity else (iterIndex -% 1) % capacity;

            switch (data.0[nat(iterIndex)]) {
              case (null) if (iterIndex == front) return iter;
              case (_) return iter;
            };
          };

          case (_) iter;
        };
      };

      moveNext = func(): Iter<(K, V)> {
        started := true;

        switch (dataOpt) {
          case (?data) loop {
            iterIndex := if (iterIndex == back) (front +% 1) % capacity else (iterIndex +% 1) % capacity;

            switch (data.0[nat(iterIndex)]) {
              case (null) if (iterIndex == back) return iter;
              case (_) return iter;
            };
          };

          case (_) iter;
        };
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func entriesDesc<K, V>(map: Map<K, V>): Iter<(K, V)> {
    let dataOpt = map[DATA];

    let capacity = switch (dataOpt) { case (?data) nat32(data.0.size()); case (_) 0:Nat32 };
    let front = switch (dataOpt) { case (?data) data.3[FRONT]; case (_) 0:Nat32 };
    let back = switch (dataOpt) { case (?data) data.3[BACK]; case (_) 0:Nat32 };

    var started = false;
    var iterIndex = back;

    let iter = {
      prev = func(): ?(K, V) {
        started := true;

        switch (dataOpt) {
          case (?data) loop {
            iterIndex := if (iterIndex == back) (front +% 1) % capacity else (iterIndex +% 1) % capacity;

            let iterIndexNat = nat(iterIndex);

            switch (data.0[iterIndexNat]) {
              case (?key) return ?(key, switch (data.1[iterIndexNat]) { case (?value) value; case (_) trap("unreachable") });
              case (_) if (iterIndex == back) return null;
            };
          };

          case (_) null;
        };
      };

      next = func(): ?(K, V) {
        started := true;

        switch (dataOpt) {
          case (?data) loop {
            iterIndex := if (iterIndex == front) (back -% 1) % capacity else (iterIndex -% 1) % capacity;

            let iterIndexNat = nat(iterIndex);

            switch (data.0[iterIndexNat]) {
              case (?key) return ?(key, switch (data.1[iterIndexNat]) { case (?value) value; case (_) trap("unreachable") });
              case (_) if (iterIndex == front) return null;
            };
          };

          case (_) null;
        };
      };

      peekPrev = func(): ?(K, V) {
        var newIndex = iterIndex;

        switch (dataOpt) {
          case (?data) loop {
            newIndex := if (newIndex == back) (front +% 1) % capacity else (newIndex +% 1) % capacity;

            let iterIndexNat = nat(iterIndex);

            switch (data.0[iterIndexNat]) {
              case (?key) return ?(key, switch (data.1[iterIndexNat]) { case (?value) value; case (_) trap("unreachable") });
              case (_) if (newIndex == back) return null;
            };
          };

          case (_) null;
        };
      };

      peekNext = func(): ?(K, V) {
        var newIndex = iterIndex;

        switch (dataOpt) {
          case (?data) loop {
            newIndex := if (newIndex == front) (back -% 1) % capacity else (newIndex -% 1) % capacity;

            let iterIndexNat = nat(iterIndex);

            switch (data.0[iterIndexNat]) {
              case (?key) return ?(key, switch (data.1[iterIndexNat]) { case (?value) value; case (_) trap("unreachable") });
              case (_) if (newIndex == front) return null;
            };
          };

          case (_) null;
        };
      };

      current = func(): ?(K, V) {
        switch (dataOpt) {
          case (?data) {
            let iterIndexNat = nat(iterIndex);

            switch (data.0[iterIndexNat]) {
              case (?key) return ?(key, switch (data.1[iterIndexNat]) { case (?value) value; case (_) trap("unreachable") });
              case (_) null;
            };
          };

          case (_) null;
        };
      };

      started = func(): Bool {
        started;
      };

      finished = func(): Bool {
        started and (iterIndex == front or iterIndex == back);
      };

      reset = func(): Iter<(K, V)> {
        started := false;

        iterIndex := back;

        iter;
      };

      movePrev = func(): Iter<(K, V)> {
        started := true;

        switch (dataOpt) {
          case (?data) loop {
            iterIndex := if (iterIndex == back) (front +% 1) % capacity else (iterIndex +% 1) % capacity;

            switch (data.0[nat(iterIndex)]) {
              case (null) if (iterIndex == back) return iter;
              case (_) return iter;
            };
          };

          case (_) iter;
        };
      };

      moveNext = func(): Iter<(K, V)> {
        started := true;

        switch (dataOpt) {
          case (?data) loop {
            iterIndex := if (iterIndex == front) (back -% 1) % capacity else (iterIndex -% 1) % capacity;

            switch (data.0[nat(iterIndex)]) {
              case (null) if (iterIndex == front) return iter;
              case (_) return iter;
            };
          };

          case (_) iter;
        };
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func keysFrom<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: ?K): Iter<K> {
    let dataOpt = map[DATA];

    let capacity = switch (dataOpt) { case (?data) nat32(data.0.size()); case (_) 0:Nat32 };
    let front = switch (dataOpt) { case (?data) data.3[FRONT]; case (_) 0:Nat32 };
    let back = switch (dataOpt) { case (?data) data.3[BACK]; case (_) 0:Nat32 };
    let keys = switch (dataOpt) { case (?data) data.0; case (_) [var]:[var ?K] };
    let indexes = switch (dataOpt) { case (?data) data.2; case (_) [var]:[var Nat] };

    var index = switch (keyParam) { case (?someKey) indexes[nat(hashUtils.0(someKey) % capacity +% capacity)]; case (_) NULL };

    loop if ((
      index == NULL
    ) or (
      hashUtils.1(
        switch (keys[index]) { case (?key) key; case (_) trap("unreachable") },
        switch (keyParam) { case (?key) key; case (_) trap("unreachable") },
      )
    )) {
      var started = index != NULL;

      var iterIndex = if (index != NULL) nat32(index) else front;

      return let iter = {
        prev = func(): ?K {
          started := true;

          switch (dataOpt) {
            case (?data) loop {
              iterIndex := if (iterIndex == front) (back -% 1) % capacity else (iterIndex -% 1) % capacity;

              switch (data.0[nat(iterIndex)]) {
                case (null) if (iterIndex == front) return null;
                case (key) return key;
              };
            };

            case (_) null;
          };
        };

        next = func(): ?K {
          started := true;

          switch (dataOpt) {
            case (?data) loop {
              iterIndex := if (iterIndex == back) (front +% 1) % capacity else (iterIndex +% 1) % capacity;

              switch (data.0[nat(iterIndex)]) {
                case (null) if (iterIndex == back) return null;
                case (key) return key;
              };
            };

            case (_) null;
          };
        };

        peekPrev = func(): ?K {
          var newIndex = iterIndex;

          switch (dataOpt) {
            case (?data) loop {
              newIndex := if (newIndex == front) (back -% 1) % capacity else (newIndex -% 1) % capacity;

              switch (data.0[nat(newIndex)]) {
                case (null) if (newIndex == front) return null;
                case (key) return key;
              };
            };

            case (_) null;
          };
        };

        peekNext = func(): ?K {
          var newIndex = iterIndex;

          switch (dataOpt) {
            case (?data) loop {
              newIndex := if (newIndex == back) (front +% 1) % capacity else (newIndex +% 1) % capacity;

              switch (data.0[nat(newIndex)]) {
                case (null) if (newIndex == back) return null;
                case (key) return key;
              };
            };

            case (_) null;
          };
        };

        current = func(): ?K {
          switch (dataOpt) {
            case (?data) switch (data.0[nat(iterIndex)]) { case (null) null; case (key) key };
            case (_) null;
          };
        };

        started = func(): Bool {
          started;
        };

        finished = func(): Bool {
          started and (iterIndex == front or iterIndex == back);
        };

        reset = func(): Iter<K> {
          started := false;

          iterIndex := front;

          iter;
        };

        movePrev = func(): Iter<K> {
          started := true;

          switch (dataOpt) {
            case (?data) loop {
              iterIndex := if (iterIndex == front) (back -% 1) % capacity else (iterIndex -% 1) % capacity;

              switch (data.0[nat(iterIndex)]) {
                case (null) if (iterIndex == front) return iter;
                case (_) return iter;
              };
            };

            case (_) iter;
          };
        };

        moveNext = func(): Iter<K> {
          started := true;

          switch (dataOpt) {
            case (?data) loop {
              iterIndex := if (iterIndex == back) (front +% 1) % capacity else (iterIndex +% 1) % capacity;

              switch (data.0[nat(iterIndex)]) {
                case (null) if (iterIndex == back) return iter;
                case (_) return iter;
              };
            };

            case (_) iter;
          };
        };
      };
    } else {
      index := indexes[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func keysFromDesc<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: ?K): Iter<K> {
    let dataOpt = map[DATA];

    let capacity = switch (dataOpt) { case (?data) nat32(data.0.size()); case (_) 0:Nat32 };
    let front = switch (dataOpt) { case (?data) data.3[FRONT]; case (_) 0:Nat32 };
    let back = switch (dataOpt) { case (?data) data.3[BACK]; case (_) 0:Nat32 };
    let keys = switch (dataOpt) { case (?data) data.0; case (_) [var]:[var ?K] };
    let indexes = switch (dataOpt) { case (?data) data.2; case (_) [var]:[var Nat] };

    var index = switch (keyParam) { case (?someKey) indexes[nat(hashUtils.0(someKey) % capacity +% capacity)]; case (_) NULL };

    loop if ((
      index == NULL
    ) or (
      hashUtils.1(
        switch (keys[index]) { case (?key) key; case (_) trap("unreachable") },
        switch (keyParam) { case (?key) key; case (_) trap("unreachable") },
      )
    )) {
      var started = index != NULL;

      var iterIndex = if (index != NULL) nat32(index) else front;

      return let iter = {
        prev = func(): ?K {
          started := true;

          switch (dataOpt) {
            case (?data) loop {
              iterIndex := if (iterIndex == back) (front +% 1) % capacity else (iterIndex +% 1) % capacity;

              switch (data.0[nat(iterIndex)]) {
                case (null) if (iterIndex == back) return null;
                case (key) return key;
              };
            };

            case (_) null;
          };
        };

        next = func(): ?K {
          started := true;

          switch (dataOpt) {
            case (?data) loop {
              iterIndex := if (iterIndex == front) (back -% 1) % capacity else (iterIndex -% 1) % capacity;

              switch (data.0[nat(iterIndex)]) {
                case (null) if (iterIndex == front) return null;
                case (key) return key;
              };
            };

            case (_) null;
          };
        };

        peekPrev = func(): ?K {
          var newIndex = iterIndex;

          switch (dataOpt) {
            case (?data) loop {
              newIndex := if (newIndex == back) (front +% 1) % capacity else (newIndex +% 1) % capacity;

              switch (data.0[nat(newIndex)]) {
                case (null) if (newIndex == back) return null;
                case (key) return key;
              };
            };

            case (_) null;
          };
        };

        peekNext = func(): ?K {
          var newIndex = iterIndex;

          switch (dataOpt) {
            case (?data) loop {
              newIndex := if (newIndex == front) (back -% 1) % capacity else (newIndex -% 1) % capacity;

              switch (data.0[nat(newIndex)]) {
                case (null) if (newIndex == front) return null;
                case (key) return key;
              };
            };

            case (_) null;
          };
        };

        current = func(): ?K {
          switch (dataOpt) {
            case (?data) switch (data.0[nat(iterIndex)]) { case (null) null; case (key) key };
            case (_) null;
          };
        };

        started = func(): Bool {
          started;
        };

        finished = func(): Bool {
          started and (iterIndex == front or iterIndex == back);
        };

        reset = func(): Iter<K> {
          started := false;

          iterIndex := back;

          iter;
        };

        movePrev = func(): Iter<K> {
          started := true;

          switch (dataOpt) {
            case (?data) loop {
              iterIndex := if (iterIndex == back) (front +% 1) % capacity else (iterIndex +% 1) % capacity;

              switch (data.0[nat(iterIndex)]) {
                case (null) if (iterIndex == back) return iter;
                case (_) return iter;
              };
            };

            case (_) iter;
          };
        };

        moveNext = func(): Iter<K> {
          started := true;

          switch (dataOpt) {
            case (?data) loop {
              iterIndex := if (iterIndex == front) (back -% 1) % capacity else (iterIndex -% 1) % capacity;

              switch (data.0[nat(iterIndex)]) {
                case (null) if (iterIndex == front) return iter;
                case (_) return iter;
              };
            };

            case (_) iter;
          };
        };
      };
    } else {
      index := indexes[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func valsFrom<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: ?K): Iter<V> {
    let dataOpt = map[DATA];

    let capacity = switch (dataOpt) { case (?data) nat32(data.0.size()); case (_) 0:Nat32 };
    let front = switch (dataOpt) { case (?data) data.3[FRONT]; case (_) 0:Nat32 };
    let back = switch (dataOpt) { case (?data) data.3[BACK]; case (_) 0:Nat32 };
    let keys = switch (dataOpt) { case (?data) data.0; case (_) [var]:[var ?K] };
    let indexes = switch (dataOpt) { case (?data) data.2; case (_) [var]:[var Nat] };

    var index = switch (keyParam) { case (?someKey) indexes[nat(hashUtils.0(someKey) % capacity +% capacity)]; case (_) NULL };

    loop if ((
      index == NULL
    ) or (
      hashUtils.1(
        switch (keys[index]) { case (?key) key; case (_) trap("unreachable") },
        switch (keyParam) { case (?key) key; case (_) trap("unreachable") },
      )
    )) {
      var started = index != NULL;

      var iterIndex = if (index != NULL) nat32(index) else front;

      return let iter = {
        prev = func(): ?V {
          started := true;

          switch (dataOpt) {
            case (?data) loop {
              iterIndex := if (iterIndex == front) (back -% 1) % capacity else (iterIndex -% 1) % capacity;

              switch (data.1[nat(iterIndex)]) {
                case (null) if (iterIndex == front) return null;
                case (value) return value;
              };
            };

            case (_) null;
          };
        };

        next = func(): ?V {
          started := true;

          switch (dataOpt) {
            case (?data) loop {
              iterIndex := if (iterIndex == back) (front +% 1) % capacity else (iterIndex +% 1) % capacity;

              switch (data.1[nat(iterIndex)]) {
                case (null) if (iterIndex == back) return null;
                case (value) return value;
              };
            };

            case (_) null;
          };
        };

        peekPrev = func(): ?V {
          var newIndex = iterIndex;

          switch (dataOpt) {
            case (?data) loop {
              newIndex := if (newIndex == front) (back -% 1) % capacity else (newIndex -% 1) % capacity;

              switch (data.1[nat(newIndex)]) {
                case (null) if (newIndex == front) return null;
                case (value) return value;
              };
            };

            case (_) null;
          };
        };

        peekNext = func(): ?V {
          var newIndex = iterIndex;

          switch (dataOpt) {
            case (?data) loop {
              newIndex := if (newIndex == back) (front +% 1) % capacity else (newIndex +% 1) % capacity;

              switch (data.1[nat(newIndex)]) {
                case (null) if (newIndex == back) return null;
                case (value) return value;
              };
            };

            case (_) null;
          };
        };

        current = func(): ?V {
          switch (dataOpt) {
            case (?data) switch (data.1[nat(iterIndex)]) { case (null) null; case (value) value };
            case (_) null;
          };
        };

        started = func(): Bool {
          started;
        };

        finished = func(): Bool {
          started and (iterIndex == front or iterIndex == back);
        };

        reset = func(): Iter<V> {
          started := false;

          iterIndex := front;

          iter;
        };

        movePrev = func(): Iter<V> {
          started := true;

          switch (dataOpt) {
            case (?data) loop {
              iterIndex := if (iterIndex == front) (back -% 1) % capacity else (iterIndex -% 1) % capacity;

              switch (data.1[nat(iterIndex)]) {
                case (null) if (iterIndex == front) return iter;
                case (_) return iter;
              };
            };

            case (_) iter;
          };
        };

        moveNext = func(): Iter<V> {
          started := true;

          switch (dataOpt) {
            case (?data) loop {
              iterIndex := if (iterIndex == back) (front +% 1) % capacity else (iterIndex +% 1) % capacity;

              switch (data.1[nat(iterIndex)]) {
                case (null) if (iterIndex == back) return iter;
                case (_) return iter;
              };
            };

            case (_) iter;
          };
        };
      };
    } else {
      index := indexes[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func valsFromDesc<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: ?K): Iter<V> {
    let dataOpt = map[DATA];

    let capacity = switch (dataOpt) { case (?data) nat32(data.0.size()); case (_) 0:Nat32 };
    let front = switch (dataOpt) { case (?data) data.3[FRONT]; case (_) 0:Nat32 };
    let back = switch (dataOpt) { case (?data) data.3[BACK]; case (_) 0:Nat32 };
    let keys = switch (dataOpt) { case (?data) data.0; case (_) [var]:[var ?K] };
    let indexes = switch (dataOpt) { case (?data) data.2; case (_) [var]:[var Nat] };

    var index = switch (keyParam) { case (?someKey) indexes[nat(hashUtils.0(someKey) % capacity +% capacity)]; case (_) NULL };

    loop if ((
      index == NULL
    ) or (
      hashUtils.1(
        switch (keys[index]) { case (?key) key; case (_) trap("unreachable") },
        switch (keyParam) { case (?key) key; case (_) trap("unreachable") },
      )
    )) {
      var started = index != NULL;

      var iterIndex = if (index != NULL) nat32(index) else front;

      return let iter = {
        prev = func(): ?V {
          started := true;

          switch (dataOpt) {
            case (?data) loop {
              iterIndex := if (iterIndex == back) (front +% 1) % capacity else (iterIndex +% 1) % capacity;

              switch (data.1[nat(iterIndex)]) {
                case (null) if (iterIndex == back) return null;
                case (value) return value;
              };
            };

            case (_) null;
          };
        };

        next = func(): ?V {
          started := true;

          switch (dataOpt) {
            case (?data) loop {
              iterIndex := if (iterIndex == front) (back -% 1) % capacity else (iterIndex -% 1) % capacity;

              switch (data.1[nat(iterIndex)]) {
                case (null) if (iterIndex == front) return null;
                case (value) return value;
              };
            };

            case (_) null;
          };
        };

        peekPrev = func(): ?V {
          var newIndex = iterIndex;

          switch (dataOpt) {
            case (?data) loop {
              newIndex := if (newIndex == back) (front +% 1) % capacity else (newIndex +% 1) % capacity;

              switch (data.1[nat(newIndex)]) {
                case (null) if (newIndex == back) return null;
                case (value) return value;
              };
            };

            case (_) null;
          };
        };

        peekNext = func(): ?V {
          var newIndex = iterIndex;

          switch (dataOpt) {
            case (?data) loop {
              newIndex := if (newIndex == front) (back -% 1) % capacity else (newIndex -% 1) % capacity;

              switch (data.1[nat(newIndex)]) {
                case (null) if (newIndex == front) return null;
                case (value) return value;
              };
            };

            case (_) null;
          };
        };

        current = func(): ?V {
          switch (dataOpt) {
            case (?data) switch (data.1[nat(iterIndex)]) { case (null) null; case (value) value };
            case (_) null;
          };
        };

        started = func(): Bool {
          started;
        };

        finished = func(): Bool {
          started and (iterIndex == front or iterIndex == back);
        };

        reset = func(): Iter<V> {
          started := false;

          iterIndex := back;

          iter;
        };

        movePrev = func(): Iter<V> {
          started := true;

          switch (dataOpt) {
            case (?data) loop {
              iterIndex := if (iterIndex == back) (front +% 1) % capacity else (iterIndex +% 1) % capacity;

              switch (data.1[nat(iterIndex)]) {
                case (null) if (iterIndex == back) return iter;
                case (_) return iter;
              };
            };

            case (_) iter;
          };
        };

        moveNext = func(): Iter<V> {
          started := true;

          switch (dataOpt) {
            case (?data) loop {
              iterIndex := if (iterIndex == front) (back -% 1) % capacity else (iterIndex -% 1) % capacity;

              switch (data.1[nat(iterIndex)]) {
                case (null) if (iterIndex == front) return iter;
                case (_) return iter;
              };
            };

            case (_) iter;
          };
        };
      };
    } else {
      index := indexes[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func entriesFrom<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: ?K): Iter<(K, V)> {
    let dataOpt = map[DATA];

    let capacity = switch (dataOpt) { case (?data) nat32(data.0.size()); case (_) 0:Nat32 };
    let front = switch (dataOpt) { case (?data) data.3[FRONT]; case (_) 0:Nat32 };
    let back = switch (dataOpt) { case (?data) data.3[BACK]; case (_) 0:Nat32 };
    let keys = switch (dataOpt) { case (?data) data.0; case (_) [var]:[var ?K] };
    let indexes = switch (dataOpt) { case (?data) data.2; case (_) [var]:[var Nat] };

    var index = switch (keyParam) { case (?someKey) indexes[nat(hashUtils.0(someKey) % capacity +% capacity)]; case (_) NULL };

    loop if ((
      index == NULL
    ) or (
      hashUtils.1(
        switch (keys[index]) { case (?key) key; case (_) trap("unreachable") },
        switch (keyParam) { case (?key) key; case (_) trap("unreachable") },
      )
    )) {
      var started = index != NULL;

      var iterIndex = if (index != NULL) nat32(index) else front;

      return let iter = {
        prev = func(): ?(K, V) {
          started := true;

          switch (dataOpt) {
            case (?data) loop {
              iterIndex := if (iterIndex == front) (back -% 1) % capacity else (iterIndex -% 1) % capacity;

              let iterIndexNat = nat(iterIndex);

              switch (data.0[iterIndexNat]) {
                case (?key) return ?(key, switch (data.1[iterIndexNat]) { case (?value) value; case (_) trap("unreachable") });
                case (_) if (iterIndex == front) return null;
              };
            };

            case (_) null;
          };
        };

        next = func(): ?(K, V) {
          started := true;

          switch (dataOpt) {
            case (?data) loop {
              iterIndex := if (iterIndex == back) (front +% 1) % capacity else (iterIndex +% 1) % capacity;

              let iterIndexNat = nat(iterIndex);

              switch (data.0[iterIndexNat]) {
                case (?key) return ?(key, switch (data.1[iterIndexNat]) { case (?value) value; case (_) trap("unreachable") });
                case (_) if (iterIndex == back) return null;
              };
            };

            case (_) null;
          };
        };

        peekPrev = func(): ?(K, V) {
          var newIndex = iterIndex;

          switch (dataOpt) {
            case (?data) loop {
              newIndex := if (newIndex == front) (back -% 1) % capacity else (newIndex -% 1) % capacity;

              let iterIndexNat = nat(iterIndex);

              switch (data.0[iterIndexNat]) {
                case (?key) return ?(key, switch (data.1[iterIndexNat]) { case (?value) value; case (_) trap("unreachable") });
                case (_) if (newIndex == front) return null;
              };
            };

            case (_) null;
          };
        };

        peekNext = func(): ?(K, V) {
          var newIndex = iterIndex;

          switch (dataOpt) {
            case (?data) loop {
              newIndex := if (newIndex == back) (front +% 1) % capacity else (newIndex +% 1) % capacity;

              let iterIndexNat = nat(iterIndex);

              switch (data.0[iterIndexNat]) {
                case (?key) return ?(key, switch (data.1[iterIndexNat]) { case (?value) value; case (_) trap("unreachable") });
                case (_) if (newIndex == back) return null;
              };
            };

            case (_) null;
          };
        };

        current = func(): ?(K, V) {
          switch (dataOpt) {
            case (?data) {
              let iterIndexNat = nat(iterIndex);

              switch (data.0[iterIndexNat]) {
                case (?key) return ?(key, switch (data.1[iterIndexNat]) { case (?value) value; case (_) trap("unreachable") });
                case (_) null;
              };
            };

            case (_) null;
          };
        };

        started = func(): Bool {
          started;
        };

        finished = func(): Bool {
          started and (iterIndex == front or iterIndex == back);
        };

        reset = func(): Iter<(K, V)> {
          started := false;

          iterIndex := front;

          iter;
        };

        movePrev = func(): Iter<(K, V)> {
          started := true;

          switch (dataOpt) {
            case (?data) loop {
              iterIndex := if (iterIndex == front) (back -% 1) % capacity else (iterIndex -% 1) % capacity;

              switch (data.0[nat(iterIndex)]) {
                case (null) if (iterIndex == front) return iter;
                case (_) return iter;
              };
            };

            case (_) iter;
          };
        };

        moveNext = func(): Iter<(K, V)> {
          started := true;

          switch (dataOpt) {
            case (?data) loop {
              iterIndex := if (iterIndex == back) (front +% 1) % capacity else (iterIndex +% 1) % capacity;

              switch (data.0[nat(iterIndex)]) {
                case (null) if (iterIndex == back) return iter;
                case (_) return iter;
              };
            };

            case (_) iter;
          };
        };
      };
    } else {
      index := indexes[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func entriesFromDesc<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: ?K): Iter<(K, V)> {
    let dataOpt = map[DATA];

    let capacity = switch (dataOpt) { case (?data) nat32(data.0.size()); case (_) 0:Nat32 };
    let front = switch (dataOpt) { case (?data) data.3[FRONT]; case (_) 0:Nat32 };
    let back = switch (dataOpt) { case (?data) data.3[BACK]; case (_) 0:Nat32 };
    let keys = switch (dataOpt) { case (?data) data.0; case (_) [var]:[var ?K] };
    let indexes = switch (dataOpt) { case (?data) data.2; case (_) [var]:[var Nat] };

    var index = switch (keyParam) { case (?someKey) indexes[nat(hashUtils.0(someKey) % capacity +% capacity)]; case (_) NULL };

    loop if ((
      index == NULL
    ) or (
      hashUtils.1(
        switch (keys[index]) { case (?key) key; case (_) trap("unreachable") },
        switch (keyParam) { case (?key) key; case (_) trap("unreachable") },
      )
    )) {
      var started = index != NULL;

      var iterIndex = if (index != NULL) nat32(index) else front;

      return let iter = {
        prev = func(): ?(K, V) {
          started := true;

          switch (dataOpt) {
            case (?data) loop {
              iterIndex := if (iterIndex == back) (front +% 1) % capacity else (iterIndex +% 1) % capacity;

              let iterIndexNat = nat(iterIndex);

              switch (data.0[iterIndexNat]) {
                case (?key) return ?(key, switch (data.1[iterIndexNat]) { case (?value) value; case (_) trap("unreachable") });
                case (_) if (iterIndex == back) return null;
              };
            };

            case (_) null;
          };
        };

        next = func(): ?(K, V) {
          started := true;

          switch (dataOpt) {
            case (?data) loop {
              iterIndex := if (iterIndex == front) (back -% 1) % capacity else (iterIndex -% 1) % capacity;

              let iterIndexNat = nat(iterIndex);

              switch (data.0[iterIndexNat]) {
                case (?key) return ?(key, switch (data.1[iterIndexNat]) { case (?value) value; case (_) trap("unreachable") });
                case (_) if (iterIndex == front) return null;
              };
            };

            case (_) null;
          };
        };

        peekPrev = func(): ?(K, V) {
          var newIndex = iterIndex;

          switch (dataOpt) {
            case (?data) loop {
              newIndex := if (newIndex == back) (front +% 1) % capacity else (newIndex +% 1) % capacity;

              let iterIndexNat = nat(iterIndex);

              switch (data.0[iterIndexNat]) {
                case (?key) return ?(key, switch (data.1[iterIndexNat]) { case (?value) value; case (_) trap("unreachable") });
                case (_) if (newIndex == back) return null;
              };
            };

            case (_) null;
          };
        };

        peekNext = func(): ?(K, V) {
          var newIndex = iterIndex;

          switch (dataOpt) {
            case (?data) loop {
              newIndex := if (newIndex == front) (back -% 1) % capacity else (newIndex -% 1) % capacity;

              let iterIndexNat = nat(iterIndex);

              switch (data.0[iterIndexNat]) {
                case (?key) return ?(key, switch (data.1[iterIndexNat]) { case (?value) value; case (_) trap("unreachable") });
                case (_) if (newIndex == front) return null;
              };
            };

            case (_) null;
          };
        };

        current = func(): ?(K, V) {
          switch (dataOpt) {
            case (?data) {
              let iterIndexNat = nat(iterIndex);

              switch (data.0[iterIndexNat]) {
                case (?key) return ?(key, switch (data.1[iterIndexNat]) { case (?value) value; case (_) trap("unreachable") });
                case (_) null;
              };
            };

            case (_) null;
          };
        };

        started = func(): Bool {
          started;
        };

        finished = func(): Bool {
          started and (iterIndex == front or iterIndex == back);
        };

        reset = func(): Iter<(K, V)> {
          started := false;

          iterIndex := back;

          iter;
        };

        movePrev = func(): Iter<(K, V)> {
          started := true;

          switch (dataOpt) {
            case (?data) loop {
              iterIndex := if (iterIndex == back) (front +% 1) % capacity else (iterIndex +% 1) % capacity;

              switch (data.0[nat(iterIndex)]) {
                case (null) if (iterIndex == back) return iter;
                case (_) return iter;
              };
            };

            case (_) iter;
          };
        };

        moveNext = func(): Iter<(K, V)> {
          started := true;

          switch (dataOpt) {
            case (?data) loop {
              iterIndex := if (iterIndex == front) (back -% 1) % capacity else (iterIndex -% 1) % capacity;

              switch (data.0[nat(iterIndex)]) {
                case (null) if (iterIndex == front) return iter;
                case (_) return iter;
              };
            };

            case (_) iter;
          };
        };
      };
    } else {
      index := indexes[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func find<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool): ?(K, V) {
    let data = switch (map[DATA]) { case (?data) data; case (_) return null };

    let values = data.1;
    let keys = data.0;
    let capacity = nat32(keys.size());

    let lastIndex = data.3[BACK];
    var index = (data.3[FRONT] +% 1) % capacity;

    while (index != lastIndex) {
      let indexNat = nat(index);

      switch (keys[indexNat]) {
        case (?someKey) {
          if (acceptEntry(someKey, switch (values[indexNat]) { case (?value) value; case (_) trap("unreachable") })) {
            return ?(someKey, switch (values[indexNat]) { case (?value) value; case (_) trap("unreachable") });
          };
        };

        case (_) {};
      };

      index := (index +% 1) % capacity;
    };

    null;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func findDesc<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool): ?(K, V) {
    let data = switch (map[DATA]) { case (?data) data; case (_) return null };

    let values = data.1;
    let keys = data.0;
    let capacity = nat32(keys.size());

    let lastIndex = data.3[FRONT];
    var index = (data.3[BACK] -% 1) % capacity;

    while (index != lastIndex) {
      let indexNat = nat(index);

      switch (keys[indexNat]) {
        case (?someKey) {
          if (acceptEntry(someKey, switch (values[indexNat]) { case (?value) value; case (_) trap("unreachable") })) {
            return ?(someKey, switch (values[indexNat]) { case (?value) value; case (_) trap("unreachable") });
          };
        };

        case (_) {};
      };

      index := (index -% 1) % capacity;
    };

    null;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func some<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool): Bool {
    let data = switch (map[DATA]) { case (?data) data; case (_) return false };

    let values = data.1;
    let keys = data.0;
    let capacity = nat32(keys.size());

    let lastIndex = data.3[BACK];
    var index = (data.3[FRONT] +% 1) % capacity;

    while (index != lastIndex) {
      let indexNat = nat(index);

      switch (keys[indexNat]) {
        case (?someKey) {
          if (acceptEntry(someKey, switch (values[indexNat]) { case (?value) value; case (_) trap("unreachable") })) {
            return true;
          };
        };

        case (_) {};
      };

      index := (index +% 1) % capacity;
    };

    false;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func someDesc<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool): Bool {
    let data = switch (map[DATA]) { case (?data) data; case (_) return false };

    let values = data.1;
    let keys = data.0;
    let capacity = nat32(keys.size());

    let lastIndex = data.3[FRONT];
    var index = (data.3[BACK] -% 1) % capacity;

    while (index != lastIndex) {
      let indexNat = nat(index);

      switch (keys[indexNat]) {
        case (?someKey) {
          if (acceptEntry(someKey, switch (values[indexNat]) { case (?value) value; case (_) trap("unreachable") })) {
            return true;
          };
        };

        case (_) {};
      };

      index := (index -% 1) % capacity;
    };

    false;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func every<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool): Bool {
    let data = switch (map[DATA]) { case (?data) data; case (_) return true };

    let values = data.1;
    let keys = data.0;
    let capacity = nat32(keys.size());

    let lastIndex = data.3[BACK];
    var index = (data.3[FRONT] +% 1) % capacity;

    while (index != lastIndex) {
      let indexNat = nat(index);

      switch (keys[indexNat]) {
        case (?someKey) {
          if (not acceptEntry(someKey, switch (values[indexNat]) { case (?value) value; case (_) trap("unreachable") })) {
            return false;
          };
        };

        case (_) {};
      };

      index := (index +% 1) % capacity;
    };

    true;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func everyDesc<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool): Bool {
    let data = switch (map[DATA]) { case (?data) data; case (_) return true };

    let values = data.1;
    let keys = data.0;
    let capacity = nat32(keys.size());

    let lastIndex = data.3[FRONT];
    var index = (data.3[BACK] -% 1) % capacity;

    while (index != lastIndex) {
      let indexNat = nat(index);

      switch (keys[indexNat]) {
        case (?someKey) {
          if (not acceptEntry(someKey, switch (values[indexNat]) { case (?value) value; case (_) trap("unreachable") })) {
            return false;
          };
        };

        case (_) {};
      };

      index := (index -% 1) % capacity;
    };

    true;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func forEach<K, V>(map: Map<K, V>, mapEntry: (K, V) -> ()) {
    let data = switch (map[DATA]) { case (?data) data; case (_) return };

    let values = data.1;
    let keys = data.0;
    let capacity = nat32(keys.size());

    let lastIndex = data.3[BACK];
    var index = (data.3[FRONT] +% 1) % capacity;

    while (index != lastIndex) {
      let indexNat = nat(index);

      switch (keys[indexNat]) {
        case (?someKey) mapEntry(someKey, switch (values[indexNat]) { case (?value) value; case (_) trap("unreachable") });
        case (_) {};
      };

      index := (index +% 1) % capacity;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func forEachDesc<K, V>(map: Map<K, V>, mapEntry: (K, V) -> ()) {
    let data = switch (map[DATA]) { case (?data) data; case (_) return };

    let values = data.1;
    let keys = data.0;
    let capacity = nat32(keys.size());

    let lastIndex = data.3[FRONT];
    var index = (data.3[BACK] -% 1) % capacity;

    while (index != lastIndex) {
      let indexNat = nat(index);

      switch (keys[indexNat]) {
        case (?someKey) mapEntry(someKey, switch (values[indexNat]) { case (?value) value; case (_) trap("unreachable") });
        case (_) {};
      };

      index := (index -% 1) % capacity;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func fromIter<K, V>(iter: IterNext<(K, V)>, hashUtils: HashUtils<K>): Map<K, V> {
    let map = [var null]:Map<K, V>;

    var dataOpt = map[DATA];
    var front = 1:Nat32;
    var back = 1:Nat32;
    var size = 1:Nat32;
    var capacity = 2:Nat32;

    for (item in iter) label loopBody {
      let data = switch (dataOpt) {
        case (?data) data;

        case (_) {
          ignore init(map, hashUtils, item.0, ?item.1);

          dataOpt := map[DATA];

          break loopBody;
        };
      };

      let keys = data.0;
      let hashIndex = nat(hashUtils.0(item.0) % capacity +% capacity);

      let indexes = data.2;
      let firstIndex = indexes[hashIndex];
      var index = firstIndex;
      var prevIndex = NULL;

      loop if (index == NULL) {
        let oldBack = back;
        let backNat = nat(oldBack);

        back := (back +% 1) % capacity;
        size +%= 1;

        keys[backNat] := ?item.0;
        data.1[backNat] := ?item.1;
        indexes[backNat] := firstIndex;
        indexes[hashIndex] := backNat;

        if (oldBack == front) {
          let bounds = data.3;

          bounds[FRONT] := front;
          bounds[BACK] := back;
          bounds[SIZE] := size;

          rehash(map, hashUtils);

          dataOpt := map[DATA];

          switch (dataOpt) {
            case (?data) {
              front := data.3[FRONT];
              back := data.3[BACK];
              capacity := nat32(data.0.size());
            };

            case (_) trap("unreachable");
          };
        };

        break loopBody;
      } else {
        let key = keys[index];

        if (hashUtils.1(switch (key) { case (?key) key; case (_) trap("unreachable") }, item.0)) {
          let index32 = nat32(index);

          if (index32 == (back -% 1) % capacity) {
            data.1[index] := ?item.1;
          } else {
            let backNat = nat(back);

            back := (back +% 1) % capacity;

            keys[backNat] := key;
            data.1[backNat] := ?item.1;
            indexes[backNat] := indexes[index];
            keys[index] := null;
            data.1[index] := null;

            indexes[if (prevIndex == NULL) hashIndex else prevIndex] := backNat;

            let prevFront = (front +% 1) % capacity;

            if (index32 == prevFront) {
              front := prevFront;

              while (switch (keys[nat((front +% 1) % capacity)]) { case (null) true; case (_) false }) {
                front := (front +% 1) % capacity;
              };
            } else if (back == prevFront) {
              let bounds = data.3;

              bounds[FRONT] := front;
              bounds[BACK] := back;
              bounds[SIZE] := size;

              rehash(map, hashUtils);

              dataOpt := map[DATA];

              switch (dataOpt) {
                case (?data) {
                  front := data.3[FRONT];
                  back := data.3[BACK];
                  capacity := nat32(data.0.size());
                };

                case (_) trap("unreachable");
              };
            };
          };

          break loopBody;
        } else {
          prevIndex := index;
          index := indexes[index];
        };
      };
    };

    switch (dataOpt) {
      case (?data) {
        let bounds = data.3;

        bounds[FRONT] := front;
        bounds[BACK] := back;
        bounds[SIZE] := size;
      };

      case (_) {};
    };

    map;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func fromIterDesc<K, V>(iter: IterNext<(K, V)>, hashUtils: HashUtils<K>): Map<K, V> {
    let map = [var null]:Map<K, V>;

    var dataOpt = map[DATA];
    var front = 0:Nat32;
    var back = 0:Nat32;
    var size = 1:Nat32;
    var capacity = 2:Nat32;

    for (item in iter) label loopBody {
      let data = switch (dataOpt) {
        case (?data) data;

        case (_) {
          ignore initFront(map, hashUtils, item.0, ?item.1);

          dataOpt := map[DATA];

          break loopBody;
        };
      };

      let keys = data.0;
      let hashIndex = nat(hashUtils.0(item.0) % capacity +% capacity);

      let indexes = data.2;
      let firstIndex = indexes[hashIndex];
      var index = firstIndex;
      var prevIndex = NULL;

      loop if (index == NULL) {
        let oldFront = front;
        let frontNat = nat(oldFront);

        front := (front -% 1) % capacity;
        size +%= 1;

        keys[frontNat] := ?item.0;
        data.1[frontNat] := ?item.1;
        indexes[frontNat] := firstIndex;
        indexes[hashIndex] := frontNat;

        if (oldFront == back) {
          let bounds = data.3;

          bounds[FRONT] := front;
          bounds[BACK] := back;
          bounds[SIZE] := size;

          rehash(map, hashUtils);

          dataOpt := map[DATA];

          switch (dataOpt) {
            case (?data) {
              front := data.3[FRONT];
              back := data.3[BACK];
              capacity := nat32(data.0.size());
            };

            case (_) trap("unreachable");
          };
        };

        break loopBody;
      } else {
        let key = keys[index];

        if (hashUtils.1(switch (key) { case (?key) key; case (_) trap("unreachable") }, item.0)) {
          let index32 = nat32(index);

          if (index32 == (front +% 1) % capacity) {
            data.1[index] := ?item.1;
          } else {
            let frontNat = nat(front);

            front := (front -% 1) % capacity;

            keys[frontNat] := key;
            data.1[frontNat] := ?item.1;
            indexes[frontNat] := indexes[index];
            keys[index] := null;
            data.1[index] := null;

            indexes[if (prevIndex == NULL) hashIndex else prevIndex] := frontNat;

            let prevBack = (back -% 1) % capacity;

            if (index32 == prevBack) {
              back := prevBack;

              while (switch (keys[nat((back -% 1) % capacity)]) { case (null) true; case (_) false }) {
                back := (back -% 1) % capacity;
              };
            } else if (front == prevBack) {
              let bounds = data.3;

              bounds[FRONT] := front;
              bounds[BACK] := back;
              bounds[SIZE] := size;

              rehash(map, hashUtils);

              dataOpt := map[DATA];

              switch (dataOpt) {
                case (?data) {
                  back := data.3[BACK];
                  front := data.3[FRONT];
                  capacity := nat32(data.0.size());
                };

                case (_) trap("unreachable");
              };
            };
          };

          break loopBody;
        } else {
          prevIndex := index;
          index := indexes[index];
        };
      };
    };

    switch (dataOpt) {
      case (?data) {
        let bounds = data.3;

        bounds[FRONT] := front;
        bounds[BACK] := back;
        bounds[SIZE] := size;
      };

      case (_) {};
    };

    map;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func fromIterMap<K, V, T>(iter: IterNext<T>, hashUtils: HashUtils<K>, mapItem: (T) -> ?(K, V)): Map<K, V> {
    let map = [var null]:Map<K, V>;

    var dataOpt = map[DATA];
    var front = 1:Nat32;
    var back = 1:Nat32;
    var size = 1:Nat32;
    var capacity = 2:Nat32;

    for (item in iter) label loopBody switch (mapItem(item)) {
      case (?item) {
        let data = switch (dataOpt) {
          case (?data) data;

          case (_) {
            ignore init(map, hashUtils, item.0, ?item.1);

            dataOpt := map[DATA];

            break loopBody;
          };
        };

        let keys = data.0;
        let hashIndex = nat(hashUtils.0(item.0) % capacity +% capacity);

        let indexes = data.2;
        let firstIndex = indexes[hashIndex];
        var index = firstIndex;
        var prevIndex = NULL;

        loop if (index == NULL) {
          let oldBack = back;
          let backNat = nat(oldBack);

          back := (back +% 1) % capacity;
          size +%= 1;

          keys[backNat] := ?item.0;
          data.1[backNat] := ?item.1;
          indexes[backNat] := firstIndex;
          indexes[hashIndex] := backNat;

          if (oldBack == front) {
            let bounds = data.3;

            bounds[FRONT] := front;
            bounds[BACK] := back;
            bounds[SIZE] := size;

            rehash(map, hashUtils);

            dataOpt := map[DATA];

            switch (dataOpt) {
              case (?data) {
                front := data.3[FRONT];
                back := data.3[BACK];
                capacity := nat32(data.0.size());
              };

              case (_) trap("unreachable");
            };
          };

          break loopBody;
        } else {
          let key = keys[index];

          if (hashUtils.1(switch (key) { case (?key) key; case (_) trap("unreachable") }, item.0)) {
            let index32 = nat32(index);

            if (index32 == (back -% 1) % capacity) {
              data.1[index] := ?item.1;
            } else {
              let backNat = nat(back);

              back := (back +% 1) % capacity;

              keys[backNat] := key;
              data.1[backNat] := ?item.1;
              indexes[backNat] := indexes[index];
              keys[index] := null;
              data.1[index] := null;

              indexes[if (prevIndex == NULL) hashIndex else prevIndex] := backNat;

              let prevFront = (front +% 1) % capacity;

              if (index32 == prevFront) {
                front := prevFront;

                while (switch (keys[nat((front +% 1) % capacity)]) { case (null) true; case (_) false }) {
                  front := (front +% 1) % capacity;
                };
              } else if (back == prevFront) {
                let bounds = data.3;

                bounds[FRONT] := front;
                bounds[BACK] := back;
                bounds[SIZE] := size;

                rehash(map, hashUtils);

                dataOpt := map[DATA];

                switch (dataOpt) {
                  case (?data) {
                    front := data.3[FRONT];
                    back := data.3[BACK];
                    capacity := nat32(data.0.size());
                  };

                  case (_) trap("unreachable");
                };
              };
            };

            break loopBody;
          } else {
            prevIndex := index;
            index := indexes[index];
          };
        };
      };

      case (_) {};
    };

    switch (dataOpt) {
      case (?data) {
        let bounds = data.3;

        bounds[FRONT] := front;
        bounds[BACK] := back;
        bounds[SIZE] := size;
      };

      case (_) {};
    };

    map;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func fromIterMapDesc<K, V, T>(iter: IterNext<T>, hashUtils: HashUtils<K>, mapItem: (T) -> ?(K, V)): Map<K, V> {
    let map = [var null]:Map<K, V>;

    var dataOpt = map[DATA];
    var front = 0:Nat32;
    var back = 0:Nat32;
    var size = 1:Nat32;
    var capacity = 2:Nat32;

    for (item in iter) label loopBody switch (mapItem(item)) {
      case (?item) {
        let data = switch (dataOpt) {
          case (?data) data;

          case (_) {
            ignore initFront(map, hashUtils, item.0, ?item.1);

            dataOpt := map[DATA];

            break loopBody;
          };
        };

        let keys = data.0;
        let hashIndex = nat(hashUtils.0(item.0) % capacity +% capacity);

        let indexes = data.2;
        let firstIndex = indexes[hashIndex];
        var index = firstIndex;
        var prevIndex = NULL;

        loop if (index == NULL) {
          let oldFront = front;
          let frontNat = nat(oldFront);

          front := (front -% 1) % capacity;
          size +%= 1;

          keys[frontNat] := ?item.0;
          data.1[frontNat] := ?item.1;
          indexes[frontNat] := firstIndex;
          indexes[hashIndex] := frontNat;

          if (oldFront == back) {
            let bounds = data.3;

            bounds[FRONT] := front;
            bounds[BACK] := back;
            bounds[SIZE] := size;

            rehash(map, hashUtils);

            dataOpt := map[DATA];

            switch (dataOpt) {
              case (?data) {
                front := data.3[FRONT];
                back := data.3[BACK];
                capacity := nat32(data.0.size());
              };

              case (_) trap("unreachable");
            };
          };

          break loopBody;
        } else {
          let key = keys[index];

          if (hashUtils.1(switch (key) { case (?key) key; case (_) trap("unreachable") }, item.0)) {
            let index32 = nat32(index);

            if (index32 == (front +% 1) % capacity) {
              data.1[index] := ?item.1;
            } else {
              let frontNat = nat(front);

              front := (front -% 1) % capacity;

              keys[frontNat] := key;
              data.1[frontNat] := ?item.1;
              indexes[frontNat] := indexes[index];
              keys[index] := null;
              data.1[index] := null;

              indexes[if (prevIndex == NULL) hashIndex else prevIndex] := frontNat;

              let prevBack = (back -% 1) % capacity;

              if (index32 == prevBack) {
                back := prevBack;

                while (switch (keys[nat((back -% 1) % capacity)]) { case (null) true; case (_) false }) {
                  back := (back -% 1) % capacity;
                };
              } else if (front == prevBack) {
                let bounds = data.3;

                bounds[FRONT] := front;
                bounds[BACK] := back;
                bounds[SIZE] := size;

                rehash(map, hashUtils);

                dataOpt := map[DATA];

                switch (dataOpt) {
                  case (?data) {
                    back := data.3[BACK];
                    front := data.3[FRONT];
                    capacity := nat32(data.0.size());
                  };

                  case (_) trap("unreachable");
                };
              };
            };

            break loopBody;
          } else {
            prevIndex := index;
            index := indexes[index];
          };
        };
      };

      case (_) {};
    };

    switch (dataOpt) {
      case (?data) {
        let bounds = data.3;

        bounds[FRONT] := front;
        bounds[BACK] := back;
        bounds[SIZE] := size;
      };

      case (_) {};
    };

    map;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func toArray<K, V>(map: Map<K, V>): [(K, V)] {
    let data = switch (map[DATA]) { case (?data) data; case (_) return [] };

    let capacity = nat32(data.0.size());
    var index = data.3[FRONT];

    tabulateArray<(K, V)>(nat(data.3[SIZE]), func(i) = loop {
      index := (index +% 1) % capacity;

      let indexNat = nat(index);

      switch (data.0[indexNat]) {
        case (?key) return (key, switch (data.1[indexNat]) { case (?value) value; case (_) trap("unreachable") });
        case (_) {};
      };
    });
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func toArrayDesc<K, V>(map: Map<K, V>): [(K, V)] {
    let data = switch (map[DATA]) { case (?data) data; case (_) return [] };

    let capacity = nat32(data.0.size());
    var index = data.3[BACK];

    tabulateArray<(K, V)>(nat(data.3[SIZE]), func(i) = loop {
      index := (index -% 1) % capacity;

      let indexNat = nat(index);

      switch (data.0[indexNat]) {
        case (?key) return (key, switch (data.1[indexNat]) { case (?value) value; case (_) trap("unreachable") });
        case (_) {};
      };
    });
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func toArrayMap<K, V, T>(map: Map<K, V>, mapEntry: (K, V) -> ?T): [T] {
    let data = switch (map[DATA]) { case (?data) data; case (_) return [] };

    let values = data.1;
    let keys = data.0;
    let capacity = nat32(keys.size());

    var array = [var null, null]:[var ?T];
    var arraySize = 2:Nat32;
    var arrayIndex = 0:Nat32;

    let lastIndex = data.3[BACK];
    var index = (data.3[FRONT] +% 1) % capacity;

    while (index != lastIndex) {
      let indexNat = nat(index);

      switch (keys[indexNat]) {
        case (?someKey) {
          switch (mapEntry(someKey, switch (values[indexNat]) { case (?value) value; case (_) trap("unreachable") })) {
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
        };

        case (_) {};
      };

      index := (index +% 1) % capacity;
    };

    tabulateArray<T>(nat(arrayIndex), func(i) = switch (array[i]) { case (?item) item; case (_) trap("unreachable") });
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func toArrayMapDesc<K, V, T>(map: Map<K, V>, mapEntry: (K, V) -> ?T): [T] {
    let data = switch (map[DATA]) { case (?data) data; case (_) return [] };

    let values = data.1;
    let keys = data.0;
    let capacity = nat32(keys.size());

    var array = [var null, null]:[var ?T];
    var arraySize = 2:Nat32;
    var arrayIndex = 0:Nat32;

    let lastIndex = data.3[FRONT];
    var index = (data.3[BACK] -% 1) % capacity;

    while (index != lastIndex) {
      let indexNat = nat(index);

      switch (keys[indexNat]) {
        case (?someKey) {
          switch (mapEntry(someKey, switch (values[indexNat]) { case (?value) value; case (_) trap("unreachable") })) {
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
        };

        case (_) {};
      };

      index := (index -% 1) % capacity;
    };

    tabulateArray<T>(nat(arrayIndex), func(i) = switch (array[i]) { case (?item) item; case (_) trap("unreachable") });
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func hashInt(key: Int): Nat32 {
    var hash = Prim.intToNat64Wrap(key);

    hash := hash >> 30 ^ hash *% 0xbf58476d1ce4e5b9;
    hash := hash >> 27 ^ hash *% 0x94d049bb133111eb;

    Prim.intToNat32Wrap(Prim.nat64ToNat(hash >> 31 ^ hash & 0x3fffffff));
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func hashInt8(key: Int8): Nat32 {
    var hash = Prim.intToNat32Wrap(Prim.int8ToInt(key));

    hash := hash >> 16 ^ hash *% 0x21f0aaad;
    hash := hash >> 15 ^ hash *% 0x735a2d97;

    hash >> 15 ^ hash & 0x3fffffff;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func hashInt16(key: Int16): Nat32 {
    var hash = Prim.intToNat32Wrap(Prim.int16ToInt(key));

    hash := hash >> 16 ^ hash *% 0x21f0aaad;
    hash := hash >> 15 ^ hash *% 0x735a2d97;

    hash >> 15 ^ hash & 0x3fffffff;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func hashInt32(key: Int32): Nat32 {
    var hash = Prim.int32ToNat32(key);

    hash := hash >> 16 ^ hash *% 0x21f0aaad;
    hash := hash >> 15 ^ hash *% 0x735a2d97;

    hash >> 15 ^ hash & 0x3fffffff;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

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

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func hashNat8(key: Nat8): Nat32 {
    var hash = Prim.intToNat32Wrap(Prim.nat8ToNat(key));

    hash := hash >> 16 ^ hash *% 0x21f0aaad;
    hash := hash >> 15 ^ hash *% 0x735a2d97;

    hash >> 15 ^ hash & 0x3fffffff;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func hashNat16(key: Nat16): Nat32 {
    var hash = Prim.intToNat32Wrap(Prim.nat16ToNat(key));

    hash := hash >> 16 ^ hash *% 0x21f0aaad;
    hash := hash >> 15 ^ hash *% 0x735a2d97;

    hash >> 15 ^ hash & 0x3fffffff;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func hashNat32(key: Nat32): Nat32 {
    var hash = key;

    hash := hash >> 16 ^ hash *% 0x21f0aaad;
    hash := hash >> 15 ^ hash *% 0x735a2d97;

    hash >> 15 ^ hash & 0x3fffffff;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

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

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func hashPrincipal(key: Principal): Nat32 {
    Prim.hashBlob(Prim.blobOfPrincipal(key)) & 0x3fffffff;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func hashBlob(key: Blob): Nat32 {
    Prim.hashBlob(key) & 0x3fffffff;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

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

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func calcHash<K>(hashUtils: HashUtils<K>, key: K): HashUtils<K> {
    let hash = hashUtils.0(key);

    (func(key) = hash, hashUtils.1);
  };
};
