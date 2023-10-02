import Const "../const";
import Types "../types";
import { putMove } "./put";
import { Array_init = initArray; natToNat32 = nat32; nat32ToNat = nat; trap } "mo:prim";

module {
  type Map<K, V> = Types.Map<K, V>;

  type HashUtils<K> = Types.HashUtils<K>;

  let DATA = Const.DATA;

  let FRONT = Const.FRONT;

  let BACK = Const.BACK;

  let SIZE = Const.SIZE;

  let NULL = Const.NULL;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func mapFilter<K, V1, V2>(map: Map<K, V1>, hashUtils: HashUtils<K>, mapEntry: (K, V1) -> ?V2): Map<K, V2> {
    let data = switch (map[DATA]) { case (?data) data; case (_) return [var null] };

    let capacity = nat32(data.0.size());

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

    let lastIndex = capacity *% 2;
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
    let lastIndex = capacity *% 2;
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

    let lastIndex = capacity *% 2;
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
    let lastIndex = capacity *% 2;
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
};
