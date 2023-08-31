import Const "../const";
import Types "../types";
import { putMove } "./put";
import { Array_init = initArray; natToNat32 = nat32; nat32ToNat = nat } "mo:prim";

module {
  type Set<K> = Types.Set<K>;

  type HashUtils<K> = Types.HashUtils<K>;

  let DATA = Const.DATA;

  let FRONT = Const.FRONT;

  let BACK = Const.BACK;

  let SIZE = Const.SIZE;

  let NULL = Const.NULL;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func filter<K>(map: Set<K>, hashUtils: HashUtils<K>, acceptEntry: (K) -> Bool): Set<K> {
    let data = switch (map[DATA]) { case (?data) data; case (_) return [var null] };

    let capacity = nat32(data.0.size());
    let lastHashIndex = capacity -% 1;

    let newMap = [var null]:Set<K>;

    let lastIndex = data.2[BACK];
    var index = (data.2[FRONT] +% 1) % capacity;

    while (index != lastIndex) {
      switch (data.0[nat(index)]) {
        case (?someKey) if (acceptEntry(someKey)) ignore putMove(newMap, hashUtils, someKey);
        case (_) {};
      };

      index := (index +% 1) % capacity;
    };

    newMap;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func filterDesc<K>(map: Set<K>, hashUtils: HashUtils<K>, acceptEntry: (K) -> Bool): Set<K> {
    let data = switch (map[DATA]) { case (?data) data; case (_) return [var null] };

    let capacity = nat32(data.0.size());
    let lastHashIndex = capacity -% 1;

    let newMap = [var null]:Set<K>;

    let lastIndex = data.2[FRONT];
    var index = (data.2[BACK] -% 1) % capacity;

    while (index != lastIndex) {
      switch (data.0[nat(index)]) {
        case (?someKey) if (acceptEntry(someKey)) ignore putMove(newMap, hashUtils, someKey);

        case (_) {};
      };

      index := (index -% 1) % capacity;
    };

    newMap;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func clone<K>(map: Set<K>): Set<K> {
    let data = switch (map[DATA]) { case (?data) data; case (_) return [var null] };

    let indexes = data.1;
    let keys = data.0;
    let capacity = nat32(keys.size());
    let capacityNat = nat(capacity);

    var lastIndex = capacity *% 2;
    var index = capacity;

    let newKeys = initArray<?K>(capacityNat, null);
    let newIndexes = initArray<Nat>(nat(lastIndex), NULL);

    while (index < lastIndex) {
      let indexNat = nat(index);
      let hashIndex = indexes[indexNat];

      if (hashIndex != NULL) newIndexes[indexNat] := hashIndex;

      index +%= 1;
    };

    let bounds = data.2;
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
        };

        case (_) {};
      };

      index := (index +% 1) % capacity;
    };

    return [var ?(newKeys, newIndexes, [var front, back, bounds[SIZE]])];
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func cloneDesc<K>(map: Set<K>): Set<K> {
    let data = switch (map[DATA]) { case (?data) data; case (_) return [var null] };

    let indexes = data.1;
    let keys = data.0;
    let capacity = nat32(keys.size());
    let capacityNat = nat(capacity);

    let lastHashIndex = capacity -% 1;
    var lastIndex = capacity *% 2;
    var index = capacity;

    let newKeys = initArray<?K>(capacityNat, null);
    let newIndexes = initArray<Nat>(nat(lastIndex), NULL);

    while (index < lastIndex) {
      let indexNat = nat(index);
      let hashIndex = indexes[indexNat];

      if (hashIndex != NULL) newIndexes[indexNat] := nat(lastHashIndex -% nat32(hashIndex));

      index +%= 1;
    };

    let bounds = data.2;
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
        };
      };

      index := (index -% 1) % capacity;
    };

    return [var ?(newKeys, newIndexes, [var lastHashIndex -% back, lastHashIndex -% front, bounds[SIZE]])];
  };
};
