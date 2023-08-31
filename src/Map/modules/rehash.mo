import Const "../const";
import Types "../types";
import { Array_init = initArray; natToNat32 = nat32; nat32ToNat = nat; clzNat32; nat32ToNat64; nat64ToNat32; trap } "mo:prim";

module {
  type Map<K, V> = Types.Map<K, V>;

  type HashUtils<K> = Types.HashUtils<K>;

  let DATA = Const.DATA;

  let FRONT = Const.FRONT;

  let BACK = Const.BACK;

  let SIZE = Const.SIZE;

  let NULL = Const.NULL;

  let MAX_CAPACITY = Const.MAX_CAPACITY;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func rehash<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>) {
    let data = switch (map[DATA]) { case (?data) data; case (_) return };

    let bounds = data.3;
    let size = bounds[SIZE];

    if (size == 0) return map[DATA] := null;

    let newCapacity = 2 **% (32 -% clzNat32(nat64ToNat32(nat32ToNat64(size) *% 8 / 7)));
    let newCapacityNat = nat(newCapacity);

    if (newCapacity >= MAX_CAPACITY) trap("Map capacity limit reached (2 ** 30)");

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
};
