import Const "../const";
import Types "../types";
import { Array_tabulate = tabulateArray; Array_init = initArray; natToNat32 = nat32; nat32ToNat = nat; trap } "mo:prim";

module {
  type Map<K, V> = Types.Map<K, V>;

  let DATA = Const.DATA;

  let FRONT = Const.FRONT;

  let BACK = Const.BACK;

  let SIZE = Const.SIZE;

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
};
