import Const "../const";
import Types "../types";
import { Array_tabulate = tabulateArray; Array_init = initArray; natToNat32 = nat32; nat32ToNat = nat; trap } "mo:prim";

module {
  type Set<K> = Types.Set<K>;

  let DATA = Const.DATA;

  let FRONT = Const.FRONT;

  let BACK = Const.BACK;

  let SIZE = Const.SIZE;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func toArray<K>(map: Set<K>): [K] {
    let data = switch (map[DATA]) { case (?data) data; case (_) return [] };

    let capacity = nat32(data.0.size());
    var index = data.2[FRONT];

    tabulateArray<K>(nat(data.2[SIZE]), func(i) = loop {
      index := (index +% 1) % capacity;

      switch (data.0[nat(index)]) { case (?key) return key; case (null) {} };
    });
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func toArrayDesc<K>(map: Set<K>): [K] {
    let data = switch (map[DATA]) { case (?data) data; case (_) return [] };

    let capacity = nat32(data.0.size());
    var index = data.2[BACK];

    tabulateArray<K>(nat(data.2[SIZE]), func(i) = loop {
      index := (index -% 1) % capacity;

      switch (data.0[nat(index)]) { case (?key) return key; case (_) {} };
    });
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func toArrayMap<K, T>(map: Set<K>, mapEntry: (K) -> ?T): [T] {
    let data = switch (map[DATA]) { case (?data) data; case (_) return [] };

    let keys = data.0;
    let capacity = nat32(keys.size());

    var array = [var null, null]:[var ?T];
    var arraySize = 2:Nat32;
    var arrayIndex = 0:Nat32;

    let lastIndex = data.2[BACK];
    var index = (data.2[FRONT] +% 1) % capacity;

    while (index != lastIndex) {
      switch (keys[nat(index)]) {
        case (?someKey) {
          switch (mapEntry(someKey)) {
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

  public func toArrayMapDesc<K, T>(map: Set<K>, mapEntry: (K) -> ?T): [T] {
    let data = switch (map[DATA]) { case (?data) data; case (_) return [] };

    let keys = data.0;
    let capacity = nat32(keys.size());

    var array = [var null, null]:[var ?T];
    var arraySize = 2:Nat32;
    var arrayIndex = 0:Nat32;

    let lastIndex = data.2[FRONT];
    var index = (data.2[BACK] -% 1) % capacity;

    while (index != lastIndex) {
      switch (keys[nat(index)]) {
        case (?someKey) {
          switch (mapEntry(someKey)) {
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
