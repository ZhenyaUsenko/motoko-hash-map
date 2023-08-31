import Const "../const";
import Types "../types";
import { rehash } "./rehash";
import { natToNat32 = nat32; nat32ToNat = nat; trap } "mo:prim";

module {
  type Set<K> = Types.Set<K>;

  type HashUtils<K> = Types.HashUtils<K>;

  let DATA = Const.DATA;

  let FRONT = Const.FRONT;

  let BACK = Const.BACK;

  let SIZE = Const.SIZE;

  let NULL = Const.NULL;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func remove<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    let data = switch (map[DATA]) { case (?data) data; case (_) return false };

    let keys = data.0;
    let capacity = nat32(keys.size());
    let hashIndex = nat(hashUtils.0(keyParam) % capacity +% capacity);

    let indexes = data.1;
    var index = indexes[hashIndex];
    var prevIndex = NULL;

    loop if (index == NULL) {
      return false;
    } else if (hashUtils.1(switch (keys[index]) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
      let bounds = data.2;
      let newSize = bounds[SIZE] -% 1;

      bounds[SIZE] := newSize;

      keys[index] := null;

      if (prevIndex == NULL) indexes[hashIndex] := indexes[index] else indexes[prevIndex] := indexes[index];

      if (newSize < (capacity *% 3 +% 2) / 8) {
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

      return true;
    } else {
      prevIndex := index;
      index := indexes[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func delete<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K) {
    let data = switch (map[DATA]) { case (?data) data; case (_) return };

    let keys = data.0;
    let capacity = nat32(keys.size());
    let hashIndex = nat(hashUtils.0(keyParam) % capacity +% capacity);

    let indexes = data.1;
    var index = indexes[hashIndex];
    var prevIndex = NULL;

    loop if (index == NULL) {
      return;
    } else if (hashUtils.1(switch (keys[index]) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
      let bounds = data.2;
      let newSize = bounds[SIZE] -% 1;

      bounds[SIZE] := newSize;

      keys[index] := null;

      if (prevIndex == NULL) indexes[hashIndex] := indexes[index] else indexes[prevIndex] := indexes[index];

      if (newSize < (capacity *% 3 +% 2) / 8) {
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
};
