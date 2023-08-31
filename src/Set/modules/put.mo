import Const "../const";
import Types "../types";
import { rehash } "./rehash";
import { init; initFront } "./init";
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

  public func put<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    let data = switch (map[DATA]) { case (?data) data; case (_) return init(map, hashUtils, keyParam) };

    let keys = data.0;
    let capacity = nat32(keys.size());
    let hashIndex = nat(hashUtils.0(keyParam) % capacity +% capacity);

    let indexes = data.1;
    let firstIndex = indexes[hashIndex];
    var index = firstIndex;

    loop if (index == NULL) {
      let bounds = data.2;
      let back = bounds[BACK];
      let backNat = nat(back);

      bounds[BACK] := (back +% 1) % capacity;
      bounds[SIZE] +%= 1;

      keys[backNat] := ?keyParam;
      indexes[backNat] := firstIndex;
      indexes[hashIndex] := backNat;

      if (back == bounds[FRONT]) rehash(map, hashUtils);

      return false;
    } else if (hashUtils.1(switch (keys[index]) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
      return true;
    } else {
      index := indexes[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func putFront<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    let data = switch (map[DATA]) { case (?data) data; case (_) return initFront(map, hashUtils, keyParam) };

    let keys = data.0;
    let capacity = nat32(keys.size());
    let hashIndex = nat(hashUtils.0(keyParam) % capacity +% capacity);

    let indexes = data.1;
    let firstIndex = indexes[hashIndex];
    var index = firstIndex;

    loop if (index == NULL) {
      let bounds = data.2;
      let front = bounds[FRONT];
      let frontNat = nat(front);

      bounds[FRONT] := (front -% 1) % capacity;
      bounds[SIZE] +%= 1;

      keys[frontNat] := ?keyParam;
      indexes[frontNat] := firstIndex;
      indexes[hashIndex] := frontNat;

      if (front == bounds[BACK]) rehash(map, hashUtils);

      return false;
    } else if (hashUtils.1(switch (keys[index]) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
      return true;
    } else {
      index := indexes[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func add<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K) {
    let data = switch (map[DATA]) { case (?data) data; case (_) return ignore init(map, hashUtils, keyParam) };

    let keys = data.0;
    let capacity = nat32(keys.size());
    let hashIndex = nat(hashUtils.0(keyParam) % capacity +% capacity);

    let indexes = data.1;
    let firstIndex = indexes[hashIndex];
    var index = firstIndex;

    loop if (index == NULL) {
      let bounds = data.2;
      let back = bounds[BACK];
      let backNat = nat(back);

      bounds[BACK] := (back +% 1) % capacity;
      bounds[SIZE] +%= 1;

      keys[backNat] := ?keyParam;
      indexes[backNat] := firstIndex;
      indexes[hashIndex] := backNat;

      if (back == bounds[FRONT]) rehash(map, hashUtils);

      return;
    } else if (hashUtils.1(switch (keys[index]) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
      return;
    } else {
      index := indexes[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func addFront<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K) {
    let data = switch (map[DATA]) { case (?data) data; case (_) return ignore initFront(map, hashUtils, keyParam) };

    let keys = data.0;
    let capacity = nat32(keys.size());
    let hashIndex = nat(hashUtils.0(keyParam) % capacity +% capacity);

    let indexes = data.1;
    let firstIndex = indexes[hashIndex];
    var index = firstIndex;

    loop if (index == NULL) {
      let bounds = data.2;
      let front = bounds[FRONT];
      let frontNat = nat(front);

      bounds[FRONT] := (front -% 1) % capacity;
      bounds[SIZE] +%= 1;

      keys[frontNat] := ?keyParam;
      indexes[frontNat] := firstIndex;
      indexes[hashIndex] := frontNat;

      if (front == bounds[BACK]) rehash(map, hashUtils);

      return;
    } else if (hashUtils.1(switch (keys[index]) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
      return;
    } else {
      index := indexes[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func putMove<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    let data = switch (map[DATA]) { case (?data) data; case (_) return init(map, hashUtils, keyParam) };

    let keys = data.0;
    let capacity = nat32(keys.size());
    let hashIndex = nat(hashUtils.0(keyParam) % capacity +% capacity);

    let indexes = data.1;
    let firstIndex = indexes[hashIndex];
    var index = firstIndex;
    var prevIndex = NULL;

    loop if (index == NULL) {
      let bounds = data.2;
      let back = bounds[BACK];
      let backNat = nat(back);

      bounds[BACK] := (back +% 1) % capacity;
      bounds[SIZE] +%= 1;

      keys[backNat] := ?keyParam;
      indexes[backNat] := firstIndex;
      indexes[hashIndex] := backNat;

      if (back == bounds[FRONT]) rehash(map, hashUtils);

      return false;
    } else {
      let key = keys[index];

      if (hashUtils.1(switch (key) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
        let bounds = data.2;
        let back = bounds[BACK];
        let index32 = nat32(index);

        if (index32 != (back -% 1) % capacity) {
          let backNat = nat(back);

          bounds[BACK] := (back +% 1) % capacity;

          keys[backNat] := key;
          indexes[backNat] := indexes[index];
          keys[index] := null;

          if (prevIndex == NULL) indexes[hashIndex] := backNat else indexes[prevIndex] := backNat;

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

        return true;
      } else {
        prevIndex := index;
        index := indexes[index];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func putMoveFront<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    let data = switch (map[DATA]) { case (?data) data; case (_) return initFront(map, hashUtils, keyParam) };

    let keys = data.0;
    let capacity = nat32(keys.size());
    let hashIndex = nat(hashUtils.0(keyParam) % capacity +% capacity);

    let indexes = data.1;
    let firstIndex = indexes[hashIndex];
    var index = firstIndex;
    var prevIndex = NULL;

    loop if (index == NULL) {
      let bounds = data.2;
      let front = bounds[FRONT];
      let frontNat = nat(front);

      bounds[FRONT] := (front -% 1) % capacity;
      bounds[SIZE] +%= 1;

      keys[frontNat] := ?keyParam;
      indexes[frontNat] := firstIndex;
      indexes[hashIndex] := frontNat;

      if (front == bounds[BACK]) rehash(map, hashUtils);

      return false;
    } else {
      let key = keys[index];

      if (hashUtils.1(switch (key) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
        let bounds = data.2;
        let front = bounds[FRONT];
        let index32 = nat32(index);

        if (index32 != (front +% 1) % capacity) {
          let frontNat = nat(front);

          bounds[FRONT] := (front -% 1) % capacity;

          keys[frontNat] := key;
          indexes[frontNat] := indexes[index];
          keys[index] := null;

          if (prevIndex == NULL) indexes[hashIndex] := frontNat else indexes[prevIndex] := frontNat;

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

        return true;
      } else {
        prevIndex := index;
        index := indexes[index];
      };
    };
  };
};
