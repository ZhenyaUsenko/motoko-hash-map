import Const "../const";
import Types "../types";
import { rehash } "./rehash";
import { natToNat32 = nat32; nat32ToNat = nat; trap } "mo:prim";

module {
  type Map<K, V> = Types.Map<K, V>;

  type HashUtils<K> = Types.HashUtils<K>;

  let DATA = Const.DATA;

  let FRONT = Const.FRONT;

  let BACK = Const.BACK;

  let SIZE = Const.SIZE;

  let NULL = Const.NULL;

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

  public func pop<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>): ?(K, V) {
    let data = switch (map[DATA]) { case (?data) data; case (_) return null };

    let keys = data.0;
    let capacity = nat32(keys.size());

    let bounds = data.3;
    var back = (bounds[BACK] -% 1) % capacity;
    let backNat = nat(back);

    let targetKey = switch (keys[backNat]) { case (?key) key; case (_) trap("unreachable") };

    let hashIndex = nat(hashUtils.0(targetKey) % capacity +% capacity);
    let indexes = data.2;
    var index = indexes[hashIndex];
    var prevIndex = NULL;

    loop if (index == NULL) {
      return null;
    } else if (index == backNat) {
      let value = data.1[index];
      let newSize = bounds[SIZE] -% 1;

      bounds[SIZE] := newSize;

      keys[index] := null;
      data.1[index] := null;

      if (prevIndex == NULL) indexes[hashIndex] := indexes[index] else indexes[prevIndex] := indexes[index];

      if (newSize < (capacity *% 3 +% 2) / 8) {
        rehash(map, hashUtils);
      } else {
        while (switch (keys[nat((back -% 1) % capacity)]) { case (null) true; case (_) false }) {
          back := (back -% 1) % capacity;
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

    let bounds = data.3;
    var front = (bounds[FRONT] +% 1) % capacity;
    let frontNat = nat(front);

    let targetKey = switch (keys[frontNat]) { case (?key) key; case (_) trap("unreachable") };

    let hashIndex = nat(hashUtils.0(targetKey) % capacity +% capacity);
    let indexes = data.2;
    var index = indexes[hashIndex];
    var prevIndex = NULL;

    loop if (index == NULL) {
      return null;
    } else if (index == frontNat) {
      let value = data.1[index];
      let newSize = bounds[SIZE] -% 1;

      bounds[SIZE] := newSize;

      keys[index] := null;
      data.1[index] := null;

      if (prevIndex == NULL) indexes[hashIndex] := indexes[index] else indexes[prevIndex] := indexes[index];

      if (newSize < (capacity *% 3 +% 2) / 8) {
        rehash(map, hashUtils);
      } else {
        while (switch (keys[nat((front +% 1) % capacity)]) { case (null) true; case (_) false }) {
          front := (front +% 1) % capacity;
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

    let bounds = data.3;
    var back = (bounds[BACK] -% 1) % capacity;
    let backNat = nat(back);
    let targetKeyOpt = keys[backNat];

    let targetKey = switch (targetKeyOpt) { case (?key) key; case (_) trap("unreachable") };

    let hashIndex = nat(hashUtils.0(targetKey) % capacity +% capacity);
    let indexes = data.2;
    var index = indexes[hashIndex];
    var prevIndex = NULL;

    loop if (index == NULL) {
      return null;
    } else if (index == backNat) {
      let values = data.1;
      let value = values[index];
      let front = bounds[FRONT];
      let frontNat = nat(front);

      bounds[FRONT] := (front -% 1) % capacity;

      keys[frontNat] := targetKeyOpt;
      values[frontNat] := value;
      indexes[frontNat] := indexes[index];
      keys[index] := null;
      values[index] := null;

      if (prevIndex == NULL) indexes[hashIndex] := frontNat else indexes[prevIndex] := frontNat;

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

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func cycleFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>): ?(K, V) {
    let data = switch (map[DATA]) { case (?data) data; case (_) return null };

    let keys = data.0;
    let capacity = nat32(keys.size());

    let bounds = data.3;
    var front = (bounds[FRONT] +% 1) % capacity;
    let frontNat = nat(front);
    let targetKeyOpt = keys[frontNat];

    let targetKey = switch (targetKeyOpt) { case (?key) key; case (_) trap("unreachable") };

    let hashIndex = nat(hashUtils.0(targetKey) % capacity +% capacity);
    let indexes = data.2;
    var index = indexes[hashIndex];
    var prevIndex = NULL;

    loop if (index == NULL) {
      return null;
    } else if (index == frontNat) {
      let values = data.1;
      let value = values[index];
      let back = bounds[BACK];
      let backNat = nat(back);

      bounds[BACK] := (back +% 1) % capacity;

      keys[backNat] := targetKeyOpt;
      values[backNat] := value;
      indexes[backNat] := indexes[index];
      keys[index] := null;
      values[index] := null;

      if (prevIndex == NULL) indexes[hashIndex] := backNat else indexes[prevIndex] := backNat;

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
