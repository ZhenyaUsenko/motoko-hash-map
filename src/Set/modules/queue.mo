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

  public func peek<K>(map: Set<K>): ?K {
    let data = switch (map[DATA]) { case (?data) data; case (_) return null };

    switch (data.0[nat((data.2[BACK] -% 1) % nat32(data.0.size()))]) { case (null) null; case (key) key };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func peekFront<K>(map: Set<K>): ?K {
    let data = switch (map[DATA]) { case (?data) data; case (_) return null };

    switch (data.0[nat((data.2[FRONT] +% 1) % nat32(data.0.size()))]) { case (null) null; case (key) key };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func pop<K>(map: Set<K>, hashUtils: HashUtils<K>): ?K {
    let data = switch (map[DATA]) { case (?data) data; case (_) return null };

    let keys = data.0;
    let capacity = nat32(keys.size());

    let bounds = data.2;
    var back = (bounds[BACK] -% 1) % capacity;
    let backNat = nat(back);
    let targetKeyOpt = keys[backNat];

    let targetKey = switch (targetKeyOpt) { case (?key) key; case (_) trap("unreachable") };

    let hashIndex = nat(hashUtils.0(targetKey) % capacity +% capacity);
    let indexes = data.1;
    var index = indexes[hashIndex];
    var prevIndex = NULL;

    loop if (index == NULL) {
      return null;
    } else if (index == backNat) {
      let newSize = bounds[SIZE] -% 1;

      bounds[SIZE] := newSize;

      keys[index] := null;

      if (prevIndex == NULL) indexes[hashIndex] := indexes[index] else indexes[prevIndex] := indexes[index];

      if (newSize < (capacity *% 3 +% 2) / 8) {
        rehash(map, hashUtils);
      } else {
        while (switch (keys[nat((back -% 1) % capacity)]) { case (null) true; case (_) false }) {
          back := (back -% 1) % capacity;
        };

        bounds[BACK] := back;
      };

      return targetKeyOpt;
    } else {
      prevIndex := index;
      index := indexes[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func popFront<K>(map: Set<K>, hashUtils: HashUtils<K>): ?K {
    let data = switch (map[DATA]) { case (?data) data; case (_) return null };

    let keys = data.0;
    let capacity = nat32(keys.size());

    let bounds = data.2;
    var front = (bounds[FRONT] +% 1) % capacity;
    let frontNat = nat(front);
    let targetKeyOpt = keys[frontNat];

    let targetKey = switch (targetKeyOpt) { case (?key) key; case (_) trap("unreachable") };

    let hashIndex = nat(hashUtils.0(targetKey) % capacity +% capacity);
    let indexes = data.1;
    var index = indexes[hashIndex];
    var prevIndex = NULL;

    loop if (index == NULL) {
      return null;
    } else if (index == frontNat) {
      let newSize = bounds[SIZE] -% 1;

      bounds[SIZE] := newSize;

      keys[index] := null;

      if (prevIndex == NULL) indexes[hashIndex] := indexes[index] else indexes[prevIndex] := indexes[index];

      if (newSize < (capacity *% 3 +% 2) / 8) {
        rehash(map, hashUtils);
      } else {
        while (switch (keys[nat((front +% 1) % capacity)]) { case (null) true; case (_) false }) {
          front := (front +% 1) % capacity;
        };

        bounds[FRONT] := front;
      };

      return targetKeyOpt;
    } else {
      prevIndex := index;
      index := indexes[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func cycle<K>(map: Set<K>, hashUtils: HashUtils<K>): ?K {
    let data = switch (map[DATA]) { case (?data) data; case (_) return null };

    let keys = data.0;
    let capacity = nat32(keys.size());

    let bounds = data.2;
    var back = (bounds[BACK] -% 1) % capacity;
    let backNat = nat(back);
    let targetKeyOpt = keys[backNat];

    let targetKey = switch (targetKeyOpt) { case (?key) key; case (_) trap("unreachable") };

    let hashIndex = nat(hashUtils.0(targetKey) % capacity +% capacity);
    let indexes = data.1;
    var index = indexes[hashIndex];
    var prevIndex = NULL;

    loop if (index == NULL) {
      return null;
    } else if (index == backNat) {
      let front = bounds[FRONT];
      let frontNat = nat(front);

      bounds[FRONT] := (front -% 1) % capacity;

      keys[frontNat] := targetKeyOpt;
      indexes[frontNat] := indexes[index];
      keys[index] := null;

      if (prevIndex == NULL) indexes[hashIndex] := frontNat else indexes[prevIndex] := frontNat;

      while (switch (keys[nat((back -% 1) % capacity)]) { case (null) true; case (_) false }) {
        back := (back -% 1) % capacity;
      };

      bounds[BACK] := back;

      return targetKeyOpt;
    } else {
      prevIndex := index;
      index := indexes[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func cycleFront<K>(map: Set<K>, hashUtils: HashUtils<K>): ?K {
    let data = switch (map[DATA]) { case (?data) data; case (_) return null };

    let keys = data.0;
    let capacity = nat32(keys.size());

    let bounds = data.2;
    var front = (bounds[FRONT] +% 1) % capacity;
    let frontNat = nat(front);
    let targetKeyOpt = keys[frontNat];

    let targetKey = switch (targetKeyOpt) { case (?key) key; case (_) trap("unreachable") };

    let hashIndex = nat(hashUtils.0(targetKey) % capacity +% capacity);
    let indexes = data.1;
    var index = indexes[hashIndex];
    var prevIndex = NULL;

    loop if (index == NULL) {
      return null;
    } else if (index == frontNat) {
      let back = bounds[BACK];
      let backNat = nat(back);

      bounds[BACK] := (back +% 1) % capacity;

      keys[backNat] := targetKeyOpt;
      indexes[backNat] := indexes[index];
      keys[index] := null;

      if (prevIndex == NULL) indexes[hashIndex] := backNat else indexes[prevIndex] := backNat;

      while (switch (keys[nat((front +% 1) % capacity)]) { case (null) true; case (_) false }) {
        front := (front +% 1) % capacity;
      };

      bounds[FRONT] := front;

      return targetKeyOpt;
    } else {
      prevIndex := index;
      index := indexes[index];
    };
  };
};
