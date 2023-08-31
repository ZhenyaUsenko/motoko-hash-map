import Const "../const";
import Types "../types";
import { natToNat32 = nat32; nat32ToNat = nat } "mo:prim";

module {
  type Set<K> = Types.Set<K>;

  let DATA = Const.DATA;

  let FRONT = Const.FRONT;

  let BACK = Const.BACK;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func find<K>(map: Set<K>, acceptEntry: (K) -> Bool): ?K {
    let data = switch (map[DATA]) { case (?data) data; case (_) return null };

    let keys = data.0;
    let capacity = nat32(keys.size());

    let lastIndex = data.2[BACK];
    var index = (data.2[FRONT] +% 1) % capacity;

    while (index != lastIndex) {
      switch (keys[nat(index)]) {
        case (?someKey) if (acceptEntry(someKey)) return ?(someKey);
        case (_) {};
      };

      index := (index +% 1) % capacity;
    };

    null;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func findDesc<K>(map: Set<K>, acceptEntry: (K) -> Bool): ?K {
    let data = switch (map[DATA]) { case (?data) data; case (_) return null };

    let keys = data.0;
    let capacity = nat32(keys.size());

    let lastIndex = data.2[FRONT];
    var index = (data.2[BACK] -% 1) % capacity;

    while (index != lastIndex) {
      switch (keys[nat(index)]) {
        case (?someKey) if (acceptEntry(someKey)) return ?(someKey);
        case (_) {};
      };

      index := (index -% 1) % capacity;
    };

    null;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func some<K>(map: Set<K>, acceptEntry: (K) -> Bool): Bool {
    let data = switch (map[DATA]) { case (?data) data; case (_) return false };

    let keys = data.0;
    let capacity = nat32(keys.size());

    let lastIndex = data.2[BACK];
    var index = (data.2[FRONT] +% 1) % capacity;

    while (index != lastIndex) {
      let indexNat = nat(index);

      switch (keys[indexNat]) {
        case (?someKey) if (acceptEntry(someKey)) return true;
        case (_) {};
      };

      index := (index +% 1) % capacity;
    };

    false;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func someDesc<K>(map: Set<K>, acceptEntry: (K) -> Bool): Bool {
    let data = switch (map[DATA]) { case (?data) data; case (_) return false };

    let keys = data.0;
    let capacity = nat32(keys.size());

    let lastIndex = data.2[FRONT];
    var index = (data.2[BACK] -% 1) % capacity;

    while (index != lastIndex) {
      switch (keys[nat(index)]) {
        case (?someKey) if (acceptEntry(someKey)) return true;
        case (_) {};
      };

      index := (index -% 1) % capacity;
    };

    false;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func every<K>(map: Set<K>, acceptEntry: (K) -> Bool): Bool {
    let data = switch (map[DATA]) { case (?data) data; case (_) return true };

    let keys = data.0;
    let capacity = nat32(keys.size());

    let lastIndex = data.2[BACK];
    var index = (data.2[FRONT] +% 1) % capacity;

    while (index != lastIndex) {
      switch (keys[nat(index)]) {
        case (?someKey) if (not acceptEntry(someKey)) return false;
        case (_) {};
      };

      index := (index +% 1) % capacity;
    };

    true;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func everyDesc<K>(map: Set<K>, acceptEntry: (K) -> Bool): Bool {
    let data = switch (map[DATA]) { case (?data) data; case (_) return true };

    let keys = data.0;
    let capacity = nat32(keys.size());

    let lastIndex = data.2[FRONT];
    var index = (data.2[BACK] -% 1) % capacity;

    while (index != lastIndex) {

      switch (keys[nat(index)]) {
        case (?someKey) if (not acceptEntry(someKey)) return false;
        case (_) {};
      };

      index := (index -% 1) % capacity;
    };

    true;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func forEach<K>(map: Set<K>, mapEntry: (K) -> ()) {
    let data = switch (map[DATA]) { case (?data) data; case (_) return };

    let keys = data.0;
    let capacity = nat32(keys.size());

    let lastIndex = data.2[BACK];
    var index = (data.2[FRONT] +% 1) % capacity;

    while (index != lastIndex) {
      switch (keys[nat(index)]) {
        case (?someKey) mapEntry(someKey);
        case (_) {};
      };

      index := (index +% 1) % capacity;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func forEachDesc<K>(map: Set<K>, mapEntry: (K) -> ()) {
    let data = switch (map[DATA]) { case (?data) data; case (_) return };

    let keys = data.0;
    let capacity = nat32(keys.size());

    let lastIndex = data.2[FRONT];
    var index = (data.2[BACK] -% 1) % capacity;

    while (index != lastIndex) {
      switch (keys[nat(index)]) {
        case (?someKey) mapEntry(someKey);
        case (_) {};
      };

      index := (index -% 1) % capacity;
    };
  };
};
