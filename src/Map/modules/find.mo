import Const "../const";
import Types "../types";
import { natToNat32 = nat32; nat32ToNat = nat; trap } "mo:prim";

module {
  type Map<K, V> = Types.Map<K, V>;

  let DATA = Const.DATA;

  let FRONT = Const.FRONT;

  let BACK = Const.BACK;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func find<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool): ?(K, V) {
    let data = switch (map[DATA]) { case (?data) data; case (_) return null };

    let values = data.1;
    let keys = data.0;
    let capacity = nat32(keys.size());

    let lastIndex = data.3[BACK];
    var index = (data.3[FRONT] +% 1) % capacity;

    while (index != lastIndex) {
      let indexNat = nat(index);

      switch (keys[indexNat]) {
        case (?someKey) {
          if (acceptEntry(someKey, switch (values[indexNat]) { case (?value) value; case (_) trap("unreachable") })) {
            return ?(someKey, switch (values[indexNat]) { case (?value) value; case (_) trap("unreachable") });
          };
        };

        case (_) {};
      };

      index := (index +% 1) % capacity;
    };

    null;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func findDesc<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool): ?(K, V) {
    let data = switch (map[DATA]) { case (?data) data; case (_) return null };

    let values = data.1;
    let keys = data.0;
    let capacity = nat32(keys.size());

    let lastIndex = data.3[FRONT];
    var index = (data.3[BACK] -% 1) % capacity;

    while (index != lastIndex) {
      let indexNat = nat(index);

      switch (keys[indexNat]) {
        case (?someKey) {
          if (acceptEntry(someKey, switch (values[indexNat]) { case (?value) value; case (_) trap("unreachable") })) {
            return ?(someKey, switch (values[indexNat]) { case (?value) value; case (_) trap("unreachable") });
          };
        };

        case (_) {};
      };

      index := (index -% 1) % capacity;
    };

    null;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func some<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool): Bool {
    let data = switch (map[DATA]) { case (?data) data; case (_) return false };

    let values = data.1;
    let keys = data.0;
    let capacity = nat32(keys.size());

    let lastIndex = data.3[BACK];
    var index = (data.3[FRONT] +% 1) % capacity;

    while (index != lastIndex) {
      let indexNat = nat(index);

      switch (keys[indexNat]) {
        case (?someKey) {
          if (acceptEntry(someKey, switch (values[indexNat]) { case (?value) value; case (_) trap("unreachable") })) {
            return true;
          };
        };

        case (_) {};
      };

      index := (index +% 1) % capacity;
    };

    false;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func someDesc<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool): Bool {
    let data = switch (map[DATA]) { case (?data) data; case (_) return false };

    let values = data.1;
    let keys = data.0;
    let capacity = nat32(keys.size());

    let lastIndex = data.3[FRONT];
    var index = (data.3[BACK] -% 1) % capacity;

    while (index != lastIndex) {
      let indexNat = nat(index);

      switch (keys[indexNat]) {
        case (?someKey) {
          if (acceptEntry(someKey, switch (values[indexNat]) { case (?value) value; case (_) trap("unreachable") })) {
            return true;
          };
        };

        case (_) {};
      };

      index := (index -% 1) % capacity;
    };

    false;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func every<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool): Bool {
    let data = switch (map[DATA]) { case (?data) data; case (_) return true };

    let values = data.1;
    let keys = data.0;
    let capacity = nat32(keys.size());

    let lastIndex = data.3[BACK];
    var index = (data.3[FRONT] +% 1) % capacity;

    while (index != lastIndex) {
      let indexNat = nat(index);

      switch (keys[indexNat]) {
        case (?someKey) {
          if (not acceptEntry(someKey, switch (values[indexNat]) { case (?value) value; case (_) trap("unreachable") })) {
            return false;
          };
        };

        case (_) {};
      };

      index := (index +% 1) % capacity;
    };

    true;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func everyDesc<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool): Bool {
    let data = switch (map[DATA]) { case (?data) data; case (_) return true };

    let values = data.1;
    let keys = data.0;
    let capacity = nat32(keys.size());

    let lastIndex = data.3[FRONT];
    var index = (data.3[BACK] -% 1) % capacity;

    while (index != lastIndex) {
      let indexNat = nat(index);

      switch (keys[indexNat]) {
        case (?someKey) {
          if (not acceptEntry(someKey, switch (values[indexNat]) { case (?value) value; case (_) trap("unreachable") })) {
            return false;
          };
        };

        case (_) {};
      };

      index := (index -% 1) % capacity;
    };

    true;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func forEach<K, V>(map: Map<K, V>, mapEntry: (K, V) -> ()) {
    let data = switch (map[DATA]) { case (?data) data; case (_) return };

    let values = data.1;
    let keys = data.0;
    let capacity = nat32(keys.size());

    let lastIndex = data.3[BACK];
    var index = (data.3[FRONT] +% 1) % capacity;

    while (index != lastIndex) {
      let indexNat = nat(index);

      switch (keys[indexNat]) {
        case (?someKey) mapEntry(someKey, switch (values[indexNat]) { case (?value) value; case (_) trap("unreachable") });
        case (_) {};
      };

      index := (index +% 1) % capacity;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func forEachDesc<K, V>(map: Map<K, V>, mapEntry: (K, V) -> ()) {
    let data = switch (map[DATA]) { case (?data) data; case (_) return };

    let values = data.1;
    let keys = data.0;
    let capacity = nat32(keys.size());

    let lastIndex = data.3[FRONT];
    var index = (data.3[BACK] -% 1) % capacity;

    while (index != lastIndex) {
      let indexNat = nat(index);

      switch (keys[indexNat]) {
        case (?someKey) mapEntry(someKey, switch (values[indexNat]) { case (?value) value; case (_) trap("unreachable") });
        case (_) {};
      };

      index := (index -% 1) % capacity;
    };
  };
};
