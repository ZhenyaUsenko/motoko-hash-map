import Const "../const";
import Types "../types";
import { natToNat32 = nat32; nat32ToNat = nat; trap } "mo:prim";

module {
  type Set<K> = Types.Set<K>;

  type HashUtils<K> = Types.HashUtils<K>;

  let DATA = Const.DATA;

  let NULL = Const.NULL;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func has<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    let data = switch (map[DATA]) { case (?data) data; case (_) return false };

    let keys = data.0;
    let capacity = nat32(keys.size());
    var index = data.1[nat(hashUtils.0(keyParam) % capacity +% capacity)];

    loop if (index == NULL) {
      return false;
    } else if (hashUtils.1(switch (keys[index]) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
      return true;
    } else {
      index := data.1[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func contains<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): ?Bool {
    let data = switch (map[DATA]) { case (?data) data; case (_) return null };

    let keys = data.0;
    let capacity = nat32(keys.size());
    var index = data.1[nat(hashUtils.0(keyParam) % capacity +% capacity)];

    loop if (index == NULL) {
      return ?false;
    } else if (hashUtils.1(switch (keys[index]) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
      return ?true;
    } else {
      index := data.1[index];
    };
  };
};
