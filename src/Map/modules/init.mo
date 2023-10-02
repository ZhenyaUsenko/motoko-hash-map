import Const "../const";
import Types "../types";
import { nat32ToNat = nat } "mo:prim";

module {
  type Map<K, V> = Types.Map<K, V>;

  type HashUtils<K> = Types.HashUtils<K>;

  let DATA = Const.DATA;

  let SIZE = Const.SIZE;

  let NULL = Const.NULL;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func new<K, V>(): Map<K, V> {
    [var null];
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func clear<K, V>(map: Map<K, V>) {
    map[DATA] := null;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func size<K, V>(map: Map<K, V>): Nat {
    switch (map[DATA]) { case (?data) nat(data.3[SIZE]); case (_) 0 };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func empty<K, V>(map: Map<K, V>): Bool {
    switch (map[DATA]) { case (null) true; case (_) false };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func make<K, V>(hashUtils: HashUtils<K>, keyParam: K, valueParam: V): Map<K, V> {
    [var ?(
      [var ?keyParam, null],
      [var ?valueParam, null],
      if (hashUtils.0(keyParam) % 2 == 0) [var NULL, NULL, 0, NULL] else [var NULL, NULL, NULL, 0],
      [var 1, 1, 1],
    )];
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func init<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: ?V): Null {
    switch (valueParam) {
      case (null) {};

      case (_) map[DATA] := ?(
        [var ?keyParam, null],
        [var valueParam, null],
        if (hashUtils.0(keyParam) % 2 == 0) [var NULL, NULL, 0, NULL] else [var NULL, NULL, NULL, 0],
        [var 1, 1, 1],
      );
    };

    null;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func initFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: ?V): Null {
    switch (valueParam) {
      case (null) {};

      case (_) map[DATA] := ?(
        [var null, ?keyParam],
        [var null, valueParam],
        if (hashUtils.0(keyParam) % 2 == 0) [var NULL, NULL, 1, NULL] else [var NULL, NULL, NULL, 1],
        [var 0, 0, 1],
      );
    };

    null;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func initUpdate<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, getNewValue: (K, ?V) -> ?V): ?V {
    switch (getNewValue(keyParam, null)) {
      case (null) null;

      case (valueParam) {
        map[DATA] := ?(
          [var ?keyParam, null],
          [var valueParam, null],
          if (hashUtils.0(keyParam) % 2 == 0) [var NULL, NULL, 0, NULL] else [var NULL, NULL, NULL, 0],
          [var 1, 1, 1],
        );

        valueParam;
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func initUpdateFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, getNewValue: (K, ?V) -> ?V): ?V {
    switch (getNewValue(keyParam, null)) {
      case (null) null;

      case (valueParam) {
        map[DATA] := ?(
          [var null, ?keyParam],
          [var null, valueParam],
          if (hashUtils.0(keyParam) % 2 == 0) [var NULL, NULL, 1, NULL] else [var NULL, NULL, NULL, 1],
          [var 0, 0, 1],
        );

        valueParam;
      };
    };
  };
};
