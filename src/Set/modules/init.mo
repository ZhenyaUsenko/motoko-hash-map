import Const "../const";
import Types "../types";
import { nat32ToNat = nat } "mo:prim";

module {
  type Set<K> = Types.Set<K>;

  type HashUtils<K> = Types.HashUtils<K>;

  let DATA = Const.DATA;

  let SIZE = Const.SIZE;

  let NULL = Const.NULL;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func new<K>(): Set<K> {
    [var null];
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func clear<K>(map: Set<K>) {
    map[DATA] := null;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func make<K>(hashUtils: HashUtils<K>, keyParam: K): Set<K> {
    [var ?(
      [var ?keyParam, null],
      if (hashUtils.0(keyParam) % 2 == 0) [var NULL, NULL, 0, NULL] else [var NULL, NULL, NULL, 0],
      [var 1, 1, 1],
    )];
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func init<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    map[DATA] := ?(
      [var ?keyParam, null],
      if (hashUtils.0(keyParam) % 2 == 0) [var NULL, NULL, 0, NULL] else [var NULL, NULL, NULL, 0],
      [var 1, 1, 1],
    );

    false;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func initFront<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: K): Bool {
    map[DATA] := ?(
      [var null, ?keyParam],
      if (hashUtils.0(keyParam) % 2 == 0) [var NULL, NULL, 1, NULL] else [var NULL, NULL, NULL, 1],
      [var 0, 0, 1],
    );

    false;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func size<K>(map: Set<K>): Nat {
    switch (map[DATA]) { case (?data) nat(data.2[SIZE]); case (_) 0 };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func empty<K>(map: Set<K>): Bool {
    switch (map[DATA]) { case (null) true; case (_) false };
  };
};