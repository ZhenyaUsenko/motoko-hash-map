import Prim "mo:prim";
import Types "./types";

module {
  type HashUtils<K> = Types.HashUtils<K>;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func hashInt(key: Int): Nat32 {
    var hash = Prim.intToNat64Wrap(key);

    hash := hash >> 30 ^ hash *% 0xbf58476d1ce4e5b9;
    hash := hash >> 27 ^ hash *% 0x94d049bb133111eb;

    Prim.nat64ToNat32(hash >> 31 ^ hash & 0x3fffffff);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func hashInt8(key: Int8): Nat32 {
    var hash = Prim.nat16ToNat32(Prim.nat8ToNat16(Prim.int8ToNat8(key)));

    hash := hash >> 16 ^ hash *% 0x21f0aaad;
    hash := hash >> 15 ^ hash *% 0x735a2d97;

    hash >> 15 ^ hash & 0x3fffffff;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func hashInt16(key: Int16): Nat32 {
    var hash = Prim.nat16ToNat32(Prim.int16ToNat16(key));

    hash := hash >> 16 ^ hash *% 0x21f0aaad;
    hash := hash >> 15 ^ hash *% 0x735a2d97;

    hash >> 15 ^ hash & 0x3fffffff;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func hashInt32(key: Int32): Nat32 {
    var hash = Prim.int32ToNat32(key);

    hash := hash >> 16 ^ hash *% 0x21f0aaad;
    hash := hash >> 15 ^ hash *% 0x735a2d97;

    hash >> 15 ^ hash & 0x3fffffff;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func hashInt64(key: Int64): Nat32 {
    var hash = Prim.int64ToNat64(key);

    hash := hash >> 30 ^ hash *% 0xbf58476d1ce4e5b9;
    hash := hash >> 27 ^ hash *% 0x94d049bb133111eb;

    Prim.nat64ToNat32(hash >> 31 ^ hash & 0x3fffffff);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func hashNat(key: Nat): Nat32 {
    var hash = Prim.intToNat64Wrap(key);

    hash := hash >> 30 ^ hash *% 0xbf58476d1ce4e5b9;
    hash := hash >> 27 ^ hash *% 0x94d049bb133111eb;

    Prim.nat64ToNat32(hash >> 31 ^ hash & 0x3fffffff);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func hashNat8(key: Nat8): Nat32 {
    var hash = Prim.nat16ToNat32(Prim.nat8ToNat16(key));

    hash := hash >> 16 ^ hash *% 0x21f0aaad;
    hash := hash >> 15 ^ hash *% 0x735a2d97;

    hash >> 15 ^ hash & 0x3fffffff;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func hashNat16(key: Nat16): Nat32 {
    var hash = Prim.nat16ToNat32(key);

    hash := hash >> 16 ^ hash *% 0x21f0aaad;
    hash := hash >> 15 ^ hash *% 0x735a2d97;

    hash >> 15 ^ hash & 0x3fffffff;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func hashNat32(key: Nat32): Nat32 {
    var hash = key;

    hash := hash >> 16 ^ hash *% 0x21f0aaad;
    hash := hash >> 15 ^ hash *% 0x735a2d97;

    hash >> 15 ^ hash & 0x3fffffff;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func hashNat64(key: Nat64): Nat32 {
    var hash = key;

    hash := hash >> 30 ^ hash *% 0xbf58476d1ce4e5b9;
    hash := hash >> 27 ^ hash *% 0x94d049bb133111eb;

    Prim.nat64ToNat32(hash >> 31 ^ hash & 0x3fffffff);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func hashText(key: Text): Nat32 {
    Prim.hashBlob(Prim.encodeUtf8(key)) & 0x3fffffff;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func hashPrincipal(key: Principal): Nat32 {
    Prim.hashBlob(Prim.blobOfPrincipal(key)) & 0x3fffffff;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func hashBlob(key: Blob): Nat32 {
    Prim.hashBlob(key) & 0x3fffffff;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func hashBool(key: Bool): Nat32 {
    if (key) 114489971 else 0;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public let ihash = (hashInt, func(a, b) = a == b):HashUtils<Int>;

  public let i8hash = (hashInt8, func(a, b) = a == b):HashUtils<Int8>;

  public let i16hash = (hashInt16, func(a, b) = a == b):HashUtils<Int16>;

  public let i32hash = (hashInt32, func(a, b) = a == b):HashUtils<Int32>;

  public let i64hash = (hashInt64, func(a, b) = a == b):HashUtils<Int64>;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public let nhash = (hashNat, func(a, b) = a == b):HashUtils<Nat>;

  public let n8hash = (hashNat8, func(a, b) = a == b):HashUtils<Nat8>;

  public let n16hash = (hashNat16, func(a, b) = a == b):HashUtils<Nat16>;

  public let n32hash = (hashNat32, func(a, b) = a == b):HashUtils<Nat32>;

  public let n64hash = (hashNat64, func(a, b) = a == b):HashUtils<Nat64>;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public let thash = (hashText, func(a, b) = a == b):HashUtils<Text>;

  public let phash = (hashPrincipal, func(a, b) = a == b):HashUtils<Principal>;

  public let bhash = (hashBlob, func(a, b) = a == b):HashUtils<Blob>;

  public let lhash = (hashBool, func(a, b) = a == b):HashUtils<Bool>;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func combineHash<K1, K2>(hashUtils1: HashUtils<K1>, hashUtils2: HashUtils<K2>): HashUtils<(K1, K2)> {
    let getHash1 = hashUtils1.0;
    let getHash2 = hashUtils2.0;
    let areEqual1 = hashUtils1.1;
    let areEqual2 = hashUtils2.1;

    (
      func(key) = (getHash1(key.0) +% getHash2(key.1)) & 0x3fffffff,
      func(a, b) = areEqual1(a.0, b.0) and areEqual2(a.1, b.1),
    )
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func useHash<K>(hashUtils: HashUtils<K>, hash: Nat32): HashUtils<K> {
    (func(key) = hash, hashUtils.1);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func calcHash<K>(hashUtils: HashUtils<K>, key: K): HashUtils<K> {
    let hash = hashUtils.0(key);

    (func(key) = hash, hashUtils.1);
  };
};
