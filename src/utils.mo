import Blob "mo:base/Principal";
import Prim "mo:prim";

module {
  public type HashUtils<K> = (
    getHash: (K) -> Nat32,
    areEqual: (K, K) -> Bool,
    getNullKey: () -> K,
  );

  public type NullHashUtils = (
    getHash: (Any) -> Nat32,
    areEqual: (Any, Any) -> Bool,
    getNullKey: () -> None,
  );

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func hashNat32Helper(key: Nat32): Nat32 {
    var hash = key;

    hash := hash >> 16 ^ hash *% 0x21f0aaad;
    hash := hash >> 15 ^ hash *% 0x735a2d97;

    return hash >> 15 ^ hash & 0x3fffffff;
  };

  func hashNat64Helper(key: Nat64): Nat32 {
    var hash = key;

    hash := hash >> 30 ^ hash *% 0xbf58476d1ce4e5b9;
    hash := hash >> 27 ^ hash *% 0x94d049bb133111eb;

    return Prim.intToNat32Wrap(Prim.nat64ToNat(hash >> 31 ^ hash & 0x3fffffff));
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func hashInt(key: Int): Nat32 {
    return hashNat64Helper(Prim.intToNat64Wrap(key));
  };

  public func hashInt8(key: Int8): Nat32 {
    return hashNat32Helper(Prim.intToNat32Wrap(Prim.int8ToInt(key)));
  };

  public func hashInt16(key: Int16): Nat32 {
    return hashNat32Helper(Prim.intToNat32Wrap(Prim.int16ToInt(key)));
  };

  public func hashInt32(key: Int32): Nat32 {
    return hashNat32Helper(Prim.int32ToNat32(key));
  };

  public func hashInt64(key: Int64): Nat32 {
    return hashNat64Helper(Prim.int64ToNat64(key));
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func hashNat(key: Nat): Nat32 {
    return hashNat64Helper(Prim.intToNat64Wrap(key));
  };

  public func hashNat8(key: Nat8): Nat32 {
    return hashNat32Helper(Prim.intToNat32Wrap(Prim.nat8ToNat(key)));
  };

  public func hashNat16(key: Nat16): Nat32 {
    return hashNat32Helper(Prim.intToNat32Wrap(Prim.nat16ToNat(key)));
  };

  public func hashNat32(key: Nat32): Nat32 {
    return hashNat32Helper(key);
  };

  public func hashNat64(key: Nat64): Nat32 {
    return hashNat64Helper(key);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func hashText(key: Text): Nat32 {
    return Prim.hashBlob(Prim.encodeUtf8(key)) & 0x3fffffff;
  };

  public func hashPrincipal(key: Principal): Nat32 {
    return Prim.hashBlob(Prim.blobOfPrincipal(key)) & 0x3fffffff;
  };

  public func hashBlob(key: Blob): Nat32 {
    return Prim.hashBlob(key) & 0x3fffffff;
  };

  public func hashBool(key: Bool): Nat32 {
    return if (key) 114489971 else 0;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public let ihash = (hashInt, func(a, b) = a == b, func() = 0):HashUtils<Int>;

  public let i8hash = (hashInt8, func(a, b) = a == b, func() = 0):HashUtils<Int8>;

  public let i16hash = (hashInt16, func(a, b) = a == b, func() = 0):HashUtils<Int16>;

  public let i32hash = (hashInt32, func(a, b) = a == b, func() = 0):HashUtils<Int32>;

  public let i64hash = (hashInt64, func(a, b) = a == b, func() = 0):HashUtils<Int64>;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public let nhash = (hashNat, func(a, b) = a == b, func() = 0):HashUtils<Nat>;

  public let n8hash = (hashNat8, func(a, b) = a == b, func() = 0):HashUtils<Nat8>;

  public let n16hash = (hashNat16, func(a, b) = a == b, func() = 0):HashUtils<Nat16>;

  public let n32hash = (hashNat32, func(a, b) = a == b, func() = 0):HashUtils<Nat32>;

  public let n64hash = (hashNat64, func(a, b) = a == b, func() = 0):HashUtils<Nat64>;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public let thash = (hashText, func(a, b) = a == b, func() = ""):HashUtils<Text>;

  public let phash = (hashPrincipal, func(a, b) = a == b, func() = Prim.principalOfBlob("")):HashUtils<Principal>;

  public let bhash = (hashBlob, func(a, b) = a == b, func() = ""):HashUtils<Blob>;

  public let lhash = (hashBool, func(a, b) = a == b, func() = false):HashUtils<Bool>;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func useHash<K>((getHash, areEqual, getNullKey): HashUtils<K>, hash: Nat32): HashUtils<K> {
    return (func(key) = hash, areEqual, getNullKey);
  };

  public func calcHash<K>((getHash, areEqual, getNullKey): HashUtils<K>, key: K): HashUtils<K> {
    let hash = getHash(key);

    return (func(key) = hash, areEqual, getNullKey);
  };
};
