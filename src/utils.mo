import Blob "mo:base/Principal";
import Prim "mo:prim";

module {
  public type HashUtils<K> = (
    getHash: (K) -> Nat32,
    areEqual: (K, K) -> Bool,
    getNullKey: () -> K,
  );

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func hashInt(key: Int): Nat32 {
    var hash = Prim.intToNat32Wrap(key);

    hash := hash >> 16 ^ hash *% 0x21f0aaad;
    hash := hash >> 15 ^ hash *% 0x735a2d97;

    return hash >> 15 ^ hash & 0x3fffffff;
  };

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
    return if (key) 1 else 0;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public let ihash: HashUtils<Int> = (hashInt, func(a, b) = a == b, func() = 0);

  public let nhash: HashUtils<Nat> = (hashInt, func(a, b) = a == b, func() = 0);

  public let thash: HashUtils<Text> = (hashText, func(a, b) = a == b, func() = "");

  public let phash: HashUtils<Principal> = (hashPrincipal, func(a, b) = a == b, func() = Prim.principalOfBlob(""));

  public let bhash: HashUtils<Blob> = (hashBlob, func(a, b) = a == b, func() = "");

  public let lhash: HashUtils<Bool> = (hashBool, func(a, b) = true, func() = false);

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func useHash<K>((getHash, areEqual, getNullKey): HashUtils<K>, hash: Nat32): HashUtils<K> {
    return (func(key) = hash, areEqual, getNullKey);
  };

  public func calcHash<K>((getHash, areEqual, getNullKey): HashUtils<K>, key: K): HashUtils<K> {
    let hash = getHash(key);

    return (func(key) = hash, areEqual, getNullKey);
  };
};
