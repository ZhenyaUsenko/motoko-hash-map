import Prim "mo:prim";

module {
  public type HashUtils<K> = (
    getHash: (key: K) -> Nat32,
    areEqual: (a: K, b: K) -> Bool,
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

  public let ihash: HashUtils<Int> = (hashInt, func(a, b) { a == b });

  public let nhash: HashUtils<Nat> = (hashInt, func(a, b) { a == b });

  public let thash: HashUtils<Text> = (hashText, func(a, b) { Prim.encodeUtf8(a) == Prim.encodeUtf8(b) });

  public let phash: HashUtils<Principal> = (hashPrincipal, func(a, b) { a == b });

  public let bhash: HashUtils<Blob> = (hashBlob, func(a, b) { a == b });

  public let lhash: HashUtils<Bool> = (hashBool, func(a, b) { true });

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func useHash<K>((getHash, areEqual): HashUtils<K>, hash: Nat32): HashUtils<K> {
    return (func(key) { hash }, areEqual);
  };

  public func calcHash<K>((getHash, areEqual): HashUtils<K>, key: K): HashUtils<K> {
    let hash = getHash(key);

    return (func(key) { hash }, areEqual);
  };
};
