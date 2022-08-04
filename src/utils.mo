import Prim "mo:prim";

module {
  public type HashUtils<K> = (
    getHash: (key: K) -> Nat,
    areEqual: (a: K, b: K) -> Bool,
  );

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func hashInt(key: Int): Nat {
    var hash = Prim.intToNat32Wrap(key);

    hash := (hash << 15) -% hash -% 1;
    hash := hash ^ (hash >> 12);
    hash := hash +% (hash << 2);
    hash := hash ^ (hash >> 4);
    hash := hash *% 2057;
    hash := hash ^ (hash >> 16);

    return Prim.nat32ToNat(hash & 0x3fffffff);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func hashText(key: Text): Nat {
    return Prim.nat32ToNat(Prim.hashBlob(Prim.encodeUtf8(key)) & 0x3fffffff);
  };

  public func hashPrincipal(key: Principal): Nat {
    return Prim.nat32ToNat(Prim.hashBlob(Prim.blobOfPrincipal(key)) & 0x3fffffff);
  };

  public func hashBlob(key: Blob): Nat {
    return Prim.nat32ToNat(Prim.hashBlob(key) & 0x3fffffff);
  };

  public func hashBool(key: Bool): Nat {
    return if (key) 1 else 0;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public let ihash: HashUtils<Int> = (hashInt, func(a, b) { a == b });

  public let nhash: HashUtils<Nat> = (hashInt, func(a, b) { a == b });

  public let thash: HashUtils<Text> = (hashText, func(a, b) { Prim.encodeUtf8(a) == Prim.encodeUtf8(b) });

  public let phash: HashUtils<Principal> = (hashPrincipal, func(a, b) { a == b });

  public let bhash: HashUtils<Blob> = (hashBlob, func(a, b) { a == b });

  public let lhash: HashUtils<Bool> = (hashBool, func(a, b) { a == b });

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func useHash<K>((getHash, areEqual): HashUtils<K>, hash: Nat): HashUtils<K> {
    return (func(key) { hash }, areEqual);
  };

  public func calcHash<K>((getHash, areEqual): HashUtils<K>, key: K): HashUtils<K> {
    let hash = getHash(key);

    return (func(key) { hash }, areEqual);
  };
};
