import Prim "mo:prim";

module {
  public type HashUtils<K> = (
    getHash: (key: K) -> Nat32,
    areEqual: (a: K, b: K) -> Bool,
  );

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func hashInt(key: Int): Nat32 {
    var hash = Prim.intToNat32Wrap(key);

    hash := (hash << 15) -% hash -% 1;
    hash := hash ^ (hash >> 12);
    hash := hash +% (hash << 2);
    hash := hash ^ (hash >> 4);
    hash := hash *% 2057;
    hash := hash ^ (hash >> 16);

    return hash & 0x3fffffff;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func hashText(key: Text): Nat32 {
    var hash = 0:Nat32;

    for (char in key.chars()) {
      hash := hash +% Prim.charToNat32(char);
      hash := hash +% (hash << 10);
      hash := hash ^ (hash >> 6);
    };

    hash := hash +% (hash << 3);
    hash := hash ^ (hash >> 11);
    hash := hash +% (hash << 15);

    return hash & 0x3fffffff;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

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

  public let nhash = (hashInt, func(a: Int, b: Int): Bool { a == b });

  public let thash = (hashText, func(a: Text, b: Text): Bool { a == b });

  public let phash = (hashPrincipal, func(a: Principal, b: Principal): Bool { a == b });

  public let bhash = (hashBlob, func(a: Blob, b: Blob): Bool { a == b });

  public let lhash = (hashBool, func(a: Bool, b: Bool): Bool { a == b });

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func calcHash<K>((getHash, areEqual): HashUtils<K>, key: K): HashUtils<K> {
    let hash = getHash(key);

    return (func(key: K): Nat32 { hash }, areEqual);
  };
};
