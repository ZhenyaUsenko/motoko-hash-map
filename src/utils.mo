import Prim "mo:prim";

module {
  public func hashInt(int: Int): Nat32 {
    var hash = Prim.intToNat32Wrap(int);

    hash := (hash << 15) -% hash -% 1;
    hash := hash ^ (hash >> 12);
    hash := hash +% (hash << 2);
    hash := hash ^ (hash >> 4);
    hash := hash *% 2057;
    hash := hash ^ (hash >> 16);

    return hash & 0x3fffffff;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func hashText(text: Text): Nat32 {
    var hash = 0:Nat32;

    for (char in text.chars()) {
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

  public func hashPrincipal(principal: Principal): Nat32 {
    return Prim.hashBlob(Prim.blobOfPrincipal(principal)) & 0x3fffffff;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func hashBlob(blob: Blob): Nat32 {
    return Prim.hashBlob(blob) & 0x3fffffff;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public let nhash = (hashInt, func(a: Int, b: Int): Bool { a == b });

  public let thash = (hashText, func(a: Text, b: Text): Bool { a == b });

  public let phash = (hashPrincipal, func(a: Principal, b: Principal): Bool { a == b });

  public let bhash = (hashBlob, func(a: Blob, b: Blob): Bool { a == b });
};
