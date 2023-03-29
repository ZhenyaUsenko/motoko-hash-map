import Set "./optimized";

module {
  public type Entry<K> = Set.Entry<K>;

  public type Set<K> = Set.Set<K>;

  public type Iter<T> = Set.Iter<T>;

  public type IterNext<T> = Set.IterNext<T>;

  public type HashUtils<K> = Set.HashUtils<K>;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public let { hashInt; hashInt8; hashInt16; hashInt32; hashInt64 } = Set;

  public let { hashNat; hashNat8; hashNat16; hashNat32; hashNat64 } = Set;

  public let { hashText; hashPrincipal; hashBlob; hashBool } = Set;

  public let { ihash; i8hash; i16hash; i32hash; i64hash } = Set;

  public let { nhash; n8hash; n16hash; n32hash; n64hash } = Set;

  public let { thash; phash; bhash; lhash } = Set;

  public let { useHash; calcHash } = Set;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public let { new; clear } = Set;

  public let { size; empty } = Set;

  public let { peek; peekFront } = Set;

  public let { has; contains } = Set;

  public let { put; putFront; add; addFront } = Set;

  public let { putMove; putMoveFront; putBefore; putAfter } = Set;

  public let { remove; delete; pop; popFront; cycle; cycleFront } = Set;

  public let { filter; filterDesc } = Set;

  public let { clone; cloneDesc } = Set;

  public let { keys; keysDesc } = Set;

  public let { keysFrom; keysFromDesc } = Set;

  public let { find; findDesc; some; someDesc; every; everyDesc; forEach; forEachDesc } = Set;

  public let { fromIter; fromIterDesc; fromIterMap; fromIterMapDesc } = Set;

  public let { toArray; toArrayDesc } = Set;

  public let { toArrayMap; toArrayMapDesc } = Set;
};
