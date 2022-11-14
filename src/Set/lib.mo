import Set "./optimized";

module {
  public type Entry<K> = Set.Entry<K>;

  public type Set<K> = Set.Set<K>;

  public type Iter<T> = Set.Iter<T>;

  public type IterNext<T> = Set.IterNext<T>;

  public type HashUtils<K> = Set.HashUtils<K>;

  public let { hashInt; hashText; hashPrincipal; hashBlob; hashBool } = Set;

  public let { ihash; nhash; thash; phash; bhash; lhash; useHash; calcHash } = Set;

  public let { new; clear; size } = Set;

  public let { has } = Set;

  public let { put; putFront; add; addFront; putMove; putMoveFront; addMove; addMoveFront } = Set;

  public let { putBefore; putAfter; addBefore; addAfter } = Set;

  public let { remove; delete; pop; popFront } = Set;

  public let { peek; peekFront; cycle; cycleFront } = Set;

  public let { filter; filterDesc; clone; cloneDesc } = Set;

  public let { keys; keysDesc } = Set;

  public let { keysFrom; keysFromDesc } = Set;

  public let { find; findDesc; some; someDesc; every; everyDesc; forEach; forEachDesc } = Set;

  public let { fromIter; fromIterDesc; fromIterMap; fromIterMapDesc } = Set;

  public let { toArray; toArrayDesc; toArrayMap; toArrayMapDesc } = Set;
};
