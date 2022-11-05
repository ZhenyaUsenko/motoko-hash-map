import Set "./optimized";

module {
  public type Entry<K> = Set.Entry<K>;

  public type Set<K> = Set.Set<K>;

  public type Iter<T> = Set.Iter<T>;

  public type IterNative<T> = Set.IterNative<T>;

  public type HashUtils<K> = Set.HashUtils<K>;

  public let { ihash; nhash; thash; phash; bhash; lhash; useHash; calcHash } = Set;

  public let { new; clear; size } = Set;

  public let { has } = Set;

  public let { put; putFront; add; addFront; putMove; putMoveFront; addMove; addMoveFront } = Set;

  public let { putBefore; putAfter; addBefore; addAfter } = Set;

  public let { remove; delete } = Set;

  public let { pop; popFront } = Set;

  public let { cycle; cycleFront } = Set;

  public let { peek; peekFront } = Set;

  public let { filter; clone } = Set;

  public let { keys; keysDesc } = Set;

  public let { keysFrom; keysFromDesc } = Set;

  public let { forEach; forEachDesc } = Set;

  public let { every; everyDesc } = Set;

  public let { some; someDesc } = Set;

  public let { find; findDesc } = Set;

  public let { fromIter; fromIterDesc } = Set;

  public let { fromIterMap; fromIterMapDesc } = Set;

  public let { toArray; toArrayDesc } = Set;

  public let { toArrayMap; toArrayMapDesc } = Set;
};
