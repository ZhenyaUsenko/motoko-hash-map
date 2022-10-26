import Set "./optimized";

module {
  public type Entry<K> = Set.Entry<K>;

  public type Set<K> = Set.Set<K>;

  public type HashUtils<K> = Set.HashUtils<K>;

  public let { ihash; nhash; thash; phash; bhash; lhash; useHash; calcHash } = Set;

  public let { has; put; putFront; add; addFront; remove; delete; pop; popFront; peek; peekFront } = Set;

  public let { filter; keys; keysDesc } = Set;

  public let { forEach; forEachDesc; every; everyDesc; some; someDesc; find; findDesc } = Set;

  public let { fromIter; toArray; new; clear; size } = Set;
};
