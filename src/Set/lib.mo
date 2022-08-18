import Set "./Set";
import OptimizedSet "./Set";

module {
  public type Entry<K> = OptimizedSet.Entry<K>;

  public type Set<K> = OptimizedSet.Set<K>;

  public type HashUtils<K> = OptimizedSet.HashUtils<K>;

  public let { ihash; nhash; thash; phash; bhash; lhash; useHash; calcHash } = OptimizedSet;

  public let { has; put; putFront; add; addFront; remove; delete; pop; popFront; peek; peekFront } = OptimizedSet;

  public let { filter; keys; keysDesc } = OptimizedSet;

  public let { forEach; forEachDesc; some; someDesc; every; everyDesc; find; findDesc } = OptimizedSet;

  public let { rehash; new; clear; fromIter; size } = OptimizedSet;
};
