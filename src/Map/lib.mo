import Map "./Map";
import OptimizedMap "./Map";

module {
  public type Entry<K, V> = OptimizedMap.Entry<K, V>;

  public type Map<K, V> = OptimizedMap.Map<K, V>;

  public type HashUtils<K> = OptimizedMap.HashUtils<K>;

  public let { ihash; nhash; thash; phash; bhash; lhash; useHash; calcHash } = OptimizedMap;

  public let { get; has; put; putFront; set; setFront; update; updateFront; remove; delete; pop; popFront; peek; peekFront } = OptimizedMap;

  public let { map; mapFilter; filter; keys; keysDesc; vals; valsDesc; entries; entriesDesc } = OptimizedMap;

  public let { forEach; forEachDesc; some; someDesc; every; everyDesc; find; findDesc } = OptimizedMap;

  public let { fromIter; toArray; rehash; new; clear; size } = OptimizedMap;
};
