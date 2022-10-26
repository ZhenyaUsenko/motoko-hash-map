import Map "./optimized";

module {
  public type Entry<K, V> = Map.Entry<K, V>;

  public type Map<K, V> = Map.Map<K, V>;

  public type HashUtils<K> = Map.HashUtils<K>;

  public let { ihash; nhash; thash; phash; bhash; lhash; useHash; calcHash } = Map;

  public let { get; has; put; putFront; set; setFront; update; updateFront; remove; delete; pop; popFront; peek; peekFront } = Map;

  public let { mapFilter; map; filter; clone; keys; keysDesc; vals; valsDesc; entries; entriesDesc } = Map;

  public let { forEach; forEachDesc; every; everyDesc; some; someDesc; find; findDesc } = Map;

  public let { fromIter; toArray; new; clear; size } = Map;
};
