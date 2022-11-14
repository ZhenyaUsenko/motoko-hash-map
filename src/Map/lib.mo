import Map "./optimized";

module {
  public type Entry<K, V> = Map.Entry<K, V>;

  public type Map<K, V> = Map.Map<K, V>;

  public type Iter<T> = Map.Iter<T>;

  public type IterNext<T> = Map.IterNext<T>;

  public type HashUtils<K> = Map.HashUtils<K>;

  public let { hashInt; hashText; hashPrincipal; hashBlob; hashBool } = Map;

  public let { ihash; nhash; thash; phash; bhash; lhash; useHash; calcHash } = Map;

  public let { new; clear; size } = Map;

  public let { get; has } = Map;

  public let { put; putFront; set; setFront; putMove; putMoveFront; setMove; setMoveFront } = Map;

  public let { putBefore; putAfter; setBefore; setAfter; update; updateFront; updateMove; updateMoveFront } = Map;

  public let { remove; delete; pop; popFront } = Map;

  public let { peek; peekFront; cycle; cycleFront } = Map;

  public let { mapFilter; mapFilterDesc; map; mapDesc; filter; filterDesc; clone; cloneDesc } = Map;

  public let { keys; keysDesc; vals; valsDesc; entries; entriesDesc } = Map;

  public let { keysFrom; keysFromDesc; valsFrom; valsFromDesc; entriesFrom; entriesFromDesc } = Map;

  public let { find; findDesc; some; someDesc; every; everyDesc; forEach; forEachDesc } = Map;

  public let { fromIter; fromIterDesc; fromIterMap; fromIterMapDesc } = Map;

  public let { toArray; toArrayDesc; toArrayMap; toArrayMapDesc } = Map;
};
