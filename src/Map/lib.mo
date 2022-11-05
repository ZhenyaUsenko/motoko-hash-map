import Map "./optimized";

module {
  public type Entry<K, V> = Map.Entry<K, V>;

  public type Map<K, V> = Map.Map<K, V>;

  public type Iter<T> = Map.Iter<T>;

  public type IterNative<T> = Map.IterNative<T>;

  public type HashUtils<K> = Map.HashUtils<K>;

  public let { ihash; nhash; thash; phash; bhash; lhash; useHash; calcHash } = Map;

  public let { new; clear; size } = Map;

  public let { get; has } = Map;

  public let { put; putFront; set; setFront; putMove; putMoveFront; setMove; setMoveFront } = Map;

  public let { putBefore; putAfter; setBefore; setAfter } = Map;

  public let { update; updateFront; updateMove; updateMoveFront } = Map;

  public let { remove; delete } = Map;

  public let { pop; popFront } = Map;

  public let { cycle; cycleFront } = Map;

  public let { peek; peekFront } = Map;

  public let { mapFilter; map; filter; clone } = Map;

  public let { keys; keysDesc; vals; valsDesc; entries; entriesDesc } = Map;

  public let { keysFrom; keysFromDesc; valsFrom; valsFromDesc; entriesFrom; entriesFromDesc } = Map;

  public let { forEach; forEachDesc } = Map;

  public let { every; everyDesc } = Map;

  public let { some; someDesc } = Map;

  public let { find; findDesc } = Map;

  public let { fromIter; fromIterDesc } = Map;

  public let { fromIterMap; fromIterMapDesc } = Map;

  public let { toArray; toArrayDesc } = Map;

  public let { toArrayMap; toArrayMapDesc } = Map;
};
