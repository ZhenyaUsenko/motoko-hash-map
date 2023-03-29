import Map "./optimized";

module {
  public type Entry<K, V> = Map.Entry<K, V>;

  public type Map<K, V> = Map.Map<K, V>;

  public type Iter<T> = Map.Iter<T>;

  public type IterNext<T> = Map.IterNext<T>;

  public type HashUtils<K> = Map.HashUtils<K>;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public let { hashInt; hashInt8; hashInt16; hashInt32; hashInt64 } = Map;

  public let { hashNat; hashNat8; hashNat16; hashNat32; hashNat64 } = Map;

  public let { hashText; hashPrincipal; hashBlob; hashBool } = Map;

  public let { ihash; i8hash; i16hash; i32hash; i64hash } = Map;

  public let { nhash; n8hash; n16hash; n32hash; n64hash } = Map;

  public let { thash; phash; bhash; lhash } = Map;

  public let { useHash; calcHash } = Map;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public let { new; clear } = Map;

  public let { size; empty } = Map;

  public let { peek; peekFront } = Map;

  public let { get; has; contains } = Map;

  public let { put; putFront; set; setFront; add; addFront; replace; update; updateFront } = Map;

  public let { putMove; putMoveFront; replaceMove; replaceMoveFront; updateMove; updateMoveFront; putBefore; putAfter } = Map;

  public let { remove; delete; pop; popFront; cycle; cycleFront } = Map;

  public let { mapFilter; mapFilterDesc; map; mapDesc; filter; filterDesc } = Map;

  public let { clone; cloneDesc } = Map;

  public let { keys; keysDesc; vals; valsDesc; entries; entriesDesc } = Map;

  public let { keysFrom; keysFromDesc; valsFrom; valsFromDesc; entriesFrom; entriesFromDesc } = Map;

  public let { find; findDesc; some; someDesc; every; everyDesc; forEach; forEachDesc } = Map;

  public let { fromIter; fromIterDesc; fromIterMap; fromIterMapDesc } = Map;

  public let { toArray; toArrayDesc } = Map;

  public let { toArrayMap; toArrayMapDesc } = Map;
};
