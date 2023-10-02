import Clone "./modules/clone";
import Find "./modules/find";
import FromIter "./modules/fromIter";
import Get "./modules/get";
import Init "./modules/init";
import Iterate "./modules/iterate";
import Put "./modules/put";
import Queue "./modules/queue";
import Rehash "./modules/rehash";
import Remove "./modules/remove";
import ToArray "./modules/toArray";
import Types "./types";
import Utils "./utils";

module {
  public type Map<K, V> = Types.Map<K, V>;

  public type Iter<T> = Types.Iter<T>;

  public type IterNext<T> = Types.IterNext<T>;

  public type HashUtils<K> = Types.HashUtils<K>;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public let { hashInt; hashInt8; hashInt16; hashInt32; hashInt64 } = Utils;

  public let { hashNat; hashNat8; hashNat16; hashNat32; hashNat64 } = Utils;

  public let { hashText; hashPrincipal; hashBlob; hashBool } = Utils;

  public let { ihash; i8hash; i16hash; i32hash; i64hash } = Utils;

  public let { nhash; n8hash; n16hash; n32hash; n64hash } = Utils;

  public let { thash; phash; bhash; lhash } = Utils;

  public let { combineHash; useHash; calcHash } = Utils;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public let { mapFilter; mapFilterDesc; filter; filterDesc; map; mapDesc; clone; cloneDesc } = Clone;

  public let { find; findDesc; some; someDesc; every; everyDesc; forEach; forEachDesc } = Find;

  public let { fromIter; fromIterDesc; fromIterMap; fromIterMapDesc } = FromIter;

  public let { get; has; contains } = Get;

  public let { new; clear; size; empty; make } = Init;

  public let { keys; keysDesc; vals; valsDesc; entries; entriesDesc } = Iterate;

  public let { keysFrom; keysFromDesc; valsFrom; valsFromDesc; entriesFrom; entriesFromDesc } = Iterate;

  public let { put; putFront; set; setFront; add; addFront; replace; update; updateFront } = Put;

  public let { putMove; putMoveFront; replaceMove; replaceMoveFront; updateMove; updateMoveFront } = Put;

  public let { peek; peekFront; pop; popFront; cycle; cycleFront } = Queue;

  public let { rehash } = Rehash;

  public let { remove; delete } = Remove;

  public let { toArray; toArrayDesc; toArrayMap; toArrayMapDesc } = ToArray;
};