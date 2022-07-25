import Iter "mo:base/Iter";
import Prim "mo:prim";
import Utils "./utils";

module {
  type HashUtils<K> = (
    getHash: (K) -> Nat32,
    areEqual: (K, K) -> Bool,
  );

  type Entry<K> = (
    key: K,
    hash: Nat32,
    nextIndex: Nat32,
  );

  type Slot<K> = {
    #item: Entry<K>;
    #nextIndex: Nat32;
  };

  public type Set<K> = {
    var body: (
      buckets: [var Nat32],
      data: [var Slot<K>],
      capacity: Nat32,
      takenSize: Nat32,
      size: Nat32,
    );
  };

  public let { nhash; thash; phash; bhash } = Utils;

  let toNat = Prim.nat32ToNat;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func iter<K, T>(map: Set<K>, fn: (Entry<K>) -> T): Iter.Iter<T> {
    let (_, data, _, takenSize, _) = map.body;

    var index = 0:Nat32;

    return {
      next = func(): ?T {
        loop if (index < takenSize) switch (data[toNat(index)]) {
          case (#item item) {
            index += 1;

            return ?fn(item);
          };

          case (_) index += 1;
        } else {
          return null;
        };
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func rehash<K>(map: Set<K>) {
    let (_, data, capacity, _, size) = map.body;

    let newCapacity = if (size < capacity / 4) capacity / 2 else if (size > capacity * 3 / 4) capacity * 2 else capacity;
    var newTakenSize = 0:Nat32;

    let newBuckets: [var Nat32] = Prim.Array_init(toNat(newCapacity), 0:Nat32);
    let newData: [var Slot<K>] = Prim.Array_init(toNat(newCapacity), #nextIndex (0:Nat32));

    for (item in data.vals()) switch (item) {
      case (#item (key, hash, _)) {
        let bucketIndex = toNat(hash % newCapacity);

        newData[toNat(newTakenSize)] := #item (key, hash, newBuckets[bucketIndex]);

        newTakenSize += 1;

        newBuckets[bucketIndex] := newTakenSize;
      };

      case (_) {};
    };

    map.body := (newBuckets, newData, newCapacity, newTakenSize, size);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func has<K>(map: Set<K>, (getHash, areEqual): HashUtils<K>, key: K): Bool {
    let (buckets, data, capacity, _, _) = map.body;

    var index = buckets[toNat(getHash(key) % capacity)];

    loop if (index == 0) return false else switch (data[toNat(index - 1)]) {
      case (#item (itemKey, _, nextIndex)) {
        if (areEqual(itemKey, key)) return true;

        index := nextIndex;
      };

      case (#nextIndex nextIndex) index := nextIndex;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func put<K>(map: Set<K>, (getHash, areEqual): HashUtils<K>, key: K): Bool {
    let (buckets, data, capacity, takenSize, size) = map.body;

    let hash = getHash(key);
    let bucketIndex = toNat(hash % capacity);
    let firstIndex = buckets[bucketIndex];
    var index = firstIndex;

    loop if (index == 0) {
      let newTakenSize = takenSize + 1;

      data[toNat(takenSize)] := #item (key, hash, firstIndex);
      buckets[bucketIndex] := newTakenSize;

      map.body := (buckets, data, capacity, newTakenSize, size + 1);

      if (newTakenSize == capacity) rehash(map);

      return false;
    } else {
      let dataIndex = toNat(index - 1);

      switch (data[dataIndex]) {
        case (#item (itemKey, _, nextIndex)) {
          if (areEqual(itemKey, key)) return true;

          index := nextIndex;
        };

        case (#nextIndex nextIndex) index := nextIndex;
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func remove<K>(map: Set<K>, (getHash, areEqual): HashUtils<K>, key: K): Bool {
    let (buckets, data, capacity, takenSize, size) = map.body;

    var index = buckets[toNat(getHash(key) % capacity)];

    loop if (index == 0) return false else {
      let dataIndex = toNat(index - 1);

      switch (data[dataIndex]) {
        case (#item (itemKey, _, nextIndex)) {
          if (areEqual(itemKey, key)) {
            let newSize = size - 1;

            data[dataIndex] := #nextIndex nextIndex;

            map.body := (buckets, data, capacity, takenSize, newSize);

            if (newSize < capacity / 4) rehash(map);

            return true;
          };

          index := nextIndex;
        };

        case (#nextIndex nextIndex) index := nextIndex;
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func filter<K>(map: Set<K>, fn: (K) -> Bool): Set<K> {
    let (_, data, capacity, _, _) = map.body;

    let newData: [var Slot<K>] = Prim.Array_init(toNat(capacity), #nextIndex (0:Nat32));
    var newCapacity = 2:Nat32;
    var newSize = 0:Nat32;

    for (item in data.vals()) switch (item) {
      case (#item (key, hash, _)) if (fn(key)) {
        newData[toNat(newSize)] := #item (key, hash, 0);
          
        newSize += 1;

        if (newSize == newCapacity) newCapacity *= 2;
      };

      case (_) {};
    };

    let newMap: Set<K> = { var body = ([var], newData, newCapacity, newSize, newSize) };

    rehash(newMap);

    return newMap;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func new<K>(): Set<K> {
    return { var body = (Prim.Array_init(2, 0:Nat32), Prim.Array_init(2, #nextIndex (0:Nat32)), 2, 0, 0) };
  };

  public func clear<K>(map: Set<K>) {
    map.body := (Prim.Array_init(2, 0:Nat32), Prim.Array_init(2, #nextIndex (0:Nat32)), 2, 0, 0);
  };

  public func size<K>(map: Set<K>): Nat {
    return toNat(map.body.4);
  };

  public func add<K>(map: Set<K>, hashUtils: HashUtils<K>, key: K) {
    ignore put(map, hashUtils, key);
  };

  public func delete<K>(map: Set<K>, hashUtils: HashUtils<K>, key: K) {
    ignore remove(map, hashUtils, key);
  };

  public func keys<K>(map: Set<K>): Iter.Iter<K> {
    return iter(map, func((key, _, _): Entry<K>): K { key });
  };
};
