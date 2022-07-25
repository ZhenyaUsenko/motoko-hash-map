import Iter "mo:base/Iter";
import Prim "mo:prim";
import Types "./types";
import Hash "../hash";

module {
  public type Key = Types.Key;
  public type Set = Types.Set;

  let toNat = Prim.nat32ToNat;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func getHash(key: Key): Nat32 {
    return switch (key) {
      case (#principal key) Prim.hashBlob(Prim.blobOfPrincipal(key));
      case (#text key) Hash.hashText(key);
      case (#nat key) Hash.hashInt(key);
      case (#int key) Hash.hashInt(key);
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func iter<T>(map: Set, fn: (item: Types.Item) -> T): Iter.Iter<T> {
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

  func rehash(map: Set) {
    let (_, data, capacity, _, size) = map.body;

    let newCapacity = if (size < capacity / 4) capacity / 2 else if (size > capacity * 3 / 4) capacity * 2 else capacity;
    var newTakenSize = 0:Nat32;

    let newBuckets: [var Nat32] = Prim.Array_init(toNat(newCapacity), 0:Nat32);
    let newData: [var Types.Slot] = Prim.Array_init(toNat(newCapacity), #nextIndex (0:Nat32));

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

  public func has(map: Set, key: Key): Bool {
    let (buckets, data, capacity, _, _) = map.body;

    var index = buckets[toNat(getHash(key) % capacity)];

    loop if (index == 0) return false else switch (data[toNat(index - 1)]) {
      case (#item (itemKey, _, nextIndex)) {
        if (itemKey == key) return true;

        index := nextIndex;
      };

      case (#nextIndex nextIndex) index := nextIndex;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func put(map: Set, key: Key): Bool {
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
          if (itemKey == key) return true;

          index := nextIndex;
        };

        case (#nextIndex nextIndex) index := nextIndex;
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func remove(map: Set, key: Key): Bool {
    let (buckets, data, capacity, takenSize, size) = map.body;

    var index = buckets[toNat(getHash(key) % capacity)];

    loop if (index == 0) return false else {
      let dataIndex = toNat(index - 1);

      switch (data[dataIndex]) {
        case (#item (itemKey, _, nextIndex)) {
          if (itemKey == key) {
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

  public func filter(map: Set, fn: (key: Key) -> Bool): Set {
    let (_, data, capacity, _, _) = map.body;

    let newData: [var Types.Slot] = Prim.Array_init(toNat(capacity), #nextIndex (0:Nat32));
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

    let newMap: Set = { var body = ([var], newData, newCapacity, newSize, newSize) };

    rehash(newMap);

    return newMap;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func new(): Set {
    return { var body = (Prim.Array_init(2, 0:Nat32), Prim.Array_init(2, #nextIndex (0:Nat32)), 2, 0, 0) };
  };

  public func clear(map: Set) {
    map.body := (Prim.Array_init(2, 0:Nat32), Prim.Array_init(2, #nextIndex (0:Nat32)), 2, 0, 0);
  };

  public func size(map: Set): Nat {
    return toNat(map.body.4);
  };

  public func add(map: Set, key: Key) {
    ignore put(map, key);
  };

  public func delete(map: Set, key: Key) {
    ignore remove(map, key);
  };

  public func keys(map: Set): Iter.Iter<Key> {
    return iter(map, func((key, _, _): Types.Item): Key { key });
  };
};
