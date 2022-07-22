import Array "mo:base/Array";
import Hash "mo:base/Hash";
import Int "mo:base/Int";
import Nat "mo:base/Nat";
import Nat32 "mo:base/Nat32";
import Option "mo:base/Option";
import Principal "mo:base/Principal";
import Text "mo:base/Text";
import Types "./types";

module {
  public type Key = Types.Key;
  public type Item<V> = Types.Item<V>;
  public type Map<V> = Types.Map<V>;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  
  func getHash(key: Types.Key): Hash.Hash {
    return switch (key) {
      case (#principal key) Principal.hash(key);
      case (#text key) Text.hash(key);
      case (#nat key) Int.hash(key);
      case (#int key) Int.hash(key);
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  func rehash<V>(map: Types.Map<V>) {
    let prevData = map.data;
    let capacity = prevData.size();
    let newCapacity = if (map.size < capacity / 4) Nat.max(capacity / 2, 4) else if (map.size >= capacity / 4 * 3) capacity * 2 else capacity;
    let newBucketsSize = newCapacity / 2;

    map.buckets := Array.init(newBucketsSize, null);
    map.data := Array.init(newCapacity, null);
    map.actualSize := 0;

    label dataLoop for (item in prevData.vals()) switch (item) {
      case (?item) switch (item.key, item.hash) {
        case (?key, ?hash) {
          let bucketIndex = Nat32.toNat(hash) % newBucketsSize;

          item.next := map.buckets[bucketIndex];

          map.buckets[bucketIndex] := ?item;
          map.data[map.actualSize] := ?item;

          map.actualSize += 1;
        };

        case (_) {};
      };

      case (_) break dataLoop;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func new<V>(): Types.Map<V> {
    return {
      var buckets = Array.init(2, null);
      var data = Array.init(4, null);
      var actualSize = 0;
      var size = 0;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func get<V>(map: Types.Map<V>, key: Types.Key): ?V {
    var nextItem = map.buckets[Nat32.toNat(getHash(key)) % map.buckets.size()];

    loop switch (nextItem) {
      case (?item) {
        if (item.key == ?key) return item.value;

        nextItem := item.next;
      };

      case (_) return null;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func replace<V>(map: Types.Map<V>, key: Types.Key, value: V): ?V {
    let hash = getHash(key);
    let firstItem = map.buckets[Nat32.toNat(hash) % map.buckets.size()];
    var nextItem = firstItem;

    loop switch (nextItem) {
      case (?item) {
        if (item.key == ?key) {
          let prevValue = item.value;

          item.value := ?value;

          return prevValue;
        };

        nextItem := item.next;
      };

      case (_) {
        let newItem: ?Types.Item<V> = ?{ var key = ?key; var value = ?value; var hash = ?hash; var next = firstItem };

        if (map.actualSize == map.data.size()) rehash(map);

        map.buckets[Nat32.toNat(hash) % map.buckets.size()] := newItem;
        map.data[map.actualSize] := newItem;
        map.actualSize += 1;
        map.size += 1;

        return null;
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func remove<V>(map: Types.Map<V>, key: Types.Key): ?V {
    var nextItem = map.buckets[Nat32.toNat(getHash(key)) % map.buckets.size()];

    loop switch (nextItem) {
      case (?item) {
        if (item.key == ?key) {
          let prevValue = item.value;

          item.key := null;
          item.value := null;
          item.hash := null;

          map.size -= 1;

          if (map.size < map.data.size() / 4) rehash(map);

          return prevValue;
        };

        nextItem := item.next;
      };

      case (_) return null;
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func has<V>(map: Types.Map<V>, key: Types.Key): Bool {
    return Option.isSome(get(map, key));
  };

  public func set<V>(map: Types.Map<V>, key: Types.Key, value: V) {
    let _ = replace(map, key, value);
  };

  public func delete<V>(map: Types.Map<V>, key: Types.Key) {
    let _ = remove(map, key);
  };
};