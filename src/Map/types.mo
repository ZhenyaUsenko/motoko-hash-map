module {
  public type Key = {
    #principal: Principal;
    #text: Text;
    #nat: Nat;
    #int: Int;
  };

  public type Item<V> = {
    var key: ?Key;
    var value: ?V;
    var next: ?Item<V>;
    hash: Nat32;
  };

  public type Map<V> = {
    var buckets: [var ?Item<V>];
    var data: [var ?Item<V>];
    var bucketsSize: Nat32;
    var dataSize: Nat32;
    var actualSize: Nat32;
    var size: Nat32;
  };
};