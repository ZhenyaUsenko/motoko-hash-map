import Hash "mo:base/Hash";

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
    var hash: ?Hash.Hash;
    var next: ?Item<V>;
  };

  public type Map<V> = {
    var buckets: [var ?Item<V>];
    var data: [var ?Item<V>];
    var actualSize: Nat;
    var size: Nat;
  };
};