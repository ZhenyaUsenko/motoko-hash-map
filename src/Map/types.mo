module {
  public type Key = {
    #principal: Principal;
    #text: Text;
    #nat: Nat;
    #int: Int;
  };

  public type Item<V> = (
    key: Key,
    value: V,
    hash: Nat32,
    nextIndex: Nat32,
  );

  public type Slot<V> = {
    #item: Item<V>;
    #nextIndex: Nat32;
  };

  public type Map<V> = {
    var body: (
      buckets: [var Nat32],
      data: [var Slot<V>],
      capacity: Nat32,
      takenSize: Nat32,
      size: Nat32,
    );
  };
};
