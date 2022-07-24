module {
  public type Key = {
    #principal: Principal;
    #text: Text;
    #nat: Nat;
    #int: Int;
  };

  public type Item = (
    key: Key,
    hash: Nat32,
    nextIndex: Nat32,
  );

  public type Slot = {
    #item: Item;
    #nextIndex: Nat32;
  };

  public type Set = {
    var body: (
      buckets: [var Nat32],
      data: [var Slot],
      capacity: Nat32,
      takenSize: Nat32,
      size: Nat32,
    );
  };
};
