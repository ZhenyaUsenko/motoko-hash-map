module {
  public type Key = { #principal: Principal; #text: Text; #nat: Nat; #int: Int };

  public type Value<V> = V;
  public type Hash = Nat32;
  public type NextIndex = Nat32;

  public type Item<V> = { #item: (Key, Value<V>, Hash, NextIndex); #nextIndex: NextIndex };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public type Buckets = [var Nat32];
  public type Data<V> = [var Item<V>];
  public type Capacity = Nat32;
  public type TakenSize = Nat32;
  public type Size = Nat32;

  public type Map<V> = { var data: (Buckets, Data<V>, Capacity, TakenSize, Size) };
};