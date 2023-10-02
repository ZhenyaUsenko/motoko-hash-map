import Array "mo:base/Array";
import Debug "mo:base/Debug";
import IC "mo:base/ExperimentalInternetComputer";
import Map "./src/Map";
import Prim "mo:prim";
import Set "./src/Set";
import TestMap "./test/Map";
import TestSet "./test/Set";
import { ihash; nhash; n32hash; n64hash; thash; phash; bhash; lhash } "./src/Map";

actor Test {
  public query func testPerf(): async [Text] {
    let map = Map.new<Nat32, Nat32>();

    let startSpace = Prim.rts_heap_size();

    let setCost = IC.countInstructions(func() {
      var i = 0:Nat32;

      while (i != 100000) { Map.set(map, n32hash, i, i); i +%= 1 };
    });

    let setSpace = Prim.rts_heap_size() - startSpace:Nat;

    let getCost = IC.countInstructions(func() {
      var i = 0:Nat32;

      while (i != 100000) { ignore Map.get(map, n32hash, i); i +%= 1 };
    });

    let getSpace = Prim.rts_heap_size() - startSpace:Nat - setSpace:Nat;

    let updateCost = IC.countInstructions(func() {
      var i = 0:Nat32;

      while (i != 100000) { Map.set(map, n32hash, i, i); i +%= 1 };
    });

    let updateSpace = Prim.rts_heap_size() - startSpace:Nat - setSpace:Nat - getSpace:Nat;

    let deleteCost = IC.countInstructions(func() {
      var i = 0:Nat32;

      while (i != 100000) { Map.delete(map, n32hash, i); i +%= 1 };
    });

    let deleteSpace = Prim.rts_heap_size() - startSpace:Nat - setSpace:Nat - getSpace:Nat - updateSpace:Nat;

    var i = 0:Nat32;

    while (i != 100000) { Map.set(map, n32hash, i, i); i +%= 1 };

    let deleteDescCost = IC.countInstructions(func() {
      var i = 100000:Nat32;

      while (i != 0) { i -%= 1; Map.delete(map, n32hash, i) };
    });

    return [
      "set cost - " # debug_show(setCost),
      "get cost - " # debug_show(getCost),
      "update cost - " # debug_show(updateCost),
      "delete cost - " # debug_show(deleteCost),
      "delete desc cost - " # debug_show(deleteDescCost),
      "---",
      "set space - " # debug_show(setSpace),
      "get space - " # debug_show(getSpace),
      "update space - " # debug_show(updateSpace),
      "delete space - " # debug_show(deleteSpace),
    ];
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public query func test(): async [Text] {
    let map = Map.new<Nat32, Nat32>();

    var i = 0:Nat32;

    while (i != 100000) { Map.set(map, n32hash, i, i); i +%= 1 };

    let startSpace = Prim.rts_heap_size();

    let cost = IC.countInstructions(func() {
      var i = 0:Nat32;

      ignore Map.toArrayMap<Nat32, Nat32, Nat32>(map, func(key, value) = ?value);
    });

    let space = Prim.rts_heap_size() - startSpace:Nat;

    return [
      "cost - " # debug_show(cost),
      "---",
      "space - " # debug_show(space),
    ];
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public query func heapSize(): async Nat {
    return Prim.rts_heap_size();
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public query func runTests(): async () {
    TestMap.runTests();
    TestSet.runTests();
  };
};
