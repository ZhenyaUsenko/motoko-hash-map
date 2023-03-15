import Map "../src/Map";

module {
  let { thash } = Map;

  let entries = [
    ("importance", 1),
    ("technology", 2),
    ("elevator", 3),
    ("department", 4),
    ("climate", 5),
    ("language", 6),
    ("ear", 7),
    ("administration", 8),
    ("independence", 9),
    ("decision", 10),
    ("way", 11),
    ("supermarket", 12),
    ("preparation", 13),
    ("employment", 14),
    ("topic", 15),
    ("suggestion", 16),
    ("initiative", 17),
    ("food", 18),
    ("preference", 19),
    ("assistance", 20),
  ];

  let entriesDesc = [
    ("assistance", 20),
    ("preference", 19),
    ("food", 18),
    ("initiative", 17),
    ("suggestion", 16),
    ("topic", 15),
    ("employment", 14),
    ("preparation", 13),
    ("supermarket", 12),
    ("way", 11),
    ("decision", 10),
    ("independence", 9),
    ("administration", 8),
    ("ear", 7),
    ("language", 6),
    ("climate", 5),
    ("department", 4),
    ("elevator", 3),
    ("technology", 2),
    ("importance", 1),
  ];

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func runTests() {
    let map = Map.new<Text, Nat>(thash);

    for ((key, value) in entries.vals()) Map.set(map, thash, key, value);

    let mapIter = Map.entries(map);

    let mapFromIter = Map.fromIter<Text, Nat>(entries.vals(), thash);

    let mapFromIterDesc = Map.fromIterDesc<Text, Nat>(entries.vals(), thash);

    let mapFromIterMap = Map.fromIterMap<Text, Nat, (Text, Nat)>(entries.vals(), thash, func(key, value) = ?(key, value + 100));

    let mapFromIterMapDesc = Map.fromIterMapDesc<Text, Nat, (Text, Nat)>(entries.vals(), thash, func(key, value) = ?(key, value + 100));

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    let mappedFilteredMap = Map.mapFilter<Text, Nat, Text>(map, thash, func(key, value) = if (value % 5 != 0) ?key else null);

    let mappedFilteredDescMap = Map.mapFilterDesc<Text, Nat, Text>(map, thash, func(key, value) = if (value % 5 != 0) ?key else null);

    let mappedMap = Map.map<Text, Nat, Nat>(map, thash, func(key, value) = value + 100);

    let mappedDescMap = Map.mapDesc<Text, Nat, Nat>(map, thash, func(key, value) = value + 100);

    let filteredMap = Map.filter<Text, Nat>(map, thash, func(key, value) = value % 5 != 0);

    let filteredDescMap = Map.filterDesc<Text, Nat>(map, thash, func(key, value) = value % 5 != 0);

    let clonedMap = Map.clone(map);

    let clonedDescMap = Map.cloneDesc(map);

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    let arrayFromMap = Map.toArray(map);

    let arrayFromMapDesc = Map.toArrayDesc(map);

    let mappedArrayFromMap = Map.toArrayMap<Text, Nat, (Text, Nat)>(map, func(key, value) = ?(key, value + 100));

    let mappedArrayFromMapDesc = Map.toArrayMapDesc<Text, Nat, (Text, Nat)>(map, func(key, value) = ?(key, value + 100));

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    assert Map.get(map, thash, "climate") == ?5;
    assert Map.get(map, thash, "decision") == ?10;
    assert Map.get(map, thash, "topic") == ?15;
    assert Map.get(map, thash, "assistance") == ?20;
    assert Map.get(map, thash, "xxx") == null;

    assert Map.get(mapFromIter, thash, "climate") == ?5;
    assert Map.get(mapFromIter, thash, "decision") == ?10;
    assert Map.get(mapFromIter, thash, "topic") == ?15;
    assert Map.get(mapFromIter, thash, "assistance") == ?20;
    assert Map.get(mapFromIter, thash, "xxx") == null;

    assert Map.get(mapFromIterMap, thash, "climate") == ?105;
    assert Map.get(mapFromIterMap, thash, "decision") == ?110;
    assert Map.get(mapFromIterMap, thash, "topic") == ?115;
    assert Map.get(mapFromIterMap, thash, "assistance") == ?120;
    assert Map.get(mapFromIterMap, thash, "xxx") == null;

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    assert Map.get(mappedFilteredMap, thash, "climate") == null;
    assert Map.get(mappedFilteredMap, thash, "decision") == null;
    assert Map.get(mappedFilteredMap, thash, "topic") == null;
    assert Map.get(mappedFilteredMap, thash, "assistance") == null;
    assert Map.get(mappedFilteredMap, thash, "xxx") == null;
    assert Map.get(mappedFilteredMap, thash, "department") == ?"department";
    assert Map.get(mappedFilteredMap, thash, "independence") == ?"independence";
    assert Map.get(mappedFilteredMap, thash, "employment") == ?"employment";
    assert Map.get(mappedFilteredMap, thash, "preference") == ?"preference";

    assert Map.get(mappedMap, thash, "climate") == ?105;
    assert Map.get(mappedMap, thash, "decision") == ?110;
    assert Map.get(mappedMap, thash, "topic") == ?115;
    assert Map.get(mappedMap, thash, "assistance") == ?120;
    assert Map.get(mappedMap, thash, "xxx") == null;

    assert Map.get(filteredMap, thash, "climate") == null;
    assert Map.get(filteredMap, thash, "decision") == null;
    assert Map.get(filteredMap, thash, "topic") == null;
    assert Map.get(filteredMap, thash, "assistance") == null;
    assert Map.get(filteredMap, thash, "xxx") == null;
    assert Map.get(filteredMap, thash, "department") == ?4;
    assert Map.get(filteredMap, thash, "independence") == ?9;
    assert Map.get(filteredMap, thash, "employment") == ?14;
    assert Map.get(filteredMap, thash, "preference") == ?19;

    assert Map.get(clonedMap, thash, "climate") == ?5;
    assert Map.get(clonedMap, thash, "decision") == ?10;
    assert Map.get(clonedMap, thash, "topic") == ?15;
    assert Map.get(clonedMap, thash, "assistance") == ?20;
    assert Map.get(clonedMap, thash, "xxx") == null;

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    for (index in entries.keys()) assert arrayFromMap[index] == entries[index];

    for (index in entriesDesc.keys()) assert arrayFromMapDesc[index] == entriesDesc[index];

    for (index in entries.keys()) assert mappedArrayFromMap[index] == (entries[index].0, entries[index].1 + 100);

    for (index in entriesDesc.keys()) assert mappedArrayFromMapDesc[index] == (entriesDesc[index].0, entriesDesc[index].1 + 100);

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    for ((key, value) in entries.vals()) assert Map.get(map, thash, key) == ?value;

    for ((key, value) in entries.vals()) assert Map.get(mapFromIter, thash, key) == ?value;

    for ((key, value) in entries.vals()) assert Map.get(mapFromIterMap, thash, key) == ?(value + 100);

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    for ((key, value) in entries.vals()) assert Map.get(mappedFilteredMap, thash, key) == (if (value % 5 != 0) ?key else null);

    for ((key, value) in entries.vals()) assert Map.get(mappedMap, thash, key) == ?(value + 100);

    for ((key, value) in entries.vals()) assert Map.get(filteredMap, thash, key) == (if (value % 5 != 0) ?value else null);

    for ((key, value) in entries.vals()) assert Map.get(clonedMap, thash, key) == ?value;

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    let mapFromIterDescIter = Map.entries(mapFromIterDesc);

    for (entry in Map.entries(mapFromIter)) assert mapFromIterDescIter.prev() == ?entry;

    let mapFromIterMapDescIter = Map.entries(mapFromIterMapDesc);

    for (entry in Map.entries(mapFromIterMap)) assert mapFromIterMapDescIter.prev() == ?entry;

    let mappedFilteredDescMapIter = Map.entries(mappedFilteredDescMap);

    for (entry in Map.entries(mappedFilteredMap)) assert mappedFilteredDescMapIter.prev() == ?entry;

    let mappedDescMapIter = Map.entries(mappedDescMap);

    for (entry in Map.entries(mappedMap)) assert mappedDescMapIter.prev() == ?entry;

    let filteredDescMapIter = Map.entries(filteredDescMap);

    for (entry in Map.entries(filteredMap)) assert filteredDescMapIter.prev() == ?entry;

    let clonedDescMapIter = Map.entries(clonedDescMap);

    for (entry in Map.entries(clonedMap)) assert clonedDescMapIter.prev() == ?entry;

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    assert Map.find<Text, Nat>(map, func(key, value) = value > 5) == ?("language", 6);
    assert Map.find<Text, Nat>(map, func(key, value) = value < 15) == ?("importance", 1);

    assert Map.findDesc<Text, Nat>(map, func(key, value) = value > 5) == ?("assistance", 20);
    assert Map.findDesc<Text, Nat>(map, func(key, value) = value < 15) == ?("employment", 14);

    assert Map.some<Text, Nat>(map, func(key, value) = key == "ear" and value == 7) == true;
    assert Map.some<Text, Nat>(map, func(key, value) = key == "xxx" and value == 0) == false;

    assert Map.someDesc<Text, Nat>(map, func(key, value) = key == "ear" and value == 7) == true;
    assert Map.someDesc<Text, Nat>(map, func(key, value) = key == "xxx" and value == 0) == false;

    assert Map.every<Text, Nat>(map, func(key, value) = value > 0) == true;
    assert Map.every<Text, Nat>(map, func(key, value) = value != 10) == false;

    assert Map.everyDesc<Text, Nat>(map, func(key, value) = value > 0) == true;
    assert Map.everyDesc<Text, Nat>(map, func(key, value) = value != 10) == false;

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    let entriesIter = entries.vals();

    Map.forEach<Text, Nat>(map, func(key, value) = assert entriesIter.next() == ?(key, value));

    let entriesDescIter = entriesDesc.vals();

    Map.forEachDesc<Text, Nat>(map, func(key, value) = assert entriesDescIter.next() == ?(key, value));

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    assert Map.putAfter(map, thash, "independence", ?"assistance", null) == ?9;
    assert Map.putAfter(map, thash, "importance", ?"independence", null) == ?1;

    assert Map.peek(map) == ?("importance", 1);
    assert Map.peekFront(map) == ?("technology", 2);

    assert Map.size(map) == 20;

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    assert Map.putBefore(map, thash, "independence", ?"decision", null) == ?9;
    assert Map.putBefore(map, thash, "importance", ?"technology", null) == ?1;

    assert Map.peek(map) == ?("assistance", 20);
    assert Map.peekFront(map) == ?("importance", 1);

    assert Map.size(map) == 20;

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    Map.set(map, thash, "xxx", 21);
    assert Map.put(map, thash, "yyy", 21) == null;

    Map.setFront(map, thash, "aaa", 0);
    assert Map.putFront(map, thash, "bbb", 0) == null;

    assert Map.size(map) == 24;

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    assert Map.pop(map) == ?("yyy", 21);
    assert Map.pop(map) == ?("xxx", 21);

    assert Map.popFront(map) == ?("bbb", 0);
    assert Map.popFront(map) == ?("aaa", 0);

    assert Map.size(map) == 20;

    ignore mapIter.reset();

    for (entry in entries.vals()) assert mapIter.next() == ?entry;

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    assert Map.putMove(map, thash, "topic", null) == ?15;
    assert Map.putMove(map, thash, "suggestion", null) == ?16;

    assert Map.putMoveFront(map, thash, "language", ?21) == ?6;
    assert Map.putMoveFront(map, thash, "climate", ?21) == ?5;

    assert Map.size(map) == 20;

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    assert Map.pop(map) == ?("suggestion", 16);
    assert Map.pop(map) == ?("topic", 15);

    assert Map.popFront(map) == ?("climate", 21);
    assert Map.popFront(map) == ?("language", 21);

    assert Map.size(map) == 16;

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    assert Map.update<Text, Nat>(map, thash, "way", func(key, value) = switch (value) { case (?value) ?(value + 100); case (_) ?222 }) == ?111;
    assert Map.update<Text, Nat>(map, thash, "xxx", func(key, value) = switch (value) { case (?value) ?(value + 100); case (_) ?222 }) == ?222;

    assert Map.updateFront<Text, Nat>(map, thash, "ear", func(key, value) = switch (value) { case (?value) ?(value + 100); case (_) ?222 }) == ?107;
    assert Map.updateFront<Text, Nat>(map, thash, "yyy", func(key, value) = switch (value) { case (?value) ?(value + 100); case (_) ?222 }) == ?222;

    assert Map.size(map) == 18;

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    assert Map.updateMove<Text, Nat>(map, thash, "way", func(key, value) = switch (value) { case (?value) ?(value - 108); case (_) ?0 }) == ?3;

    assert Map.updateMoveFront<Text, Nat>(map, thash, "ear", func(key, value) = switch (value) { case (?value) ?(value - 105); case (_) ?0 }) == ?2;

    assert Map.size(map) == 18;

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    assert Map.pop(map) == ?("way", 3);
    assert Map.pop(map) == ?("xxx", 222);

    assert Map.popFront(map) == ?("ear", 2);
    assert Map.popFront(map) == ?("yyy", 222);

    assert Map.size(map) == 14;

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    Map.delete(map, thash, "decision");
    assert Map.remove(map, thash, "food") == ?18;

    assert Map.get(map, thash, "decision") == null;
    assert Map.get(map, thash, "food") == null;

    assert Map.size(map) == 12;

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    Map.clear(map);

    assert Map.get(map, thash, "importance") == null;
    assert Map.get(map, thash, "ear") == null;

    assert Map.size(map) == 0;
  };
};
