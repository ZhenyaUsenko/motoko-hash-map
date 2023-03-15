import Set "../src/Set";

module {
  let { thash } = Set;

  let keys = [
    "importance",
    "technology",
    "elevator",
    "department",
    "climate",
    "language",
    "ear",
    "administration",
    "independence",
    "decision",
    "way",
    "supermarket",
    "preparation",
    "employment",
    "topic",
    "suggestion",
    "initiative",
    "food",
    "preference",
    "assistance",
  ];

  let keysDesc = [
    "assistance",
    "preference",
    "food",
    "initiative",
    "suggestion",
    "topic",
    "employment",
    "preparation",
    "supermarket",
    "way",
    "decision",
    "independence",
    "administration",
    "ear",
    "language",
    "climate",
    "department",
    "elevator",
    "technology",
    "importance",
  ];

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func runTests() {
    let map = Set.new(thash);

    for (key in keys.vals()) Set.add(map, thash, key);

    let mapIter = Set.keys(map);

    let mapFromIter = Set.fromIter(keys.vals(), thash);

    let mapFromIterDesc = Set.fromIterDesc(keys.vals(), thash);

    let mapFromIterMap = Set.fromIterMap<Text, Text>(keys.vals(), thash, func(key) = ?key);

    let mapFromIterMapDesc = Set.fromIterMapDesc<Text, Text>(keys.vals(), thash, func(key) = ?key);

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    let filteredMap = Set.filter<Text>(map, thash, func(key) = key != "climate");

    let filteredDescMap = Set.filterDesc<Text>(map, thash, func(key) = key != "climate");

    let clonedMap = Set.clone(map);

    let clonedDescMap = Set.cloneDesc(map);

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    let arrayFromMap = Set.toArray(map);

    let arrayFromMapDesc = Set.toArrayDesc(map);

    let mappedArrayFromMap = Set.toArrayMap<Text, Text>(map, func(key) = ?key);

    let mappedArrayFromMapDesc = Set.toArrayMapDesc<Text, Text>(map, func(key) = ?key);

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    assert Set.has(map, thash, "climate") == true;
    assert Set.has(map, thash, "decision") == true;
    assert Set.has(map, thash, "topic") == true;
    assert Set.has(map, thash, "assistance") == true;
    assert Set.has(map, thash, "xxx") == false;

    assert Set.has(mapFromIter, thash, "climate") == true;
    assert Set.has(mapFromIter, thash, "decision") == true;
    assert Set.has(mapFromIter, thash, "topic") == true;
    assert Set.has(mapFromIter, thash, "assistance") == true;
    assert Set.has(mapFromIter, thash, "xxx") == false;

    assert Set.has(mapFromIterMap, thash, "climate") == true;
    assert Set.has(mapFromIterMap, thash, "decision") == true;
    assert Set.has(mapFromIterMap, thash, "topic") == true;
    assert Set.has(mapFromIterMap, thash, "assistance") == true;
    assert Set.has(mapFromIterMap, thash, "xxx") == false;

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    assert Set.has(filteredMap, thash, "climate") == false;
    assert Set.has(filteredMap, thash, "department") == true;
    assert Set.has(filteredMap, thash, "independence") == true;
    assert Set.has(filteredMap, thash, "employment") == true;
    assert Set.has(filteredMap, thash, "preference") == true;

    assert Set.has(clonedMap, thash, "climate") == true;
    assert Set.has(clonedMap, thash, "decision") == true;
    assert Set.has(clonedMap, thash, "topic") == true;
    assert Set.has(clonedMap, thash, "assistance") == true;
    assert Set.has(clonedMap, thash, "xxx") == false;

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    for (index in keys.keys()) assert arrayFromMap[index] == keys[index];

    for (index in keysDesc.keys()) assert arrayFromMapDesc[index] == keysDesc[index];

    for (index in keys.keys()) assert mappedArrayFromMap[index] == keys[index];

    for (index in keysDesc.keys()) assert mappedArrayFromMapDesc[index] == keysDesc[index];

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    for (key in keys.vals()) assert Set.has(map, thash, key) == true;

    for (key in keys.vals()) assert Set.has(mapFromIter, thash, key) == true;

    for (key in keys.vals()) assert Set.has(mapFromIterMap, thash, key) == true;

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    for (key in keys.vals()) assert Set.has(filteredMap, thash, key) == (key != "climate");

    for (key in keys.vals()) assert Set.has(clonedMap, thash, key) == true;

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    let mapFromIterDescIter = Set.keys(mapFromIterDesc);

    for (entry in Set.keys(mapFromIter)) assert mapFromIterDescIter.prev() == ?entry;

    let mapFromIterMapDescIter = Set.keys(mapFromIterMapDesc);

    for (entry in Set.keys(mapFromIterMap)) assert mapFromIterMapDescIter.prev() == ?entry;

    let filteredDescMapIter = Set.keys(filteredDescMap);

    for (entry in Set.keys(filteredMap)) assert filteredDescMapIter.prev() == ?entry;

    let clonedDescMapIter = Set.keys(clonedDescMap);

    for (entry in Set.keys(clonedMap)) assert clonedDescMapIter.prev() == ?entry;

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    assert Set.find<Text>(map, func(key) = key.size() == 3) == ?"ear";
    assert Set.find<Text>(map, func(key) = key.size() == 8) == ?"elevator";

    assert Set.findDesc<Text>(map, func(key) = key.size() == 3) == ?"way";
    assert Set.findDesc<Text>(map, func(key) = key.size() == 8) == ?"decision";

    assert Set.some<Text>(map, func(key) = key == "ear") == true;
    assert Set.some<Text>(map, func(key) = key == "xxx") == false;

    assert Set.someDesc<Text>(map, func(key) = key == "ear") == true;
    assert Set.someDesc<Text>(map, func(key) = key == "xxx") == false;

    assert Set.every<Text>(map, func(key) = key != "xxx") == true;
    assert Set.every<Text>(map, func(key) = key != "ear") == false;

    assert Set.everyDesc<Text>(map, func(key) = key != "xxx") == true;
    assert Set.everyDesc<Text>(map, func(key) = key != "ear") == false;

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    let keysIter = keys.vals();

    Set.forEach<Text>(map, func(key) = assert keysIter.next() == ?key);

    let keysDescIter = keysDesc.vals();

    Set.forEachDesc<Text>(map, func(key) = assert keysDescIter.next() == ?key);

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    assert Set.putAfter(map, thash, "independence", ?"assistance") == true;
    assert Set.putAfter(map, thash, "importance", ?"independence") == true;

    assert Set.peek(map) == ?"importance";
    assert Set.peekFront(map) == ?"technology";

    assert Set.size(map) == 20;

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    assert Set.putBefore(map, thash, "independence", ?"decision") == true;
    assert Set.putBefore(map, thash, "importance", ?"technology") == true;

    assert Set.peek(map) == ?"assistance";
    assert Set.peekFront(map) == ?"importance";

    assert Set.size(map) == 20;

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    Set.add(map, thash, "xxx");
    assert Set.put(map, thash, "yyy") == false;

    Set.addFront(map, thash, "aaa");
    assert Set.putFront(map, thash, "bbb") == false;

    assert Set.size(map) == 24;

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    assert Set.pop(map) == ?"yyy";
    assert Set.pop(map) == ?"xxx";

    assert Set.popFront(map) == ?"bbb";
    assert Set.popFront(map) == ?"aaa";

    assert Set.size(map) == 20;

    ignore mapIter.reset();

    for (entry in keys.vals()) assert mapIter.next() == ?entry;

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    assert Set.putMove(map, thash, "topic") == true;
    assert Set.putMove(map, thash, "suggestion") == true;

    assert Set.putMoveFront(map, thash, "language") == true;
    assert Set.putMoveFront(map, thash, "climate") == true;

    assert Set.size(map) == 20;

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    assert Set.pop(map) == ?"suggestion";
    assert Set.pop(map) == ?"topic";

    assert Set.popFront(map) == ?"climate";
    assert Set.popFront(map) == ?"language";

    assert Set.size(map) == 16;

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    Set.delete(map, thash, "decision");
    assert Set.remove(map, thash, "food") == true;

    assert Set.has(map, thash, "decision") == false;
    assert Set.has(map, thash, "food") == false;

    assert Set.size(map) == 14;

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    Set.clear(map);

    assert Set.has(map, thash, "importance") == false;
    assert Set.has(map, thash, "ear") == false;

    assert Set.size(map) == 0;
  };
};
