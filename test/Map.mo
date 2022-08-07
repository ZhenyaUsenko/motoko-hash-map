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

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func runTests() {
    let map = Map.new<Text, Nat>();

    for ((key, value) in entries.vals()) Map.set(map, thash, key, value);

    let mapFromIter = Map.fromIter<Text, Nat>(entries.vals(), thash);

    let mappedMap = Map.map<Text, Nat, Nat>(map, func(key, value) { value + 100 });

    let mappedFilteredMap = Map.mapFilter<Text, Nat, Text>(map, func(key, value) { if (value % 5 != 0) ?key else null });

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

    assert Map.get(mappedMap, thash, "climate") == ?105;
    assert Map.get(mappedMap, thash, "decision") == ?110;
    assert Map.get(mappedMap, thash, "topic") == ?115;
    assert Map.get(mappedMap, thash, "assistance") == ?120;
    assert Map.get(mappedMap, thash, "xxx") == null;

    assert Map.get(mappedFilteredMap, thash, "climate") == null;
    assert Map.get(mappedFilteredMap, thash, "decision") == null;
    assert Map.get(mappedFilteredMap, thash, "topic") == null;
    assert Map.get(mappedFilteredMap, thash, "assistance") == null;
    assert Map.get(mappedFilteredMap, thash, "xxx") == null;

    assert Map.get(mappedFilteredMap, thash, "department") == ?"department";
    assert Map.get(mappedFilteredMap, thash, "independence") == ?"independence";
    assert Map.get(mappedFilteredMap, thash, "employment") == ?"employment";
    assert Map.get(mappedFilteredMap, thash, "preference") == ?"preference";

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    assert Map.size(map) == 20;

    assert Map.size(mapFromIter) == 20;

    assert Map.size(mappedMap) == 20;

    assert Map.size(mappedFilteredMap) == 16;

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    for ((key, value) in entries.vals()) assert Map.get(map, thash, key) == ?value;

    for ((key, value) in entries.vals()) assert Map.get(mapFromIter, thash, key) == ?value;

    for ((key, value) in entries.vals()) assert Map.get(mappedMap, thash, key) == ?(value + 100);

    for ((key, value) in entries.vals()) assert Map.get(mappedFilteredMap, thash, key) == (if (value % 5 != 0) ?key else null);

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    assert Map.find<Text, Nat>(map, func(key, value) { key == "climate" }) == ?("climate", 5);
    assert Map.find<Text, Nat>(map, func(key, value) { key == "decision" }) == ?("decision", 10);
    assert Map.find<Text, Nat>(map, func(key, value) { key == "topic" }) == ?("topic", 15);
    assert Map.find<Text, Nat>(map, func(key, value) { key == "assistance" }) == ?("assistance", 20);

    assert Map.findLast<Text, Nat>(map, func(key, value) { key == "climate" }) == ?("climate", 5);
    assert Map.findLast<Text, Nat>(map, func(key, value) { key == "decision" }) == ?("decision", 10);
    assert Map.findLast<Text, Nat>(map, func(key, value) { key == "topic" }) == ?("topic", 15);
    assert Map.findLast<Text, Nat>(map, func(key, value) { key == "assistance" }) == ?("assistance", 20);

    assert Map.find<Text, Nat>(map, func(key, value) { value > 5 }) == ?("language", 6);
    assert Map.find<Text, Nat>(map, func(key, value) { value < 15 }) == ?("importance", 1);

    assert Map.findLast<Text, Nat>(map, func(key, value) { value > 5 }) == ?("assistance", 20);
    assert Map.findLast<Text, Nat>(map, func(key, value) { value < 15 }) == ?("employment", 14);

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    for (key in Map.keys(map)) assert Map.has(map, thash, key);

    for (value in Map.vals(map)) assert Map.find<Text, Nat>(map, func(key, mapValue) { mapValue == value }) != null;

    for ((key, value) in Map.entries(map)) assert Map.get(map, thash, key) == ?value;

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    Map.forEach<Text, Nat>(map, func(key, value) { assert Map.get(map, thash, key) == ?value });

    assert Map.some<Text, Nat>(map, func(key, value) { key == "ear" and value == 7 }) == true;
    assert Map.some<Text, Nat>(map, func(key, value) { key == "xxx" and value == 0 }) == false;

    assert Map.every<Text, Nat>(map, func(key, value) { value > 0 }) == true;
    assert Map.every<Text, Nat>(map, func(key, value) { value != 10 }) == false;

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    Map.delete(map, thash, "way");
    Map.delete(map, thash, "food");

    assert Map.get(map, thash, "way") == null;
    assert Map.get(map, thash, "food") == null;

    assert Map.size(map) == 18;

    for ((key, value) in Map.entries(map)) if (value > 4) Map.delete(map, thash, key);

    assert Map.get(map, thash, "importance") == ?1;
    assert Map.get(map, thash, "ear") == null;

    assert Map.size(map) == 4;

    Map.clear(map);

    assert Map.get(map, thash, "importance") == null;
    assert Map.get(map, thash, "ear") == null;

    assert Map.size(map) == 0;
  };
};
