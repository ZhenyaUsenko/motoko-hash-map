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

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func runTests() {
    let set = Set.new<Text>();

    for (key in keys.vals()) Set.add(set, thash, key);

    let setFromIter = Set.fromIter<Text>(keys.vals(), thash);

    let filteredSet = Set.filter<Text>(set, func(key) { key != "climate" and key != "decision" });

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    assert Set.has(set, thash, "climate");
    assert Set.has(set, thash, "decision");
    assert Set.has(set, thash, "topic");
    assert Set.has(set, thash, "assistance");
    assert not Set.has(set, thash, "xxx");

    assert Set.has(setFromIter, thash, "climate");
    assert Set.has(setFromIter, thash, "decision");
    assert Set.has(setFromIter, thash, "topic");
    assert Set.has(setFromIter, thash, "assistance");
    assert not Set.has(setFromIter, thash, "xxx");

    assert not Set.has(filteredSet, thash, "climate");
    assert not Set.has(filteredSet, thash, "decision");
    assert Set.has(filteredSet, thash, "topic");
    assert Set.has(filteredSet, thash, "assistance");
    assert not Set.has(filteredSet, thash, "xxx");

    assert Set.has(filteredSet, thash, "department");
    assert Set.has(filteredSet, thash, "independence");
    assert Set.has(filteredSet, thash, "employment");
    assert Set.has(filteredSet, thash, "preference");

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    assert Set.size(set) == 20;

    assert Set.size(setFromIter) == 20;

    assert Set.size(filteredSet) == 18;

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    for (key in keys.vals()) assert Set.has(set, thash, key);

    for (key in keys.vals()) assert Set.has(setFromIter, thash, key);

    for (key in keys.vals()) assert Set.has(filteredSet, thash, key) == (key != "climate" and key != "decision");

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    assert Set.find<Text>(set, func(key) { key == "climate" }) == ?"climate";
    assert Set.find<Text>(set, func(key) { key == "decision" }) == ?"decision";
    assert Set.find<Text>(set, func(key) { key == "topic" }) == ?"topic";
    assert Set.find<Text>(set, func(key) { key == "assistance" }) == ?"assistance";

    assert Set.findLast<Text>(set, func(key) { key == "climate" }) == ?"climate";
    assert Set.findLast<Text>(set, func(key) { key == "decision" }) == ?"decision";
    assert Set.findLast<Text>(set, func(key) { key == "topic" }) == ?"topic";
    assert Set.findLast<Text>(set, func(key) { key == "assistance" }) == ?"assistance";

    assert Set.find<Text>(set, func(key) { key.size() < 5 }) == ?"ear";
    assert Set.find<Text>(set, func(key) { key.size() == 10 }) == ?"importance";

    assert Set.findLast<Text>(set, func(key) { key.size() < 5 }) == ?"food";
    assert Set.findLast<Text>(set, func(key) { key.size() == 10 }) == ?"assistance";

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    for (key in Set.keys(set)) assert Set.has(set, thash, key);

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    Set.forEach<Text>(set, func(key) { assert Set.has(set, thash, key) });

    assert Set.some<Text>(set, func(key) { key == "ear" }) == true;
    assert Set.some<Text>(set, func(key) { key == "xxx" }) == false;

    assert Set.every<Text>(set, func(key) { key.size() < 20 }) == true;
    assert Set.every<Text>(set, func(key) { key.size() > 4 }) == false;

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    Set.delete(set, thash, "way");
    Set.delete(set, thash, "food");

    assert not Set.has(set, thash, "way");
    assert not Set.has(set, thash, "food");

    assert Set.size(set) == 18;

    for (key in Set.keys(set)) if (key.size() > 5) Set.delete(set, thash, key);

    assert not Set.has(set, thash, "importance");
    assert Set.has(set, thash, "ear");

    assert Set.size(set) == 2;

    Set.clear(set);

    assert not Set.has(set, thash, "importance");
    assert not Set.has(set, thash, "ear");

    assert Set.size(set) == 0;
  };
};
