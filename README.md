```ts
import Map "mo:map/Map";
import { nhash } "mo:map/Map";

let map = Map.new<Nat, Nat>();

Map.set(map, nhash, 1, 1);

Map.delete(map, nhash, 1);
```

# Table of contents

## [**Map** interface description](#map-interface-description-1)
- [Initializing / measuring](#initializing--measuring)
  - [**new**](#new)
  - [**clear**](#clear)
  - [**size**](#size)
  - [**empty**](#empty)
  - [**make**](#make)
- [Inserting / updating](#inserting--updating)
  - [**put**](#put)
  - [**putFront**](#putfront)
  - [**set**](#set)
  - [**setFront**](#setfront)
  - [**add**](#add)
  - [**addFront**](#addfront)
  - [**replace**](#replace)
  - [**update**](#update)
  - [**updateFront**](#updatefront)
  - [**putMove**](#putmove)
  - [**putMoveFront**](#putmovefront)
  - [**replaceMove**](#replacemove)
  - [**replaceMoveFront**](#replacemovefront)
  - [**updateMove**](#updatemove)
  - [**updateMoveFront**](#updatemovefront)
- [Getting](#getting)
  - [**get**](#get)
  - [**has**](#has)
  - [**contains**](#contains)
- [Deleting](#deleting)
  - [**remove**](#remove)
  - [**delete**](#delete)
- [Performing queue operations](#performing-queue-operations)
  - [**peek**](#peek)
  - [**peekFront**](#peekfront)
  - [**pop**](#pop)
  - [**popFront**](#popfront)
  - [**cycle**](#cycle)
  - [**cycleFront**](#cyclefront)
- [Mapping / filtering / cloning](#mapping--filtering--cloning)
  - [**mapFilter**](#mapfilter)
  - [**mapFilterDesc**](#mapfilterdesc)
  - [**filter**](#filter)
  - [**filterDesc**](#filterdesc)
  - [**map**](#map)
  - [**mapDesc**](#mapdesc)
  - [**clone**](#clone)
  - [**cloneDesc**](#clonedesc)
- [Iterating](#iterating)
  - [**keys**](#keys)
  - [**keysDesc**](#keysdesc)
  - [**vals**](#vals)
  - [**valsDesc**](#valsdesc)
  - [**entries**](#entries)
  - [**entriesDesc**](#entriesdesc)
  - [**keysFrom**](#keysfrom)
  - [**keysFromDesc**](#keysfromdesc)
  - [**valsFrom**](#valsfrom)
  - [**valsFromDesc**](#valsfromdesc)
  - [**entriesFrom**](#entriesfrom)
  - [**entriesFromDesc**](#entriesfromdesc)
- [Searching](#searching)
  - [**find**](#find)
  - [**findDesc**](#finddesc)
  - [**some**](#some)
  - [**someDesc**](#somedesc)
  - [**every**](#every)
  - [**everyDesc**](#everydesc)
  - [**forEach**](#foreach)
  - [**forEachDesc**](#foreachdesc)
- [Constructing from iterators](#constructing-from-iterators)
  - [**fromIter**](#fromiter)
  - [**fromIterDesc**](#fromiterdesc)
  - [**fromIterMap**](#fromitermap)
  - [**fromIterMapDesc**](#fromitermapdesc)
- [Converting to arrays](#converting-to-arrays)
  - [**toArray**](#toarray)
  - [**toArrayDesc**](#toarraydesc)
  - [**toArrayMap**](#toarraymap)
  - [**toArrayMapDesc**](#toarraymapdesc)
- [Rehashing](#rehashing)
  - [**rehash**](#rehash)

## [**Set** interface description](#set-interface-description-1)
- [Initializing / measuring](#initializing--measuring-1)
  - [**new**](#new-1)
  - [**clear**](#clear-1)
  - [**size**](#size-1)
  - [**empty**](#empty-1)
  - [**make**](#make-1)
- [Inserting / updating](#inserting--updating-1)
  - [**put**](#put-1)
  - [**putFront**](#putfront-1)
  - [**add**](#add-1)
  - [**addFront**](#addfront-1)
  - [**putMove**](#putmove-1)
  - [**putMoveFront**](#putmovefront-1)
- [Getting](#getting-1)
  - [**has**](#has-1)
  - [**contains**](#contains-1)
- [Deleting](#deleting-1)
  - [**remove**](#remove-1)
  - [**delete**](#delete-1)
- [Performing queue operations](#performing-queue-operations-1)
  - [**peek**](#peek-1)
  - [**peekFront**](#peekfront-1)
  - [**pop**](#pop-1)
  - [**popFront**](#popfront-1)
  - [**cycle**](#cycle-1)
  - [**cycleFront**](#cyclefront-1)
- [Filtering / cloning](#filtering--cloning)
  - [**filter**](#filter-1)
  - [**filterDesc**](#filterdesc-1)
  - [**clone**](#clone-1)
  - [**cloneDesc**](#clonedesc-1)
- [Iterating](#iterating-1)
  - [**keys**](#keys-1)
  - [**keysDesc**](#keysdesc-1)
  - [**keysFrom**](#keysfrom-1)
  - [**keysFromDesc**](#keysfromdesc-1)
- [Searching](#searching-1)
  - [**find**](#find-1)
  - [**findDesc**](#finddesc-1)
  - [**some**](#some-1)
  - [**someDesc**](#somedesc-1)
  - [**every**](#every-1)
  - [**everyDesc**](#everydesc-1)
  - [**forEach**](#foreach-1)
  - [**forEachDesc**](#foreachdesc-1)
- [Constructing from iterators](#constructing-from-iterators-1)
  - [**fromIter**](#fromiter-1)
  - [**fromIterDesc**](#fromiterdesc-1)
  - [**fromIterMap**](#fromitermap-1)
  - [**fromIterMapDesc**](#fromitermapdesc-1)
- [Converting to arrays](#converting-to-arrays-1)
  - [**toArray**](#toarray-1)
  - [**toArrayDesc**](#toarraydesc-1)
  - [**toArrayMap**](#toarraymap-1)
  - [**toArrayMapDesc**](#toarraymapdesc-1)
- [Rehashing](#rehashing-1)
  - [**rehash**](#rehash-1)

## [**Iterator** interface description](#iterator-interface-description-1)
- [Moving a pointer](#moving-a-pointer)
  - [**prev**](#prev)
  - [**next**](#next)
  - [**peekPrev**](#peekprev)
  - [**peekNext**](#peeknext)
  - [**movePrev**](#moveprev)
  - [**moveNext**](#movenext)
- [Getting current item](#getting-current-item)
  - [**current**](#current)
- [Getting iterator state](#getting-iterator-state)
  - [**started**](#started)
  - [**finished**](#finished)
- [Resetting](#resetting)
  - [**reset**](#reset)

## [**Hash Utils**](#hash-utils-1)
- [Calculating hashes](#calculating-hashes)
  - [**hashInt**](#hashint)
  - [**hashInt8**](#hashint8)
  - [**hashInt16**](#hashint16)
  - [**hashInt32**](#hashint32)
  - [**hashInt64**](#hashint64)
  - [**hashNat**](#hashnat)
  - [**hashNat8**](#hashnat8)
  - [**hashNat16**](#hashnat16)
  - [**hashNat32**](#hashnat32)
  - [**hashNat64**](#hashnat64)
  - [**hashText**](#hashtext)
  - [**hashPrincipal**](#hashprincipal)
  - [**hashBlob**](#hashblob)
  - [**hashBool**](#hashbool)
- [Composite utils](#composite-utils)
  - [**ihash**](#ihash)
  - [**i8hash**](#i8hash)
  - [**i16hash**](#i16hash)
  - [**i32hash**](#i32hash)
  - [**i64hash**](#i64hash)
  - [**nhash**](#nhash)
  - [**n8hash**](#n8hash)
  - [**n16hash**](#n16hash)
  - [**n32hash**](#n32hash)
  - [**n64hash**](#n64hash)
  - [**thash**](#thash)
  - [**phash**](#phash)
  - [**bhash**](#bhash)
  - [**lhash**](#lhash)
- [Constructing new utils](#constructing-new-utils)
  - [**combineHash**](#combinehash)
  - [**useHash**](#usehash)
  - [**calcHash**](#calchash)

# **Map** Interface description

```ts
type Map<K, V> = [var ?(
  keys: [var ?K],
  values: [var ?V],
  indexes: [var Nat],
  bounds: [var Nat32],
)];
```

## Initializing / measuring

### `new`

```ts
func new<K, V>(): Map<K, V>
```

Creates and returns an empty **Map**. Empty **Map** has a special optimization and takes only **12 bytes** of space. The **Map** structure will be initialized only after the insertion of the **first** **entry** and will occupy minimum **112 bytes**.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(1)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `clear`

```ts
func clear<K, V>(map: Map<K, V>): ()
```

Removes all entries from the **Map**.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(1)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `size`

```ts
func size<K, V>(map: Map<K, V>): Nat
```

Returns the amount of entries in the **Map**.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(1)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `empty`

```ts
func empty<K, V>(map: Map<K, V>): Bool
```

Returns a boolean indicating whether the **Map** is empty (has zero entries) or not.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(1)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `make`

```ts
func make<K, V>(hashUtils: HashUtils<K>, key: K, value: V): Map<K, V>
```

Creates a new **Map**, initializes it with the single provided **(key, value)** pair and returns the **Map**. The minimum amount of space a non-empty **Map** can occupy is **112 bytes**.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the first argument.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(1)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

## Inserting / updating

### `put`

```ts
func put<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, key: K, value: V): ?V
```

If the provided **key** is not present in the **Map**, inserts a new **(key, value)** pair at the **back** of the **Map** and returns **null**.

If the provided **key** is present in the **Map**, replaces the **previous value** with the provided **value** and returns the previous **value**. The position of the **(key, value)** pair within the **Map** is not changed.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

If the **Map** is empty, on the first insert the **Map** structure will be initialized which will take additional **100 bytes** of space. The minimum amount of space a non-empty **Map** can occupy is **112 bytes**.

Most **put** calls will not cause any space allocations and will be executed in **O(1)** time. If the **Map** is full, on the other hand, and a new **entry** is about to be inserted, [**rehash**](#rehash) will happen, meaning additional space will be allocated, equal to the **power of 2** that can hold **8/7** of all current entries (either equal or double the current capacity) times **16 bytes**, all entries will be moved to the new allocated space, holes will be eliminated and the operation time will be proportional to the **Map** size.

**Map** being full does not necessarily mean **size == capacity** as on deletion holes (unused array indexes) may (if a deletion happens inside the **Map**) or may not (if you delete the **first** or the **last** **entry**) be created.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(n)**|

---

### `putFront`

```ts
func putFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, key: K, value: V): ?V
```

If the provided **key** is not present in the **Map**, inserts a new **(key, value)** pair at the **front** of the **Map** and returns **null**.

If the provided **key** is present in the **Map**, replaces the **previous value** with the provided **value** and returns the previous **value**. The position of the **(key, value)** pair within the **Map** is not changed.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

If the **Map** is empty, on the first insert the **Map** structure will be initialized which will take additional **100 bytes** of space. The minimum amount of space a non-empty **Map** can occupy is **112 bytes**.

Most **putFront** calls will not cause any space allocations and will be executed in **O(1)** time. If the **Map** is full, on the other hand, and a new **entry** is about to be inserted, [**rehash**](#rehash) will happen, meaning additional space will be allocated, equal to the **power of 2** that can hold **8/7** of all current entries (either equal or double the current capacity) times **16 bytes**, all entries will be moved to the new allocated space, holes will be eliminated and the operation time will be proportional to the **Map** size.

**Map** being full does not necessarily mean **size == capacity** as on deletion holes (unused array indexes) may (if a deletion happens inside the **Map**) or may not (if you delete the **first** or the **last** **entry**) be created.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(n)**|

---

### `set`

```ts
func set<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, key: K, value: V): ()
```

If the provided **key** is not present in the **Map**, inserts a new **(key, value)** pair at the **back** of the **Map**.

If the provided **key** is present in the **Map**, replaces the **previous value** with the provided **value**. The position of the **(key, value)** pair within the **Map** is not changed.

This operation is the same as [**put**](#put) but doesn't have a return value.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

If the **Map** is empty, on the first insert the **Map** structure will be initialized which will take additional **100 bytes** of space. The minimum amount of space a non-empty **Map** can occupy is **112 bytes**.

Most **set** calls will not cause any space allocations and will be executed in **O(1)** time. If the **Map** is full, on the other hand, and a new **entry** is about to be inserted, [**rehash**](#rehash) will happen, meaning additional space will be allocated, equal to the **power of 2** that can hold **8/7** of all current entries (either equal or double the current capacity) times **16 bytes**, all entries will be moved to the new allocated space, holes will be eliminated and the operation time will be proportional to the **Map** size.

**Map** being full does not necessarily mean **size == capacity** as on deletion holes (unused array indexes) may (if a deletion happens inside the **Map**) or may not (if you delete the **first** or the **last** **entry**) be created.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(n)**|

---

### `setFront`

```ts
func setFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, key: K, value: V): ()
```

If the provided **key** is not present in the **Map**, inserts a new **(key, value)** pair at the **front** of the **Map**.

If the provided **key** is present in the **Map**, replaces the **previous value** with the provided **value**. The position of the **(key, value)** pair within the **Map** is not changed.

This operation is the same as [**putFront**](#putfront) but doesn't have a return value.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

If the **Map** is empty, on the first insert the **Map** structure will be initialized which will take additional **100 bytes** of space. The minimum amount of space a non-empty **Map** can occupy is **112 bytes**.

Most **setFront** calls will not cause any space allocations and will be executed in **O(1)** time. If the **Map** is full, on the other hand, and a new **entry** is about to be inserted, [**rehash**](#rehash) will happen, meaning additional space will be allocated, equal to the **power of 2** that can hold **8/7** of all current entries (either equal or double the current capacity) times **16 bytes**, all entries will be moved to the new allocated space, holes will be eliminated and the operation time will be proportional to the **Map** size.

**Map** being full does not necessarily mean **size == capacity** as on deletion holes (unused array indexes) may (if a deletion happens inside the **Map**) or may not (if you delete the **first** or the **last** **entry**) be created.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(n)**|

---

### `add`

```ts
func add<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, key: K, value: V): ?V
```

If the provided **key** is not present in the **Map**, inserts a new **(key, value)** pair at the **back** of the **Map** and returns **null**.

If the provided **key** is present in the **Map**, does no modifications and returns the previous **value**.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

If the **Map** is empty, on the first insert the **Map** structure will be initialized which will take additional **100 bytes** of space. The minimum amount of space a non-empty **Map** can occupy is **112 bytes**.

Most **add** calls will not cause any space allocations and will be executed in **O(1)** time. If the **Map** is full, on the other hand, and a new **entry** is about to be inserted, [**rehash**](#rehash) will happen, meaning additional space will be allocated, equal to the **power of 2** that can hold **8/7** of all current entries (either equal or double the current capacity) times **16 bytes**, all entries will be moved to the new allocated space, holes will be eliminated and the operation time will be proportional to the **Map** size.

**Map** being full does not necessarily mean **size == capacity** as on deletion holes (unused array indexes) may (if a deletion happens inside the **Map**) or may not (if you delete the **first** or the **last** **entry**) be created.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(n)**|

---

### `addFront`

```ts
func addFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, key: K, value: V): ?V
```

If the provided **key** is not present in the **Map**, inserts a new **(key, value)** pair at the **front** of the **Map** and returns **null**.

If the provided **key** is present in the **Map**, does no modifications and returns the previous **value**.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

If the **Map** is empty, on the first insert the **Map** structure will be initialized which will take additional **100 bytes** of space. The minimum amount of space a non-empty **Map** can occupy is **112 bytes**.

Most **addFront** calls will not cause any space allocations and will be executed in **O(1)** time. If the **Map** is full, on the other hand, and a new **entry** is about to be inserted, [**rehash**](#rehash) will happen, meaning additional space will be allocated, equal to the **power of 2** that can hold **8/7** of all current entries (either equal or double the current capacity) times **16 bytes**, all entries will be moved to the new allocated space, holes will be eliminated and the operation time will be proportional to the **Map** size.

**Map** being full does not necessarily mean **size == capacity** as on deletion holes (unused array indexes) may (if a deletion happens inside the **Map**) or may not (if you delete the **first** or the **last** **entry**) be created.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(n)**|

---

### `replace`

```ts
func replace<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, key: K, value: V): ?V
```

If the provided **key** is not present in the **Map**, does no modifications and returns **null**.

If the provided **key** is present in the **Map**, replaces the **previous value** with the provided **value** and returns the previous **value**. The position of the **(key, value)** pair within the **Map** is not changed.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

If the **Map** is empty, on the first insert the **Map** structure will be initialized which will take additional **100 bytes** of space. The minimum amount of space a non-empty **Map** can occupy is **112 bytes**.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)** unlikely under normal conditions|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `update`

```ts
func update<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, key: K, getNewValue: (K, ?V) -> ?V): ?V
```

If the provided **key** is not present in the **Map**, calls the provided **getNewValue** function with **(key, null)** parameters. If the result of **getNewValue** call is a **value**, inserts a new **(key, value)** pair at the **back** of the **Map** and returns the new **value**. If the result of **getNewValue** call is **null**, does no modifications and returns **null**.

If the provided **key** is present in the **Map**, calls the provided **getNewValue** function with **(key, previous value)** parameters. If the result of **getNewValue** call is a **value**, replaces the **previous value** with the new **value** and returns the new **value**. The position of the **(key, value)** pair within the **Map** is not changed. If the result of **getNewValue** call is **null**, does no modifications and returns the **previous value** (which remains current).

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

If the **Map** is empty, on the first insert the **Map** structure will be initialized which will take additional **100 bytes** of space. The minimum amount of space a non-empty **Map** can occupy is **112 bytes**.

Most **update** calls will not cause any space allocations and will be executed in **O(1)** time. If the **Map** is full, on the other hand, and a new **entry** is about to be inserted, [**rehash**](#rehash) will happen, meaning additional space will be allocated, equal to the **power of 2** that can hold **8/7** of all current entries (either equal or double the current capacity) times **16 bytes**, all entries will be moved to the new allocated space, holes will be eliminated and the operation time will be proportional to the **Map** size.

**Map** being full does not necessarily mean **size == capacity** as on deletion holes (unused array indexes) may (if a deletion happens inside the **Map**) or may not (if you delete the **first** or the **last** **entry**) be created.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(n)**|

---

### `updateFront`

```ts
func updateFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, key: K, getNewValue: (K, ?V) -> ?V): ?V
```

If the provided **key** is not present in the **Map**, calls the provided **getNewValue** function with **(key, null)** parameters. If the result of **getNewValue** call is a **value**, inserts a new **(key, value)** pair at the **front** of the **Map** and returns the new **value**. If the result of **getNewValue** call is **null**, does no modifications and returns **null**.

If the provided **key** is present in the **Map**, calls the provided **getNewValue** function with **(key, previous value)** parameters. If the result of **getNewValue** call is a **value**, replaces the **previous value** with the new **value** and returns the new **value**. The position of the **(key, value)** pair within the **Map** is not changed. If the result of **getNewValue** call is **null**, does no modifications and returns the **previous value** (which remains current).

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

If the **Map** is empty, on the first insert the **Map** structure will be initialized which will take additional **100 bytes** of space. The minimum amount of space a non-empty **Map** can occupy is **112 bytes**.

Most **updateFront** calls will not cause any space allocations and will be executed in **O(1)** time. If the **Map** is full, on the other hand, and a new **entry** is about to be inserted, [**rehash**](#rehash) will happen, meaning additional space will be allocated, equal to the **power of 2** that can hold **8/7** of all current entries (either equal or double the current capacity) times **16 bytes**, all entries will be moved to the new allocated space, holes will be eliminated and the operation time will be proportional to the **Map** size.

**Map** being full does not necessarily mean **size == capacity** as on deletion holes (unused array indexes) may (if a deletion happens inside the **Map**) or may not (if you delete the **first** or the **last** **entry**) be created.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(n)**|

---

### `putMove`

```ts
func putMove<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, key: K, value: ?V): ?V
```

If the provided **key** is not present in the **Map**, checks the optional **value** parameter. If the provided optional **value** is a **value**, inserts a new **(key, value)** pair at the **back** of the **Map** and returns **null**. If the provided optional **value** is **null**, does no modifications and returns **null**.

If the provided **key** is present in the **Map**, checks the optional **value** parameter. If the provided optional **value** is a **value**, replaces the **previous value** with the provided **value**, moves the **entry** to the **back** of the **Map** (if it's not there yet) and returns the previous **value**. If the provided optional **value** is **null**, moves the **entry** to the **back** of the **Map** (if it's not there yet) and returns **null**.

If the **entry** is being moved from the **front** to the **back**, all holes before the new **first** **entry** will be eliminated and [**rehash**](#rehash) will never happen. If the **entry** is being moved from any **non-front** position to the **back**, the hole will be created and [**rehash**](#rehash) may be called (if the **Map** is full).

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

If the **Map** is empty, on the first insert the **Map** structure will be initialized which will take additional **100 bytes** of space. The minimum amount of space a non-empty **Map** can occupy is **112 bytes**.

Most **putMove** calls will not cause any space allocations and will be executed in **O(1)** time. If the **Map** is full, on the other hand, and a new **entry** is about to be inserted (or moved from any **non-front** position to the **back**), [**rehash**](#rehash) will happen, meaning additional space will be allocated, equal to the **power of 2** that can hold **8/7** of all current entries (either equal or double the current capacity) times **16 bytes**, all entries will be moved to the new allocated space, holes will be eliminated and the operation time will be proportional to the **Map** size.

**Map** being full does not necessarily mean **size == capacity** as on deletion holes (unused array indexes) may (if a deletion happens inside the **Map**) or may not (if you delete the **first** or the **last** **entry**) be created.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(n)**|

---

### `putMoveFront`

```ts
func putMoveFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, key: K, value: ?V): ?V
```

If the provided **key** is not present in the **Map**, checks the optional **value** parameter. If the provided optional **value** is a **value**, inserts a new **(key, value)** pair at the **front** of the **Map** and returns **null**. If the provided optional **value** is **null**, does no modifications and returns **null**.

If the provided **key** is present in the **Map**, checks the optional **value** parameter. If the provided optional **value** is a **value**, replaces the **previous value** with the provided **value**, moves the **entry** to the **front** of the **Map** (if it's not there yet) and returns the previous **value**. If the provided optional **value** is **null**, moves the **entry** to the **front** of the **Map** (if it's not there yet) and returns **null**.

If the **entry** is being moved from the **back** to the **front**, all holes after the new **last** **entry** will be eliminated and [**rehash**](#rehash) will never happen. If the **entry** is being moved from any **non-back** position to the **front**, the hole will be created and [**rehash**](#rehash) may be called (if the **Map** is full).

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

If the **Map** is empty, on the first insert the **Map** structure will be initialized which will take additional **100 bytes** of space. The minimum amount of space a non-empty **Map** can occupy is **112 bytes**.

Most **putMoveFront** calls will not cause any space allocations and will be executed in **O(1)** time. If the **Map** is full, on the other hand, and a new **entry** is about to be inserted (or moved from any **non-back** position to the **front**), [**rehash**](#rehash) will happen, meaning additional space will be allocated, equal to the **power of 2** that can hold **8/7** of all current entries (either equal or double the current capacity) times **16 bytes**, all entries will be moved to the new allocated space, holes will be eliminated and the operation time will be proportional to the **Map** size.

**Map** being full does not necessarily mean **size == capacity** as on deletion holes (unused array indexes) may (if a deletion happens inside the **Map**) or may not (if you delete the **first** or the **last** **entry**) be created.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(n)**|

---

### `replaceMove`

```ts
func replaceMove<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, key: K, value: ?V): ?V
```

If the provided **key** is not present in the **Map**, does no modifications and returns **null**.

If the provided **key** is present in the **Map**, checks the optional **value** parameter. If the provided optional **value** is a **value**, replaces the **previous value** with the provided **value**, moves the **entry** to the **back** of the **Map** (if it's not there yet) and returns the previous **value**. If the provided optional **value** is **null**, moves the **entry** to the **back** of the **Map** (if it's not there yet) and returns **null**.

If the **entry** is being moved from the **front** to the **back**, all holes before the new **first** **entry** will be eliminated and [**rehash**](#rehash) will never happen. If the **entry** is being moved from any **non-front** position to the **back**, the hole will be created and [**rehash**](#rehash) may be called (if the **Map** is full).

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

If the **Map** is empty, on the first insert the **Map** structure will be initialized which will take additional **100 bytes** of space. The minimum amount of space a non-empty **Map** can occupy is **112 bytes**.

Most **replaceMove** calls will not cause any space allocations and will be executed in **O(1)** time. If the **Map** is full, on the other hand, and a new **entry** is about to be moved from any **non-front** position to the **back**, [**rehash**](#rehash) will happen, meaning additional space will be allocated, equal to the **power of 2** that can hold **8/7** of all current entries (either equal or double the current capacity) times **16 bytes**, all entries will be moved to the new allocated space, holes will be eliminated and the operation time will be proportional to the **Map** size.

**Map** being full does not necessarily mean **size == capacity** as on deletion holes (unused array indexes) may (if a deletion happens inside the **Map**) or may not (if you delete the **first** or the **last** **entry**) be created.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(n)**|

---

### `replaceMoveFront`

```ts
func replaceMoveFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, key: K, value: ?V): ?V
```

If the provided **key** is not present in the **Map**, does no modifications and returns **null**.

If the provided **key** is present in the **Map**, checks the optional **value** parameter. If the provided optional **value** is a **value**, replaces the **previous value** with the provided **value**, moves the **entry** to the **front** of the **Map** (if it's not there yet) and returns the previous **value**. If the provided optional **value** is **null**, moves the **entry** to the **front** of the **Map** (if it's not there yet) and returns **null**.

If the **entry** is being moved from the **back** to the **front**, all holes after the new **last** **entry** will be eliminated and [**rehash**](#rehash) will never happen. If the **entry** is being moved from any **non-back** position to the **front**, the hole will be created and [**rehash**](#rehash) may be called (if the **Map** is full).

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

If the **Map** is empty, on the first insert the **Map** structure will be initialized which will take additional **100 bytes** of space. The minimum amount of space a non-empty **Map** can occupy is **112 bytes**.

Most **replaceMoveFront** calls will not cause any space allocations and will be executed in **O(1)** time. If the **Map** is full, on the other hand, and a new **entry** is about to be moved from any **non-back** position to the **front**, [**rehash**](#rehash) will happen, meaning additional space will be allocated, equal to the **power of 2** that can hold **8/7** of all current entries (either equal or double the current capacity) times **16 bytes**, all entries will be moved to the new allocated space, holes will be eliminated and the operation time will be proportional to the **Map** size.

**Map** being full does not necessarily mean **size == capacity** as on deletion holes (unused array indexes) may (if a deletion happens inside the **Map**) or may not (if you delete the **first** or the **last** **entry**) be created.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(n)**|

---

### `updateMove`

```ts
func updateMove<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, key: K, getNewValue: (K, ?V) -> ?V): ?V
```

If the provided **key** is not present in the **Map**, calls the provided **getNewValue** function with **(key, null)** parameters. If the result of **getNewValue** call is a **value**, inserts a new **(key, value)** pair at the **back** of the **Map** and returns the new **value**. If the result of **getNewValue** call is **null**, does no modifications and returns **null**.

If the provided **key** is present in the **Map**, calls the provided **getNewValue** function with **(key, previous value)** parameters. If the result of **getNewValue** call is a **value**, replaces the **previous value** with the new **value**, moves the **entry** to the **back** of the **Map** (if it's not there yet) and returns the new **value**. If the result of **getNewValue** call is **null**, moves the **entry** to the **back** of the **Map** (if it's not there yet) and returns the **previous value** (which remains current).

If the **entry** is being moved from the **front** to the **back**, all holes before the new **first** **entry** will be eliminated and [**rehash**](#rehash) will never happen. If the **entry** is being moved from any **non-front** position to the **back**, the hole will be created and [**rehash**](#rehash) may be called (if the **Map** is full).

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

If the **Map** is empty, on the first insert the **Map** structure will be initialized which will take additional **100 bytes** of space. The minimum amount of space a non-empty **Map** can occupy is **112 bytes**.

Most **updateMove** calls will not cause any space allocations and will be executed in **O(1)** time. If the **Map** is full, on the other hand, and a new **entry** is about to be inserted (or moved from any **non-front** position to the **back**), [**rehash**](#rehash) will happen, meaning additional space will be allocated, equal to the **power of 2** that can hold **8/7** of all current entries (either equal or double the current capacity) times **16 bytes**, all entries will be moved to the new allocated space, holes will be eliminated and the operation time will be proportional to the **Map** size.

**Map** being full does not necessarily mean **size == capacity** as on deletion holes (unused array indexes) may (if a deletion happens inside the **Map**) or may not (if you delete the **first** or the **last** **entry**) be created.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(n)**|

---

### `updateMoveFront`

```ts
func updateMoveFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, key: K, getNewValue: (K, ?V) -> ?V): ?V
```

If the provided **key** is not present in the **Map**, calls the provided **getNewValue** function with **(key, null)** parameters. If the result of **getNewValue** call is a **value**, inserts a new **(key, value)** pair at the **front** of the **Map** and returns the new **value**. If the result of **getNewValue** call is **null**, does no modifications and returns **null**.

If the provided **key** is present in the **Map**, calls the provided **getNewValue** function with **(key, previous value)** parameters. If the result of **getNewValue** call is a **value**, replaces the **previous value** with the new **value**, moves the **entry** to the **front** of the **Map** (if it's not there yet) and returns the new **value**. If the result of **getNewValue** call is **null**, moves the **entry** to the **front** of the **Map** (if it's not there yet) and returns the **previous value** (which remains current).

If the **entry** is being moved from the **back** to the **front**, all holes after the new **last** **entry** will be eliminated and [**rehash**](#rehash) will never happen. If the **entry** is being moved from any **non-back** position to the **front**, the hole will be created and [**rehash**](#rehash) may be called (if the **Map** is full).

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

If the **Map** is empty, on the first insert the **Map** structure will be initialized which will take additional **100 bytes** of space. The minimum amount of space a non-empty **Map** can occupy is **112 bytes**.

Most **updateMoveFront** calls will not cause any space allocations and will be executed in **O(1)** time. If the **Map** is full, on the other hand, and a new **entry** is about to be inserted (or moved from any **non-back** position to the **front**), [**rehash**](#rehash) will happen, meaning additional space will be allocated, equal to the **power of 2** that can hold **8/7** of all current entries (either equal or double the current capacity) times **16 bytes**, all entries will be moved to the new allocated space, holes will be eliminated and the operation time will be proportional to the **Map** size.

**Map** being full does not necessarily mean **size == capacity** as on deletion holes (unused array indexes) may (if a deletion happens inside the **Map**) or may not (if you delete the **first** or the **last** **entry**) be created.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(n)**|

## Getting

### `get`

```ts
func get<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, key: K): ?V
```

If the provided **key** is present in the **Map**, returns the corresponding **value**. Otherwise, returns **null**.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)** unlikely under normal conditions|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `has`

```ts
func has<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, key: K): Bool
```

If the provided **key** is present in the **Map**, returns **true**. Otherwise, returns **false**.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)** unlikely under normal conditions|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `contains`

```ts
func contains<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, key: K): ?Bool
```

If the **Map** is empty, returns **null**.

If the provided **key** is present in the **Map**, returns **true**. Otherwise, returns **false** (if the **Map** is not empty).

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)** unlikely under normal conditions|
|Space average|**O(1)**|
|Space max|**O(1)**|

## Deleting

### `remove`

```ts
func remove<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, key: K): ?V
```

If the provided **key** is not present in the **Map**, does no modifications and returns **null**.

If the provided **key** is present in the **Map**, removes the corresponding **(key, value)** pair from the **Map** and returns the **value**.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

If the size of the **Map** after the deletion is less than **3 / 8 of capacity**, [**rehash**](#rehash) will happen, meaning additional space will be allocated, equal to the **power of 2** that can hold **8/7** of all current entries (either equal or double the current capacity) times **16 bytes**, all entries will be moved to the new allocated space, holes will be eliminated and the operation time will be proportional to the **Map** size.

If [**rehash**](#rehash) is not called and the **entry** is being removed from the **front** or the **back** of the **Map**, all holes before the new **first** or after the new **last** **entry** will be eliminated.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(n)**|

---

### `delete`

```ts
func delete<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, key: K): ()
```

If the provided **key** is not present in the **Map**, does no modifications.

If the provided **key** is present in the **Map**, removes the corresponding **(key, value)** pair from the **Map**.

This operation is the same as [**remove**](#remove) but doesn't have a return value.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

If the size of the **Map** after the deletion is less than **3 / 8 of capacity**, [**rehash**](#rehash) will happen, meaning additional space will be allocated, equal to the **power of 2** that can hold **8/7** of all current entries (either equal or double the current capacity) times **16 bytes**, all entries will be moved to the new allocated space, holes will be eliminated and the operation time will be proportional to the **Map** size.

If [**rehash**](#rehash) is not called and the **entry** is being removed from the **front** or the **back** of the **Map**, all holes before the new **first** or after the new **last** **entry** will be eliminated.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(n)**|

## Performing queue operations

### `peek`

```ts
func peek<K, V>(map: Map<K, V>): ?(K, V)
```

If the **Map** is not empty, returns the **last** **(key, value)** pair in the **Map**. Otherwise, returns **null**.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(1)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `peekFront`

```ts
func peekFront<K, V>(map: Map<K, V>): ?(K, V)
```

If the **Map** is not empty, returns the **first** **(key, value)** pair in the **Map**. Otherwise, returns **null**.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(1)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `pop`

```ts
func pop<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>): ?(K, V)
```

If the **Map** is empty, does no modifications and returns **null**.

If the **Map** is not empty, removes the **last** **(key, value)** pair in the **Map** and returns it.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

If the size of the **Map** after the deletion is less than **3 / 8 of capacity**, [**rehash**](#rehash) will happen, meaning additional space will be allocated, equal to the **power of 2** that can hold **8/7** of all current entries (either equal or double the current capacity) times **16 bytes**, all entries will be moved to the new allocated space, holes will be eliminated and the operation time will be proportional to the **Map** size.

If [**rehash**](#rehash) is not called, all holes after the new **last** **entry** will be eliminated.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(n)**|

---

### `popFront`

```ts
func popFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>): ?(K, V)
```

If the **Map** is empty, does no modifications and returns **null**.

If the **Map** is not empty, removes the **first** **(key, value)** pair in the **Map** and returns it.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

If the size of the **Map** after the deletion is less than **3 / 8 of capacity**, [**rehash**](#rehash) will happen, meaning additional space will be allocated, equal to the **power of 2** that can hold **8/7** of all current entries (either equal or double the current capacity) times **16 bytes**, all entries will be moved to the new allocated space, holes will be eliminated and the operation time will be proportional to the **Map** size.

If [**rehash**](#rehash) is not called, all holes before the new **first** **entry** will be eliminated.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(n)**|

---

### `cycle`

```ts
func cycle<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>): ?(K, V)
```

If the **Map** is empty, does no modifications and returns **null**.

If the **Map** is not empty, moves the **last** **(key, value)** pair in the **Map** to the **front** and returns it. All holes after the new **last** **entry** will be eliminated and [**rehash**](#rehash) will never happen.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)** unlikely under normal conditions|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `cycleFront`

```ts
func cycleFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>): ?(K, V)
```

If the **Map** is empty, does no modifications and returns **null**.

If the **Map** is not empty, moves the **first** **(key, value)** pair in the **Map** to the **back** and returns it. All holes before the new **first** **entry** will be eliminated and [**rehash**](#rehash) will never happen.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)** unlikely under normal conditions|
|Space average|**O(1)**|
|Space max|**O(1)**|

## Mapping / filtering / cloning

### `mapFilter`

```ts
func mapFilter<K, V1, V2>(map: Map<K, V1>, hashUtils: HashUtils<K>, mapEntry: (K, V1) -> ?V2): Map<K, V2>
```

For every **entry** in the **Map**, from the **first** to the **last** **entry** (in ascending order) calls the provided **mapEntry** function with **(key, value)** parameters. Constructs a new **Map** with all **(key, mapEntry result)** pairs for which **mapEntry** returned **non-null** result (in the same order **mapEntry** was called). Skips all entries for which **acceptEntry** returned **null**. Returns the new **Map**.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(n)**|
|Space max|**O(n)**|

---

### `mapFilterDesc`

```ts
func mapFilterDesc<K, V1, V2>(map: Map<K, V1>, hashUtils: HashUtils<K>, mapEntry: (K, V1) -> ?V2): Map<K, V2>
```

For every **entry** in the **Map**, from the **last** to the **first** **entry** (in descending order) calls the provided **mapEntry** function with **(key, value)** parameters. Constructs a new **Map** with all **(key, mapEntry result)** pairs for which **mapEntry** returned **non-null** result (in the same order **mapEntry** was called). Skips all entries for which **acceptEntry** returned **null**. Returns the new **Map**.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(n)**|
|Space max|**O(n)**|

---

### `filter`

```ts
func filter<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, acceptEntry: (K, V) -> Bool): Map<K, V>
```

For every **entry** in the **Map**, from the **first** to the **last** **entry** (in ascending order) calls the provided **acceptEntry** function with **(key, value)** parameters. Constructs a new **Map** with all **(key, value)** pairs for which **acceptEntry** returned **true** (in the same order **acceptEntry** was called). Skips all entries for which **acceptEntry** returned **false**. Returns the new **Map**.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(n)**|
|Space max|**O(n)**|

---

### `filterDesc`

```ts
func filterDesc<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, acceptEntry: (K, V) -> Bool): Map<K, V>
```

For every **entry** in the **Map**, from the **last** to the **first** **entry** (in descending order) calls the provided **acceptEntry** function with **(key, value)** parameters. Constructs a new **Map** with all **(key, value)** pairs for which **acceptEntry** returned **true** (in the same order **acceptEntry** was called). Skips all entries for which **acceptEntry** returned **false**. Returns the new **Map**.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(n)**|
|Space max|**O(n)**|

---

### `map`

```ts
func map<K, V1, V2>(map: Map<K, V1>, hashUtils: HashUtils<K>, mapEntry: (K, V1) -> V2): Map<K, V2>
```

For every **entry** in the **Map**, from the **first** to the **last** **entry** (in ascending order) calls the provided **mapEntry** function with **(key, value)** parameters. Constructs a new **Map** with all **(key, mapEntry result)** pairs (in the same order **mapEntry** was called). Returns the new **Map**.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(n)**|
|Space max|**O(n)**|

---

### `mapDesc`

```ts
func mapDesc<K, V1, V2>(map: Map<K, V1>, hashUtils: HashUtils<K>, mapEntry: (K, V1) -> V2): Map<K, V2>
```

For every **entry** in the **Map**, from the **last** to the **first** **entry** (in descending order) calls the provided **mapEntry** function with **(key, value)** parameters. Constructs a new **Map** with all **(key, mapEntry result)** pairs (in the same order **mapEntry** was called). Returns the new **Map**.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(n)**|
|Space max|**O(n)**|

---

### `clone`

```ts
func clone<K, V>(map: Map<K, V>): Map<K, V>
```

Iterates through the **Map** from the **first** to the **last** **entry** (in ascending order) and copies each **entry** into the new **Map** (in the order of iteration). Returns the new **Map**.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(n)**|
|Space max|**O(n)**|

---

### `cloneDesc`

```ts
func cloneDesc<K, V>(map: Map<K, V>): Map<K, V>
```

Iterates through the **Map** from the **last** to the **first** **entry** (in descending order) and copies each **entry** into the new **Map** (in the order of iteration). Returns the new **Map**.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(n)**|
|Space max|**O(n)**|

## Iterating

### `keys`

```ts
func keys<K, V>(map: Map<K, V>): Iter<K>
```

Creates an iterator object over the keys of the **Map** which starts at the **first** **key** and ends at the **last** **key**. The iterator can be used to iterate over all keys of the **Map** with **for in** loop in ascending order. Or you can call any method on the iterator object directly. The interface of an iterator object is described in the section [**Iterator** interface description](#iterator-interface-description).

All **Map** iterators are reusable, meaning after returning **null** once (at the end of iteration), iterators end up in their initial state and a new iteration cycle can be started without creating a new iterator object.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(1)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `keysDesc`

```ts
func keysDesc<K, V>(map: Map<K, V>): Iter<K>
```

Creates an iterator object over the keys of the **Map** which starts at the **last** **key** and ends at the **first** **key**. The iterator can be used to iterate over all keys of the **Map** with **for in** loop in descending order. Or you can call any method on the iterator object directly. The interface of an iterator object is described in the section [**Iterator** interface description](#iterator-interface-description).

All **Map** iterators are reusable, meaning after returning **null** once (at the end of iteration), iterators end up in their initial state and a new iteration cycle can be started without creating a new iterator object.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(1)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `vals`

```ts
func vals<K, V>(map: Map<K, V>): Iter<V>
```

Creates an iterator object over the values of the **Map** which starts at the **first** **value** and ends at the **last** **value**. The iterator can be used to iterate over all values of the **Map** with **for in** loop in ascending order. Or you can call any method on the iterator object directly. The interface of an iterator object is described in the section [**Iterator** interface description](#iterator-interface-description).

All **Map** iterators are reusable, meaning after returning **null** once (at the end of iteration), iterators end up in their initial state and a new iteration cycle can be started without creating a new iterator object.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(1)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `valsDesc`

```ts
func valsDesc<K, V>(map: Map<K, V>): Iter<V>
```

Creates an iterator object over the values of the **Map** which starts at the **last** **value** and ends at the **first** **value**. The iterator can be used to iterate over all values of the **Map** with **for in** loop in descending order. Or you can call any method on the iterator object directly. The interface of an iterator object is described in the section [**Iterator** interface description](#iterator-interface-description).

All **Map** iterators are reusable, meaning after returning **null** once (at the end of iteration), iterators end up in their initial state and a new iteration cycle can be started without creating a new iterator object.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(1)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `entries`

```ts
func entries<K, V>(map: Map<K, V>): Iter<(K, V)>
```

Creates an iterator object over the **(key, value)** pairs of the **Map** which starts at the **first** **entry** and ends at the **last** **entry**. The iterator can be used to iterate over all **(key, value)** pairs of the **Map** with **for in** loop in ascending order. Or you can call any method on the iterator object directly. The interface of an iterator object is described in the section [**Iterator** interface description](#iterator-interface-description).

All **Map** iterators are reusable, meaning after returning **null** once (at the end of iteration), iterators end up in their initial state and a new iteration cycle can be started without creating a new iterator object.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(1)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `entriesDesc`

```ts
func entriesDesc<K, V>(map: Map<K, V>): Iter<(K, V)>
```

Creates an iterator object over the **(key, value)** pairs of the **Map** which starts at the **last** **entry** and ends at the **first** **entry**. The iterator can be used to iterate over all **(key, value)** pairs of the **Map** with **for in** loop in descending order. Or you can call any method on the iterator object directly. The interface of an iterator object is described in the section [**Iterator** interface description](#iterator-interface-description).

All **Map** iterators are reusable, meaning after returning **null** once (at the end of iteration), iterators end up in their initial state and a new iteration cycle can be started without creating a new iterator object.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(1)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `keysFrom`

```ts
func keysFrom<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, key: ?K): Iter<K>
```

If the optional **key** parameter is present, performs a search for it in the **Map**. Creates an iterator object over the keys of the **Map** which starts at the **key** after the found **key** (if the optional **key** parameter is present and found) or the **first** **key** (if the optional **key** parameter is either not present or not found) and ends at the **last** **key**. The iterator can be used to iterate over all keys of the **Map** after the specified **key** with **for in** loop in ascending order. Or you can call any method on the iterator object directly. The interface of an iterator object is described in the section [**Iterator** interface description](#iterator-interface-description).

Note that after the iterator creation, the current **key** will indeed be the found **key** (if it is found). But it will be omitted by **for in** loop so the iteration begins from the **key** right after it. This behavior can bit changed and the found **key** can be included in the iteration by calling [**movePrev**](#moveprev) after the iterator creation.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

All **Map** iterators are reusable, meaning after returning **null** once (at the end of iteration), iterators end up in their initial state (initial state in this case corresponds to the **first** **key** and not to the found **key**) and a new iteration cycle can be started without creating a new iterator object.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)** unlikely under normal conditions|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `keysFromDesc`

```ts
func keysFromDesc<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, key: ?K): Iter<K>
```

If the optional **key** parameter is present, performs a search for it in the **Map**. Creates an iterator object over the keys of the **Map** which starts at the **key** before the found **key** (if the optional **key** parameter is present and found) or the **last** **key** (if the optional **key** parameter is either not present or not found) and ends at the **first** **key**. The iterator can be used to iterate over all keys of the **Map** before the specified **key** with **for in** loop in descending order. Or you can call any method on the iterator object directly. The interface of an iterator object is described in the section [**Iterator** interface description](#iterator-interface-description).

Note that after the iterator creation, the current **key** will indeed be the found **key** (if it is found). But it will be omitted by **for in** loop so the iteration begins from the **key** right before it. This behavior can bit changed and the found **key** can be included in the iteration by calling [**movePrev**](#moveprev) after the iterator creation.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

All **Map** iterators are reusable, meaning after returning **null** once (at the end of iteration), iterators end up in their initial state (initial state in this case corresponds to the **last** **key** and not to the found **key**) and a new iteration cycle can be started without creating a new iterator object.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)** unlikely under normal conditions|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `valsFrom`

```ts
func valsFrom<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, key: ?K): Iter<V>
```

If the optional **key** parameter is present, performs a search for it in the **Map**. Creates an iterator object over the values of the **Map** which starts at the **value** after the **value** associated with the found **key** (if the optional **key** parameter is present and found) or the **first** **value** (if the optional **key** parameter is either not present or not found) and ends at the **last** **value**. The iterator can be used to iterate over all values of the **Map** after the **value** associated with the specified **key** with **for in** loop in ascending order. Or you can call any method on the iterator object directly. The interface of an iterator object is described in the section [**Iterator** interface description](#iterator-interface-description).

Note that after the iterator creation, the current **value** will indeed be the **value** associated with the found **key** (if it is found). But it will be omitted by **for in** loop so the iteration begins from the **value** right after it. This behavior can bit changed and the **value** associated with the found **key** can be included in the iteration by calling [**movePrev**](#moveprev) after the iterator creation.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

All **Map** iterators are reusable, meaning after returning **null** once (at the end of iteration), iterators end up in their initial state (initial state in this case corresponds to the **first** **value** and not to the **value** associated with the found **key**) and a new iteration cycle can be started without creating a new iterator object.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)** unlikely under normal conditions|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `valsFromDesc`

```ts
func valsFromDesc<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, key: ?K): Iter<V>
```

If the optional **key** parameter is present, performs a search for it in the **Map**. Creates an iterator object over the values of the **Map** which starts at the **value** before the **value** associated with the found **key** (if the optional **key** parameter is present and found) or the **last** **value** (if the optional **key** parameter is either not present or not found) and ends at the **first** **value**. The iterator can be used to iterate over all values of the **Map** before the **value** associated with the specified **key** with **for in** loop in descending order. Or you can call any method on the iterator object directly. The interface of an iterator object is described in the section [**Iterator** interface description](#iterator-interface-description).

Note that after the iterator creation, the current **value** will indeed be the **value** associated with the found **key** (if it is found). But it will be omitted by **for in** loop so the iteration begins from the **value** right before it. This behavior can bit changed and the **value** associated with the found **key** can be included in the iteration by calling [**movePrev**](#moveprev) after the iterator creation.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

All **Map** iterators are reusable, meaning after returning **null** once (at the end of iteration), iterators end up in their initial state (initial state in this case corresponds to the **last** **value** and not to the **value** associated with the found **key**) and a new iteration cycle can be started without creating a new iterator object.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)** unlikely under normal conditions|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `entriesFrom`

```ts
func entriesFrom<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, key: ?K): Iter<(K, V)>
```

If the optional **key** parameter is present, performs a search for it in the **Map**. Creates an iterator object over the **(key, value)** pairs of the **Map** which starts at the **entry** after the **entry** with the found **key** (if the optional **key** parameter is present and found) or the **first** **entry** (if the optional **key** parameter is either not present or not found) and ends at the **last** **entry**. The iterator can be used to iterate over all **(key, value)** pairs of the **Map** after the **entry** with the specified **key** with **for in** loop in ascending order. Or you can call any method on the iterator object directly. The interface of an iterator object is described in the section [**Iterator** interface description](#iterator-interface-description).

Note that after the iterator creation, the current **entry** will indeed be the **entry** with the found **key** (if it is found). But it will be omitted by **for in** loop so the iteration begins from the **entry** right after it. This behavior can bit changed and the **entry** with the found **key** can be included in the iteration by calling [**movePrev**](#moveprev) after the iterator creation.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

All **Map** iterators are reusable, meaning after returning **null** once (at the end of iteration), iterators end up in their initial state (initial state in this case corresponds to the **first** **entry** and not to the **entry** with the found **key**) and a new iteration cycle can be started without creating a new iterator object.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)** unlikely under normal conditions|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `entriesFromDesc`

```ts
func entriesFromDesc<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, key: ?K): Iter<(K, V)>
```

If the optional **key** parameter is present, performs a search for it in the **Map**. Creates an iterator object over the **(key, value)** pairs of the **Map** which starts at the **entry** before the **entry** with the found **key** (if the optional **key** parameter is present and found) or the **last** **entry** (if the optional **key** parameter is either not present or not found) and ends at the **first** **entry**. The iterator can be used to iterate over all **(key, value)** pairs of the **Map** before the **entry** with the specified **key** with **for in** loop in descending order. Or you can call any method on the iterator object directly. The interface of an iterator object is described in the section [**Iterator** interface description](#iterator-interface-description).

Note that after the iterator creation, the current **entry** will indeed be the **entry** with the found **key** (if it is found). But it will be omitted by **for in** loop so the iteration begins from the **entry** right before it. This behavior can bit changed and the **entry** with the found **key** can be included in the iteration by calling [**movePrev**](#moveprev) after the iterator creation.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

All **Map** iterators are reusable, meaning after returning **null** once (at the end of iteration), iterators end up in their initial state (initial state in this case corresponds to the **last** **entry** and not to the **entry** with the found **key**) and a new iteration cycle can be started without creating a new iterator object.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)** unlikely under normal conditions|
|Space average|**O(1)**|
|Space max|**O(1)**|

## Searching

### `find`

```ts
func find<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool): ?(K, V)
```

For every **entry** in the **Map**, from the **first** to the **last** **entry** (in ascending order) calls the provided **acceptEntry** function with **(key, value)** parameters until the **acceptEntry** function returns **true**. If **true** is returned, returns the current **(key, value)** pair immediately. If **acceptEntry** didn't return **true** for any **entry**, returns **null**.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `findDesc`

```ts
func findDesc<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool): ?(K, V)
```

For every **entry** in the **Map**, from the **last** to the **first** **entry** (in descending order) calls the provided **acceptEntry** function with **(key, value)** parameters until the **acceptEntry** function returns **true**. If **true** is returned, returns the current **(key, value)** pair immediately. If **acceptEntry** didn't return **true** for any **entry**, returns **null**.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `some`

```ts
func some<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool): Bool
```

For every **entry** in the **Map**, from the **first** to the **last** **entry** (in ascending order) calls the provided **acceptEntry** function with **(key, value)** parameters until the **acceptEntry** function returns **true**. If **true** is returned, returns **true** immediately. If **acceptEntry** didn't return **true** for any **entry**, returns **false**.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `someDesc`

```ts
func someDesc<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool): Bool
```

For every **entry** in the **Map**, from the **last** to the **first** **entry** (in descending order) calls the provided **acceptEntry** function with **(key, value)** parameters until the **acceptEntry** function returns **true**. If **true** is returned, returns **true** immediately. If **acceptEntry** didn't return **true** for any **entry**, returns **false**.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `every`

```ts
func every<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool): Bool
```

For every **entry** in the **Map**, from the **first** to the **last** **entry** (in ascending order) calls the provided **acceptEntry** function with **(key, value)** parameters until the **acceptEntry** function returns **false**. If **false** is returned, returns **false** immediately. If **acceptEntry** didn't return **false** for any **entry**, returns **true**.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `everyDesc`

```ts
func everyDesc<K, V>(map: Map<K, V>, acceptEntry: (K, V) -> Bool): Bool
```

For every **entry** in the **Map**, from the **last** to the **first** **entry** (in descending order) calls the provided **acceptEntry** function with **(key, value)** parameters until the **acceptEntry** function returns **false**. If **false** is returned, returns **false** immediately. If **acceptEntry** didn't return **false** for any **entry**, returns **true**.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `forEach`

```ts
func forEach<K, V>(map: Map<K, V>, mapEntry: (K, V) -> ()): ()
```

For every **entry** in the **Map**, from the **first** to the **last** **entry** (in ascending order) calls the provided **mapEntry** function with **(key, value)** parameters.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `forEachDesc`

```ts
func forEachDesc<K, V>(map: Map<K, V>, mapEntry: (K, V) -> ()): ()
```

For every **entry** in the **Map**, from the **last** to the **first** **entry** (in descending order) calls the provided **mapEntry** function with **(key, value)** parameters.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

## Constructing from iterators

### `fromIter`

```ts
func fromIter<K, V>(iter: IterNext<(K, V)>, hashUtils: HashUtils<K>): Map<K, V>
```

Creates a new empty **Map**. For every **(key, value)** pair in the iterator, inserts the **(key, value)** pair at the **back** of the **Map** if the **key** is not present in the **Map**. If the **key** is present in the **Map**, replaces the **previous value** with the **value** and moves the **entry** to the **back** of the **Map** (if it's not there yet). Returns the new **Map**.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(n)**|
|Space max|**O(n)**|

---

### `fromIterDesc`

```ts
func fromIterDesc<K, V>(iter: IterNext<(K, V)>, hashUtils: HashUtils<K>): Map<K, V>
```

Creates a new empty **Map**. For every **(key, value)** pair in the iterator, inserts the **(key, value)** pair at the **front** of the **Map** if the **key** is not present in the **Map**. If the **key** is present in the **Map**, replaces the **previous value** with the **value** and moves the **entry** to the **front** of the **Map** (if it's not there yet). Returns the new **Map**.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(n)**|
|Space max|**O(n)**|

---

### `fromIterMap`

```ts
func fromIterMap<K, V, T>(iter: IterNext<T>, hashUtils: HashUtils<K>, mapItem: (T) -> ?(K, V)): Map<K, V>
```

Creates a new empty **Map**. For every item in the iterator, calls the provided **mapItem** function. For every item **mapItem** returned a **(key, value)** pair for, inserts the **(key, value)** pair at the **back** of the **Map** if the **key** is not present in the **Map**. If the **key** is present in the **Map**, replaces the **previous value** with the **value** and moves the **entry** to the **back** of the **Map** (if it's not there yet). Skips all items **mapItem** returned **null** for. Returns the new **Map**.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(n)**|
|Space max|**O(n)**|

---

### `fromIterMapDesc`

```ts
func fromIterMapDesc<K, V, T>(iter: IterNext<T>, hashUtils: HashUtils<K>, mapItem: (T) -> ?(K, V)): Map<K, V>
```

Creates a new empty **Map**. For every item in the iterator, calls the provided **mapItem** function. For every item **mapItem** returned a **(key, value)** pair for, inserts the **(key, value)** pair at the **front** of the **Map** if the **key** is not present in the **Map**. If the **key** is present in the **Map**, replaces the **previous value** with the **value** and moves the **entry** to the **front** of the **Map** (if it's not there yet). Skips all items **mapItem** returned **null** for. Returns the new **Map**.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(n)**|
|Space max|**O(n)**|

## Converting to arrays

### `toArray`

```ts
func toArray<K, V>(map: Map<K, V>): [(K, V)]
```

Constructs a new array with all **(key, value)** pairs present in the **Map**, from the **first** to the **last** **entry** (in ascending order). Returns the array.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(n)**|
|Space max|**O(n)**|

---

### `toArrayDesc`

```ts
func toArrayDesc<K, V>(map: Map<K, V>): [(K, V)]
```

Constructs a new array with all **(key, value)** pairs present in the **Map**, from the **last** to the **first** **entry** (in descending order). Returns the array.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(n)**|
|Space max|**O(n)**|

---

### `toArrayMap`

```ts
func toArrayMap<K, V, T>(map: Map<K, V>, mapEntry: (K, V) -> ?T): [T]
```

For every **entry** in the **Map**, from the **first** to the **last** **entry** (in ascending order), calls the provided **mapEntry** function with **(key, value)** parameters. Constructs a new array with all **non-null** **mapEntry result** items (in the same order **mapEntry** was called). Skips all **null** **mapEntry result** items. Returns the array.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(n)**|
|Space max|**O(n)**|

---

### `toArrayMapDesc`

```ts
func toArrayMapDesc<K, V, T>(map: Map<K, V>, mapEntry: (K, V) -> ?T): [T]
```

For every **entry** in the **Map**, from the **last** to the **first** **entry** (in descending order), calls the provided **mapEntry** function with **(key, value)** parameters. Constructs a new array with all **non-null** **mapEntry result** items (in the same order **mapEntry** was called). Skips all **null** **mapEntry result** items. Returns the array.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(n)**|
|Space max|**O(n)**|

## Rehashing

### `rehash`

```ts
func rehash<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>): ()
```

If the **Map** is empty, returns immediately. Otherwise, allocates additional space, equal to the **power of 2** that can hold **8/7** of all current entries (either half, equal or double the current capacity) times **16 bytes**. Moves all entries to the new allocated space, while eliminating the holes (unused array indexes). Recalculates the hash table (mapping from a hash to the first **entry** in a bucket). Recalculates bucket chains (entries that have equal **hash % Map size** result in a collision and end up in the same bucket). To perform those recalculation, hashes for all keys must also be recalculated.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

This method is usually called under the hood on inserts (when the **Map** is full) and deletes (when the **Map** size is less than **3 / 8 of capacity**). It is not necessary to call this method manually under normal conditions.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(n)**|
|Space max|**O(n)**|

# **Set** interface description

```ts
type Set<K> = [var ?(
  keys: [var ?K],
  indexes: [var Nat],
  bounds: [var Nat32],
)];
```

## Initializing / measuring

### `new`

```ts
func new<K>(): Set<K>
```

Creates and returns an empty **Set**. Empty **Set** has a special optimization and takes only **12 bytes** of space. The **Set** structure will be initialized only after the insertion of the **first** **key** and will occupy minimum **92 bytes**.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(1)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `clear`

```ts
func clear<K>(set: Set<K>): ()
```

Removes all keys from the **Set**.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(1)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `size`

```ts
func size<K>(set: Set<K>): Nat
```

Returns the amount of keys in the **Set**.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(1)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `empty`

```ts
func empty<K>(set: Set<K>): Bool
```

Returns a boolean indicating whether the **Set** is empty (has zero keys) or not.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(1)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `make`

```ts
func make<K>(hashUtils: HashUtils<K>, key: K): Set<K>
```

Creates a new **Set**, initializes it with the single provided **key** and returns the **Set**. The minimum amount of space a non-empty **Set** can occupy is **92 bytes**.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the first argument.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(1)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

## Inserting / updating

### `put`

```ts
func put<K>(set: Set<K>, hashUtils: HashUtils<K>, key: K): Bool
```

If the provided **key** is not present in the **Set**, inserts it as the **last** **key** of the **Set** and returns **false**.

If the provided **key** is present in the **Set**, does no modifications and returns **true**.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

If the **Set** is empty, on the first insert the **Set** structure will be initialized which will take additional **80 bytes** of space. The minimum amount of space a non-empty **Set** can occupy is **92 bytes**.

Most **put** calls will not cause any space allocations and will be executed in **O(1)** time. If the **Set** is full, on the other hand, and a new **key** is about to be inserted, [**rehash**](#rehash-1) will happen, meaning additional space will be allocated, equal to the **power of 2** that can hold **8/7** of all current keys (either equal or double the current capacity) times **12 bytes**, all keys will be moved to the new allocated space, holes will be eliminated and the operation time will be proportional to the **Set** size.

**Set** being full does not necessarily mean **size == capacity** as on deletion holes (unused array indexes) may (if a deletion happens inside the **Set**) or may not (if you delete the **first** or the **last** **key**) be created.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(n)**|

---

### `putFront`

```ts
func putFront<K>(set: Set<K>, hashUtils: HashUtils<K>, key: K): Bool
```

If the provided **key** is not present in the **Set**, inserts it as the **first** **key** of the **Set** and returns **false**.

If the provided **key** is present in the **Set**, does no modifications and returns **true**.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

If the **Set** is empty, on the first insert the **Set** structure will be initialized which will take additional **80 bytes** of space. The minimum amount of space a non-empty **Set** can occupy is **92 bytes**.

Most **putFront** calls will not cause any space allocations and will be executed in **O(1)** time. If the **Set** is full, on the other hand, and a new **key** is about to be inserted, [**rehash**](#rehash-1) will happen, meaning additional space will be allocated, equal to the **power of 2** that can hold **8/7** of all current keys (either equal or double the current capacity) times **12 bytes**, all keys will be moved to the new allocated space, holes will be eliminated and the operation time will be proportional to the **Set** size.

**Set** being full does not necessarily mean **size == capacity** as on deletion holes (unused array indexes) may (if a deletion happens inside the **Set**) or may not (if you delete the **first** or the **last** **key**) be created.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(n)**|

---

### `add`

```ts
func add<K>(set: Set<K>, hashUtils: HashUtils<K>, key: K): ()
```

If the provided **key** is not present in the **Set**, inserts it as the **last** **key** of the **Set**.

If the provided **key** is present in the **Set**, does no modifications.

This operation is the same as [**put**](#put-1) but doesn't have a return value.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

If the **Set** is empty, on the first insert the **Set** structure will be initialized which will take additional **80 bytes** of space. The minimum amount of space a non-empty **Set** can occupy is **92 bytes**.

Most **add** calls will not cause any space allocations and will be executed in **O(1)** time. If the **Set** is full, on the other hand, and a new **key** is about to be inserted, [**rehash**](#rehash-1) will happen, meaning additional space will be allocated, equal to the **power of 2** that can hold **8/7** of all current keys (either equal or double the current capacity) times **12 bytes**, all keys will be moved to the new allocated space, holes will be eliminated and the operation time will be proportional to the **Set** size.

**Set** being full does not necessarily mean **size == capacity** as on deletion holes (unused array indexes) may (if a deletion happens inside the **Set**) or may not (if you delete the **first** or the **last** **key**) be created.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(n)**|

---

### `addFront`

```ts
func addFront<K>(set: Set<K>, hashUtils: HashUtils<K>, key: K): ()
```

If the provided **key** is not present in the **Set**, inserts it as the **first** **key** of the **Set**.

If the provided **key** is present in the **Set**, does no modifications.

This operation is the same as [**putFront**](#putfront-1) but doesn't have a return value.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

If the **Set** is empty, on the first insert the **Set** structure will be initialized which will take additional **80 bytes** of space. The minimum amount of space a non-empty **Set** can occupy is **92 bytes**.

Most **addFront** calls will not cause any space allocations and will be executed in **O(1)** time. If the **Set** is full, on the other hand, and a new **key** is about to be inserted, [**rehash**](#rehash-1) will happen, meaning additional space will be allocated, equal to the **power of 2** that can hold **8/7** of all current keys (either equal or double the current capacity) times **12 bytes**, all keys will be moved to the new allocated space, holes will be eliminated and the operation time will be proportional to the **Set** size.

**Set** being full does not necessarily mean **size == capacity** as on deletion holes (unused array indexes) may (if a deletion happens inside the **Set**) or may not (if you delete the **first** or the **last** **key**) be created.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(n)**|

---

### `putMove`

```ts
func putMove<K>(set: Set<K>, hashUtils: HashUtils<K>, key: K): Bool
```

If the provided **key** is not present in the **Set**, inserts it as the **last** **key** of the **Set** and returns **false**.

If the provided **key** is present in the **Set**, moves the **key** to the **back** of the **Set** (if it's not there yet) and returns **true**.

If the **key** is being moved from the **front** to the **back**, all holes before the new **first** **key** will be eliminated and [**rehash**](#rehash-1) will never happen. If the **key** is being moved from any **non-front** position to the **back**, the hole will be created and [**rehash**](#rehash-1) may be called (if the **Set** is full).

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

If the **Set** is empty, on the first insert the **Set** structure will be initialized which will take additional **80 bytes** of space. The minimum amount of space a non-empty **Set** can occupy is **92 bytes**.

Most **putMove** calls will not cause any space allocations and will be executed in **O(1)** time. If the **Set** is full, on the other hand, and a new **key** is about to be inserted (or moved from any **non-front** position to the **back**), [**rehash**](#rehash-1) will happen, meaning additional space will be allocated, equal to the **power of 2** that can hold **8/7** of all current keys (either equal or double the current capacity) times **12 bytes**, all keys will be moved to the new allocated space, holes will be eliminated and the operation time will be proportional to the **Set** size.

**Set** being full does not necessarily mean **size == capacity** as on deletion holes (unused array indexes) may (if a deletion happens inside the **Set**) or may not (if you delete the **first** or the **last** **key**) be created.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(n)**|

---

### `putMoveFront`

```ts
func putMoveFront<K>(set: Set<K>, hashUtils: HashUtils<K>, key: K): Bool
```

If the provided **key** is not present in the **Set**, inserts it as the **first** **key** of the **Set** and returns **false**.

If the provided **key** is present in the **Set**, moves the **key** to the **front** of the **Set** (if it's not there yet) and returns **true**.

If the **key** is being moved from the **back** to the **front**, all holes after the new **last** **key** will be eliminated and [**rehash**](#rehash-1) will never happen. If the **key** is being moved from any **non-back** position to the **front**, the hole will be created and [**rehash**](#rehash-1) may be called (if the **Set** is full).

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

If the **Set** is empty, on the first insert the **Set** structure will be initialized which will take additional **80 bytes** of space. The minimum amount of space a non-empty **Set** can occupy is **92 bytes**.

Most **putMoveFront** calls will not cause any space allocations and will be executed in **O(1)** time. If the **Set** is full, on the other hand, and a new **key** is about to be inserted (or moved from any **non-back** position to the **front**), [**rehash**](#rehash-1) will happen, meaning additional space will be allocated, equal to the **power of 2** that can hold **8/7** of all current keys (either equal or double the current capacity) times **12 bytes**, all keys will be moved to the new allocated space, holes will be eliminated and the operation time will be proportional to the **Set** size.

**Set** being full does not necessarily mean **size == capacity** as on deletion holes (unused array indexes) may (if a deletion happens inside the **Set**) or may not (if you delete the **first** or the **last** **key**) be created.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(n)**|

## Getting

### `has`

```ts
func has<K>(set: Set<K>, hashUtils: HashUtils<K>, key: K): Bool
```

If the provided **key** is present in the **Set**, returns **true**. Otherwise, returns **false**.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)** unlikely under normal conditions|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `contains`

```ts
func contains<K>(set: Set<K>, hashUtils: HashUtils<K>, key: K): ?Bool
```

If the **Set** is empty, returns **null**.

If the provided **key** is present in the **Set**, returns **true**. Otherwise, returns **false** (if the **Set** is not empty).

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)** unlikely under normal conditions|
|Space average|**O(1)**|
|Space max|**O(1)**|

## Deleting

### `remove`

```ts
func remove<K>(set: Set<K>, hashUtils: HashUtils<K>, key: K): Bool
```

If the provided **key** is not present in the **Set**, does no modifications and returns **false**.

If the provided **key** is present in the **Set**, removes it from the **Set** and returns **true**.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

If the size of the **Set** after the deletion is less than **3 / 8 of capacity**, [**rehash**](#rehash-1) will happen, meaning additional space will be allocated, equal to the **power of 2** that can hold **8/7** of all current keys (either equal or double the current capacity) times **12 bytes**, all keys will be moved to the new allocated space, holes will be eliminated and the operation time will be proportional to the **Set** size.

If [**rehash**](#rehash-1) is not called and the **key** is being removed from the **front** or the **back** of the **Set**, all holes before the new **first** or after the new **last** **key** will be eliminated.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(n)**|

---

### `delete`

```ts
func delete<K>(set: Set<K>, hashUtils: HashUtils<K>, key: K): ()
```

If the provided **key** is not present in the **Set**, does no modifications.

If the provided **key** is present in the **Set**, removes it from the **Set**.

This operation is the same as [**remove**](#remove-1) but doesn't have a return value.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

If the size of the **Set** after the deletion is less than **3 / 8 of capacity**, [**rehash**](#rehash-1) will happen, meaning additional space will be allocated, equal to the **power of 2** that can hold **8/7** of all current keys (either equal or double the current capacity) times **12 bytes**, all keys will be moved to the new allocated space, holes will be eliminated and the operation time will be proportional to the **Set** size.

If [**rehash**](#rehash-1) is not called and the **key** is being removed from the **front** or the **back** of the **Set**, all holes before the new **first** or after the new **last** **key** will be eliminated.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(n)**|

## Performing queue operations

### `peek`

```ts
func peek<K>(set: Set<K>): ?K
```

If the **Set** is not empty, returns the **last** **key** in the **Set**. Otherwise, returns **null**.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(1)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `peekFront`

```ts
func peekFront<K>(set: Set<K>): ?K
```

If the **Set** is not empty, returns the **first** **key** in the **Set**. Otherwise, returns **null**.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(1)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `pop`

```ts
func pop<K>(set: Set<K>, hashUtils: HashUtils<K>): ?K
```

If the **Set** is empty, does no modifications and returns **null**.

If the **Set** is not empty, removes the **last** **key** in the **Set** and returns it.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

If the size of the **Set** after the deletion is less than **3 / 8 of capacity**, [**rehash**](#rehash-1) will happen, meaning additional space will be allocated, equal to the **power of 2** that can hold **8/7** of all current keys (either equal or double the current capacity) times **12 bytes**, all keys will be moved to the new allocated space, holes will be eliminated and the operation time will be proportional to the **Set** size.

If [**rehash**](#rehash-1) is not called, all holes after the new **last** **key** will be eliminated.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(n)**|

---

### `popFront`

```ts
func popFront<K>(set: Set<K>, hashUtils: HashUtils<K>): ?K
```

If the **Set** is empty, does no modifications and returns **null**.

If the **Set** is not empty, removes the **first** **key** in the **Set** and returns it.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

If the size of the **Set** after the deletion is less than **3 / 8 of capacity**, [**rehash**](#rehash-1) will happen, meaning additional space will be allocated, equal to the **power of 2** that can hold **8/7** of all current keys (either equal or double the current capacity) times **12 bytes**, all keys will be moved to the new allocated space, holes will be eliminated and the operation time will be proportional to the **Set** size.

If [**rehash**](#rehash-1) is not called, all holes before the new **first** **key** will be eliminated.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(n)**|

---

### `cycle`

```ts
func cycle<K>(set: Set<K>, hashUtils: HashUtils<K>): ?K
```

If the **Set** is empty, does no modifications and returns **null**.

If the **Set** is not empty, moves the **last** **key** in the **Set** to the **front** and returns it. All holes after the new **last** **key** will be eliminated and [**rehash**](#rehash-1) will never happen.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)** unlikely under normal conditions|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `cycleFront`

```ts
func cycleFront<K>(set: Set<K>, hashUtils: HashUtils<K>): ?K
```

If the **Set** is empty, does no modifications and returns **null**.

If the **Set** is not empty, moves the **first** **key** in the **Set** to the **back** and returns it. All holes before the new **first** **key** will be eliminated and [**rehash**](#rehash-1) will never happen.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)** unlikely under normal conditions|
|Space average|**O(1)**|
|Space max|**O(1)**|

## Filtering / cloning

### `filter`

```ts
func filter<K>(set: Set<K>, hashUtils: HashUtils<K>, acceptKey: (K) -> Bool): Set<K>
```

For every **key** in the **Set**, from the **first** to the **last** **key** (in ascending order) calls the provided **acceptKey** function. Constructs a new **Set** with all keys for which **acceptKey** returned **true** (in the same order **acceptKey** was called). Skips all keys for which **acceptKey** returned **false**. Returns the new **Set**.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(n)**|
|Space max|**O(n)**|

---

### `filterDesc`

```ts
func filterDesc<K>(set: Set<K>, hashUtils: HashUtils<K>, acceptKey: (K) -> Bool): Set<K>
```

For every **key** in the **Set**, from the **last** to the **first** **key** (in descending order) calls the provided **acceptKey** function. Constructs a new **Set** with all keys for which **acceptKey** returned **true** (in the same order **acceptKey** was called). Skips all keys for which **acceptKey** returned **false**. Returns the new **Set**.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(n)**|
|Space max|**O(n)**|

---

### `clone`

```ts
func clone<K>(set: Set<K>): Set<K>
```

Iterates through the **Set** from the **first** to the **last** **key** (in ascending order) and copies each **key** into the new **Set** (in the order of iteration). Returns the new **Set**.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(n)**|
|Space max|**O(n)**|

---

### `cloneDesc`

```ts
func cloneDesc<K>(set: Set<K>): Set<K>
```

Iterates through the **Set** from the **last** to the **first** **key** (in descending order) and copies each **key** into the new **Set** (in the order of iteration). Returns the new **Set**.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(n)**|
|Space max|**O(n)**|

## Iterating

### `keys`

```ts
func keys<K>(set: Set<K>): Iter<K>
```

Creates an iterator object over the keys of the **Set** which starts at the **first** **key** and ends at the **last** **key**. The iterator can be used to iterate over all keys of the **Set** with **for in** loop in ascending order. Or you can call any method on the iterator object directly. The interface of an iterator object is described in the section [**Iterator** interface description](#iterator-interface-description).

All **Set** iterators are reusable, meaning after returning **null** once (at the end of iteration), iterators end up in their initial state and a new iteration cycle can be started without creating a new iterator object.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(1)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `keysDesc`

```ts
func keysDesc<K>(set: Set<K>): Iter<K>
```

Creates an iterator object over the keys of the **Set** which starts at the **last** **key** and ends at the **first** **key**. The iterator can be used to iterate over all keys of the **Set** with **for in** loop in descending order. Or you can call any method on the iterator object directly. The interface of an iterator object is described in the section [**Iterator** interface description](#iterator-interface-description).

All **Set** iterators are reusable, meaning after returning **null** once (at the end of iteration), iterators end up in their initial state and a new iteration cycle can be started without creating a new iterator object.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(1)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `keysFrom`

```ts
func keysFrom<K>(set: Set<K>, hashUtils: HashUtils<K>, key: ?K): Iter<K>
```

If the optional **key** parameter is present, performs a search for it in the **Set**. Creates an iterator object over the keys of the **Set** which starts at the **key** after the found **key** (if the optional **key** parameter is present and found) or the **first** **key** (if the optional **key** parameter is either not present or not found) and ends at the **last** **key**. The iterator can be used to iterate over all keys of the **Set** after the specified **key** with **for in** loop in ascending order. Or you can call any method on the iterator object directly. The interface of an iterator object is described in the section [**Iterator** interface description](#iterator-interface-description).

Note that after the iterator creation, the current **key** will indeed be the found **key** (if it is found). But it will be omitted by **for in** loop so the iteration begins from the **key** right after it. This behavior can bit changed and the found **key** can be included in the iteration by calling [**movePrev**](#moveprev) after the iterator creation.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

All **Set** iterators are reusable, meaning after returning **null** once (at the end of iteration), iterators end up in their initial state (initial state in this case corresponds to the **first** **key** and not to the found **key**) and a new iteration cycle can be started without creating a new iterator object.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)** unlikely under normal conditions|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `keysFromDesc`

```ts
func keysFromDesc<K>(set: Set<K>, hashUtils: HashUtils<K>, key: ?K): Iter<K>
```

If the optional **key** parameter is present, performs a search for it in the **Set**. Creates an iterator object over the keys of the **Set** which starts at the **key** before the found **key** (if the optional **key** parameter is present and found) or the **last** **key** (if the optional **key** parameter is either not present or not found) and ends at the **first** **key**. The iterator can be used to iterate over all keys of the **Set** before the specified **key** with **for in** loop in descending order. Or you can call any method on the iterator object directly. The interface of an iterator object is described in the section [**Iterator** interface description](#iterator-interface-description).

Note that after the iterator creation, the current **key** will indeed be the found **key** (if it is found). But it will be omitted by **for in** loop so the iteration begins from the **key** right before it. This behavior can bit changed and the found **key** can be included in the iteration by calling [**movePrev**](#moveprev) after the iterator creation.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

All **Set** iterators are reusable, meaning after returning **null** once (at the end of iteration), iterators end up in their initial state (initial state in this case corresponds to the **last** **key** and not to the found **key**) and a new iteration cycle can be started without creating a new iterator object.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)** unlikely under normal conditions|
|Space average|**O(1)**|
|Space max|**O(1)**|

## Searching

### `find`

```ts
func find<K>(set: Set<K>, acceptKey: (K) -> Bool): ?K
```

For every **key** in the **Set**, from the **first** to the **last** **key** (in ascending order) calls the provided **acceptKey** function until the **acceptKey** function returns **true**. If **true** is returned, returns the current **key** immediately. If **acceptKey** didn't return **true** for any **key**, returns **null**.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `findDesc`

```ts
func findDesc<K>(set: Set<K>, acceptKey: (K) -> Bool): ?K
```

For every **key** in the **Set**, from the **last** to the **first** **key** (in descending order) calls the provided **acceptKey** function until the **acceptKey** function returns **true**. If **true** is returned, returns the current **key** immediately. If **acceptKey** didn't return **true** for any **key**, returns **null**.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `some`

```ts
func some<K>(set: Set<K>, acceptKey: (K) -> Bool): Bool
```

For every **key** in the **Set**, from the **first** to the **last** **key** (in ascending order) calls the provided **acceptKey** function until the **acceptKey** function returns **true**. If **true** is returned, returns **true** immediately. If **acceptKey** didn't return **true** for any **key**, returns **false**.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `someDesc`

```ts
func someDesc<K>(set: Set<K>, acceptKey: (K) -> Bool): Bool
```

For every **key** in the **Set**, from the **last** to the **first** **key** (in descending order) calls the provided **acceptKey** function until the **acceptKey** function returns **true**. If **true** is returned, returns **true** immediately. If **acceptKey** didn't return **true** for any **key**, returns **false**.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `every`

```ts
func every<K>(set: Set<K>, acceptKey: (K) -> Bool): Bool
```

For every **key** in the **Set**, from the **first** to the **last** **key** (in ascending order) calls the provided **acceptKey** function until the **acceptKey** function returns **false**. If **false** is returned, returns **false** immediately. If **acceptKey** didn't return **false** for any **key**, returns **true**.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `everyDesc`

```ts
func everyDesc<K>(set: Set<K>, acceptKey: (K) -> Bool): Bool
```

For every **key** in the **Set**, from the **last** to the **first** **key** (in descending order) calls the provided **acceptKey** function until the **acceptKey** function returns **false**. If **false** is returned, returns **false** immediately. If **acceptKey** didn't return **false** for any **key**, returns **true**.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `forEach`

```ts
func forEach<K>(set: Set<K>, mapKey: (K) -> ()): ()
```

For every **key** in the **Set**, from the **first** to the **last** **key** (in ascending order) calls the provided **mapKey** function.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `forEachDesc`

```ts
func forEachDesc<K>(set: Set<K>, mapKey: (K) -> ()): ()
```

For every **key** in the **Set**, from the **last** to the **first** **key** (in descending order) calls the provided **mapKey** function.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

## Constructing from iterators

### `fromIter`

```ts
func fromIter<K>(iter: IterNext<K>, hashUtils: HashUtils<K>): Set<K>
```

Creates a new empty **Set**. For every **key** in the iterator, inserts the **key** at the **back** of the **Set** if the **key** is not present in the **Set**. If the **key** is present in the **Set**, moves the **key** to the **back** of the **Set** (if it's not there yet). Returns the new **Set**.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(n)**|
|Space max|**O(n)**|

---

### `fromIterDesc`

```ts
func fromIterDesc<K>(iter: IterNext<K>, hashUtils: HashUtils<K>): Set<K>
```

Creates a new empty **Set**. For every **key** in the iterator, inserts the **key** at the **front** of the **Set** if the **key** is not present in the **Set**. If the **key** is present in the **Set**, moves the **key** to the **front** of the **Set** (if it's not there yet). Returns the new **Set**.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(n)**|
|Space max|**O(n)**|

---

### `fromIterMap`

```ts
func fromIterMap<K, T>(iter: IterNext<T>, hashUtils: HashUtils<K>, mapItem: (T) -> ?K): Set<K>
```

Creates a new empty **Set**. For every item in the iterator, calls the provided **mapItem** function. For every item **mapItem** returned a **key** for, inserts the **key** at the **back** of the **Set** if the **key** is not present in the **Set**. If the **key** is present in the **Set**, moves the **key** to the **back** of the **Set** (if it's not there yet). Skips all items **mapItem** returned **null** for. Returns the new **Set**.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(n)**|
|Space max|**O(n)**|

---

### `fromIterMapDesc`

```ts
func fromIterMapDesc<K, T>(iter: IterNext<T>, hashUtils: HashUtils<K>, mapItem: (T) -> ?K): Set<K>
```

Creates a new empty **Set**. For every item in the iterator, calls the provided **mapItem** function. For every item **mapItem** returned a **key** for, inserts the **key** at the **front** of the **Set** if the **key** is not present in the **Set**. If the **key** is present in the **Set**, moves the **key** to the **front** of the **Set** (if it's not there yet). Skips all items **mapItem** returned **null** for. Returns the new **Set**.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(n)**|
|Space max|**O(n)**|

## Converting to arrays

### `toArray`

```ts
func toArray<K>(set: Set<K>): [K]
```

Constructs a new array with all keys present in the **Set**, from the **first** to the **last** **key** (in ascending order). Returns the array.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(n)**|
|Space max|**O(n)**|

---

### `toArrayDesc`

```ts
func toArrayDesc<K>(set: Set<K>): [K]
```

Constructs a new array with all keys present in the **Set**, from the **last** to the **first** **key** (in descending order). Returns the array.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(n)**|
|Space max|**O(n)**|

---

### `toArrayMap`

```ts
func toArrayMap<K, T>(set: Set<K>, mapKey: (K) -> ?T): [T]
```

For every **key** in the **Set**, from the **first** to the **last** **key** (in ascending order), calls the provided **mapKey** function. Constructs a new array with all **non-null** **mapKey result** items (in the same order **mapKey** was called). Skips all **null** **mapKey result** items. Returns the array.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(n)**|
|Space max|**O(n)**|

---

### `toArrayMapDesc`

```ts
func toArrayMapDesc<K, T>(set: Set<K>, mapKey: (K) -> ?T): [T]
```

For every **key** in the **Set**, from the **last** to the **first** **key** (in descending order), calls the provided **mapKey** function. Constructs a new array with all **non-null** **mapKey result** items (in the same order **mapKey** was called). Skips all **null** **mapKey result** items. Returns the array.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(n)**|
|Space max|**O(n)**|

## Rehashing

### `rehash`

```ts
func rehash<K>(set: Set<K>, hashUtils: HashUtils<K>): ()
```

If the **Set** is empty, returns immediately. Otherwise, allocates additional space, equal to the **power of 2** that can hold **8/7** of all current keys (either half, equal or double the current capacity) times **12 bytes**. Moves all keys to the new allocated space, while eliminating the holes (unused array indexes). Recalculates the hash table (mapping from a hash to the first **key** in a bucket). Recalculates bucket chains (keys that have equal **hash % Set size** result in a collision and end up in the same bucket). To perform those recalculation, hashes for all keys must also be recalculated.

Requires [**Hash Utils**](#hash-utils-1) to be provided as the second argument.

This method is usually called under the hood on inserts (when the **Set** is full) and deletes (when the **Set** size is less than **3 / 8 of capacity**). It is not necessary to call this method manually under normal conditions.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(n)**|
|Space max|**O(n)**|

# **Iterator** interface description

```ts
type Iter<T> = {
  prev: () -> ?T;
  next: () -> ?T;
  peekPrev: () -> ?T;
  peekNext: () -> ?T;
  movePrev: () -> Iter<T>;
  moveNext: () -> Iter<T>;
  current: () -> ?T;
  started: () -> Bool;
  finished: () -> Bool;
  reset: () -> Iter<T>;
};
```

## Moving a pointer

### `prev`

```ts
func prev(): ?K

func prev(): ?V

func prev(): ?(K, V)
```

Sets the **started** flag to **true**. If the **Map**/**Set** is empty, returns **null**. Otherwise, moves the iterator pointer to the previous **key**/**value**/**entry** for ascending iterators and to the next **key**/**value**/**entry** for descending iterators and saves the pointer position in the iterator object. If the current **entry** before the pointer move was the **first** **entry** for ascending iterators or the **last** **entry** for descending iterators, the iterator ends up in its initial position and in the finished state, the new iteration cycle can be started and the method returns **null**. Otherwise returns the **key**/**value**/**entry** which becomes current after a pointer move.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)** unlikely under normal conditions|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `next`

```ts
func next(): ?K

func next(): ?V

func next(): ?(K, V)
```

Sets the **started** flag to **true**. If the **Map**/**Set** is empty, returns **null**. Otherwise, moves the iterator pointer to the next **key**/**value**/**entry** for ascending iterators and to the previous **key**/**value**/**entry** for descending iterators and saves the pointer position in the iterator object. If the current **entry** before the pointer move was the **last** **entry** for ascending iterators or the **first** **entry** for descending iterators, the iterator ends up in its initial position and in the finished state, the new iteration cycle can be started and the method returns **null**. Otherwise returns the **key**/**value**/**entry** which becomes current after a pointer move.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)** unlikely under normal conditions|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `peekPrev`

```ts
func peekPrev(): ?K

func peekPrev(): ?V

func peekPrev(): ?(K, V)
```

If the **Map**/**Set** is empty, returns **null**. Otherwise, moves the iterator pointer to the previous **key**/**value**/**entry** for ascending iterators and to the next **key**/**value**/**entry** for descending iterators but doesn't save the pointer position in the iterator object. If the current **entry** before the pointer move was the **first** **entry** for ascending iterators or the **last** **entry** for descending iterators, returns **null**. Otherwise returns the **key**/**value**/**entry** which becomes current after a pointer move.

This operation is the same as [**prev**](#prev) but doesn't save the pointer position, meaning the return value will be the same but the iterator object state won't change after the operation. It also doesn't change the **started** flag.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)** unlikely under normal conditions|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `peekNext`

```ts
func peekNext(): ?K

func peekNext(): ?V

func peekNext(): ?(K, V)
```

If the **Map**/**Set** is empty, returns **null**. Otherwise, moves the iterator pointer to the next **key**/**value**/**entry** for ascending iterators and to the previous **key**/**value**/**entry** for descending iterators but doesn't save the pointer position in the iterator object. If the current **entry** before the pointer move was the **last** **entry** for ascending iterators or the **first** **entry** for descending iterators, returns **null**. Otherwise returns the **key**/**value**/**entry** which becomes current after a pointer move.

This operation is the same as [**next**](#next) but doesn't save the pointer position, meaning the return value will be the same but the iterator object state won't change after the operation. It also doesn't change the **started** flag.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)** unlikely under normal conditions|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `movePrev`

```ts
func movePrev(): Iter<K>

func movePrev(): Iter<V>

func movePrev(): Iter<(K, V)>
```

Sets the **started** flag to **true**. If the **Map**/**Set** is empty, returns the iterator object. Otherwise, moves the iterator pointer to the previous **key**/**value**/**entry** for ascending iterators and to the next **key**/**value**/**entry** for descending iterators and saves the pointer position in the iterator object. If the current **entry** before the pointer move was the **first** **entry** for ascending iterators or the **last** **entry** for descending iterators, the iterator ends up in its initial position and in the finished state and the new iteration cycle can be started. Returns the iterator object.

This operation is the same as [**prev**](#prev) but always returns the iterator object.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)** unlikely under normal conditions|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `moveNext`

```ts
func moveNext(): Iter<K>

func moveNext(): Iter<V>

func moveNext(): Iter<(K, V)>
```

Sets the **started** flag to **true**. If the **Map**/**Set** is empty, returns the iterator object. Otherwise, moves the iterator pointer to the next **key**/**value**/**entry** for ascending iterators and to the previous **key**/**value**/**entry** for descending iterators and saves the pointer position in the iterator object. If the current **entry** before the pointer move was the **last** **entry** for ascending iterators or the **first** **entry** for descending iterators, the iterator ends up in its initial position and in the finished state and the new iteration cycle can be started. Returns the iterator object.

This operation is the same as [**next**](#next) but always returns the iterator object.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(n)** unlikely under normal conditions|
|Space average|**O(1)**|
|Space max|**O(1)**|

## Getting current item

### `current`

```ts
func current(): ?K

func current(): ?V

func current(): ?(K, V)
```

If the **Map**/**Set** is empty or the iterator is in its initial position, returns **null**, otherwise returns the current **key**/**value**/**entry**.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(1)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

## Getting iterator state

### `started`

```ts
func started(): Bool
```

Returns **true** if any [**prev**](#prev)/[**next**](#next)/[**movePrev**](#moveprev)/[**moveNext**](#movenext) method was called after the iterator object was created or reset with the [**reset**](#reset) method. Otherwise, returns **false**.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(1)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `finished`

```ts
func finished(): Bool
```

Returns **true** if the iterator was [**started**](#started) (meaning any [**prev**](#prev)/[**next**](#next)/[**movePrev**](#moveprev)/[**moveNext**](#movenext) method was called after the iterator object was created or reset with the [**reset**](#reset) method) and is currently in its initial position. Otherwise, returns **false**.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(1)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

## Resetting

### `reset`

```ts
func reset(): Iter<K>

func reset(): Iter<V>

func reset(): Iter<(K, V)>
```

Resets the **started** flag to **false**. Resets the iterator to its initial position. Returns the iterator object.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(1)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

# **Hash Utils**

```ts
type HashUtils<K> = (
  getHash: (K) -> Nat32,
  areEqual: (K, K) -> Bool,
);
```

## Calculating hashes

### `hashInt`

```ts
func hashInt(key: Int): Nat32
```

Truncates the **key** to a **64 bit** unsigned integer. Calculates the hash using a **64 bit** variant of the **Multiply-Xorshift** hashing technique. Truncates the hash to a **32 bit** unsigned integer. Drops **2** most significant bits of the hash to prevent it going to the heap. Returns the hash.

This function is exposed separately, although most of the time you will be using it as a part of [**ihash**](#ihash).

[Prospecting for Hash Functions](https://nullprogram.com/blog/2018/07/31/)

[New best known functions](https://github.com/skeeto/hash-prospector/issues/19)

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(1)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

### `hashInt8`

```ts
func hashInt8(key: Int8): Nat32
```

Converts the **key** to a **32 bit** unsigned integer. Calculates the hash using a **32 bit** variant of the **Multiply-Xorshift** hashing technique. Drops **2** most significant bits of the hash to prevent it going to the heap. Returns the hash.

This function is exposed separately, although most of the time you will be using it as a part of [**i8hash**](#i8hash).

[Prospecting for Hash Functions](https://nullprogram.com/blog/2018/07/31/)

[New best known functions](https://github.com/skeeto/hash-prospector/issues/19)

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(1)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

### `hashInt16`

```ts
func hashInt16(key: Int16): Nat32
```

Converts the **key** to a **32 bit** unsigned integer. Calculates the hash using a **32 bit** variant of the **Multiply-Xorshift** hashing technique. Drops **2** most significant bits of the hash to prevent it going to the heap. Returns the hash.

This function is exposed separately, although most of the time you will be using it as a part of [**i16hash**](#i16hash).

[Prospecting for Hash Functions](https://nullprogram.com/blog/2018/07/31/)

[New best known functions](https://github.com/skeeto/hash-prospector/issues/19)

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(1)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

### `hashInt32`

```ts
func hashInt32(key: Int32): Nat32
```

Converts the **key** to a **32 bit** unsigned integer. Calculates the hash using a **32 bit** variant of the **Multiply-Xorshift** hashing technique. Drops **2** most significant bits of the hash to prevent it going to the heap. Returns the hash.

This function is exposed separately, although most of the time you will be using it as a part of [**i32hash**](#i32hash).

[Prospecting for Hash Functions](https://nullprogram.com/blog/2018/07/31/)

[New best known functions](https://github.com/skeeto/hash-prospector/issues/19)

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(1)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `hashInt64`

```ts
func hashInt64(key: Int64): Nat32
```

Converts the **key** to a **64 bit** unsigned integer. Calculates the hash using a **64 bit** variant of the **Multiply-Xorshift** hashing technique. Truncates the hash to a **32 bit** unsigned integer. Drops **2** most significant bits of the hash to prevent it going to the heap. Returns the hash.

This function is exposed separately, although most of the time you will be using it as a part of [**i64hash**](#i64hash).

[Prospecting for Hash Functions](https://nullprogram.com/blog/2018/07/31/)

[New best known functions](https://github.com/skeeto/hash-prospector/issues/19)

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(1)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

### `hashNat`

```ts
func hashNat(key: Nat): Nat32
```

Truncates the **key** to a **64 bit** unsigned integer. Calculates the hash using a **64 bit** variant of the **Multiply-Xorshift** hashing technique. Truncates the hash to a **32 bit** unsigned integer. Drops **2** most significant bits of the hash to prevent it going to the heap. Returns the hash.

This function is exposed separately, although most of the time you will be using it as a part of [**nhash**](#nhash).

[Prospecting for Hash Functions](https://nullprogram.com/blog/2018/07/31/)

[New best known functions](https://github.com/skeeto/hash-prospector/issues/19)

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(1)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

### `hashNat8`

```ts
func hashNat8(key: Nat8): Nat32
```

Converts the **key** to a **32 bit** unsigned integer. Calculates the hash using a **32 bit** variant of the **Multiply-Xorshift** hashing technique. Drops **2** most significant bits of the hash to prevent it going to the heap. Returns the hash.

This function is exposed separately, although most of the time you will be using it as a part of [**n8hash**](#n8hash).

[Prospecting for Hash Functions](https://nullprogram.com/blog/2018/07/31/)

[New best known functions](https://github.com/skeeto/hash-prospector/issues/19)

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(1)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

### `hashNat16`

```ts
func hashNat16(key: Nat16): Nat32
```

Converts the **key** to a **32 bit** unsigned integer. Calculates the hash using a **32 bit** variant of the **Multiply-Xorshift** hashing technique. Drops **2** most significant bits of the hash to prevent it going to the heap. Returns the hash.

This function is exposed separately, although most of the time you will be using it as a part of [**n16hash**](#n16hash).

[Prospecting for Hash Functions](https://nullprogram.com/blog/2018/07/31/)

[New best known functions](https://github.com/skeeto/hash-prospector/issues/19)

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(1)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

### `hashNat32`

```ts
func hashNat32(key: Nat32): Nat32
```

Calculates the hash using a **32 bit** variant of the **Multiply-Xorshift** hashing technique. Drops **2** most significant bits of the hash to prevent it going to the heap. Returns the hash.

This function is exposed separately, although most of the time you will be using it as a part of [**n32hash**](#n32hash).

[Prospecting for Hash Functions](https://nullprogram.com/blog/2018/07/31/)

[New best known functions](https://github.com/skeeto/hash-prospector/issues/19)

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(1)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `hashNat64`

```ts
func hashNat64(key: Nat64): Nat32
```

Calculates the hash using a **64 bit** variant of the **Multiply-Xorshift** hashing technique. Truncates the hash to a **32 bit** unsigned integer. Drops **2** most significant bits of the hash to prevent it going to the heap. Returns the hash.

This function is exposed separately, although most of the time you will be using it as a part of [**n64hash**](#n64hash).

[Prospecting for Hash Functions](https://nullprogram.com/blog/2018/07/31/)

[New best known functions](https://github.com/skeeto/hash-prospector/issues/19)

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(1)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `hashText`

```ts
func hashText(key: Text): Nat32
```

Converts the **key** to a **UTF-8** encoded blob. Calculates the hash using the internal hash function for blob, which is based on the **CRC32** algorithm. Drops **2** most significant bits of the hash to prevent it going to the heap. Returns the hash.

This function is exposed separately, although most of the time you will be using it as a part of [**thash**](#thash).

> **NOTE:** Currently, this is the only hash function in the library that can (occasionally) allocate additional temporary space. Allocation happens at the **text to blob** conversion step (when the internal text representation consists of multiple substrings). This will be a subject to change when we have optimized **for in** text iteration that does not create an iterator object under the hood.

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(n)**|

---

### `hashPrincipal`

```ts
func hashPrincipal(key: Principal): Nat32
```

Converts the **key** to blob. Calculates the hash using the internal hash function for blob, which is based on the **CRC32** algorithm. Drops **2** most significant bits of the hash to prevent it going to the heap. Returns the hash.

This function is exposed separately, although most of the time you will be using it as a part of [**phash**](#phash).

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(1)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `hashBlob`

```ts
func hashBlob(key: Blob): Nat32
```

Calculates the hash using the internal hash function for blob, which is based on the **CRC32** algorithm. Drops **2** most significant bits of the hash to prevent it going to the heap. Returns the hash.

This function is exposed separately, although most of the time you will be using it as a part of [**bhash**](#bhash).

|Complexity||
|-|-|
|Runtime average|**O(n)**|
|Runtime max|**O(n)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `hashBool`

```ts
func hashBool(key: Bool): Nat32
```

Returns numeric alternatives for **true** and **false** values.

This function is exposed separately, although most of the time you will be using it as a part of [**lhash**](#lhash).

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(1)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

## Composite utils

### `ihash`

```ts
let ihash: HashUtils<Int> = (hashInt, areEqual)
```

Composite utils for **Int** keys that allow calculating hashes using [**hashInt**](#hashint) function and checking equality.

A lot of **Map**/**Set** methods require [**Hash Utils**](#hash-utils-1) to be provided with each call.

---

### `i8hash`

```ts
let i8hash: HashUtils<Int8> = (hashInt8, areEqual)
```

Composite utils for **Int8** keys that allow calculating hashes using [**hashInt8**](#hashint8) function and checking equality.

A lot of **Map**/**Set** methods require [**Hash Utils**](#hash-utils-1) to be provided with each call.

---

### `i16hash`

```ts
let i16hash: HashUtils<Int16> = (hashInt16, areEqual)
```

Composite utils for **Int16** keys that allow calculating hashes using [**hashInt16**](#hashint16) function and checking equality.

A lot of **Map**/**Set** methods require [**Hash Utils**](#hash-utils-1) to be provided with each call.

---

### `i32hash`

```ts
let i32hash: HashUtils<Int32> = (hashInt32, areEqual)
```

Composite utils for **Int32** keys that allow calculating hashes using [**hashInt32**](#hashint32) function and checking equality.

A lot of **Map**/**Set** methods require [**Hash Utils**](#hash-utils-1) to be provided with each call.

---

### `i64hash`

```ts
let i64hash: HashUtils<Int64> = (hashInt64, areEqual)
```

Composite utils for **Int64** keys that allow calculating hashes using [**hashInt64**](#hashint64) function and checking equality.

A lot of **Map**/**Set** methods require [**Hash Utils**](#hash-utils-1) to be provided with each call.

---

### `nhash`

```ts
let nhash: HashUtils<Nat> = (hashNat, areEqual)
```

Composite utils for **Nat** keys that allow calculating hashes using [**hashNat**](#hashnat) function and checking equality.

A lot of **Map**/**Set** methods require [**Hash Utils**](#hash-utils-1) to be provided with each call.

---

### `n8hash`

```ts
let n8hash: HashUtils<Nat8> = (hashNat8, areEqual)
```

Composite utils for **Nat8** keys that allow calculating hashes using [**hashNat8**](#hashnat8) function and checking equality.

A lot of **Map**/**Set** methods require [**Hash Utils**](#hash-utils-1) to be provided with each call.

---

### `n16hash`

```ts
let n16hash: HashUtils<Nat16> = (hashNat16, areEqual)
```

Composite utils for **Nat16** keys that allow calculating hashes using [**hashNat16**](#hashnat16) function and checking equality.

A lot of **Map**/**Set** methods require [**Hash Utils**](#hash-utils-1) to be provided with each call.

---

### `n32hash`

```ts
let n32hash: HashUtils<Nat32> = (hashNat32, areEqual)
```

Composite utils for **Nat32** keys that allow calculating hashes using [**hashNat32**](#hashnat32) function and checking equality.

A lot of **Map**/**Set** methods require [**Hash Utils**](#hash-utils-1) to be provided with each call.

---

### `n64hash`

```ts
let n64hash: HashUtils<Nat64> = (hashNat64, areEqual)
```

Composite utils for **Nat64** keys that allow calculating hashes using [**hashNat64**](#hashnat64) function and checking equality.

A lot of **Map**/**Set** methods require [**Hash Utils**](#hash-utils-1) to be provided with each call.

---

### `thash`

```ts
let thash: HashUtils<Text> = (hashText, areEqual)
```

Composite utils for **Text** keys that allow calculating hashes using [**hashText**](#hashtext) function and checking equality.

A lot of **Map**/**Set** methods require [**Hash Utils**](#hash-utils-1) to be provided with each call.

---

### `phash`

```ts
let phash: HashUtils<Principal> = (hashPrincipal, areEqual)
```

Composite utils for **Principal** keys that allow calculating hashes using [**hashPrincipal**](#hashprincipal) function and checking equality.

A lot of **Map**/**Set** methods require [**Hash Utils**](#hash-utils-1) to be provided with each call.

---

### `bhash`

```ts
let bhash: HashUtils<Blob> = (hashBlob, areEqual)
```

Composite utils for **Blob** keys that allow calculating hashes using [**hashBlob**](#hashblob) function and checking equality.

A lot of **Map**/**Set** methods require [**Hash Utils**](#hash-utils-1) to be provided with each call.

---

### `lhash`

```ts
let lhash: HashUtils<Bool> = (hashBool, areEqual)
```

Composite utils for **Bool** keys that allow calculating hashes using [**hashBool**](#hashbool) function and checking equality.

A lot of **Map**/**Set** methods require [**Hash Utils**](#hash-utils-1) to be provided with each call.

## Constructing new utils

### `combineHash`

```ts
func combineHash<K1, K2>(hashUtils1: HashUtils<K1>, hashUtils2: HashUtils<K2>): HashUtils<(K1, K2)>
```

Constructs new [**Hash Utils**](#hash-utils-1) capable of handling composite keys (tuples of **2** components). Takes **2** [**Hash Utils**](#hash-utils-1) as an input which will handle the first and the second component of a composite **key** separately. The combined [**Hash Utils**](#hash-utils-1) will handle the merging of both hashes and ensuring that equality function returns **true** only when both components are equal. Returns the new [**Hash Utils**](#hash-utils-1).

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(1)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `useHash`

```ts
func useHash<K>(hashUtils: HashUtils<K>, hash: Nat32): HashUtils<K>
```

Constructs new [**Hash Utils**](#hash-utils-1) with its hashing component being a function that returns a constant hash provided as a second argument and its equality component equal to the provided [**Hash Utils**](#hash-utils-1). Returns the new [**Hash Utils**](#hash-utils-1).

> **WARNING:** Incorrect usage of this function can result in a corrupted **Map**/**Set** state.

|Complexity||
|-|-|
|Runtime average|**O(1)**|
|Runtime max|**O(1)**|
|Space average|**O(1)**|
|Space max|**O(1)**|

---

### `calcHash`

```ts
func calcHash<K>(hashUtils: HashUtils<K>, key: K): HashUtils<K>
```

Constructs new [**Hash Utils**](#hash-utils-1) with its hashing component being a function that returns a constant hash calculated from the provided **key** and its equality component equal to the provided [**Hash Utils**](#hash-utils-1). Returns the new [**Hash Utils**](#hash-utils-1).

> **WARNING:** Incorrect usage of this function can result in a corrupted **Map**/**Set** state.

|Complexity||
|-|-|
|Runtime average|depends on arguments|
|Runtime max|depends on arguments|
|Space average|depends on arguments|
|Space max|depends on arguments|
