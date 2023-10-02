import Const "../const";
import Types "../types";
import { rehash } "./rehash";
import { init; initFront; initUpdate; initUpdateFront } "./init";
import { natToNat32 = nat32; nat32ToNat = nat; trap } "mo:prim";

module {
  type Map<K, V> = Types.Map<K, V>;

  type HashUtils<K> = Types.HashUtils<K>;

  let DATA = Const.DATA;

  let FRONT = Const.FRONT;

  let BACK = Const.BACK;

  let SIZE = Const.SIZE;

  let NULL = Const.NULL;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func put<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V): ?V {
    let data = switch (map[DATA]) { case (?data) data; case (_) return init(map, hashUtils, keyParam, ?valueParam) };

    let keys = data.0;
    let capacity = nat32(keys.size());
    let hashIndex = nat(hashUtils.0(keyParam) % capacity +% capacity);

    let indexes = data.2;
    let firstIndex = indexes[hashIndex];
    var index = firstIndex;

    loop if (index == NULL) {
      let bounds = data.3;
      let back = bounds[BACK];
      let backNat = nat(back);

      bounds[BACK] := (back +% 1) % capacity;
      bounds[SIZE] +%= 1;

      keys[backNat] := ?keyParam;
      data.1[backNat] := ?valueParam;
      indexes[backNat] := firstIndex;
      indexes[hashIndex] := backNat;

      if (back == bounds[FRONT]) rehash(map, hashUtils);

      return null;
    } else if (hashUtils.1(switch (keys[index]) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
      let prevValue = data.1[index];

      data.1[index] := ?valueParam;

      return prevValue;
    } else {
      index := indexes[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func putFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V): ?V {
    let data = switch (map[DATA]) { case (?data) data; case (_) return initFront(map, hashUtils, keyParam, ?valueParam) };

    let keys = data.0;
    let capacity = nat32(keys.size());
    let hashIndex = nat(hashUtils.0(keyParam) % capacity +% capacity);

    let indexes = data.2;
    let firstIndex = indexes[hashIndex];
    var index = firstIndex;

    loop if (index == NULL) {
      let bounds = data.3;
      let front = bounds[FRONT];
      let frontNat = nat(front);

      bounds[FRONT] := (front -% 1) % capacity;
      bounds[SIZE] +%= 1;

      keys[frontNat] := ?keyParam;
      data.1[frontNat] := ?valueParam;
      indexes[frontNat] := firstIndex;
      indexes[hashIndex] := frontNat;

      if (front == bounds[BACK]) rehash(map, hashUtils);

      return null;
    } else if (hashUtils.1(switch (keys[index]) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
      let prevValue = data.1[index];

      data.1[index] := ?valueParam;

      return prevValue;
    } else {
      index := indexes[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func set<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V) {
    let data = switch (map[DATA]) { case (?data) data; case (_) return ignore init(map, hashUtils, keyParam, ?valueParam) };

    let keys = data.0;
    let capacity = nat32(keys.size());
    let hashIndex = nat(hashUtils.0(keyParam) % capacity +% capacity);

    let indexes = data.2;
    let firstIndex = indexes[hashIndex];
    var index = firstIndex;

    loop if (index == NULL) {
      let bounds = data.3;
      let back = bounds[BACK];
      let backNat = nat(back);

      bounds[BACK] := (back +% 1) % capacity;
      bounds[SIZE] +%= 1;

      keys[backNat] := ?keyParam;
      data.1[backNat] := ?valueParam;
      indexes[backNat] := firstIndex;
      indexes[hashIndex] := backNat;

      if (back == bounds[FRONT]) rehash(map, hashUtils);

      return;
    } else if (hashUtils.1(switch (keys[index]) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
      data.1[index] := ?valueParam;

      return;
    } else {
      index := indexes[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func setFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V) {
    let data = switch (map[DATA]) { case (?data) data; case (_) return ignore initFront(map, hashUtils, keyParam, ?valueParam) };

    let keys = data.0;
    let capacity = nat32(keys.size());
    let hashIndex = nat(hashUtils.0(keyParam) % capacity +% capacity);

    let indexes = data.2;
    let firstIndex = indexes[hashIndex];
    var index = firstIndex;

    loop if (index == NULL) {
      let bounds = data.3;
      let front = bounds[FRONT];
      let frontNat = nat(front);

      bounds[FRONT] := (front -% 1) % capacity;
      bounds[SIZE] +%= 1;

      keys[frontNat] := ?keyParam;
      data.1[frontNat] := ?valueParam;
      indexes[frontNat] := firstIndex;
      indexes[hashIndex] := frontNat;

      if (front == bounds[BACK]) rehash(map, hashUtils);

      return;
    } else if (hashUtils.1(switch (keys[index]) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
      data.1[index] := ?valueParam;

      return;
    } else {
      index := indexes[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func add<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V): ?V {
    let data = switch (map[DATA]) { case (?data) data; case (_) return init(map, hashUtils, keyParam, ?valueParam) };

    let keys = data.0;
    let capacity = nat32(keys.size());
    let hashIndex = nat(hashUtils.0(keyParam) % capacity +% capacity);

    let indexes = data.2;
    let firstIndex = indexes[hashIndex];
    var index = firstIndex;

    loop if (index == NULL) {
      let bounds = data.3;
      let back = bounds[BACK];
      let backNat = nat(back);

      bounds[BACK] := (back +% 1) % capacity;
      bounds[SIZE] +%= 1;

      keys[backNat] := ?keyParam;
      data.1[backNat] := ?valueParam;
      indexes[backNat] := firstIndex;
      indexes[hashIndex] := backNat;

      if (back == bounds[FRONT]) rehash(map, hashUtils);

      return null;
    } else if (hashUtils.1(switch (keys[index]) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
      return data.1[index];
    } else {
      index := indexes[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func addFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V): ?V {
    let data = switch (map[DATA]) { case (?data) data; case (_) return initFront(map, hashUtils, keyParam, ?valueParam) };

    let keys = data.0;
    let capacity = nat32(keys.size());
    let hashIndex = nat(hashUtils.0(keyParam) % capacity +% capacity);

    let indexes = data.2;
    let firstIndex = indexes[hashIndex];
    var index = firstIndex;

    loop if (index == NULL) {
      let bounds = data.3;
      let front = bounds[FRONT];
      let frontNat = nat(front);

      bounds[FRONT] := (front -% 1) % capacity;
      bounds[SIZE] +%= 1;

      keys[frontNat] := ?keyParam;
      data.1[frontNat] := ?valueParam;
      indexes[frontNat] := firstIndex;
      indexes[hashIndex] := frontNat;

      if (front == bounds[BACK]) rehash(map, hashUtils);

      return null;
    } else if (hashUtils.1(switch (keys[index]) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
      return data.1[index];
    } else {
      index := indexes[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func replace<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: V): ?V {
    let data = switch (map[DATA]) { case (?data) data; case (_) return null };

    let keys = data.0;
    let capacity = nat32(keys.size());
    let hashIndex = nat(hashUtils.0(keyParam) % capacity +% capacity);

    let indexes = data.2;
    let firstIndex = indexes[hashIndex];
    var index = firstIndex;

    loop if (index == NULL) {
      return null;
    } else if (hashUtils.1(switch (keys[index]) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
      let prevValue = data.1[index];

      data.1[index] := ?valueParam;

      return prevValue;
    } else {
      index := indexes[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func update<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, getNewValue: (K, ?V) -> ?V): ?V {
    let data = switch (map[DATA]) { case (?data) data; case (_) return initUpdate(map, hashUtils, keyParam, getNewValue) };

    let keys = data.0;
    let capacity = nat32(keys.size());
    let hashIndex = nat(hashUtils.0(keyParam) % capacity +% capacity);

    let indexes = data.2;
    let firstIndex = indexes[hashIndex];
    var index = firstIndex;

    loop if (index == NULL) {
      switch (getNewValue(keyParam, null)) {
        case (null) return null;

        case (valueParam) {
          let bounds = data.3;
          let back = bounds[BACK];
          let backNat = nat(back);

          bounds[BACK] := (back +% 1) % capacity;
          bounds[SIZE] +%= 1;

          keys[backNat] := ?keyParam;
          data.1[backNat] := valueParam;
          indexes[backNat] := firstIndex;
          indexes[hashIndex] := backNat;

          if (back == bounds[FRONT]) rehash(map, hashUtils);

          return valueParam;
        };
      };
    } else if (hashUtils.1(switch (keys[index]) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
      let value = data.1[index];

      switch (getNewValue(keyParam, value)) {
        case (null) return value;

        case (valueParam) {
          data.1[index] := valueParam;

          return valueParam;
        };
      };
    } else {
      index := indexes[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func updateFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, getNewValue: (K, ?V) -> ?V): ?V {
    let data = switch (map[DATA]) { case (?data) data; case (_) return initUpdateFront(map, hashUtils, keyParam, getNewValue) };

    let keys = data.0;
    let capacity = nat32(keys.size());
    let hashIndex = nat(hashUtils.0(keyParam) % capacity +% capacity);

    let indexes = data.2;
    let firstIndex = indexes[hashIndex];
    var index = firstIndex;

    loop if (index == NULL) {
      switch (getNewValue(keyParam, null)) {
        case (null) return null;

        case (valueParam) {
          let bounds = data.3;
          let front = bounds[FRONT];
          let frontNat = nat(front);

          bounds[FRONT] := (front -% 1) % capacity;
          bounds[SIZE] +%= 1;

          keys[frontNat] := ?keyParam;
          data.1[frontNat] := valueParam;
          indexes[frontNat] := firstIndex;
          indexes[hashIndex] := frontNat;

          if (front == bounds[BACK]) rehash(map, hashUtils);

          return valueParam;
        };
      };
    } else if (hashUtils.1(switch (keys[index]) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
      let value = data.1[index];

      switch (getNewValue(keyParam, value)) {
        case (null) return value;

        case (valueParam) {
          data.1[index] := valueParam;

          return valueParam;
        };
      };
    } else {
      index := indexes[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func putMove<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: ?V): ?V {
    let data = switch (map[DATA]) { case (?data) data; case (_) return init(map, hashUtils, keyParam, valueParam) };

    let keys = data.0;
    let capacity = nat32(keys.size());
    let hashIndex = nat(hashUtils.0(keyParam) % capacity +% capacity);

    let indexes = data.2;
    let firstIndex = indexes[hashIndex];
    var index = firstIndex;
    var prevIndex = NULL;

    loop if (index == NULL) {
      switch (valueParam) {
        case (null) return null;

        case (_) {
          let bounds = data.3;
          let back = bounds[BACK];
          let backNat = nat(back);

          bounds[BACK] := (back +% 1) % capacity;
          bounds[SIZE] +%= 1;

          keys[backNat] := ?keyParam;
          data.1[backNat] := valueParam;
          indexes[backNat] := firstIndex;
          indexes[hashIndex] := backNat;

          if (back == bounds[FRONT]) rehash(map, hashUtils);

          return null;
        };
      };
    } else {
      let key = keys[index];

      if (hashUtils.1(switch (key) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
        let values = data.1;
        let value = values[index];

        let bounds = data.3;
        let back = bounds[BACK];
        let index32 = nat32(index);

        if (index32 == (back -% 1) % capacity) {
          switch (valueParam) { case (null) {}; case (_) values[index] := valueParam };
        } else {
          let backNat = nat(back);

          bounds[BACK] := (back +% 1) % capacity;

          keys[backNat] := key;
          values[backNat] := switch (valueParam) { case (null) value; case (_) valueParam };
          indexes[backNat] := indexes[index];
          keys[index] := null;
          values[index] := null;

          if (prevIndex == NULL) indexes[hashIndex] := backNat else indexes[prevIndex] := backNat;

          var front = bounds[FRONT];
          let prevFront = (front +% 1) % capacity;

          if (index32 == prevFront) {
            front := prevFront;

            while (switch (keys[nat((front +% 1) % capacity)]) { case (null) true; case (_) false }) {
              front := (front +% 1) % capacity;
            };

            bounds[FRONT] := front;
          } else if (back == front) {
            rehash(map, hashUtils);
          };
        };

        return value;
      } else {
        prevIndex := index;
        index := indexes[index];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func putMoveFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: ?V): ?V {
    let data = switch (map[DATA]) { case (?data) data; case (_) return initFront(map, hashUtils, keyParam, valueParam) };

    let keys = data.0;
    let capacity = nat32(keys.size());
    let hashIndex = nat(hashUtils.0(keyParam) % capacity +% capacity);

    let indexes = data.2;
    let firstIndex = indexes[hashIndex];
    var index = firstIndex;
    var prevIndex = NULL;

    loop if (index == NULL) {
      switch (valueParam) {
        case (null) return null;

        case (_) {
          let bounds = data.3;
          let front = bounds[FRONT];
          let frontNat = nat(front);

          bounds[FRONT] := (front -% 1) % capacity;
          bounds[SIZE] +%= 1;

          keys[frontNat] := ?keyParam;
          data.1[frontNat] := valueParam;
          indexes[frontNat] := firstIndex;
          indexes[hashIndex] := frontNat;

          if (front == bounds[BACK]) rehash(map, hashUtils);

          return null;
        };
      };
    } else {
      let key = keys[index];

      if (hashUtils.1(switch (key) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
        let values = data.1;
        let value = values[index];

        let bounds = data.3;
        let front = bounds[FRONT];
        let index32 = nat32(index);

        if (index32 == (front +% 1) % capacity) {
          switch (valueParam) { case (null) {}; case (_) values[index] := valueParam };
        } else {
          let frontNat = nat(front);

          bounds[FRONT] := (front -% 1) % capacity;

          keys[frontNat] := key;
          values[frontNat] := switch (valueParam) { case (null) value; case (_) valueParam };
          indexes[frontNat] := indexes[index];
          keys[index] := null;
          values[index] := null;

          if (prevIndex == NULL) indexes[hashIndex] := frontNat else indexes[prevIndex] := frontNat;

          var back = bounds[BACK];
          let prevBack = (back -% 1) % capacity;

          if (index32 == prevBack) {
            back := prevBack;

            while (switch (keys[nat((back -% 1) % capacity)]) { case (null) true; case (_) false }) {
              back := (back -% 1) % capacity;
            };

            bounds[BACK] := back;
          } else if (front == back) {
            rehash(map, hashUtils);
          };
        };

        return value;
      } else {
        prevIndex := index;
        index := indexes[index];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func replaceMove<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: ?V): ?V {
    let data = switch (map[DATA]) { case (?data) data; case (_) return null };

    let keys = data.0;
    let capacity = nat32(keys.size());
    let hashIndex = nat(hashUtils.0(keyParam) % capacity +% capacity);

    let indexes = data.2;
    let firstIndex = indexes[hashIndex];
    var index = firstIndex;
    var prevIndex = NULL;

    loop if (index == NULL) {
      return null;
    } else {
      let key = keys[index];

      if (hashUtils.1(switch (key) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
        let values = data.1;
        let value = values[index];

        let bounds = data.3;
        let back = bounds[BACK];
        let index32 = nat32(index);

        if (index32 == (back -% 1) % capacity) {
          switch (valueParam) { case (null) {}; case (_) values[index] := valueParam };
        } else {
          let backNat = nat(back);

          bounds[BACK] := (back +% 1) % capacity;

          keys[backNat] := key;
          values[backNat] := switch (valueParam) { case (null) value; case (_) valueParam };
          indexes[backNat] := indexes[index];
          keys[index] := null;
          values[index] := null;

          if (prevIndex == NULL) indexes[hashIndex] := backNat else indexes[prevIndex] := backNat;

          var front = bounds[FRONT];
          let prevFront = (front +% 1) % capacity;

          if (index32 == prevFront) {
            front := prevFront;

            while (switch (keys[nat((front +% 1) % capacity)]) { case (null) true; case (_) false }) {
              front := (front +% 1) % capacity;
            };

            bounds[FRONT] := front;
          } else if (back == front) {
            rehash(map, hashUtils);
          };
        };

        return value;
      } else {
        prevIndex := index;
        index := indexes[index];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func replaceMoveFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, valueParam: ?V): ?V {
    let data = switch (map[DATA]) { case (?data) data; case (_) return null };

    let keys = data.0;
    let capacity = nat32(keys.size());
    let hashIndex = nat(hashUtils.0(keyParam) % capacity +% capacity);

    let indexes = data.2;
    let firstIndex = indexes[hashIndex];
    var index = firstIndex;
    var prevIndex = NULL;

    loop if (index == NULL) {
      return null;
    } else {
      let key = keys[index];

      if (hashUtils.1(switch (key) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
        let values = data.1;
        let value = values[index];

        let bounds = data.3;
        let front = bounds[FRONT];
        let index32 = nat32(index);

        if (index32 == (front +% 1) % capacity) {
          switch (valueParam) { case (null) {}; case (_) values[index] := valueParam };
        } else {
          let frontNat = nat(front);

          bounds[FRONT] := (front -% 1) % capacity;

          keys[frontNat] := key;
          values[frontNat] := switch (valueParam) { case (null) value; case (_) valueParam };
          indexes[frontNat] := indexes[index];
          keys[index] := null;
          values[index] := null;

          if (prevIndex == NULL) indexes[hashIndex] := frontNat else indexes[prevIndex] := frontNat;

          var back = bounds[BACK];
          let prevBack = (back -% 1) % capacity;

          if (index32 == prevBack) {
            back := prevBack;

            while (switch (keys[nat((back -% 1) % capacity)]) { case (null) true; case (_) false }) {
              back := (back -% 1) % capacity;
            };

            bounds[BACK] := back;
          } else if (front == back) {
            rehash(map, hashUtils);
          };
        };

        return value;
      } else {
        prevIndex := index;
        index := indexes[index];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func updateMove<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, getNewValue: (K, ?V) -> ?V): ?V {
    let data = switch (map[DATA]) { case (?data) data; case (_) return initUpdate(map, hashUtils, keyParam, getNewValue) };

    let keys = data.0;
    let capacity = nat32(keys.size());
    let hashIndex = nat(hashUtils.0(keyParam) % capacity +% capacity);

    let indexes = data.2;
    let firstIndex = indexes[hashIndex];
    var index = firstIndex;
    var prevIndex = NULL;

    loop if (index == NULL) {
      switch (getNewValue(keyParam, null)) {
        case (null) return null;

        case (valueParam) {
          let bounds = data.3;
          let back = bounds[BACK];
          let backNat = nat(back);

          bounds[BACK] := (back +% 1) % capacity;
          bounds[SIZE] +%= 1;

          keys[backNat] := ?keyParam;
          data.1[backNat] := valueParam;
          indexes[backNat] := firstIndex;
          indexes[hashIndex] := backNat;

          if (back == bounds[FRONT]) rehash(map, hashUtils);

          return valueParam;
        };
      };
    } else {
      let key = keys[index];

      if (hashUtils.1(switch (key) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
        let values = data.1;
        let value = values[index];

        let bounds = data.3;
        let back = bounds[BACK];
        let index32 = nat32(index);

        if (index32 == (back -% 1) % capacity) {
          switch (getNewValue(keyParam, value)) {
            case (null) return value;

            case (valueParam) {
              values[index] := valueParam;

              return valueParam;
            };
          };
        } else {
          let backNat = nat(back);

          let valueParam = switch (getNewValue(keyParam, value)) { case (null) value; case (valueParam) valueParam };

          bounds[BACK] := (back +% 1) % capacity;

          keys[backNat] := key;
          values[backNat] := valueParam;
          indexes[backNat] := indexes[index];
          keys[index] := null;
          values[index] := null;

          if (prevIndex == NULL) indexes[hashIndex] := backNat else indexes[prevIndex] := backNat;

          var front = bounds[FRONT];
          let prevFront = (front +% 1) % capacity;

          if (index32 == prevFront) {
            front := prevFront;

            while (switch (keys[nat((front +% 1) % capacity)]) { case (null) true; case (_) false }) {
              front := (front +% 1) % capacity;
            };

            bounds[FRONT] := front;
          } else if (back == front) {
            rehash(map, hashUtils);
          };

          return valueParam;
        };
      } else {
        prevIndex := index;
        index := indexes[index];
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func updateMoveFront<K, V>(map: Map<K, V>, hashUtils: HashUtils<K>, keyParam: K, getNewValue: (K, ?V) -> ?V): ?V {
    let data = switch (map[DATA]) { case (?data) data; case (_) return initUpdateFront(map, hashUtils, keyParam, getNewValue) };

    let keys = data.0;
    let capacity = nat32(keys.size());
    let hashIndex = nat(hashUtils.0(keyParam) % capacity +% capacity);

    let indexes = data.2;
    let firstIndex = indexes[hashIndex];
    var index = firstIndex;
    var prevIndex = NULL;

    loop if (index == NULL) {
      switch (getNewValue(keyParam, null)) {
        case (null) return null;

        case (valueParam) {
          let bounds = data.3;
          let front = bounds[FRONT];
          let frontNat = nat(front);

          bounds[FRONT] := (front -% 1) % capacity;
          bounds[SIZE] +%= 1;

          keys[frontNat] := ?keyParam;
          data.1[frontNat] := valueParam;
          indexes[frontNat] := firstIndex;
          indexes[hashIndex] := frontNat;

          if (front == bounds[BACK]) rehash(map, hashUtils);

          return valueParam;
        };
      };
    } else {
      let key = keys[index];

      if (hashUtils.1(switch (key) { case (?key) key; case (_) trap("unreachable") }, keyParam)) {
        let values = data.1;
        let value = values[index];

        let bounds = data.3;
        let front = bounds[FRONT];
        let index32 = nat32(index);

        if (index32 == (front +% 1) % capacity) {
          switch (getNewValue(keyParam, value)) {
            case (null) return value;

            case (valueParam) {
              values[index] := valueParam;

              return valueParam;
            };
          };
        } else {
          let frontNat = nat(front);

          let valueParam = switch (getNewValue(keyParam, value)) { case (null) value; case (valueParam) valueParam };

          bounds[FRONT] := (front -% 1) % capacity;

          keys[frontNat] := key;
          values[frontNat] := valueParam;
          indexes[frontNat] := indexes[index];
          keys[index] := null;
          values[index] := null;

          if (prevIndex == NULL) indexes[hashIndex] := frontNat else indexes[prevIndex] := frontNat;

          var back = bounds[BACK];
          let prevBack = (back -% 1) % capacity;

          if (index32 == prevBack) {
            back := prevBack;

            while (switch (keys[nat((back -% 1) % capacity)]) { case (null) true; case (_) false }) {
              back := (back -% 1) % capacity;
            };

            bounds[BACK] := back;
          } else if (front == back) {
            rehash(map, hashUtils);
          };

          return valueParam;
        };
      } else {
        prevIndex := index;
        index := indexes[index];
      };
    };
  };
};
