import Const "../const";
import Types "../types";
import { natToNat32 = nat32; nat32ToNat = nat; trap } "mo:prim";

module {
  type Set<K> = Types.Set<K>;

  type Iter<T> = Types.Iter<T>;

  type HashUtils<K> = Types.HashUtils<K>;

  let DATA = Const.DATA;

  let FRONT = Const.FRONT;

  let BACK = Const.BACK;

  let NULL = Const.NULL;

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func keys<K>(map: Set<K>): Iter<K> {
    let dataOpt = map[DATA];

    let capacity = switch (dataOpt) { case (?data) nat32(data.0.size()); case (_) 0:Nat32 };
    let front = switch (dataOpt) { case (?data) data.2[FRONT]; case (_) 0:Nat32 };
    let back = switch (dataOpt) { case (?data) data.2[BACK]; case (_) 0:Nat32 };

    var started = false;
    var iterIndex = front;

    let iter = {
      prev = func(): ?K {
        started := true;

        switch (dataOpt) {
          case (?data) loop {
            iterIndex := if (iterIndex == front) (back -% 1) % capacity else (iterIndex -% 1) % capacity;

            switch (data.0[nat(iterIndex)]) {
              case (null) if (iterIndex == front) return null;
              case (key) return key;
            };
          };

          case (_) null;
        };
      };

      next = func(): ?K {
        started := true;

        switch (dataOpt) {
          case (?data) loop {
            iterIndex := if (iterIndex == back) (front +% 1) % capacity else (iterIndex +% 1) % capacity;

            switch (data.0[nat(iterIndex)]) {
              case (null) if (iterIndex == back) return null;
              case (key) return key;
            };
          };

          case (_) null;
        };
      };

      peekPrev = func(): ?K {
        var newIndex = iterIndex;

        switch (dataOpt) {
          case (?data) loop {
            newIndex := if (newIndex == front) (back -% 1) % capacity else (newIndex -% 1) % capacity;

            switch (data.0[nat(newIndex)]) {
              case (null) if (newIndex == front) return null;
              case (key) return key;
            };
          };

          case (_) null;
        };
      };

      peekNext = func(): ?K {
        var newIndex = iterIndex;

        switch (dataOpt) {
          case (?data) loop {
            newIndex := if (newIndex == back) (front +% 1) % capacity else (newIndex +% 1) % capacity;

            switch (data.0[nat(newIndex)]) {
              case (null) if (newIndex == back) return null;
              case (key) return key;
            };
          };

          case (_) null;
        };
      };

      movePrev = func(): Iter<K> {
        started := true;

        switch (dataOpt) {
          case (?data) loop {
            iterIndex := if (iterIndex == front) (back -% 1) % capacity else (iterIndex -% 1) % capacity;

            switch (data.0[nat(iterIndex)]) {
              case (null) if (iterIndex == front) return iter;
              case (_) return iter;
            };
          };

          case (_) iter;
        };
      };

      moveNext = func(): Iter<K> {
        started := true;

        switch (dataOpt) {
          case (?data) loop {
            iterIndex := if (iterIndex == back) (front +% 1) % capacity else (iterIndex +% 1) % capacity;

            switch (data.0[nat(iterIndex)]) {
              case (null) if (iterIndex == back) return iter;
              case (_) return iter;
            };
          };

          case (_) iter;
        };
      };

      current = func(): ?K {
        switch (dataOpt) {
          case (?data) switch (data.0[nat(iterIndex)]) { case (null) null; case (key) key };
          case (_) null;
        };
      };

      started = func(): Bool {
        started;
      };

      finished = func(): Bool {
        started and (iterIndex == front or iterIndex == back);
      };

      reset = func(): Iter<K> {
        started := false;

        iterIndex := front;

        iter;
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func keysDesc<K>(map: Set<K>): Iter<K> {
    let dataOpt = map[DATA];

    let capacity = switch (dataOpt) { case (?data) nat32(data.0.size()); case (_) 0:Nat32 };
    let front = switch (dataOpt) { case (?data) data.2[FRONT]; case (_) 0:Nat32 };
    let back = switch (dataOpt) { case (?data) data.2[BACK]; case (_) 0:Nat32 };

    var started = false;
    var iterIndex = back;

    let iter = {
      prev = func(): ?K {
        started := true;

        switch (dataOpt) {
          case (?data) loop {
            iterIndex := if (iterIndex == back) (front +% 1) % capacity else (iterIndex +% 1) % capacity;

            switch (data.0[nat(iterIndex)]) {
              case (null) if (iterIndex == back) return null;
              case (key) return key;
            };
          };

          case (_) null;
        };
      };

      next = func(): ?K {
        started := true;

        switch (dataOpt) {
          case (?data) loop {
            iterIndex := if (iterIndex == front) (back -% 1) % capacity else (iterIndex -% 1) % capacity;

            switch (data.0[nat(iterIndex)]) {
              case (null) if (iterIndex == front) return null;
              case (key) return key;
            };
          };

          case (_) null;
        };
      };

      peekPrev = func(): ?K {
        var newIndex = iterIndex;

        switch (dataOpt) {
          case (?data) loop {
            newIndex := if (newIndex == back) (front +% 1) % capacity else (newIndex +% 1) % capacity;

            switch (data.0[nat(newIndex)]) {
              case (null) if (newIndex == back) return null;
              case (key) return key;
            };
          };

          case (_) null;
        };
      };

      peekNext = func(): ?K {
        var newIndex = iterIndex;

        switch (dataOpt) {
          case (?data) loop {
            newIndex := if (newIndex == front) (back -% 1) % capacity else (newIndex -% 1) % capacity;

            switch (data.0[nat(newIndex)]) {
              case (null) if (newIndex == front) return null;
              case (key) return key;
            };
          };

          case (_) null;
        };
      };

      movePrev = func(): Iter<K> {
        started := true;

        switch (dataOpt) {
          case (?data) loop {
            iterIndex := if (iterIndex == back) (front +% 1) % capacity else (iterIndex +% 1) % capacity;

            switch (data.0[nat(iterIndex)]) {
              case (null) if (iterIndex == back) return iter;
              case (_) return iter;
            };
          };

          case (_) iter;
        };
      };

      moveNext = func(): Iter<K> {
        started := true;

        switch (dataOpt) {
          case (?data) loop {
            iterIndex := if (iterIndex == front) (back -% 1) % capacity else (iterIndex -% 1) % capacity;

            switch (data.0[nat(iterIndex)]) {
              case (null) if (iterIndex == front) return iter;
              case (_) return iter;
            };
          };

          case (_) iter;
        };
      };

      current = func(): ?K {
        switch (dataOpt) {
          case (?data) switch (data.0[nat(iterIndex)]) { case (null) null; case (key) key };
          case (_) null;
        };
      };

      started = func(): Bool {
        started;
      };

      finished = func(): Bool {
        started and (iterIndex == front or iterIndex == back);
      };

      reset = func(): Iter<K> {
        started := false;

        iterIndex := back;

        iter;
      };
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func keysFrom<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: ?K): Iter<K> {
    let dataOpt = map[DATA];

    let capacity = switch (dataOpt) { case (?data) nat32(data.0.size()); case (_) 0:Nat32 };
    let front = switch (dataOpt) { case (?data) data.2[FRONT]; case (_) 0:Nat32 };
    let back = switch (dataOpt) { case (?data) data.2[BACK]; case (_) 0:Nat32 };
    let keys = switch (dataOpt) { case (?data) data.0; case (_) [var]:[var ?K] };
    let indexes = switch (dataOpt) { case (?data) data.1; case (_) [var]:[var Nat] };

    var index = switch (keyParam) { case (?someKey) indexes[nat(hashUtils.0(someKey) % capacity +% capacity)]; case (_) NULL };

    loop if ((
      index == NULL
    ) or (
      hashUtils.1(
        switch (keys[index]) { case (?key) key; case (_) trap("unreachable") },
        switch (keyParam) { case (?key) key; case (_) trap("unreachable") },
      )
    )) {
      var started = index != NULL;

      var iterIndex = if (index != NULL) nat32(index) else front;

      return let iter = {
        prev = func(): ?K {
          started := true;

          switch (dataOpt) {
            case (?data) loop {
              iterIndex := if (iterIndex == front) (back -% 1) % capacity else (iterIndex -% 1) % capacity;

              switch (data.0[nat(iterIndex)]) {
                case (null) if (iterIndex == front) return null;
                case (key) return key;
              };
            };

            case (_) null;
          };
        };

        next = func(): ?K {
          started := true;

          switch (dataOpt) {
            case (?data) loop {
              iterIndex := if (iterIndex == back) (front +% 1) % capacity else (iterIndex +% 1) % capacity;

              switch (data.0[nat(iterIndex)]) {
                case (null) if (iterIndex == back) return null;
                case (key) return key;
              };
            };

            case (_) null;
          };
        };

        peekPrev = func(): ?K {
          var newIndex = iterIndex;

          switch (dataOpt) {
            case (?data) loop {
              newIndex := if (newIndex == front) (back -% 1) % capacity else (newIndex -% 1) % capacity;

              switch (data.0[nat(newIndex)]) {
                case (null) if (newIndex == front) return null;
                case (key) return key;
              };
            };

            case (_) null;
          };
        };

        peekNext = func(): ?K {
          var newIndex = iterIndex;

          switch (dataOpt) {
            case (?data) loop {
              newIndex := if (newIndex == back) (front +% 1) % capacity else (newIndex +% 1) % capacity;

              switch (data.0[nat(newIndex)]) {
                case (null) if (newIndex == back) return null;
                case (key) return key;
              };
            };

            case (_) null;
          };
        };

        movePrev = func(): Iter<K> {
          started := true;

          switch (dataOpt) {
            case (?data) loop {
              iterIndex := if (iterIndex == front) (back -% 1) % capacity else (iterIndex -% 1) % capacity;

              switch (data.0[nat(iterIndex)]) {
                case (null) if (iterIndex == front) return iter;
                case (_) return iter;
              };
            };

            case (_) iter;
          };
        };

        moveNext = func(): Iter<K> {
          started := true;

          switch (dataOpt) {
            case (?data) loop {
              iterIndex := if (iterIndex == back) (front +% 1) % capacity else (iterIndex +% 1) % capacity;

              switch (data.0[nat(iterIndex)]) {
                case (null) if (iterIndex == back) return iter;
                case (_) return iter;
              };
            };

            case (_) iter;
          };
        };

        current = func(): ?K {
          switch (dataOpt) {
            case (?data) switch (data.0[nat(iterIndex)]) { case (null) null; case (key) key };
            case (_) null;
          };
        };

        started = func(): Bool {
          started;
        };

        finished = func(): Bool {
          started and (iterIndex == front or iterIndex == back);
        };

        reset = func(): Iter<K> {
          started := false;

          iterIndex := front;

          iter;
        };
      };
    } else {
      index := indexes[index];
    };
  };

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  public func keysFromDesc<K>(map: Set<K>, hashUtils: HashUtils<K>, keyParam: ?K): Iter<K> {
    let dataOpt = map[DATA];

    let capacity = switch (dataOpt) { case (?data) nat32(data.0.size()); case (_) 0:Nat32 };
    let front = switch (dataOpt) { case (?data) data.2[FRONT]; case (_) 0:Nat32 };
    let back = switch (dataOpt) { case (?data) data.2[BACK]; case (_) 0:Nat32 };
    let keys = switch (dataOpt) { case (?data) data.0; case (_) [var]:[var ?K] };
    let indexes = switch (dataOpt) { case (?data) data.1; case (_) [var]:[var Nat] };

    var index = switch (keyParam) { case (?someKey) indexes[nat(hashUtils.0(someKey) % capacity +% capacity)]; case (_) NULL };

    loop if ((
      index == NULL
    ) or (
      hashUtils.1(
        switch (keys[index]) { case (?key) key; case (_) trap("unreachable") },
        switch (keyParam) { case (?key) key; case (_) trap("unreachable") },
      )
    )) {
      var started = index != NULL;

      var iterIndex = if (index != NULL) nat32(index) else front;

      return let iter = {
        prev = func(): ?K {
          started := true;

          switch (dataOpt) {
            case (?data) loop {
              iterIndex := if (iterIndex == back) (front +% 1) % capacity else (iterIndex +% 1) % capacity;

              switch (data.0[nat(iterIndex)]) {
                case (null) if (iterIndex == back) return null;
                case (key) return key;
              };
            };

            case (_) null;
          };
        };

        next = func(): ?K {
          started := true;

          switch (dataOpt) {
            case (?data) loop {
              iterIndex := if (iterIndex == front) (back -% 1) % capacity else (iterIndex -% 1) % capacity;

              switch (data.0[nat(iterIndex)]) {
                case (null) if (iterIndex == front) return null;
                case (key) return key;
              };
            };

            case (_) null;
          };
        };

        peekPrev = func(): ?K {
          var newIndex = iterIndex;

          switch (dataOpt) {
            case (?data) loop {
              newIndex := if (newIndex == back) (front +% 1) % capacity else (newIndex +% 1) % capacity;

              switch (data.0[nat(newIndex)]) {
                case (null) if (newIndex == back) return null;
                case (key) return key;
              };
            };

            case (_) null;
          };
        };

        peekNext = func(): ?K {
          var newIndex = iterIndex;

          switch (dataOpt) {
            case (?data) loop {
              newIndex := if (newIndex == front) (back -% 1) % capacity else (newIndex -% 1) % capacity;

              switch (data.0[nat(newIndex)]) {
                case (null) if (newIndex == front) return null;
                case (key) return key;
              };
            };

            case (_) null;
          };
        };

        movePrev = func(): Iter<K> {
          started := true;

          switch (dataOpt) {
            case (?data) loop {
              iterIndex := if (iterIndex == back) (front +% 1) % capacity else (iterIndex +% 1) % capacity;

              switch (data.0[nat(iterIndex)]) {
                case (null) if (iterIndex == back) return iter;
                case (_) return iter;
              };
            };

            case (_) iter;
          };
        };

        moveNext = func(): Iter<K> {
          started := true;

          switch (dataOpt) {
            case (?data) loop {
              iterIndex := if (iterIndex == front) (back -% 1) % capacity else (iterIndex -% 1) % capacity;

              switch (data.0[nat(iterIndex)]) {
                case (null) if (iterIndex == front) return iter;
                case (_) return iter;
              };
            };

            case (_) iter;
          };
        };

        current = func(): ?K {
          switch (dataOpt) {
            case (?data) switch (data.0[nat(iterIndex)]) { case (null) null; case (key) key };
            case (_) null;
          };
        };

        started = func(): Bool {
          started;
        };

        finished = func(): Bool {
          started and (iterIndex == front or iterIndex == back);
        };

        reset = func(): Iter<K> {
          started := false;

          iterIndex := back;

          iter;
        };
      };
    } else {
      index := indexes[index];
    };
  };
};
