/// Red-Black Trees

import Debug "mo:base/Debug";
import I "mo:base/Iter";
import List "mo:base/List";
import Nat "mo:base/Nat";
import O "mo:base/Order";
import Buffer "mo:base/Buffer";
import Stack "mo:base/Stack";
import Option "mo:base/Option";

module {

  /// Node color: red or black.
  public type Color = { #R; #B };

  /// Ordered, (red-black) tree of entries.
  public type Tree<K, V> = {
    #node : (Color, Tree<K, V>, (K, ?V), Tree<K, V>);
    #leaf;
  };

  /// Initializes an empty Red-Black Tree of type <K, V>
  /// Returns this empty Red-Black Tree
  public func init<K, V>(): Tree<K, V> {
    (#leaf : Tree<K, V>);
  };

  /// Tree as sharable data.
  ///
  /// Get non-OO, purely-functional representation:
  /// for drawing, pretty-printing and non-OO contexts
  /// (e.g., async args and results):
  public func share<K,V>(tree: Tree<K, V>) : Tree<K, V> {
    tree
  };

  /// Returns the value associated with a given key.
  public func get<K, V>(tree: Tree<K, V>, compareTo: (K, K) -> O.Order, k : K) : ?V {
    getRec(k, compareTo, tree);
  };

  /// Replace the value associated with a given key.
  /// Returns the replaced value (if exists) and the new tree
  public func replace<K, V>(tree: Tree<K, V>, compareTo: (K, K) -> O.Order, k : K, v : V) : (?V, Tree<K, V>) {
    insertRoot(k, compareTo, v, tree);
  };

  /// Put an entry: A value associated with a given key.
  /// Returns the new tree
  public func put<K, V>(tree: Tree<K, V>, compareTo: (K, K) -> O.Order, k : K, v : V): Tree<K,V> {
    let (res, t) = insertRoot(k, compareTo, v, tree);
    t
  };

  /// Delete the entry associated with a given key.
  /// Returns the new tree
  public func delete<K, V>(tree: Tree<K, V>, compareTo: (K, K) -> O.Order, k : K): Tree<K, V> {
    let (res, t) = removeRec(k, compareTo, tree);
    t
  };

  /// Remove the entry associated with a given key.
  /// Returns the removed entry (if exists) and the new tree
  public func remove<K, V>(tree: Tree<K, V>, compareTo: (K, K) -> O.Order, k : K) : (?V, Tree<K,V>) {
    removeRec(k, compareTo, tree);
  };

  /// Apply an update function to an entry associated with a given key, or create that entry if it does not yet exist.
  /// Returns the old value and the new tree
  public func update<K, V>(tree: Tree<K, V>, compareTo: (K, K) -> O.Order, k: K, updateFunction: (?V) -> V): (?V, Tree<K, V>) {
    updateRoot(tree, compareTo, k, updateFunction);
  };

  /// Splits a Red-Black Tree (t) into two Red-Black Trees (t1, t2). All of the nodes' keys in the first Red-Black Tree 
  /// returned will be less than the nodes' keys in the second Red-Black Tree returned.
  ///
  /// Note: this implementation mutates the tree passed in as it re-inserts the root node key values into the left child
  ///
  /// Implementation: Splits a Red-Black tree into it's left child and right child trees, then
  /// inserts the root node into the tree of the left child. Returns the right child Red-Black Tree,
  /// and the result of reinserting the root node into the left child Red-Black Tree. Transforms the roots
  /// of both trees it returns to black to protect the tree's invariants
  ///
  /// Edge cases
  /// 1. If the root is a #leaf (empty), returns two #leaf (empty) Red-Black Trees
  /// 2. If the tree contains a single #node at the root (both children are leaves, returns that node and a #leaf 
  /// 3. If the tree contains a #node at the root and one child is a #leaf returns the a new tree with the root key and 
  /// node's key and value, and the child which is a #node 
  /// 4. If the root node was deleted (with a null value), just returns the left and right child Red-Black Trees. For an
  /// explanation of how functional Red-Black Trees handle deletion, see https://matt.might.net/papers/germane2014deletion.pdf
  /// 5. If an invalid Red-Black Tree in terms of being unbalanced is passed to this function, the split will return null.
  /// This is done instead of splitting the tree, in order to prevent a loss of data in the Red-Black Tree 
  public func split<K, V>(tree: Tree<K, V>, compareTo: (K, K) -> O.Order): ?(Tree<K, V>, Tree<K, V>) {
    switch(tree) {
      // root is leaf -> return two empty Red-Black Trees
      case (#leaf) { ?(#leaf, #leaf) };
      // only single node in the Red-Black Tree at the root -> return that node and a leaf
      case (#node(_, #leaf, kvs, #leaf)) { 
        switch(kvs) {
          // root node was deleted -> return two leaf nodes
          case (k, null) { ?(#leaf, #leaf) };
          // root node was not deleted -> return that node and a leaf
          case (k, ?v) { ?(tree, #leaf) }
        };
      };
      // right child is a #node, but left child is a #leaf -> return right child and new tree with root node's key and value
      // Note: If the tree is invalid and the right child contains any #node children (unbalanced) the result of split will also be unbalanced
      case (#node(_, #leaf, kvs, #node(_, #leaf, (rk, rv), #leaf))) { 
        switch(kvs) {
          // root node was deleted -> return the right child and a leaf
          case (k, null) {
            ?(#node(#B, #leaf, (rk, rv), #leaf), #leaf)
          };
          // root node was not deleted -> return right child and new tree with root node's key and value
          case (k, ?v) {
            ?(
              #node(#B, #leaf, (k, ?v), #leaf), 
              #node(#B, #leaf, (rk, rv), #leaf), 
            )
          }
        }
      };
      // left child is a #node, but right child is a #leaf -> return left child and new tree with root node's key and value
      // In this case, to preserve ordering of the returned trees put the new left child tree first and the new tree with root key and value second
      // Note: If the tree is invalid and the left child contains any #node children (unbalanced) the result of split will also be unbalanced
      case (#node(_, #node(_, #leaf, (lk, lv), #leaf), kvs, #leaf)) { 
        switch(kvs) {
          // root node was deleted -> return the left child and a leaf
          case (k, null) {
            ?(#node(#B, #leaf, (lk, lv), #leaf), #leaf)
          };
          // root node was not deleted -> return left child and new tree with root node's key and value
          case (k, ?v) {
            ?(
              #node(#B, #leaf, (lk, lv), #leaf), 
              #node(#B, #leaf, (k, ?v), #leaf), 
            )
          }
        }
      };
      // node has both left and right #node children
      case (#node(
        _,
        #node(_, ll, (lk, lv), lr),
        kvs,
        #node(_, rl, (rk, rv), rr)
      )) {
        switch(kvs) {
          // root node was deleted -> ignore root reinsertion and just return left and right children
          case (k, null) {
            ?(
              #node(#B, ll, (lk, lv), lr),
              #node(#B, rl, (rk, rv), rr)
            )
          };
          // root node was not deleted -> return left child with the the root key and value inserted, and the right child
          case (k, ?v) {
            ?(
              put<K, V>(#node(#B, ll, (lk, lv), lr), compareTo, k, v),
              #node(#B, rl, (rk, rv), rr)
            )
          }
        }
      };
      // traps on the following cases, which should never happen, as this would invalidate the variants of the Red-Black Tree to begin with
      // and would indicate an unbalanced Red-Black Tree
      // unbalanced on the right side of the tree: case (#node(_, #leaf, (k, v), #node(_, #node(...), (rk, rv), #node(...)))) { 
      // unbalanced on the left side of the tree:  case (#node(_, #node(_, #node(...), (lk, lv), #node(...)), (k, v), #leaf)) { 
      case _ {
        Debug.trap("split() was passed an invalid and unbalanced Red-Black Tree")
      }
    }
  };

  /// An iterator for the key-value entries of the map, in ascending key order.
  ///
  /// iterator is persistent, like the tree itself
  public func entries<K, V>(tree: Tree<K, V>) : I.Iter<(K, V)> { iter(tree, #fwd) };

  /// An iterator for the key-value entries of the map, in descending key order.
  ///
  /// iterator is persistent, like the tree itself
  public func entriesRev<K, V>(tree: Tree<K, V>) : I.Iter<(K, V)> { iter(tree, #bwd) };

  type IterRep<K, V> = List.List<{ #tr:Tree<K, V>; #kv:(K, ?V) }>;

  /// Direction of iteration (forwards or backwards)
  public type Direction = { #fwd; #bwd };

  /// An iterator for the entries of the map, in ascending (`#fwd`) or descending (`#bwd`) order.
  public func iter<K, V>(t : Tree<K, V>, dir : Direction) : I.Iter<(K, V)> {
    object {
      var trees : IterRep<K, V> = ?(#tr(t), null);
      public func next() : ?(K, V) {
        switch (dir, trees) {
          case (_, null) { null };
          case (_, ?(#tr(#leaf), ts)){
            trees := ts;
            next()
          };
          case (_, ?(#kv(kv), ts)) {
            trees := ts;
            switch (kv.1) {
              case null { next() };
              case (?v) { ?(kv.0, v) }
            }
          };
          case (#fwd, ?(#tr(#node(_, l, kv, r)), ts)) {
            trees := ?(#tr(l), ?(#kv(kv), ?(#tr(r), ts)));
            next()
          };
          case (#bwd, ?(#tr(#node(_, l, kv, r)), ts)) {
            trees := ?(#tr(r), ?(#kv(kv), ?(#tr(l), ts)));
            next()
          };
        }
      };
    }
  };

  public type ScanLimitResult<K, V> = {
    results: [(K, V)];
    nextKey: ?K;
  };

  /// Performs a in-order scan of the Red-Black Tree between the provided key bounds, returning a number of matching entries in the direction specified (forwards/backwards) limited by the limit parameter specified in an array formatted as (K, V) for each entry
  public func scanLimit<K, V>(t: Tree<K, V>, compareTo: (K, K) -> O.Order, lowerBound: K, upperBound: K, dir: Direction, limit: Nat): ScanLimitResult<K, V> {
    switch(compareTo(lowerBound, upperBound)) {
      // return empty array if lower bound is greater than upper bound      
      // TODO: consider returning an error in this case?
      case (#greater) {{ results = []; nextKey = null }};
      // return the single entry if exists if the lower and upper bounds are equivalent
      case (#equal) { 
        switch(get<K, V>(t, compareTo, lowerBound)) {
          case null {{ results = []; nextKey = null }};
          case (?value) {{ results = [(lowerBound, value)]; nextKey = null }};
        }
      };
      case (#less) { 
        let (results, nextKey) = iterScanLimit<K, V>(t, compareTo, lowerBound, upperBound, dir, limit);
        { results = results; nextKey = nextKey };
      }
    }
  };

  type RBTreeNode<K, V> = { #node: (Color, Tree<K, V>, (K, ?V), Tree<K, V>) };

  func iterScanLimit<K, V>(t: Tree<K, V>, compareTo: (K, K) -> O.Order, lowerBound: K, upperBound: K, dir: Direction, limit: Nat): ([(K, V)], ?K) {
    var remaining = limit + 1;
    let resultBuffer: Buffer.Buffer<(K, V)> = Buffer.Buffer(0);
    var nextKey: ?K = null;
    var nodeStack = Stack.Stack<RBTreeNode<K, V>>();
    var currentNode = t;

    while (remaining > 0) {
      // this loop finds the next non-deleted node in order, adds it to the stack if that node exists, and then exits the loop 
      // otherwise a leaf is hit and the loop is exited 
      label l loop {
        switch(currentNode) {
          case (#node(c, l, (k, v), r)) {
            // compare the node key to see if it is within the key bounds, if so, add it to the nodeStack
            switch(dir, compareTo(k, lowerBound), compareTo(k, upperBound)) {
              // value is greater than lower and upper bounds, traverse left child regardless of direction order
              case (_, #greater, #greater) {
                currentNode := l;
              };
              // if ascending order and value is greater than lower and equal to upper, push node to stack and go left
              case (#fwd, #greater, #equal) {
                nodeStack.push(#node(c, l, (k, v), r));
                currentNode := l 
              };
              // if descending order and value is greater than lower and equal to upper
              case (#bwd, #greater, #equal) {
                // if value is not null, push to stack and break as can not go any farther to the right 
                if (Option.isSome(v)) {
                  nodeStack.push(#node(c, l, (k, v), r));
                  break l;
                // otherwise go left, as the current node was deleted and can still go to the left 
                } else {
                  currentNode := l;
                }
              };
              // value is greater than lower and less than upper, push node to stack, then traverse left or right child depending on direction order
              case (#fwd, #greater, #less) {
                nodeStack.push(#node(c, l, (k, v), r));
                currentNode := l; 
              };
              case (#bwd, #greater, #less) {
                nodeStack.push(#node(c, l, (k, v), r));
                currentNode := r;
              };
              // if ascending order and value is equal to lower and less than upper
              case (#fwd, #equal, #less) {
                // if value is not null, push to stack and break as can not go any farther to the left 
                if (Option.isSome(v)) {
                  nodeStack.push(#node(c, l, (k, v), r));
                  break l;
                // otherwise go right, as the current node was deleted and can still go to the right
                } else {
                  currentNode := r;
                }
              };
              // if descending order and value is equal to lower and less than upper
              case (#bwd, #equal, #less) {
                nodeStack.push(#node(c, l, (k, v), r));
                currentNode := r;
              };
              // if value is less than lower and upper bounds, traverse right child regardless of direction order
              case (_, #less, #less) {
                currentNode := r;
              };
              // This should never be hit as the cases where lower bound >= upper bound are covered in the main scan function
              case _ { 
                break l;
              };
            }
          };
          // have already hit the next node in order, exit the loop
          case (#leaf) { 
            break l;
          };
        }
      };

      // pop the next node from the stack
      switch(nodeStack.pop()) {
        // if the stack is empty, no more nodes within the bounds exist, so can return
        case null { 
          return (resultBuffer.toArray(), nextKey);
        };
        case (?(#node(_, l, (k, v), r))) {
          switch(v) {
            // if the popped node's value is null (was deleted), skip it and traverse to the right child
            case null {};
            // if the popped node's value is present, prepend it to the entries list and traverse to the right child
            case (?value) {
              if (remaining == 1) {
                nextKey := ?k;
              } else {
                resultBuffer.add((k, value));
              };
              remaining -= 1;
            }
          };
          // traverse to the left or right child depending on the direction order
          currentNode := switch(dir) {
            case (#fwd) { r };
            case (#bwd) { l };
          };
        }
      }
    };

    return (resultBuffer.toArray(), nextKey);
  };

  /// Recursive helper for removing the value associated with a given key.
  func removeRec<K, V>(k : K, compareTo : (K, K) -> O.Order, t : Tree<K, V>)
    : (?V, Tree<K, V>) {
    switch t {
      case (#leaf) { (null, #leaf) };
      case (#node(c, l, kv, r)) {
        switch (compareTo(k, kv.0)) {
          case (#less) {
            let (vo, l2) = removeRec(k, compareTo, l);
            (vo, #node(c, l2, kv, r))
          };
          case (#equal) {
            (kv.1, #node(c, l, (k, null), r))
          };
          case (#greater) {
            let (vo, r2) = removeRec(k, compareTo, r);
            (vo, #node(c, l, kv, r2))
          };
        }
      }
    }
  };

  func updateRoot<K, V>(tree: Tree<K, V>, compareTo: (K, K) -> O.Order, k : K, updateFn: (?V) -> V): (?V, Tree<K, V>) {
    switch (updateRec(tree, compareTo, k, updateFn)) {
      case (_, #leaf) { assert false; loop { } };
      case (vo, #node(_, l, kv, r)) { (vo, #node(#B, l, kv, r)) };
    }
  };

  // Recursive helper for inserting or updating a value associated with a given key according to the updateFn provided
  func updateRec<K, V>(tree: Tree<K, V>, compareTo: (K, K) -> O.Order, kToUpdate : K, updateFn: (?V) -> V): (?V, Tree<K, V>) {
    switch tree {
      case (#leaf) { (null, #node(#R, #leaf, (kToUpdate, ?updateFn(null)), #leaf)) };
      case (#node(c, l, (k, v), r)) {
        switch (compareTo(kToUpdate, k)) {
          case (#less) {
            let (vo, l2) = updateRec(l, compareTo, kToUpdate, updateFn);
            (vo, bal<K, V>(c, l2, (k, v), r))
          };
          case (#equal) {
            (v, #node(c, l, (k, ?updateFn(v)), r))
          };
          case (#greater) {
            let (vo, r2) = updateRec(r, compareTo, kToUpdate, updateFn);
            (vo, bal<K, V>(c, l, (k, v), r2))
          };
        }
      }
    }
  };

  public func bal<K, V>(color : Color, lt : Tree<K, V>, kv : (K, ?V), rt : Tree<K, V>) : Tree<K, V> {
    // thank you, algebraic pattern matching!
    // following notes from [Ravi Chugh](https://www.classes.cs.uchicago.edu/archive/2019/spring/22300-1/lectures/RedBlackTrees/index.html)
    switch (color, lt, kv, rt) {
      case (#B, #node(#R, #node(#R, a, k, b), v, c), z, d) {
        #node(#R, #node(#B, a, k, b), v, #node(#B, c, z, d))
      };
      case (#B, #node(#R, a, k, #node(#R, b, v, c)), z, d) {
        #node(#R, #node(#B, a, k, b), v, #node(#B, c, z, d))
      };
      case (#B, a, k, #node(#R, #node(#R, b, v, c), z, d)) {
        #node(#R, #node(#B, a, k, b), v, #node(#B, c, z, d))
      };
      case (#B, a, k, #node(#R, b, v, #node(#R, c, z, d))) {
        #node(#R, #node(#B, a, k, b), v, #node(#B, c, z, d))
      };
      case _ { #node(color, lt, kv, rt) };
    }
  };

  func insertRoot<K, V>(k : K, compareTo : (K, K) -> O.Order, v : V, t : Tree<K, V>)
    : (?V, Tree<K, V>) {
    switch (insertRec(k, compareTo, v, t)) {
      case (_, #leaf) { assert false; loop { } };
      case (vo, #node(_, l, kv, r)) { (vo, #node(#B, l, kv, r)) };
    }
  };

  func insertRec<K, V>(k : K, compareTo : (K, K) -> O.Order, v : V, t : Tree<K, V>)
    : (?V, Tree<K, V>) {
    switch t {
      case (#leaf) { (null, #node(#R, #leaf, (k, ?v), #leaf)) };
      case (#node(c, l, kv, r)) {
        switch (compareTo(k, kv.0)) {
          case (#less) {
            let (vo, l2) = insertRec(k, compareTo, v, l);
            (vo, bal(c, l2, kv, r))
          };
          case (#equal) {
            (kv.1, #node(c, l, (k, ?v), r))
          };
          case (#greater) {
            let (vo, r2) = insertRec(k, compareTo, v, r);
            (vo, bal(c, l, kv, r2))
          };
        }
      }
    }
  };

  func getRec<K, V>(k : K, compareTo : (K, K) -> O.Order, t : Tree<K, V>) : ?V {
    switch t {
      case (#leaf) { null };
      case (#node(c, l, kv, r)) {
        switch (compareTo(k, kv.0)) {
          case (#less) { getRec(k, compareTo, l) };
          case (#equal) { kv.1 };
          case (#greater) { getRec(k, compareTo, r) };
        }
      };
    }
  };

  func height<K, V>(t : Tree<K, V>) : Nat {
    switch t {
      case (#leaf) { 0 };
      case (#node(_, l, _, r)) {
        Nat.max(height(l), height(r)) + 1
      }
    }
  };

  /// The size of the tree as the number of key-value entries.
  public func size<K, V>(t : Tree<K, V>) : Nat {
    switch t {
      case (#leaf) { 0 };
      case (#node(_, l, kv, r)) {
        size(l) + size(r) + (switch (kv.1) { case null 0; case _ 1 });
      };
    }
  };

  func optionValueEquals<V>(valueEquals: (V, V) -> Bool, v1: ?V, v2: ?V): Bool {
    switch (v1, v2) {
      case (null, null) { true };
      case (?v1, ?v2) { valueEquals(v1, v2) };
      case _ { false }
    }
  };

  
  /// For most purposes, one should prefer this equalIgnoreDeleted function as opposed to equalIncludeDeleted.
  ///
  /// Functional Red-Black trees do not have efficient operations for deleting a red black tree. For reference, see 
  /// https://matt.might.net/papers/germane2014deletion.pdf.
  ///
  /// Therefore, "deleting" a node is represented as setting the value to null for a specific key.
  ///
  /// The equalIgnoreDeleted function returns a boolean value indicating if two Red-Black Trees are equivalent, ignoring node coloring
  /// and focusing solely on node location and key value equality as per the keyEquals and valueEquals methods supplied.
  ///
  /// Note the difference betweenn equalIgnoreDeleted and equalIncludeDeleted in the result of the last line in the following example.
  ///
  /// Example:
  ///
  /// ```motoko
  /// var t1 = RBT.init<Nat, Text>();
  /// var t2 = RBT.init<Nat, Text>();
  /// t1 := RBT.put<Nat, Text>(t1, Nat.compare, 35, "john");
  /// t2 := RBT.put<Nat, Text>(t1, Nat.compare, 35, "john");
  /// RBT.equalIgnoreDeleted<Nat, Text>(t1, t2, Nat.equal, Text.equal); // true
  /// RBT.equalIncludeDeleted<Nat, Text>(t1, t2, Nat.equal, Text.equal); // true
  ///
  /// t1 := RBT.put<Nat, Text>(t1, Nat.compare, 31, "alice");
  /// t1 := RBT.delete<Nat, Text>(t1, Nat.compare, 31);
  /// RBT.equalIgnoreDeleted<Nat, Text>(t1, t2, Nat.equal, Text.equal); // true
  /// RBT.equalIncludeDeleted<Nat, Text>(t1, t2, Nat.equal, Text.equal); // false 
  /// ```
  public func equalIgnoreDeleted<K, V>(t1: Tree<K, V>, t2: Tree<K, V>, keyEquals: (K, K) -> Bool, valueEquals: (V, V) -> Bool): Bool {
    let e1 = entries(t1);
    let e2 = entries(t2);
    return loop {
      switch(e1.next(), e2.next()) {
        case (null, null) { return true };
        case (?(k1, v1), ?(k2, v2)) {
          if ( not( 
            keyEquals(k1, k2) and optionValueEquals<V>(valueEquals, ?v1, ?v2)
          )) { return false }
        };
        case _ { return false }
      }
    }
  };

  /// Functional Red-Black trees do not have efficient operations for deleting a red black tree. Therefore, "deleting" a node is
  /// represented as setting the value to null for a specific key.
  ///
  /// Returns a boolean value indicating if two Red-Black Trees are equivalent, including node coloring and deleted "null" nodes,
  /// as well as node location and key value equality as per the keyEquals and valueEquals methods supplied
  ///
  /// Example:
  ///
  /// ```motoko
  /// var t1 = RBT.init<Nat, Text>();
  /// var t2 = RBT.init<Nat, Text>();
  /// t1 := RBT.put<Nat, Text>(t1, Nat.compare, 35, "john");
  /// t2 := RBT.put<Nat, Text>(t1, Nat.compare, 35, "john");
  /// RBT.equalIncludeDeleted<Nat, Text>(t1, t2, Nat.equal, Text.equal); // true
  ///
  /// t1 := RBT.put<Nat, Text>(t1, Nat.compare, 31, "alice");
  /// t1 := RBT.delete<Nat, Text>(t1, Nat.compare, 31);
  /// RBT.equalIncludeDeleted<Nat, Text>(t1, t2, Nat.equal, Text.equal); // false 
  /// ```
  ///
  public func equalIncludeDeleted<K, V>(t1: Tree<K, V>, t2: Tree<K, V>, keyEquals: (K, K) -> Bool, valueEquals: (V, V) -> Bool): Bool {
    switch(t1, t2) {
      case (#leaf, #leaf) { true };
      case (#node(c1, l1, (k1, ?v1), r1), #node(c2, l2, (k2, ?v2), r2)) {
        if (keyEquals(k1, k2) and optionValueEquals<V>(valueEquals, ?v1, ?v2)) {
          equalIncludeDeleted(l1, l2, keyEquals, valueEquals) and equalIncludeDeleted(r1, r2, keyEquals, valueEquals);
        } else {
          false
        }
      };
      case _ { false };
    }
  };
}