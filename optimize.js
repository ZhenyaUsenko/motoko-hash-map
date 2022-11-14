let fs = require('fs')

let map = fs.readFileSync('./src/Map/Map.mo').toString()
let set = fs.readFileSync('./src/Set/Set.mo').toString()
let utils = fs.readFileSync('./src/utils.mo').toString()

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

let getBody = (string, startPosition, options = {}) => {
  let bodyStarted = options.bodyStarted ?? false
  let bracesCount = options.bracesCount ?? (bodyStarted ? 1 : 0)
  let delimLeft = options.delimLeft ?? '{'
  let delimRight = options.delimRight ?? '}'

  let firstIndex = typeof startPosition === 'number' ? startPosition : string.match(startPosition).index
  let lastIndex = firstIndex

  while (lastIndex < string.length && (!bodyStarted || bracesCount !== 0)) {
    let char = string.charAt(lastIndex)

    if (char === ';') bodyStarted = true

    if (char === delimRight) bracesCount--

    if (char === delimLeft) {
      bracesCount++

      if (!bodyStarted && options.bodyOnly) firstIndex = lastIndex + 1

      bodyStarted = true
    }

    lastIndex++
  }

  return string.slice(firstIndex, options.bodyOnly ? lastIndex - 1 : lastIndex)
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

let adjustIndent = (body, offset) => body.replace(/(?<=(^|\n)) +/g, space => ' '.repeat(space.length + offset))

let getIndent = (offset, indent = '') => ' '.repeat(indent.length + offset)

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

let utilsBody = getBody(utils, /\bmodule\b/).replace(/(\S) {2,}/g, '$1 ')

let utilsTypes = getBody(utilsBody, / +public type HashUtils/, { delimLeft: '(', delimRight: ')' })

let utilsMethods = getBody(utilsBody, / +public func/, { bodyStarted: true, bodyOnly: true })

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

let structs = [[map, 'map', './src/Map/optimized.mo'], [set, 'set', './src/Set/optimized.mo']]

for (let [struct, type, path] of structs) {
  let optimizedStruct = struct

  optimizedStruct = optimizedStruct.replace(/(\S) {2,}/g, '$1 ')

  optimizedStruct = optimizedStruct.replace(/};\s*$/, `\n${getIndent(2)}${'/'.repeat(148)}\n\n${utilsMethods}\n};\n`)

  optimizedStruct = optimizedStruct.replace(/ +public type HashUtils[^;]+;/, `${utilsTypes};`)

  optimizedStruct = optimizedStruct.replace(/import Utils[^;]+;/, '')

  optimizedStruct = optimizedStruct.replace(/public let {[^}]+} = Utils;/g, '')

  let structWithUtils = optimizedStruct

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  let methods = {}

  let match = null

  let methodRegExp = /func (\w+)/g

  while ((match = methodRegExp.exec(structWithUtils)) != null) {
    let { 1: methodName } = match

    let methodBody = getBody(structWithUtils, match.index)
    let methodBodyOnly = getBody(methodBody, 0, { bodyOnly: true })

    let newMethodBody = methodBody

    methods[methodName] = methodBodyOnly

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (/^(isSome|unwrap|tempHashUtils)$|Helper$/.test(methodName)) {
      optimizedStruct = optimizedStruct.replace(methodBody, '')

      continue
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (/^(get|has)$/.test(methodName)) {
      let isHas = methodName.startsWith('has')

      newMethodBody = newMethodBody.replace(methodBodyOnly, methods.getHelper)

      if (isHas && type === 'map') {
        newMethodBody = newMethodBody.replace(/return (null|value)/g, (item) => `return ${item.endsWith('value') ? 'true' : 'false'}`)
      }
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (/^(set|add|put|update)(Move)?(Front|Before|After)?$/.test(methodName)) {
      let isFront = methodName.endsWith('Front') || methodName.endsWith('After')
      let isSet = methodName.startsWith('set') || methodName.startsWith('add')
      let isUpdate = methodName.startsWith('update')
      let isBefore = methodName.endsWith('Before') || methodName.endsWith('After')
      let isMove = isBefore || methodName.endsWith('Move') || methodName.endsWith('MoveFront')

      newMethodBody = newMethodBody.replace(methodBodyOnly, methods.putHelper)

      newMethodBody = newMethodBody.replace(/newEntry[^;]+:= deqPrev;/, '')

      newMethodBody = newMethodBody.replace(/placeEntry, placeEntry]/, isFront ? 'placeEntry, deqPrev]' : 'deqPrev, placeEntry]')

      if (!isUpdate) newMethodBody = newMethodBody.replace(/let newValue =[^;]+;/g, '')

      if (type === 'map' && (!isMove || isUpdate)) {
        newMethodBody = newMethodBody.replace(/= switch \(newValue\) {[^}]+}/, '= (key, ?newValue, hash, links)')

        let switchBody = getBody(newMethodBody, /switch \(newValue\)/)
        let caseBody = getBody(newMethodBody, /case \(_\)/, { bodyOnly: true })

        newMethodBody = newMethodBody.replace(switchBody, `${adjustIndent(caseBody, -4).replace(/newValue/g, '?newValue')}`)
      }

      if (!isUpdate) newMethodBody = newMethodBody.replace(/newValue/g, 'valueParam')

      newMethodBody = newMethodBody.replace(/if \(isUpdate\) \??([^;]+) else ([^;]+)/g, isUpdate ? '$1' : '$2')

      if (!isBefore) {
        let placeLogicBody = getBody(newMethodBody, /let placeHashParam = if/, { bodyStarted: true, bodyOnly: true })
        let mainLoopBody = getBody(newMethodBody, /if \(placeHash == NULL_HASH/, { bodyOnly: true })

        newMethodBody = newMethodBody.replace(placeLogicBody, `loop {${adjustIndent(mainLoopBody, -2)}};\n${getIndent(2)}`)

        newMethodBody = newMethodBody.replace(/usePlaceKey or /, '')

        newMethodBody = newMethodBody.replace(/placeEntry/g, 'edgeEntry')

        newMethodBody = newMethodBody.replace(/placeLinks/g, `edgeEntry.${type === 'map' ? '3' : '2'}`)
      }

      let moveMatch = newMethodBody.match(/if \(moveExisting\)/)

      let moveBody = getBody(newMethodBody, moveMatch.index)
      let moveElseBody = type === 'map' ? getBody(newMethodBody, moveMatch.index + moveBody.length) : ''

      if (isMove) {
        let moveBodyOnly = getBody(moveBody, 0, { bodyOnly: true })

        newMethodBody = newMethodBody.replace(moveBody, adjustIndent(moveBodyOnly, -2))

        if (type === 'map') newMethodBody = newMethodBody.replace(moveElseBody, '')
      }

      if (!isMove) {
        let moveElseBodyOnly = getBody(moveElseBody, 0, { bodyOnly: true })

        newMethodBody = newMethodBody.replace(moveBody, '')

        if (type === 'map') newMethodBody = newMethodBody.replace(moveElseBody, adjustIndent(moveElseBodyOnly, -2))
      }

      if (isBefore) {
        let conditionalMoveMatch = newMethodBody.match(/if \(usePlaceKey or/)

        let conditionalMoveBody = getBody(newMethodBody, conditionalMoveMatch.index)
        let conditionalMoveBodyOnly = getBody(conditionalMoveBody, 0, { bodyOnly: true })

        if (type === 'map') {
          let conditionalMoveElseBody = getBody(newMethodBody, conditionalMoveMatch.index + conditionalMoveBody.length)

          newMethodBody = newMethodBody.replace(conditionalMoveElseBody, '')
        }

        newMethodBody = newMethodBody.replace(conditionalMoveBody, adjustIndent(conditionalMoveBodyOnly, -2))

        newMethodBody = newMethodBody.replace(/if \(usePlaceKey\) ([^;]+) else[^;]+/g, '$1')
      }

      if (isSet) newMethodBody = newMethodBody.replace(/return[^;]+;/g, 'return;')

      methods[methodName] = getBody(newMethodBody, 0, { bodyOnly: true })
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (/^(remove|delete|pop(Front)?)$/.test(methodName)) {
      let isPop = methodName.startsWith('pop')
      let isDelete = methodName.startsWith('delete')

      newMethodBody = newMethodBody.replace(methodBodyOnly, methods.removeHelper)

      newMethodBody = newMethodBody.replace(/if \(removeLast\) ([^)]+) else (\w+\([^)]+\))/g, isPop ? '$1' : '$2')

      let removeBody = getBody(newMethodBody, /if \(deqLast\./)
      let removeBodyOnly = getBody(removeBody, 0, { bodyOnly: true })

      if (!isPop) {
        newMethodBody = newMethodBody.replace(removeBody, adjustIndent(removeBodyOnly, -2))

        newMethodBody = newMethodBody.replace(/let deqLast =[^;]+;/, '')

        newMethodBody = newMethodBody.replace(/return getResult[^;]+/, 'return value')

        if (type === 'set') {
          newMethodBody = newMethodBody.replace(/return (null|\?key)/g, (item) => `return ${item.endsWith('key') ? 'true' : 'false'}`)
        }
      }

      if (isPop) {
        if (type === 'map') {
          removeBodyOnly = adjustIndent(removeBodyOnly, 2)

          removeBodyOnly = `switch (deqLast.1) {\n${getIndent(6)}case (?someValue) {${removeBodyOnly}};`

          newMethodBody = newMethodBody.replace(removeBody, `${removeBodyOnly}\n\n${getIndent(6)}case (_) null;\n${getIndent(4)}}`)
        }

        newMethodBody = newMethodBody.replace(/let (hashParam|deqNext) =[^;]+;/g, '')

        newMethodBody = newMethodBody.replace(/deqNext/g, 'edgeEntry')

        newMethodBody = newMethodBody.replace(/hashParam/g, type === 'map' ? 'deqLast.2' : 'deqLast.1')

        newMethodBody = newMethodBody.replace(/else if \(hash == NULL_HASH\) {[^}]+}/, '')

        newMethodBody = newMethodBody.replace(/return null/g, 'null')

        newMethodBody = newMethodBody.replace(/return getResult[^;]+/g, 'return ?(key, someValue)')
      }

      let { 1: indent } = newMethodBody.match(/( +)if \(leafLinks/)

      let leafReplaceBody = getBody(newMethodBody, /if \(leafIndex/, { bodyStarted: true, bodyOnly: true })
      let branchLoopBody = getBody(newMethodBody, / +if \(leafLinks/, { bodyStarted: true, bodyOnly: true })
      let branchCheckBody = getBody(newMethodBody, / +if \(leafLinks/, { bodyOnly: true })

      let linksKey = type === 'map' ? '3' : '2'

      let newBranchLoopBody = ''

      let branchReplaceBody = ''

      for (let i = 1; i <= 4; i++) {
        if (i > 1) newBranchLoopBody = `${newBranchLoopBody} else {`

        branchReplaceBody = `${branchReplaceBody}\n${getIndent(4, indent)}leaf.${linksKey}[BRANCH_${i}] := entry.${linksKey}[BRANCH_${i}];`

        let newBranchCheckBody = branchCheckBody.replace(/leafLinks\[BRANCH_1\]/, `branch${i}`)

        newBranchCheckBody = newBranchCheckBody.replace('BRANCH_1', `BRANCH_${i}`)

        newBranchLoopBody = `${newBranchLoopBody}\n${getIndent(2 * i - 2, indent)}let branch${i} = leafLinks[BRANCH_${i}];\n`

        newBranchLoopBody = `${newBranchLoopBody}\n${getIndent(2 * i - 2, indent)}if (branch${i}.${type === 'map' ? '2' : '1'} != NULL_HASH) {`

        newBranchLoopBody = `${newBranchLoopBody}${adjustIndent(newBranchCheckBody, 2 * i - 2)}}`
      }

      leafReplaceBody = leafReplaceBody.replace(/for \(index in leaf[^;]+;/, branchReplaceBody)

      newBranchLoopBody = `${newBranchLoopBody} else {\n${getIndent(8, indent)}${adjustIndent(leafReplaceBody, 6)}};`

      newBranchLoopBody = `${newBranchLoopBody}\n${getIndent(4, indent)}};\n${getIndent(2, indent)}};\n${indent}};\n${getIndent(-2, indent)}`

      newMethodBody = newMethodBody.replace(branchLoopBody, newBranchLoopBody)

      if (isDelete) newMethodBody = newMethodBody.replace(/return[^;]+;/g, 'return;')
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (/^(peek|cycle)(Front)?$/.test(methodName)) {
      let isPeek = methodName.startsWith('peek')

      newMethodBody = newMethodBody.replace(methodBodyOnly, methods.peekHelper)

      if (isPeek) {
        let switchBody = getBody(newMethodBody, /switch \(value|if \(hash == NULL_HASH/)

        if (type === 'map') {
          newMethodBody = newMethodBody.replace(switchBody, 'switch (value) { case (?someValue) ?(key, someValue); case (_) null }')
        }

        if (type === 'set') {
          newMethodBody = newMethodBody.replace(switchBody, 'if (hash == NULL_HASH) null else ?key')
        }
      }

      if (!isPeek) {
        let cycleBody = getBody(newMethodBody, /if \(cycleEntry/)
        let cycleBodyOnly = getBody(cycleBody, 0, { bodyOnly: true })

        newMethodBody = newMethodBody.replace(cycleBody, adjustIndent(cycleBodyOnly, -2))

        newMethodBody = newMethodBody.replace(/return ([^;]+)/g, '$1')
      }
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (/^(map(Filter)?|filter|clone)(Desc)?$/.test(methodName)) {
      let isDesc = methodName.endsWith('Desc')
      let isMapFilter = methodName.startsWith('mapFilter')
      let isMap = !isMapFilter && methodName.startsWith('map')
      let isFilter = methodName.startsWith('filter')
      let isClone = methodName.startsWith('clone')

      newMethodBody = newMethodBody.replace(methodBodyOnly, methods.mapFilterHelper)

      newMethodBody = newMethodBody.replace(/newEntry[^;]+:= deqPrev;/, '')

      newMethodBody = newMethodBody.replace(/newEdge, newEdge]/, isDesc ? 'newEdge, deqPrev]' : 'deqPrev, newEdge]')

      if (type === 'set' && isClone) {
        let elseBody = getBody(newMethodBody, /else {/)

        newMethodBody = newMethodBody.replace(/if \(acceptEntry\(key\)\) /, '')

        newMethodBody = newMethodBody.replace(elseBody, '')
      }

      if (type === 'map') {
        let mapFilterBody = getBody(newMethodBody, /if \(acceptEntry/, { bodyOnly: true })
        let caseBody = getBody(newMethodBody, /case \(\?someValue/)

        if (isFilter || isClone) newMethodBody = newMethodBody.replace(/createEdgeEntry<K, V2>/, 'createEdgeEntry<K, V>')

        if (isMapFilter) {
          let outerSomeCase = `case (?someValue) switch (mapEntry(key, someValue))`
          let nullCase = `${getIndent(10)}case (null) entry := links[DEQ_NEXT];`
          let someCase = `${getIndent(10)}case (newValue) {${mapFilterBody}};`

          newMethodBody = newMethodBody.replace(caseBody, `${outerSomeCase} {\n${nullCase}\n\n${someCase}\n${getIndent(8)}}`)

          newMethodBody = newMethodBody.replace(/getValue\(newValue\)/, 'newValue')
        }

        if (isMap) {
          newMethodBody = newMethodBody.replace(caseBody, `case (?someValue) {${adjustIndent(mapFilterBody, -2)}}`)

          newMethodBody = newMethodBody.replace(/getValue\(newValue\)/, '?mapEntry(key, someValue)')
        }

        if (isFilter) {
          newMethodBody = newMethodBody.replace(/getValue\(newValue\)/, 'value')

          newMethodBody = newMethodBody.replace(/let newValue =[^;]+;/, '')

          newMethodBody = newMethodBody.replace(/newValue/, 'someValue')
        }

        if (isClone) {
          let switchBody = getBody(newMethodBody, /switch \(value/)
          let finalCaseBody = getBody(newMethodBody, /case \(_\)/, { bodyOnly: true })

          mapFilterBody = `if (hash == NULL_HASH) {${finalCaseBody}} else {${adjustIndent(mapFilterBody, -2)}}`

          newMethodBody = newMethodBody.replace(switchBody, adjustIndent(mapFilterBody, -2))

          newMethodBody = newMethodBody.replace(/getValue\(newValue\)/, 'value')
        }
      }
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (/^(keys|vals|entries)(From)?(Desc)?$/.test(methodName)) {
      let isKeys = methodName.startsWith('keys')
      let isVals = methodName.startsWith('vals')
      let isFrom = methodName.endsWith('From') || methodName.endsWith('FromDesc')

      if (isFrom) {
        newMethodBody = newMethodBody.replace(methodBodyOnly, methods.iterateHelper)

        newMethodBody = newMethodBody.replace(/if \(shiftingHash != NULL_HASH\) ([^;]+) else[^;]+/, '$1')
      } else {
        let indent2 = getIndent(2)
        let indent4 = getIndent(4)
        let iterObjectBody = getBody(methods.iterateHelper, /let iter =/)

        iterObjectBody = adjustIndent(iterObjectBody, -2)

        newMethodBody = newMethodBody.replace(methodBodyOnly, `\n${indent4}var entry = edgeEntry;\n\n${indent4}${iterObjectBody};\n${indent2}`)
      }

      if (type === 'map') {
        if (isKeys || isVals) {
          newMethodBody = newMethodBody.replace(/mapEntry\(key, value\)/g, () => isKeys ? '?key' : 'value')
        } else {
          let entriesIter = 'switch (value) { case (?someValue) ?(key, someValue); case (_) null };'

          newMethodBody = newMethodBody.replace(/if \(hash == NULL_HASH\) null[^;]+;/g, entriesIter)
        };

        newMethodBody = newMethodBody.replace(/\bT\b/g, () => isKeys ? 'K' : isVals ? 'V' : '(K, V)')
      };

      newMethodBody = newMethodBody.replace(/return (?!let)/g, '')
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (/^(find|some|every|forEach)(Desc)?$/.test(methodName)) {
      let isFind = methodName.startsWith('find')
      let isSome = methodName.startsWith('some')
      let isEvery = methodName.startsWith('every')
      let isForEach = methodName.startsWith('forEach')

      newMethodBody = newMethodBody.replace(methodBodyOnly, methods.findHelper)

      newMethodBody = newMethodBody.replace(/-> \(\(\)\)/, '-> ()')

      if (type === 'map') {
        if (isFind) {
          newMethodBody = newMethodBody.replace(/if \(acceptEntry[^;]+/, 'if (mapEntry(key, someValue)) return ?(key, someValue)')

          newMethodBody = newMethodBody.replace(/noMatchResult/, 'null')
        }

        if (isSome) {
          newMethodBody = newMethodBody.replace(/if \(acceptEntry[^;]+/, 'if (mapEntry(key, someValue)) return true')

          newMethodBody = newMethodBody.replace(/noMatchResult/, 'false')
        }

        if (isEvery) {
          newMethodBody = newMethodBody.replace(/if \(acceptEntry[^;]+/, 'if (not mapEntry(key, someValue)) return false')

          newMethodBody = newMethodBody.replace(/noMatchResult/, 'true')
        }

        if (isForEach) {
          newMethodBody = newMethodBody.replace(/if \(acceptEntry[^;]+/, 'mapEntry(key, someValue)')

          newMethodBody = newMethodBody.replace(/noMatchResult/, '')
        }
      }

      if (type === 'set') {
        if (isFind) {
          newMethodBody = newMethodBody.replace(/if \(acceptEntry[^;]+/, 'if (mapEntry(key)) return ?key')

          newMethodBody = newMethodBody.replace(/noMatchResult/, 'null')
        }

        if (isSome) {
          newMethodBody = newMethodBody.replace(/if \(acceptEntry[^;]+/, 'if (mapEntry(key)) return true')

          newMethodBody = newMethodBody.replace(/noMatchResult/, 'false')
        }

        if (isEvery) {
          newMethodBody = newMethodBody.replace(/if \(acceptEntry[^;]+/, 'if (not mapEntry(key)) return false')

          newMethodBody = newMethodBody.replace(/noMatchResult/, 'true')
        }

        if (isForEach) {
          newMethodBody = newMethodBody.replace(/if \(acceptEntry[^;]+/, 'mapEntry(key)')

          newMethodBody = newMethodBody.replace(/noMatchResult/, '')
        }
      }
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (/^fromIter(Map)?(Desc)?$/.test(methodName)) {
      let isDesc = methodName.endsWith('Desc')
      let isMap = methodName.endsWith('Map') || methodName.endsWith('MapDesc')

      let indentSize = isMap ? 4 : 2
      let indent = getIndent(indentSize)
      let indent2 = getIndent(indentSize + 2)

      let insertMethodName = `${type === 'set' ? 'add' : 'set'}${isDesc ? 'Front' : ''}`

      newMethodBody = newMethodBody.replace(methodBodyOnly, methods.fromIterHelper)

      newMethodBody = newMethodBody.replace(/let insertItem =[^;]+;/, '')

      newMethodBody = newMethodBody.replace(/new<([^>]+)>\(hashUtils\)/g, '(createEdgeEntry<$1>(getNullKey()), [var 0:Nat32])')

      if (!isMap) {
        let switchBody = getBody(newMethodBody, /switch \(mapItem/)

        newMethodBody = newMethodBody.replace(switchBody, `{${adjustIndent(methods[insertMethodName], indentSize)}}`)
      }

      if (isMap) newMethodBody = newMethodBody.replace(/insertItem\([^)]+\)/, `{${adjustIndent(methods[insertMethodName], indentSize)}}`)

      newMethodBody = newMethodBody.replace(/\(key, value\)/g, 'item')

      newMethodBody = newMethodBody.replace(/for \((item|key)/g, 'label fromIterLoop for (item')

      newMethodBody = newMethodBody.replace(/return;/g, 'continue fromIterLoop;')

      newMethodBody = newMethodBody.replace(/keyParam|valueParam/g, (name) => type === 'set' ? 'item' : name === 'keyParam' ? 'item.0' : 'item.1')

      newMethodBody = newMethodBody.replace(/case \(\?(item|key)/g, `\n${indent2}case (?item`)

      newMethodBody = newMethodBody.replace(/case \(_\) {} /g, `\n\n${indent2}case (_) {};\n${indent}`)
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (/^toArray(Map)?(Desc)?$/.test(methodName)) {
      let isMap = methodName.endsWith('Map') || methodName.endsWith('MapDesc')

      if (isMap) {
        newMethodBody = newMethodBody.replace(methodBodyOnly, methods.toArrayHelper)

        let tabulateFnBody = getBody(newMethodBody, /= if \(partial\) return/)

        newMethodBody = newMethodBody.replace(tabulateFnBody, '= unwrap(array[i])')

        newMethodBody = newMethodBody.replace(/if \(partial\) ([^;]+) else [^;]+/g, '$1')
      }

      if (!isMap) {
        let tabulateBody = getBody(methods.toArrayHelper, /tabulateArray/, { delimLeft: '(', delimRight: ')' })

        tabulateBody = `${adjustIndent(tabulateBody, -4)};\n${getIndent(2)}`

        newMethodBody = newMethodBody.replace(methodBodyOnly, `\n${getIndent(4)}var entry = edgeEntry;\n\n${getIndent(4)}${tabulateBody}`)

        let tabulateFnBody = getBody(newMethodBody, /= if \(partial\) return/)
        let tabulateFnBodyOnly = getBody(tabulateFnBody, 0, { bodyOnly: true })

        newMethodBody = newMethodBody.replace(tabulateFnBody, `{${tabulateFnBodyOnly}}`)

        newMethodBody = newMethodBody.replace(/\bT\b/g, type === 'map' ? '(K, V)' : 'K')

        newMethodBody = newMethodBody.replace(/arraySize/, 'size[SIZE]')

        newMethodBody = newMethodBody.replace(/return unwrap\(mapEntry[^;]+/, type === 'map' ? '(entry.0, unwrap(entry.1))' : 'entry.0')
      }
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    let entryMatch = null

    let entryRegExp = /let \([^)]+\) = (entry|leaf|placeEntry);/g

    while ((entryMatch = entryRegExp.exec(newMethodBody)) != null) {
      let { 0: entryString, 1: entryName } = entryMatch

      let entryBody = getBody(newMethodBody, entryMatch.index, { bodyStarted: true })

      let newEntryBody = entryBody

      newEntryBody = newEntryBody.replace(entryString, '')

      if (entryName === 'leaf') {
        let entryVars = type === 'map' ? { leafKey: 0, leafValue: 1, leafHash: 2, leafLinks: 3 } : { leafKey: 0, leafHash: 1, leafLinks: 2 }

        newEntryBody = newEntryBody.replace(/\b(leafKey|leafValue|leafHash|leafLinks)\b/g, (name) => `leaf.${entryVars[name]}`)
      }

      if (entryName === 'placeEntry') {
        let entryVars = type === 'map' ? { placeKey: 0, placeValue: 1, placeHash: 2, placeLinks: 3 } : { placeKey: 0, placeHash: 1, placeLinks: 2 }

        newEntryBody = newEntryBody.replace(/\b(placeKey|placeValue|placeHash|placeLinks)\b/g, (name) => `placeEntry.${entryVars[name]}`)
      }

      if (entryName === 'entry') {
        let entryVars = type === 'map' ? { key: 0, value: 1, hash: 2, links: 3 } : { key: 0, hash: 1, links: 2 }

        newEntryBody = newEntryBody.replace(/\b(key|value|hash|links)\b/g, (name) => `entry.${entryVars[name]}`)
      }

      newMethodBody = newMethodBody.replace(entryBody, newEntryBody)
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    let bodyVars = { edgeEntry: 0, size: 1 }

    let hashUtilVars = { getHash: 0, areEqual: 1, getNullKey: 2 }

    let linksKey = type === 'map' ? '3' : '2'

    let linkReplaceBody = ''

    for (let i = 1; i <= 6; i++) {
      linkReplaceBody = `${linkReplaceBody}$1$2.${linksKey}[${i === 5 ? 'DEQ_PREV' : i === 6 ? 'DEQ_NEXT' : `BRANCH_${i}`}] := $2;`
    }

    if (/(Desc|Front|After)$/.test(methodName)) {
      newMethodBody = newMethodBody.replace(/(DEQ_PREV|DEQ_NEXT)/g, (item) => item === 'DEQ_PREV' ? 'DEQ_NEXT' : 'DEQ_PREV')

      newMethodBody = newMethodBody.replace(/(deqPrev|deqNext)/g, (item) => item === 'deqPrev' ? 'deqNext' : 'deqPrev')

      newMethodBody = newMethodBody.replace(/(deqFirst|deqLast)/g, (item) => item === 'deqFirst' ? 'deqLast' : 'deqFirst')
    }

    newMethodBody = newMethodBody.replace(/(\n *)for \(index in (edgeEntry)[^;]+;/, linkReplaceBody)

    newMethodBody = newMethodBody.replace(/(\n *)for \(index in (newEdge)[^;]+;/, linkReplaceBody)

    newMethodBody = newMethodBody.replace(/let \([^)]+\) = map;/, '')

    newMethodBody = newMethodBody.replace(/\b(edgeEntry|size)\b(?!<)/g, (name) => `map.${bodyVars[name]}`)

    newMethodBody = newMethodBody.replace(/\((getHash|_), (areEqual|_), (getNullKey|_)\)/g, 'hashUtils')

    newMethodBody = newMethodBody.replace(/\b(getHash|areEqual|getNullKey)\b/g, (name) => `hashUtils.${hashUtilVars[name]}`)

    newMethodBody = newMethodBody.replace(/unwrap\(([^)]+)\)/g, 'switch ($1) { case (?value) value; case (_) noop() }')

    newMethodBody = newMethodBody.replace(/return (.+\n *})$/, '$1')

    optimizedStruct = optimizedStruct.replace(methodBody, newMethodBody)
  }

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  optimizedStruct = optimizedStruct.replace(/\s+;/g, ';')

  optimizedStruct = optimizedStruct.replace(/\/\/;+/g, '//')

  optimizedStruct = optimizedStruct.replace(/{;+/g, '{')

  optimizedStruct = optimizedStruct.replace(/;{2,}/g, ';')

  optimizedStruct = optimizedStruct.replace(/(\S) {2,}/g, '$1 ')

  optimizedStruct = optimizedStruct.replace(/\n[^\S\n]+(?=\n)/g, '\n')

  optimizedStruct = optimizedStruct.replace(/ +\n/g, '\n')

  optimizedStruct = optimizedStruct.replace(/\n{3,}/g, '\n\n')

  optimizedStruct = optimizedStruct.replace(/{\n{2,}/g, '{\n')

  optimizedStruct = optimizedStruct.replace(/\n{2,}}/g, '\n}')

  fs.writeFileSync(path, optimizedStruct)
}
