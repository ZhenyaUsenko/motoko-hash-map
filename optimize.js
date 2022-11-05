let fs = require('fs')

let map = fs.readFileSync('./src/Map/Map.mo').toString()
let set = fs.readFileSync('./src/Set/Set.mo').toString()
let utils = fs.readFileSync('./src/utils.mo').toString()

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

let getBody = (string, firstIndex, options = {}) => {
  let bodyStarted = options.bodyStarted ?? false
  let bracesCount = options.bracesCount ?? (bodyStarted ? 1 : 0)
  let delimLeft = options.delimLeft ?? '{'
  let delimRight = options.delimRight ?? '}'
  let lastIndex = firstIndex

  while (!bodyStarted || bracesCount !== 0) {
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

let { index: utilsIndex } = utils.match(/\bmodule\b/)

let utilsBody = getBody(utils, utilsIndex)

let { index: typesIndex } = utilsBody.match(/ *public\s+type\s+HashUtils/)

let utilsTypes = getBody(utilsBody, typesIndex, { delimLeft: '(', delimRight: ')' })

let { index: methodsIndex } = utilsBody.match(/ *public\s+func/)

let utilsMethods = getBody(utilsBody, methodsIndex, { bodyStarted: true, bodyOnly: true })

utilsMethods = utilsMethods.replace(/\s*$/, '')

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

let structs = [[map, 'map', './src/Map/optimized.mo'], [set, 'set', './src/Set/optimized.mo']]

for (let [struct, type, path] of structs) {
  let optimizedStruct = struct

  optimizedStruct = optimizedStruct.replace(/}\s*;?\s*$/, `\n  ${'/'.repeat(148)}\n\n${utilsMethods}\n};\n`)

  optimizedStruct = optimizedStruct.replace(/ *public\s+type\s+HashUtils.+/, `${utilsTypes};`)

  optimizedStruct = optimizedStruct.replace(/ *import\s+Utils.+;\s*?\n/, '')

  optimizedStruct = optimizedStruct.replace(/ *public\s+let\s*{[^}]+}\s*=\s*Utils\s*;\s*\n/, '')

  let structWithUtils = optimizedStruct

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  let getMethod, putHelper, putBeforeHelper, updateHelper, removeMethod, popHelper, cycleHelper, iterateHelper, iterateFromHelper, toArrayMapHelper

  let match = null

  let methodRegExp = /func\s+(\w+)/g

  while ((match = methodRegExp.exec(structWithUtils)) != null) {
    let { 1: methodName, index: methodIndex } = match

    let methodBody = getBody(structWithUtils, methodIndex)
    let methodBodyOnly = getBody(structWithUtils, methodIndex, { bodyOnly: true })

    let newMethodBody = methodBody

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (methodName === 'unwrap') {
      optimizedStruct = optimizedStruct.replace(methodBody, '')

      continue
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (methodName === 'get') {
      getMethod = methodBodyOnly
    }

    if (methodName === 'has' && type === 'map') {
      newMethodBody = newMethodBody.replace(methodBodyOnly, getMethod)

      newMethodBody = newMethodBody.replace(/(return\s*)(null|value)/g, (item) => `return ${item.endsWith('value') ? 'true' : 'false'}`)
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (methodName === 'putHelper') {
      putHelper = methodBodyOnly

      optimizedStruct = optimizedStruct.replace(methodBody, '')

      continue
    }

    if (methodName === 'putBeforeHelper') {
      putBeforeHelper = methodBodyOnly

      optimizedStruct = optimizedStruct.replace(methodBody, '')

      continue
    }

    if (methodName === 'updateHelper') {
      updateHelper = methodBodyOnly

      optimizedStruct = optimizedStruct.replace(methodBody, '')

      continue
    }

    if (/^(set|add|put|update)(Move)?(Front|Before|After)?$/.test(methodName)) {
      let isFront = methodName.endsWith('Front') || methodName.endsWith('After')
      let isSet = methodName.startsWith('set') || methodName.startsWith('add')
      let isUpdate = methodName.startsWith('update')
      let isMove = methodName.endsWith('Move') || methodName.endsWith('MoveFront')
      let isBefore = methodName.endsWith('Before') || methodName.endsWith('After')

      newMethodBody = newMethodBody.replace(methodBodyOnly, isUpdate ? updateHelper : isBefore ? putBeforeHelper : putHelper)

      newMethodBody = newMethodBody.replace(/newEntry.*:=\s*deqPrev\s*;\s*/, '')

      let moveMatch = newMethodBody.match(/if\s+\(\s*moveExisting\s*\)\s*/)

      if (moveMatch != null) {
        let moveBody = getBody(newMethodBody, moveMatch.index)
        let moveElseBody = type === 'map' ? getBody(newMethodBody, moveMatch.index + moveBody.length) : ''

        if (isMove) {
          let moveBodyOnly = getBody(newMethodBody, moveMatch.index, { bodyOnly: true })

          newMethodBody = newMethodBody.replace(moveBody, moveBodyOnly.replace(/ {4,}/g, space => ' '.repeat(space.length - 2)))

          if (type === 'map') {
            newMethodBody = newMethodBody.replace(moveElseBody, '')
          }
        } else {
          let moveElseBodyOnly = type === 'map' ? getBody(newMethodBody, moveMatch.index + moveBody.length, { bodyOnly: true }) : ''

          newMethodBody = newMethodBody.replace(moveBody, '')

          if (type === 'map') {
            newMethodBody = newMethodBody.replace(moveElseBody, moveElseBodyOnly.replace(/ {4,}/g, space => ' '.repeat(space.length - 2)))
          }
        }
      }

      if (isBefore) {
        newMethodBody = newMethodBody.replace(/placeEntry\s*,\s*placeEntry\s*]/, () => isFront ? 'placeEntry, deqPrev]' : 'deqPrev, placeEntry]')
      } else {
        newMethodBody = newMethodBody.replace(/edgeEntry\s*,\s*edgeEntry\s*]/, () => isFront ? 'edgeEntry, deqPrev]' : 'deqPrev, edgeEntry]')
      }

      if (isSet) {
        newMethodBody = newMethodBody.replace(/(return\s*)(null|value|true|false)/g, 'return')
      }

      if (isFront) {
        newMethodBody = newMethodBody.replace(/(DEQ_PREV|DEQ_NEXT)/g, (item) => item === 'DEQ_PREV' ? 'DEQ_NEXT' : 'DEQ_PREV')

        newMethodBody = newMethodBody.replace(/(deqPrev|deqNext)/g, (item) => item === 'deqPrev' ? 'deqNext' : 'deqPrev')
      }
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (methodName === 'remove') {
      removeMethod = methodBodyOnly
    }

    if (methodName === 'delete') {
      newMethodBody = newMethodBody.replace(methodBodyOnly, removeMethod)

      newMethodBody = newMethodBody.replace(/(return\s*)(null|value|true|false)/g, 'return')
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (methodName === 'popHelper') {
      popHelper = methodBodyOnly

      optimizedStruct = optimizedStruct.replace(methodBody, '')

      continue
    }

    if (/^pop(Front)?$/.test(methodName)) {
      let isFront = methodName.endsWith('Front')

      newMethodBody = newMethodBody.replace(methodBodyOnly, popHelper)

      if (isFront) {
        newMethodBody = newMethodBody.replace(/(DEQ_PREV|DEQ_NEXT)/g, (item) => item === 'DEQ_PREV' ? 'DEQ_NEXT' : 'DEQ_PREV')

        newMethodBody = newMethodBody.replace(/(deqPrev|deqNext)/g, (item) => item === 'deqPrev' ? 'deqNext' : 'deqPrev')
      }
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (methodName === 'cycleHelper') {
      cycleHelper = methodBodyOnly

      optimizedStruct = optimizedStruct.replace(methodBody, '')

      continue
    }

    if (/^cycle(Front)?$/.test(methodName)) {
      let isFront = methodName.endsWith('Front')

      newMethodBody = newMethodBody.replace(methodBodyOnly, cycleHelper)

      newMethodBody = newMethodBody.replace(/return\s+/g, '')

      if (isFront) {
        newMethodBody = newMethodBody.replace(/(DEQ_PREV|DEQ_NEXT)/g, (item) => item === 'DEQ_PREV' ? 'DEQ_NEXT' : 'DEQ_PREV')

        newMethodBody = newMethodBody.replace(/(deqPrev|deqNext)/g, (item) => item === 'deqPrev' ? 'deqNext' : 'deqPrev')

        newMethodBody = newMethodBody.replace(/(deqFirst|deqLast)/g, (item) => item === 'deqFirst' ? 'deqLast' : 'deqFirst')
      }
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (methodName === 'iterateHelper') {
      iterateHelper = methodBodyOnly

      optimizedStruct = optimizedStruct.replace(methodBody, '')

      continue
    }

    if (methodName === 'iterateFromHelper') {
      iterateFromHelper = methodBodyOnly

      optimizedStruct = optimizedStruct.replace(methodBody, '')

      continue
    }

    if (/^(keys|vals|entries)(From)?(Desc)?$/.test(methodName)) {
      let isDesc = methodName.endsWith('Desc')
      let isKeys = methodName.startsWith('keys')
      let isVals = methodName.startsWith('vals')
      let isFrom = methodName.endsWith('From') || methodName.endsWith('FromDesc')

      newMethodBody = newMethodBody.replace(methodBodyOnly, isFrom ? iterateFromHelper : iterateHelper)

      if (type === 'map') {
        newMethodBody = newMethodBody.replace(/fn\s*\(\s*key\s*,\s*value\s*\)/g, () => isKeys ? '?key' : isVals ? 'value' : '?(key, unwrap(value))')

        newMethodBody = newMethodBody.replace(/\bT\b/g, () => isKeys ? 'K' : isVals ? 'V' : '(K, V)')
      };

      if (isFrom) {
        newMethodBody = newMethodBody.replace(/return\s+(?!let)/g, '')
      } else {
        newMethodBody = newMethodBody.replace(/return\s+/g, '')
      }

      if (isDesc) {
        newMethodBody = newMethodBody.replace(/(DEQ_PREV|DEQ_NEXT)/g, (item) => item === 'DEQ_PREV' ? 'DEQ_NEXT' : 'DEQ_PREV')
      }
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (/^fromIter(Desc)?$/.test(methodName) && type === 'map') {
      newMethodBody = newMethodBody.replace(/\(\s*key\s*,\s*value\s*\)/g, 'item')

      newMethodBody = newMethodBody.replace(/\b(key|value)\b/g, (name) => name == 'key' ? 'item.0' : 'item.1')
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (/^toArray(Desc)?$/.test(methodName)) {
      newMethodBody = newMethodBody.replace(/return\s+/g, '')
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (methodName === 'toArrayMapHelper') {
      toArrayMapHelper = methodBodyOnly

      optimizedStruct = optimizedStruct.replace(methodBody, '')

      continue
    }

    if (/^toArrayMap(Desc)?$/.test(methodName)) {
      let isDesc = methodName.endsWith('Desc')

      newMethodBody = newMethodBody.replace(methodBodyOnly, toArrayMapHelper)

      newMethodBody = newMethodBody.replace(/( *)(.*) else/g, '$1$2\n$1else')

      newMethodBody = newMethodBody.replace(/( *)(.*) return/g, '$1$2\n$1return')

      if (isDesc) {
        newMethodBody = newMethodBody.replace(/(DEQ_PREV|DEQ_NEXT)/g, (item) => item === 'DEQ_PREV' ? 'DEQ_NEXT' : 'DEQ_PREV')
      }
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (/^(forEach|some|every|find)(Desc)?$/.test(methodName) && type === 'map') {
      newMethodBody = newMethodBody.replace(/( *)(.*) else/g, '$1$2\n$1else')
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    let entryMatch = null

    let entryRegExp = /( *)let\s*\([^)]+\)\s*=\s*((?:entry|edgeEntry)[^;]*);/g

    while ((entryMatch = entryRegExp.exec(newMethodBody)) != null) {
      let { 0: entryString, 1: spaceBefore, 2: entryExp, index: entryIndex } = entryMatch

      let entryBody = getBody(newMethodBody, entryIndex, { bodyStarted: true })

      let newEntryBody = entryBody

      let entryVarNames = type === 'map' ? { key: 0, value: 1, hash: 2, links: 3 } : { key: 0, hash: 1, links: 2 }

      newEntryBody = newEntryBody.replace(entryString, entryExp.trim() == 'entry' ? '' : `${spaceBefore}let entry = ${entryExp};`)

      newEntryBody = newEntryBody.replace(/\b(key|value|hash|links)\b/g, (name) => `entry.${entryVarNames[name]}`)

      newMethodBody = newMethodBody.replace(entryBody, newEntryBody)
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    let bodyVarNames = { edgeEntry: 0, size: 1 }

    let hashUtilVarNames = { getHash: 0, areEqual: 1, getNullKey: 2 }

    let linksKey = type === 'map' ? '3' : '2'

    let unwrapBody = '{ case (?value) value; case (_) trap("unreachable") }'

    let branchReplaceBody = ''

    let linkReplaceBody = ''

    for (let i = 1; i <= 4; i++) {
      branchReplaceBody = `${branchReplaceBody}$1leaf.${linksKey}[BRANCH_${i}] := entry.${linksKey}[BRANCH_${i}];`
    }

    for (let i = 1; i <= 6; i++) {
      linkReplaceBody = `${linkReplaceBody}$1$2.${linksKey}[${i === 5 ? 'DEQ_PREV' : i === 6 ? 'DEQ_NEXT' : `BRANCH_${i}`}] := $2;`
    }

    newMethodBody = newMethodBody.replace(/(\n *)for\s*\(\s*index\s+in\s+leaf.*/, branchReplaceBody)

    newMethodBody = newMethodBody.replace(/(\n *)for\s*\(\s*index\s+in\s+(edgeEntry).*/, linkReplaceBody)

    newMethodBody = newMethodBody.replace(/(\n *)for\s*\(\s*index\s+in\s+(newEdgeEntry).*/, linkReplaceBody)

    newMethodBody = newMethodBody.replace(/let\s*\([^)]+\)\s*=\s*map\s*;/, '')

    newMethodBody = newMethodBody.replace(/\b(edgeEntry|size)\b(?!\s*<)/g, (name) => `map.${bodyVarNames[name]}`)

    newMethodBody = newMethodBody.replace(/\(\s*(getHash|_)\s*,\s*(areEqual|_)\s*,\s*(getNullKey|_)\s*\)/g, 'hashUtils')

    newMethodBody = newMethodBody.replace(/\b(getHash|areEqual|getNullKey)\b/g, (name) => `hashUtils.${hashUtilVarNames[name]}`)

    newMethodBody = newMethodBody.replace(/unwrap\([^)]+\)/g, (value) => `switch (${value.slice(7, -1)}) ${unwrapBody}`)

    newMethodBody = newMethodBody.replace(/return\s+(.+\n *})$/, '$1')

    optimizedStruct = optimizedStruct.replace(methodBody, newMethodBody)
  }

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  optimizedStruct = optimizedStruct.replace(/\s+;/g, ';')

  optimizedStruct = optimizedStruct.replace(/\/\/;+/g, '//')

  optimizedStruct = optimizedStruct.replace(/{;+/g, '{')

  optimizedStruct = optimizedStruct.replace(/;{2,}/g, ';')

  optimizedStruct = optimizedStruct.replace(/\n[^\S\n]+(?=\n)/g, '\n')

  optimizedStruct = optimizedStruct.replace(/\n{3,}/g, '\n\n')

  optimizedStruct = optimizedStruct.replace(/{\n{2,}/g, '{\n')

  optimizedStruct = optimizedStruct.replace(/\n{2,}}/g, '\n}')

  fs.writeFileSync(path, optimizedStruct)
}
