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

      if (!bodyStarted && options.bodyOnly) firstIndex = lastIndex - 1

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
  let match = null

  let optimizedStruct = struct

  optimizedStruct = optimizedStruct.replace(/}\s*;?\s*$/, `\n  ${'/'.repeat(148)}\n\n${utilsMethods}\n};\n`)

  optimizedStruct = optimizedStruct.replace(/ *public\s+type\s+HashUtils.+/, `${utilsTypes};`)

  optimizedStruct = optimizedStruct.replace(/ *import\s+Utils.+;\s*?\n/, '')

  optimizedStruct = optimizedStruct.replace(/ *public\s+let\s*{[^}]+}\s*=\s*Utils\s*;\s*\n/, '')

  let structWithUtils = optimizedStruct

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  let getMethod, putHelper, removeMethod, updateHelper, popHelper, iterateHelper, forEachHelper

  let methodRegExp = /func\s+(\w+)/g

  while ((match = methodRegExp.exec(structWithUtils)) != null) {
    let { 1: methodName, index: methodIndex } = match

    if (methodName === 'next') continue

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

    if (methodName === 'updateHelper') {
      updateHelper = methodBodyOnly

      optimizedStruct = optimizedStruct.replace(methodBody, '')

      continue
    }

    if (['put', 'putFront', 'set', 'setFront', 'update', 'updateFront'].includes(methodName)) {
      let isFront = methodName.endsWith('Front')
      let isSet = methodName.startsWith('set')
      let isUpdate = methodName.startsWith('update')

      newMethodBody = newMethodBody.replace(methodBodyOnly, isUpdate ? updateHelper : putHelper)

      newMethodBody = newMethodBody.replace(/newEntry.*:=\s*deqPrev\s*;\s*/, '')

      newMethodBody = newMethodBody.replace(/edgeEntry\s*,\s*edgeEntry\s*]/, () => isFront ? 'edgeEntry, deqPrev]' : 'deqPrev, edgeEntry]')

      if (isSet) {
        newMethodBody = newMethodBody.replace(/(return\s*)(null|value)/g, 'return')
      }

      if (isFront) {
        newMethodBody = newMethodBody.replace(/(DEQ_PREV|DEQ_NEXT)/g, (item) => item === 'DEQ_PREV' ? 'DEQ_NEXT' : 'DEQ_PREV')

        newMethodBody = newMethodBody.replace(/deqPrev/g, 'deqNext')
      }
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (methodName === 'remove') {
      removeMethod = methodBodyOnly
    }

    if (methodName === 'delete') {
      newMethodBody = newMethodBody.replace(methodBodyOnly, removeMethod)

      newMethodBody = newMethodBody.replace(/(return\s*)(null|value)/g, 'return')
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (methodName === 'popHelper') {
      popHelper = methodBodyOnly

      optimizedStruct = optimizedStruct.replace(methodBody, '')

      continue
    }

    if (['pop', 'popFront'].includes(methodName)) {
      let isFront = methodName.endsWith('Front')

      newMethodBody = newMethodBody.replace(methodBodyOnly, popHelper)

      if (isFront) {
        newMethodBody = newMethodBody.replace(/(DEQ_PREV|DEQ_NEXT)/g, (item) => item === 'DEQ_PREV' ? 'DEQ_NEXT' : 'DEQ_PREV')

        newMethodBody = newMethodBody.replace(/deqPrev/g, 'deqNext')
      }
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (methodName === 'iterateHelper') {
      iterateHelper = ` = object${methodBodyOnly}`

      optimizedStruct = optimizedStruct.replace(methodBody, '')

      continue
    }

    if (['keys', 'keysDesc', 'vals', 'valsDesc', 'entries', 'entriesDesc'].includes(methodName)) {
      let isDesc = methodName.endsWith('Desc')
      let isKeys = methodName.startsWith('keys')
      let isVals = methodName.startsWith('vals')

      newMethodBody = newMethodBody.replace(methodBodyOnly, iterateHelper)

      newMethodBody = newMethodBody.replace(/fn\s*\(\s*key\s*,\s*value\s*\)/, () => isKeys ? '?key' : isVals ? 'value' : '?(key, unwrap(value))')

      newMethodBody = newMethodBody.replace(/\?T/, () => isKeys ? '?K' : isVals ? '?V' : '?(K, V)')

      newMethodBody = newMethodBody.replace(/return\s+/, '')

      if (isDesc) {
        newMethodBody = newMethodBody.replace(/(DEQ_PREV|DEQ_NEXT)/g, (item) => item === 'DEQ_PREV' ? 'DEQ_NEXT' : 'DEQ_PREV')
      }
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (methodName === 'forEachHelper') {
      forEachHelper = methodBodyOnly

      optimizedStruct = optimizedStruct.replace(methodBody, '')

      continue
    }

    if (['forEach', 'forEachDesc', 'some', 'someDesc', 'every', 'everyDesc'].includes(methodName)) {
      let isDesc = methodName.endsWith('Desc')
      let isForEach = methodName.startsWith('forEach')
      let isEvery = methodName.startsWith('every')

      newMethodBody = newMethodBody.replace(methodBodyOnly, forEachHelper)

      newMethodBody = newMethodBody.replace(/( *)(.*) else/g, '$1$2\n$1else')

      if (isForEach) {
        newMethodBody = newMethodBody.replace(/if\s*\(\s*fn.*;/, 'fn(key, unwrap(value))')

        newMethodBody = newMethodBody.replace(/return\s*false/, 'return')
      }

      if (isEvery) {
        newMethodBody = newMethodBody.replace(/if\s*\(\s*fn/, 'if (not fn')

        newMethodBody = newMethodBody.replace(/(return\s*)(true|false)/g, (item) => `return ${item.endsWith('false') ? 'true' : 'false'}`)
      }

      if (isDesc) {
        newMethodBody = newMethodBody.replace(/(DEQ_PREV|DEQ_NEXT)/g, (item) => item === 'DEQ_PREV' ? 'DEQ_NEXT' : 'DEQ_PREV')
      }
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (methodName === 'fromIter' && type === 'map') {
      newMethodBody = newMethodBody.replace(/\(\s*key\s*,\s*value\s*\)/g, 'entry')

      newMethodBody = newMethodBody.replace(/\b(key|value)\b/g, (name) => name == 'key' ? 'entry.0' : 'entry.1')
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (['find', 'findDesc', 'toArray'].includes(methodName)) {
      newMethodBody = newMethodBody.replace(/( *)(.*) else/g, '$1$2\n$1else')

      if (methodName === 'toArray') {
        newMethodBody = newMethodBody.replace(/( *)(.*) return/g, '$1$2\n$1return')
      }
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    let entryMatch = newMethodBody.match(/( *)let\s*\([^)]+\)\s*=\s*((?:entry|edgeEntry)[^;]*);(\s*\n)/)

    if (entryMatch != null) {
      let { 0: entryString, 1: spaceBefore, 2: entryExp, 3: spaceAfter, index: entryIndex } = entryMatch

      let entryBody = getBody(newMethodBody, entryIndex, { bodyStarted: true })

      let newEntryBody = entryBody

      let entryVarNames = type === 'map' ? { key: 0, value: 1, hash: 2, links: 3 } : { key: 0, hash: 1, links: 2 }

      newEntryBody = newEntryBody.replace(entryString, entryExp.trim() == 'entry' ? '' : `${spaceBefore}let entry = ${entryExp};${spaceAfter}`)

      newEntryBody = newEntryBody.replace(/\b(links|key|value|hash)\b/g, (name) => `entry.${entryVarNames[name]}`)

      newMethodBody = newMethodBody.replace(entryBody, newEntryBody)
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    let bodyVarNames = { edgeEntry: 0, size: 1 }

    let hashUtilVarNames = { getHash: 0, areEqual: 1, getNullKey: 2 }

    let unwrapBody = '{ case (?value) value; case (_) trap("unreachable") }'

    let branchReplaceBody = ''

    let linkReplaceBody = ''

    for (let i = 1; i <= 4; i++) {
      branchReplaceBody = `${branchReplaceBody}$1leaf.3[BRANCH_${i}] := entry.3[BRANCH_${i}];`
    }

    for (let i = 1; i <= 6; i++) {
      linkReplaceBody = `${linkReplaceBody}$1$2.3[${i === 5 ? 'DEQ_PREV' : i === 6 ? 'DEQ_NEXT' : `BRANCH_${i}`}] := $2;`
    }

    newMethodBody = newMethodBody.replace(/(\n *)for\s*\(\s*index\s+in\s+leaf.*/, branchReplaceBody)

    newMethodBody = newMethodBody.replace(/(\n *)for\s*\(\s*index\s+in\s+(edgeEntry).*/, linkReplaceBody)

    newMethodBody = newMethodBody.replace(/(\n *)for\s*\(\s*index\s+in\s+(newEdgeEntry).*/, linkReplaceBody)

    newMethodBody = newMethodBody.replace(/let\s*\([^)]+\)\s*=\s*(map|set)\s*;\s*/, '')

    newMethodBody = newMethodBody.replace(/\b(edgeEntry|size)\b(?!\s*<)/g, (name) => `${type}.${bodyVarNames[name]}`)

    newMethodBody = newMethodBody.replace(/\(\s*(getHash|_)\s*,\s*(areEqual|_)\s*,\s*(getNullKey|_)\s*\)/g, 'hashUtils')

    newMethodBody = newMethodBody.replace(/\b(getHash|areEqual|getNullKey)\b/g, (name) => `hashUtils.${hashUtilVarNames[name]}`)

    newMethodBody = newMethodBody.replace(/unwrap\([^)]+\)/g, (value) => `switch (${value.slice(7, -1)}) ${unwrapBody}`)

    newMethodBody = newMethodBody.replace(/return\s+(.+\n *})$/, '$1')

    optimizedStruct = optimizedStruct.replace(methodBody, newMethodBody)
  }

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  optimizedStruct = optimizedStruct.replace(/\s+;/g, ';')

  optimizedStruct = optimizedStruct.replace(/\/\/;+/g, '//')

  optimizedStruct = optimizedStruct.replace(/;{2,}/g, ';')

  optimizedStruct = optimizedStruct.replace(/\n{4,}/g, '\n\n\n')

  fs.writeFileSync(path, optimizedStruct)
}
