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

  let methodRegExp = /func\s+(\w+)/g

  while ((match = methodRegExp.exec(struct)) != null) {
    let { 1: methodName, index: methodIndex } = match

    if (methodName === 'next') continue

    let methodBody = getBody(struct, methodIndex)

    let newMethodBody = methodBody

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    let entryMatch = methodBody.match(/( *)let\s*\([^)]+\)\s*=\s*((?:entry|edgeEntry)[^;]*);(\s*\n)/)

    if (entryMatch != null) {
      let { 0: entryString, 1: spaceBefore, 2: entryExp, 3: spaceAfter, index: entryIndex } = entryMatch

      let entryBody = getBody(methodBody, entryIndex, { bodyStarted: true })

      let newEntryBody = entryBody

      let entryVarNames = type === 'map' ? { links: 0, key: 1, value: 2, hash: 3 } : { links: 0, key: 1, hash: 2 }

      newEntryBody = newEntryBody.replace(entryString, entryExp.trim() == 'entry' ? '' : `${spaceBefore}let entry = ${entryExp};${spaceAfter}`)

      newEntryBody = newEntryBody.replace(/\b(links|key|value|hash)\b/g, (name) => `entry.${entryVarNames[name]}`)

      newMethodBody = newMethodBody.replace(entryBody, newEntryBody)
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (methodName === 'fromIter' && type === 'map') {
      newMethodBody = newMethodBody.replace(/\(\s*key\s*,\s*value\s*\)/g, 'entry')

      newMethodBody = newMethodBody.replace(/\b(key|value)\b/g, (name) => name == 'key' ? 'entry.0' : 'entry.1')
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    let bodyVarNames = { buckets: 0, capacity: 1, edgeEntry: 2 }

    let hashUtilVarNames = { getHash: 0, areEqual: 1, getNullKey: 2 }

    newMethodBody = newMethodBody.replace(/let\s*\(\s*_\s*,\s*_\s*,\s*newEdgeEntry\s*\)\s*=\s*new([^;]+)/g, 'let newEdgeEntry = new$1.2')

    newMethodBody = newMethodBody.replace(/let\s*\([^)]+\)\s*=\s*(map|set)\s*\.\s*body/g, `let body = ${type}.body`)

    newMethodBody = newMethodBody.replace(/\b(buckets|capacity|edgeEntry)\b(?! limit reached)/g, (name) => `body.${bodyVarNames[name]}`)

    if (/let\s+body/.test(newMethodBody) && newMethodBody.match(/[^\.\s]\s*\bbody\b/g).length == 2) {
      newMethodBody = newMethodBody.replace(/ *let body = (map|set)\.body\s*;\s*\n/, '')

      newMethodBody = newMethodBody.replace(/([^\.\s]\s*)\bbody\b/g, `$1${type}.body`)
    }

    newMethodBody = newMethodBody.replace(/\(\s*(getHash|_)\s*,\s*(areEqual|_)\s*,\s*(getNullKey|_)\s*\)/g, 'hashUtils')

    newMethodBody = newMethodBody.replace(/\b(getHash|areEqual|getNullKey)\b/g, (name) => `hashUtils.${hashUtilVarNames[name]}`)

    optimizedStruct = optimizedStruct.replace(methodBody, newMethodBody)
  }

  fs.writeFileSync(path, optimizedStruct)
}
