let fs = require('fs')

let map = fs.readFileSync('./src/Map/Map.mo').toString()

let optimizedMap = map

let match = null

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

let getBody = (string, firstIndex, options = {}) => {
  let bodyStarted = options.bodyStarted ?? false
  let bracesCount = options.bracesCount ?? (bodyStarted ? 1 : 0)
  let lastIndex = firstIndex

  while (!bodyStarted || bracesCount !== 0) {
    let char = string.charAt(lastIndex)

    if (char === ';') bodyStarted = true

    if (char === "}") bracesCount--

    if (char === "{") {
      bracesCount++

      if (!bodyStarted && options.bodyOnly) firstIndex = lastIndex

      bodyStarted = true
    }

    lastIndex++
  }

  return string.slice(firstIndex, lastIndex)
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

let methodRegExp = /func\s+(\w+)/g

while ((match = methodRegExp.exec(map)) != null) {
  let { 1: methodName, index: methodIndex } = match

  if (methodName === 'next') continue

  let methodBody = getBody(map, methodIndex)

  let newMethodBody = methodBody

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  let caseRegExp = /(case\s*\(\s*(?:links|_)\s*,\s*(\??)\s*(?:key|_)\s*,\s*(?:value|_)\s*,\s*(?:hash|_)\s*\)| *let\s*\([^)]+\)\s*=\s*entry\s*;\s*\n)/

  let caseMatch = methodBody.match(caseRegExp)

  if (caseMatch != null) {
    let { 0: caseString, 2: keyOption, index: caseIndex } = caseMatch

    let isCase = caseString.startsWith('case')

    let caseBody = getBody(methodBody, caseIndex, { bodyStarted: !isCase })

    let newCaseBody = caseBody

    let entryVarNames = { links: 0, value: 2, hash: 3 }

    let needsKeyVar = isCase && keyOption === '?' && /[^?]\s*key/.test(caseBody)

    let entryVarRegExp = isCase ? /(\blinks\b|\?\s*\bkey\b|\bvalue\b|\bhash\b)/g : /\b(links|key|value|hash)\b/g

    newCaseBody = newCaseBody.replace(caseString, isCase ? `case (${keyOption}${needsKeyVar ? 'key' : '_'})` : '')

    newCaseBody = newCaseBody.replace(entryVarRegExp, (name) => `entry.${entryVarNames[name] ?? 1}`)

    newCaseBody = newCaseBody.replace('case (entry.1)', `case (?key)`)

    newMethodBody = newMethodBody.replace(caseBody, newCaseBody)
  }

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  if (methodName === 'fromIter') {
    newMethodBody = newMethodBody.replace(/\(\s*key\s*,\s*value\s*\)/g, 'entry')

    newMethodBody = newMethodBody.replace(/\b(key|value)\b/g, (name) => name == 'key' ? 'entry.0' : 'entry.1')
  }

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  let bodyVarNames = { buckets: 0, capacity: 1, edgeEntry: 2 }

  newMethodBody = newMethodBody.replace(/let\s*\(\s*_\s*,\s*_\s*,\s*newEdgeEntry\s*\)\s*=\s*new([^;]+)/g, 'let newEdgeEntry = new$1.2')

  newMethodBody = newMethodBody.replace(/switch\s*\(\s*(entry|bucketPrevEntry)([^)]*?)\s*\)/g, 'switch ($1$2.1)')

  newMethodBody = newMethodBody.replace(/case\s*\(\s*_\s*,\s*null\s*,\s*_\s*,\s*_\s*\)/g, 'case (null)')

  newMethodBody = newMethodBody.replace(/let\s*\([^)]+\)\s*=\s*map\s*\.\s*body/g, 'let body = map.body')

  newMethodBody = newMethodBody.replace(/\b(buckets|capacity|edgeEntry)\b(?! limit reached)/g, (name) => `body.${bodyVarNames[name]}`)

  if (/let\s+body/.test(newMethodBody) && newMethodBody.match(/\bbody\b/g).length == 3) {
    newMethodBody = newMethodBody.replace(/ *let body = map\.body\s*;\s*\n/, '')

    newMethodBody = newMethodBody.replace(/\bbody\b/g, 'map.body')
  }

  newMethodBody = newMethodBody.replace(/\(\s*getHash\s*,\s*areEqual\s*\)/g, 'hashUtils')

  newMethodBody = newMethodBody.replace(/\b(getHash|areEqual)\b/g, (name) => name == 'getHash' ? 'hashUtils.0' : 'hashUtils.1')

  optimizedMap = optimizedMap.replace(methodBody, newMethodBody)
}

fs.writeFileSync('./src/Map/optimized.mo', optimizedMap)
