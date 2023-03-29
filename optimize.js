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

let capitalize = (string) => string.charAt(0).toUpperCase() + string.slice(1)

let changeDirection = (body) => {
  body = body.replace(/(DEQ_PREV|DEQ_NEXT)/g, (item) => item === 'DEQ_PREV' ? 'DEQ_NEXT' : 'DEQ_PREV')

  body = body.replace(/(deqPrev|deqNext)/g, (item) => item === 'deqPrev' ? 'deqNext' : 'deqPrev')

  body = body.replace(/(newDeqPrev|newDeqNext)/g, (item) => item === 'newDeqPrev' ? 'newDeqNext' : 'newDeqPrev')

  body = body.replace(/(deqFirst|deqLast)/g, (item) => item === 'deqFirst' ? 'deqLast' : 'deqFirst')

  return body
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

let utilsBody = getBody(utils, /\bmodule\b/).replace(/(\S) {2,}/g, '$1 ')

let utilsTypes = getBody(utilsBody, / +public type HashUtils/, { delimLeft: '(', delimRight: ')' })

let utilsMethods = getBody(utilsBody, / +func hashNat32Helper/, { bodyStarted: true, bodyOnly: true })

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

let structs = [[map, 'map', './src/Map/optimized.mo'], [set, 'set', './src/Set/optimized.mo']]

for (let [struct, type, path] of structs) {
  let optimizedStruct = struct

  optimizedStruct = optimizedStruct.replace(/(\S) {2,}/g, '$1 ')

  optimizedStruct = optimizedStruct.replace(/};\s*$/, `\n${'/'.repeat(148)}\n\n${utilsMethods}\n};\n`)

  optimizedStruct = optimizedStruct.replace(/ +public type HashUtils[^;]+;/, `${utilsTypes};`)

  optimizedStruct = optimizedStruct.replace(/import Utils[^;]+;/, '')

  optimizedStruct = optimizedStruct.replace(/public let {[^}]+} = Utils;/g, '')

  optimizedStruct = optimizedStruct.replace(/arg1: Any, arg2: Any/, '')

  optimizedStruct = optimizedStruct.replace(/let (nullHash|MOVE|REMOVE|POP|CYCLE|(PUT|ADD|UPDATE|REPLACE)(_MOVE)?) =[^;]+;\n/g, '')

  let structWithUtils = optimizedStruct

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  let linksKey = type === 'map' ? '3' : '2'

  let hashKey = type === 'map' ? '2' : '1'

  let checkRemoved = `links[BRANCH_1].${hashKey} != hash or links[BRANCH_2].${hashKey} != hash`

  let checkRemovedVar = `links[BRANCH_1].${hashKey} != hashVar or links[BRANCH_2].${hashKey} != hashVar`

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

    if (/^(constant|isSome|negate|reject|makeOption|boolToOption|unwrap|rootKey|entryValue|getSibling)$|Helper$/.test(methodName)) {
      optimizedStruct = optimizedStruct.replace(methodBody, '')

      continue
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (/^(createRoot|clear)$/.test(methodName)) {
      let isClear = methodName.startsWith('clear')

      let linksReplaceBody = ''

      for (let i = 1; i <= 6; i++) {
        linksReplaceBody = `${linksReplaceBody}rootLinksVar[${i === 5 ? 'DEQ_PREV' : i === 6 ? 'DEQ_NEXT' : `BRANCH_${i}`}] := rootVar;\n`
      }

      if (!isClear) {
        let newRootReplaceBody = 'let rootLinksVar = $2;\nlet rootVar = ($1, rootLinksVar);'

        newMethodBody = newMethodBody.replace(/let root = \((\w+, \w+(?:, \w+)?), ([^;]+)\);/, newRootReplaceBody)

        newMethodBody = newMethodBody.replace(/for \(index in root[^;]+;/, linksReplaceBody)

        newMethodBody = newMethodBody.replace(/return root;/, 'return rootVar;')
      }

      if (isClear) {
        let linkVars = `let rootVar = root;\nlet rootLinksVar = rootVar.${linksKey};\n\n`

        newMethodBody = newMethodBody.replace(/for \(index in root[^;]+;/, `${linkVars}${linksReplaceBody}`)
      }
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (/^cloneEntry?$/.test(methodName)) {
      let branchVars = ''
      let branchList = ''

      for (let i = 1; i <= 4; i++) branchVars = `${branchVars}let branch${i} = linksVar[BRANCH_${i}];\n`

      for (let i = 1; i <= 4; i++) branchList = `${branchList}if (branch${i}.${hashKey} != ROOT) cloneEntry(branch${i}, newRoot) else newRoot,\n`

      let valueParam = type === 'map' ? ', value' : ''

      let newMethodReplaceBody = `\nlet linksVar = links;\n${branchVars}`

      newMethodReplaceBody = `${newMethodReplaceBody}\nlet newEntry = (key${valueParam}, hash, [\nvar ${branchList}newRoot,\nnewRoot,\n]);`

      newMethodReplaceBody = `${newMethodReplaceBody}\n\nlinksVar[TEMP_LINK] := newEntry;\n\nnewEntry;\n`

      newMethodBody = newMethodBody.replace(methodBodyOnly, newMethodReplaceBody)
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (/^peek(Front)?$/.test(methodName)) {
      if (type === 'map') {
        let peekBody = `\nlet entry = map.0.3[DEQ_PREV];\n\nswitch (value) { case (?someValue) ?(key, someValue); case (_) null };\n`

        newMethodBody = newMethodBody.replace(methodBodyOnly, peekBody)
      }

      if (type === 'set') {
        let peekBody = `\nlet entry = map.0.2[DEQ_PREV];\n\nif (hash == ROOT) null else ?key;\n`

        newMethodBody = newMethodBody.replace(methodBodyOnly, peekBody)
      }
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (/^(get|has|contains)$/.test(methodName)) {
      let isHas = methodName.startsWith('has')
      let isContains = methodName.startsWith('contains')

      newMethodBody = newMethodBody.replace(methodBodyOnly, methods.getHelper)

      newMethodBody = newMethodBody.replace(/\bhash\b/g, 'hashVar').replace(/(if \(hash)/, 'let hashVar = hash;\n\n$1')

      if ((isHas || isContains) && type === 'map') {
        newMethodBody = newMethodBody.replace(/return (null|value)/g, (item) => `return ${item.endsWith('value') ? 'true' : 'false'}`)
      }

      if (isContains) {
        newMethodBody = newMethodBody.replace(/return (true|false)/g, 'return ?$1')

        newMethodBody = newMethodBody.replace(/(let hashParam = )/, 'if (size[SIZE] == 0) return null;\n\n$1')
      }
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (/^(put|set|add|replace|update)(Move)?(Front|Before|After)?$/.test(methodName)) {
      let isFront = methodName.endsWith('Front') || methodName.endsWith('After')
      let isPut = methodName.startsWith('put')
      let isSet = methodName.startsWith('set')
      let isAdd = methodName.startsWith('add')
      let isReplace = methodName.startsWith('replace')
      let isUpdate = methodName.startsWith('update')
      let isPlace = methodName.endsWith('Before') || methodName.endsWith('After')
      let isMove = isPlace || methodName.endsWith('Move') || methodName.endsWith('MoveFront')

      newMethodBody = newMethodBody.replace(methodBodyOnly, methods.putHelper)

      newMethodBody = newMethodBody.replace(/newEntry[^;]+:= deqPrev;/g, '')

      newMethodBody = newMethodBody.replace(/place, place]/g, isFront ? 'place, deqPrev]' : 'deqPrev, place]')

      let updateBody = type === 'map' ? getBody(newMethodBody, /if \(method & MOVE/, { bodyStarted: true, bodyOnly: true }) : ''
      let replaceValueSwitch = type === 'map' ? getBody(updateBody, /else switch \(getNewValue\(keyParam, value\)\)/) : ''
      let replaceValueBody = type === 'map' ? getBody(replaceValueSwitch, /case \(newValue\)/, { bodyOnly: true }) : ''

      if (!isMove && (isPut || isSet || isAdd) && type === 'map') {
        let switchBody = getBody(newMethodBody, /switch \(getNewValue\(keyParam, null\)\)/)
        let caseBody = getBody(switchBody, /case \(newValue\)/, { bodyOnly: true })

        newMethodBody = newMethodBody.replace(switchBody, `${caseBody.replace(/newValue/g, '?newValue')}`)
      }

      if (isReplace) {
        let switchBody = getBody(newMethodBody, /switch \(getNewValue/)

        newMethodBody = newMethodBody.replace(switchBody, 'return null;')
      }

      if (!isMove && (isPut || isSet || isReplace) && type === 'map') {
        newMethodBody = newMethodBody.replace(updateBody, replaceValueBody.replace(/newValue/g, '?newValue'))
      }

      if (!isMove && type === 'set') {
        let moveBody = getBody(newMethodBody, /if \(method & MOVE/)

        newMethodBody = newMethodBody.replace(moveBody, '')
      }

      if (isAdd && type === 'map') {
        newMethodBody = newMethodBody.replace(updateBody, `return value;\n`)
      }

      if (!isMove && isUpdate) {
        newMethodBody = newMethodBody.replace(updateBody, `${replaceValueSwitch.replace('else switch', 'switch')};\n`)
      }

      if (isMove && type === 'map') {
        let moveBody = getBody(newMethodBody, /if \(method & MOVE/, { bodyOnly: true })
        let valueSwitchBody = getBody(moveBody, /let newValue = switch/)

        moveBody = `\nlet linksVar = links;${moveBody.replace(/\blinks\b/g, 'linksVar')}`

        if (!isUpdate) {
          moveBody = moveBody.replace(valueSwitchBody, '')

          moveBody = moveBody.replace(/(let newEntry = )\(([^,]+), ([^,]+), ([^,]+), (\[var links[^;]+)\);/, '$1(\n$2,\n$3,\n$4,\n$5,\n);')

          moveBody = moveBody.replace('newValue', 'switch (newValue) { case (null) value; case (_) newValue }')
        }

        newMethodBody = newMethodBody.replace(updateBody, moveBody)
      }

      if (isMove && !isUpdate && type === 'map') {
        newMethodBody = newMethodBody.replace('getNewValue(keyParam, null)', 'valueParam')

        newMethodBody = newMethodBody.replace('case (newValue) {', 'case (_) {')
      }

      if (isMove && type === 'set') {
        let moveBody = getBody(newMethodBody, /if \(method & MOVE/)
        let moveBodyOnly = getBody(moveBody, 0, { bodyOnly: true })

        newMethodBody = newMethodBody.replace(moveBody, `\nlet linksVar = links;${moveBodyOnly.replace(/\blinks\b/g, 'linksVar')}`)
      }

      if (!isUpdate) {
        newMethodBody = newMethodBody.replace(/newValue/g, 'valueParam')
      }

      newMethodBody = newMethodBody.replace(/if \(method & UPDATE > 0\) \??([^;]+) else ([^;]+)/g, isUpdate ? '$1' : '$2')

      newMethodBody = newMethodBody.replace(/if \(method &[^;]+;/g, '')

      if (!isPlace) {
        let placeLogicBody = getBody(newMethodBody, /let placeKeyParam = switch/, { bodyStarted: true, bodyOnly: true })
        let mainLoopBody = getBody(newMethodBody, /if \(placeHash == ROOT/, { bodyOnly: true })

        newMethodBody = newMethodBody.replace(placeLogicBody, `loop {${mainLoopBody}};\n`)

        newMethodBody = newMethodBody.replace(/\bplace\b/g, 'root')

        newMethodBody = newMethodBody.replace(/placeLinks/g, `root.${linksKey}`)
      }

      newMethodBody = newMethodBody.replace(/\broot\b/g, 'rootVar')

      newMethodBody = newMethodBody.replace(/(var parent =)/, '\nlet rootVar = root;\n$1')

      if (!isPlace && !(isReplace && !isMove)) {
        newMethodBody = newMethodBody.replace(/rootVar\.(2|3)/g, 'rootLinksVar')

        newMethodBody = newMethodBody.replace(/(var entry =)/, `\nlet rootLinksVar = parent.${linksKey};\n$1`)
      }

      if (isPlace || (isReplace && !isMove)) {
        newMethodBody = newMethodBody.replace('var entry = rootVar', 'var entry = parent')
      }

      newMethodBody = newMethodBody.replace(/\bhash\b/g, 'hashVar').replace(/(if \(hash)/, 'let hashVar = hash;\n\n$1')

      newMethodBody = newMethodBody.replace(/\bplaceHash\b/g, 'placeHashVar').replace(/(if \(placeHash)/, 'let placeHashVar = placeHash;\n\n$1')

      if (isSet || isAdd && type === 'set') {
        newMethodBody = newMethodBody.replace(/return[^;]+;/g, 'return;')
      }

      methods[methodName] = getBody(newMethodBody, 0, { bodyOnly: true })
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (/^(remove|delete|pop(Front)?|cycle(Front)?)$/.test(methodName)) {
      let isFront = methodName.endsWith('Front')
      let isPop = methodName.startsWith('pop')
      let isCycle = methodName.startsWith('cycle')
      let isDelete = methodName.startsWith('delete')
      let isRemove = isDelete || methodName.startsWith('remove')

      newMethodBody = newMethodBody.replace(methodBodyOnly, methods.removeHelper)

      newMethodBody = newMethodBody.replace(/\(?if \(method == REMOVE\) ([^{;]+) else ([^);]+)\)?/g, isRemove ? '$1' : '$2')

      newMethodBody = newMethodBody.replace(/var leafParent = parent/, 'var leafParent = leaf')

      if (!((isPop || isCycle) && type === 'set')) {
        newMethodBody = newMethodBody.replace(/if \(hashParam[^;]+;/, '')
      }

      let loopBody = getBody(newMethodBody, /let hashParam =/, { bodyStarted: true, bodyOnly: true })
      let updateBody = getBody(newMethodBody, /if \(method == CYCLE/, { bodyStarted: true, bodyOnly: true })
      let cycleBody = getBody(newMethodBody, /if \(method == CYCLE/, { bodyOnly: true })
      let removeBody = getBody(newMethodBody, /else {\s+let deqPrev =/, { bodyOnly: true })

      if (isRemove) {
        newMethodBody = newMethodBody.replace(updateBody, removeBody.replace(/\blinks\b/g, 'linksVar'))

        newMethodBody = newMethodBody.replace(/(let deqPrev =)/g, 'let linksVar = links;\n$1')

        newMethodBody = newMethodBody.replace(/return \?\(key, unwrap\(value\)\)/, 'return value')

        newMethodBody = newMethodBody.replace(/\s*(var shiftingHash =)/, '\n$1')

        newMethodBody = newMethodBody.replace(/\bhash\b/g, 'hashVar').replace(/(if \(hash)/, 'let hashVar = hash;\n\n$1')

        newMethodBody = newMethodBody.replace(/\broot\b/g, 'rootVar')

        newMethodBody = newMethodBody.replace(/(var parent =)/, '\nlet rootVar = root;\n$1')

        newMethodBody = newMethodBody.replace('var entry = rootVar', 'var entry = parent')

        if (type === 'set') {
          newMethodBody = newMethodBody.replace(/return (null|\?key)/g, (item) => `return ${item.endsWith('key') ? 'true' : 'false'}`)
        }
      }

      if (isPop && type === 'map') {
        newMethodBody = newMethodBody.replace(loopBody, `switch (deqLast.1) {\ncase (?someValue) {\n${loopBody}};\n\ncase (_) null;\n};\n`)

        newMethodBody = newMethodBody.replace(updateBody, removeBody.replace(/\blinks\b/g, 'linksVar'))

        newMethodBody = newMethodBody.replace(/(let deqPrev =)/g, 'let linksVar = links;\n$1')

        newMethodBody = newMethodBody.replace(/(deqNext\.(?:3|2)\[DEQ[^;]+;)\s+(deqPrev\.(?:3|2)\[DEQ[^;]+;)/, '$2\n$1')

        newMethodBody = newMethodBody.replace(/\s*let deqNext =[^;]+;/, '')

        newMethodBody = newMethodBody.replace(/deqNext|root/g, 'rootVar')

        newMethodBody = newMethodBody.replace(/rootVar\.(2|3)/g, 'rootLinksVar')

        let switchVars = `let rootVar = root;\nlet rootLinks = rootVar.${linksKey};\nlet deqLast = rootLinks[DEQ_PREV];`

        newMethodBody = newMethodBody.replace(/(switch \(deqLast)/, `${switchVars}\n\n$1`)

        newMethodBody = newMethodBody.replace(/let hashParam =[^;]+;\s+/, `let hashParam = deqLast.${hashKey};\n`)

        newMethodBody = newMethodBody.replace(/else if \(hash == ROOT\) {[^}]+}/, '')

        newMethodBody = newMethodBody.replace('unwrap(value)', 'someValue')
      }

      if (isPop && type === 'set') {
        newMethodBody = newMethodBody.replace(updateBody, removeBody.replace(/\blinks\b/g, 'linksVar'))

        newMethodBody = newMethodBody.replace(/(let deqPrev =)/g, 'let linksVar = links;\n$1')

        newMethodBody = newMethodBody.replace(/(deqNext\.(?:3|2)\[DEQ[^;]+;)\s+(deqPrev\.(?:3|2)\[DEQ[^;]+;)/, '$2\n$1')

        newMethodBody = newMethodBody.replace(/\s*let deqNext =[^;]+;/, '')

        newMethodBody = newMethodBody.replace(/deqNext|root/g, 'rootVar')

        newMethodBody = newMethodBody.replace(/rootVar\.(2|3)/g, 'rootLinksVar')

        newMethodBody = newMethodBody.replace(/(let hashParam =)/, `let rootVar = root;\nlet rootLinks = rootVar.${linksKey};\n$1`)

        newMethodBody = newMethodBody.replace(/else if \(hash == ROOT\) {[^}]+}/, '')
      }

      if (isCycle && type === 'map') {
        newMethodBody = newMethodBody.replace(loopBody, `switch (deqLast.1) {\ncase (?someValue) {\n${loopBody}};\n\ncase (_) null;\n};\n`)

        newMethodBody = newMethodBody.replace(updateBody, cycleBody.replace(/\blinks\b/g, 'linksVar'))

        newMethodBody = newMethodBody.replace(/newEntry[^;]+:= deqFirst;/g, '')

        newMethodBody = newMethodBody.replace(/root, root]/g, isFront ? 'deqFirst, root]' : 'root, deqFirst]')

        newMethodBody = newMethodBody.replace(/(let deqFirst =)/g, 'let linksVar = links;\n$1')

        newMethodBody = newMethodBody.replace(/\broot\b/g, 'rootVar')

        newMethodBody = newMethodBody.replace(/rootVar\.(2|3)/g, 'rootLinksVar')

        let switchVars = `let rootVar = root;\nlet rootLinks = rootVar.${linksKey};\nlet deqLast = rootLinks[DEQ_PREV];`

        newMethodBody = newMethodBody.replace(/(switch \(deqLast)/, `${switchVars}\n\n$1`)

        newMethodBody = newMethodBody.replace(/let hashParam =[^;]+;\s+/, `let hashParam = deqLast.${hashKey};\n`)

        newMethodBody = newMethodBody.replace(/else if \(hash == ROOT\) {[^}]+}/, '')

        newMethodBody = newMethodBody.replace('unwrap(value)', 'someValue')
      }

      if (isCycle && type === 'set') {
        newMethodBody = newMethodBody.replace(updateBody, cycleBody.replace(/\blinks\b/g, 'linksVar'))

        newMethodBody = newMethodBody.replace(/newEntry[^;]+:= deqFirst;/g, '')

        newMethodBody = newMethodBody.replace(/root, root]/g, isFront ? 'deqFirst, root]' : 'root, deqFirst]')

        newMethodBody = newMethodBody.replace(/(let deqFirst =)/g, 'let linksVar = links;\n$1')

        newMethodBody = newMethodBody.replace(/\broot\b/g, 'rootVar')

        newMethodBody = newMethodBody.replace(/rootVar\.(2|3)/g, 'rootLinksVar')

        newMethodBody = newMethodBody.replace(/(let hashParam =)/, `let rootVar = root;\nlet rootLinks = rootVar.${linksKey};\n$1`)

        newMethodBody = newMethodBody.replace(/else if \(hash == ROOT\) {[^}]+}/, '')
      }

      if (!isCycle) {
        let leafReplaceBody = getBody(newMethodBody, /if \(leafIndex/, { bodyStarted: true, bodyOnly: true })
        let branchLoopBody = getBody(newMethodBody, / +if \(leafLinks/, { bodyStarted: true, bodyOnly: true })
        let branchCheckBody = getBody(newMethodBody, / +if \(leafLinks/, { bodyOnly: true })

        let newBranchLoopBody = `let leafLinksVar = leaf.${linksKey};`
        let branchReplaceBody = ''

        for (let i = 1; i <= 4; i++) {
          if (i > 1) newBranchLoopBody = `${newBranchLoopBody} else {`

          branchReplaceBody = `${branchReplaceBody}\nleafLinksVar[BRANCH_${i}] := linksVar[BRANCH_${i}];`

          let newBranchCheckBody = branchCheckBody.replace(/leafLinks\[BRANCH_1\]/, `branch${i}`).replace('BRANCH_1', `BRANCH_${i}`)

          newBranchLoopBody = `${newBranchLoopBody}\nlet branch${i} = leafLinksVar[BRANCH_${i}];\n`

          newBranchLoopBody = `${newBranchLoopBody}\nif (branch${i}.${hashKey} != ROOT) {${newBranchCheckBody}}`
        }

        leafReplaceBody = leafReplaceBody.replace(/for \(index in leaf[^;]+;/, branchReplaceBody)

        newMethodBody = newMethodBody.replace(branchLoopBody, `${newBranchLoopBody} else {\n${leafReplaceBody}};\n};\n};\n};\n`)
      }

      if (isDelete) newMethodBody = newMethodBody.replace(/return[^;]+;/g, 'return;')
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (/^clone(Desc)?$/.test(methodName)) {
      newMethodBody = newMethodBody.replace(methodBodyOnly, methods.cloneHelper)

      newMethodBody = newMethodBody.replace(/root\.(2|3)/g, 'rootLinks')

      newMethodBody = newMethodBody.replace(/\broot\b/g, 'rootVar')

      newMethodBody = newMethodBody.replace('var entry = rootVar;', '')

      let branchVars = ''

      for (let i = 1; i <= 4; i++) branchVars = `${branchVars}let branch${i} = rootLinks[BRANCH_${i}];\n`

      let entryData = type === 'map' ? '(rootVar.0, null, ROOT, newRootLinks):Entry<K, V>' : '(rootVar.0, ROOT, newRootLinks)'

      let newRootReplace = `let newRootLinks = [var rootVar, rootVar, rootVar, rootVar, rootVar, rootVar];\nlet newRoot = ${entryData};`

      newRootReplace = `let rootVar = root;\nvar entry = rootVar;\n\nlet rootLinks = links;\n${branchVars}\n\n${newRootReplace}`

      newMethodBody = newMethodBody.replace(/let newRoot =[^;]+;/, newRootReplace)

      let branchList = ''

      for (let i = 1; i <= 4; i++) {
        branchList = `${branchList}newRootLinks[BRANCH_${i}] := if (branch${i}.${hashKey} != ROOT) cloneEntry(branch${i}, newRoot) else newRoot;\n`
      }

      let loopBody = getBody(newMethodBody, /for \(index in newRoot/)

      newMethodBody = newMethodBody.replace(loopBody, branchList)
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (/^(map(Filter)?|filter)(Desc)?$/.test(methodName)) {
      let isDesc = methodName.endsWith('Desc')
      let isMapFilter = methodName.startsWith('mapFilter')
      let isMap = !isMapFilter && methodName.startsWith('map')
      let isFilter = methodName.startsWith('filter')

      let putMethod = isDesc ? changeDirection(methods.putMove) : methods.putMove

      let checkRemoved = `itemLinks[BRANCH_1].${hashKey} != itemHash or itemLinks[BRANCH_2].${hashKey} != itemHash`

      newMethodBody = newMethodBody.replace(methodBodyOnly, methods.mapFilterHelper)

      newMethodBody = newMethodBody.replace(/\b(key|value|hash|links)\b/g, (name) => `item${capitalize(name)}`)

      newMethodBody = newMethodBody.replace(/\bentry\b/g, 'item')

      newMethodBody = newMethodBody.replace(/ignore putHelper\([^;]+\);/, `{${putMethod}};\n`)

      newMethodBody = newMethodBody.replace(/switch \(valueParam\) {[^}]+valueParam }/, 'valueParam')

      if (type === 'map') {
        newMethodBody = newMethodBody.replace(/(let newEntry = )\(([^,]+), ([^,]+), ([^,]+), (\[var links[^;]+)\);/, '$1(\n$2,\n$3,\n$4,\n$5,\n);')

        let switchBody = getBody(newMethodBody, /switch \(valueParam/)
        let someValueBody = getBody(switchBody, /case \(_/, { bodyOnly: true })

        newMethodBody = newMethodBody.replace(switchBody, someValueBody)
      }

      newMethodBody = newMethodBody.replace(/\s+let (rootVar|rootLinksVar) =[^;]+;/g, '')

      newMethodBody = newMethodBody.replace('size[SIZE]', 'sizeVar')

      newMethodBody = newMethodBody.replace(/(getHash|areEqual)/g, '$1Var')

      let newMapReplace = `let rootVar = createRoot<$1>(getNullKey());\nlet rootLinks = rootVar.${linksKey};`

      newMapReplace = `${newMapReplace}\nlet getHashVar = hashUtils.0;\nlet areEqualVar = hashUtils.1;\nvar sizeVar = 0:Nat32;`

      newMethodBody = newMethodBody.replace(/let newMap = new<([^>]+)>\(hashUtils\);/g, newMapReplace)

      newMethodBody = newMethodBody.replace(/item := getSibling[^;]+;/, `item := item.${linksKey}[DEQ_NEXT];`)

      newMethodBody = newMethodBody.replace(/if \(itemHash == ROOT[^;]+;/, '')

      newMethodBody = newMethodBody.replace('unwrap(itemValue)', 'someValue')

      newMethodBody = newMethodBody.replace(/(case \(null\) {};)/, '$1\n')

      newMethodBody = newMethodBody.replace(/return[^;]+;/g, 'break loopBody;')

      if (type === 'map') {
        let iterationBody = getBody(newMethodBody, /switch \(mapEntry/)
        let caseBody = getBody(iterationBody, /case \(newValue/, { bodyOnly: true })

        if (isMapFilter) {
          let iterationBodyReplace = `switch (itemValue) {\ncase (?someValue)  label loopBody if (${checkRemoved}) {\n${iterationBody};\n};`

          iterationBodyReplace = `${iterationBodyReplace}\n\ncase (_) return (rootVar, [var sizeVar]);\n};`

          newMethodBody = newMethodBody.replace(iterationBody, iterationBodyReplace)

          newMethodBody = newMethodBody.replace(/keyParam|valueParam/g, (name) => name === 'keyParam' ? 'item.0' : 'newValue')
        }

        if (isMap) {
          let iterationBodyReplace = `switch (itemValue) {\ncase (?someValue)  label loopBody if (${checkRemoved}) {${caseBody}};`

          iterationBodyReplace = `${iterationBodyReplace}\n\ncase (_) return (rootVar, [var sizeVar]);\n};`

          newMethodBody = newMethodBody.replace(iterationBody, iterationBodyReplace)

          newMethodBody = newMethodBody.replace(/keyParam|valueParam/g, (name) => name === 'keyParam' ? 'item.0' : '?mapEntry(key, someValue)')
        }

        if (isFilter) {
          let iterationBodyReplace = 'switch (itemValue) {\ncase (?someValue) label loopBody'

          iterationBodyReplace = `${iterationBodyReplace} if ((${checkRemoved}) and acceptEntry(itemKey, someValue)) {${caseBody}};`

          iterationBodyReplace = `${iterationBodyReplace}\n\ncase (_) return (rootVar, [var sizeVar]);\n};`

          newMethodBody = newMethodBody.replace(iterationBody, iterationBodyReplace)

          newMethodBody = newMethodBody.replace(/keyParam|valueParam/g, (name) => name === 'keyParam' ? 'item.0' : 'item.1')

          newMethodBody = newMethodBody.replace(/createRoot<K, V2>/, 'createRoot<K, V>')
        }
      }

      if (type === 'set') {
        let iterationCheckReplace = `let hashVar = itemHash;\n\nif (hashVar == ROOT) {\nreturn (rootVar, [var sizeVar]);\n}`

        iterationCheckReplace = `${iterationCheckReplace} else if ((${checkRemoved.replace(/itemHash/g, 'hashVar')}) and acceptEntry(item.0))`

        newMethodBody = newMethodBody.replace(/if \(acceptEntry\(itemKey\)\)/g, iterationCheckReplace)

        newMethodBody = newMethodBody.replace(/keyParam/g, 'item.0')

        newMethodBody = newMethodBody.replace('loop {', 'loop label loopBody {')
      }
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (/^(keys|vals|entries)(From)?(Desc)?$/.test(methodName)) {
      let isKeys = methodName.startsWith('keys')
      let isVals = methodName.startsWith('vals')
      let isEntries = methodName.startsWith('entries')
      let isFrom = methodName.endsWith('From') || methodName.endsWith('FromDesc')

      let checkSiblingRemoved = `$1Links[BRANCH_1].${hashKey} != $1Hash or $1Links[BRANCH_2].${hashKey} != $1Hash`

      let checkSiblingRemovedVar = `$1Links[BRANCH_1].${hashKey} != hashVar or $1Links[BRANCH_2].${hashKey} != hashVar`

      newMethodBody = newMethodBody.replace(methodBodyOnly, methods.iterateHelper)

      newMethodBody = newMethodBody.replace(/let \((key|(deqPrev|deqNext)Key), [^;]+;/g, '')

      newMethodBody = newMethodBody.replace(/entry\.(2|3)\[DEQ\w+\] := deq\w+;/g, '')

      newMethodBody = newMethodBody.replace(/return (?!let)/g, '')

      let iterCondition = `entry := links[$1];\n\nloop if (${checkRemoved} or hash == ROOT) return iter else {\nentry := links[$1];\n};`

      let keysCondition = `entry := links[$1];\n\nloop {\nlet hashVar = hash;\n\nif (hashVar == ROOT) return null`

      keysCondition = `${keysCondition} else if (${checkRemovedVar}) return ?key else {\nentry := links[$1];\n};\n};`

      let keysPeekCondition = `var $1 = links[$2];\n\nloop {\nlet hashVar = $1Hash;\n\nif (hashVar == ROOT) return null`

      keysPeekCondition = `${keysPeekCondition} else if (${checkSiblingRemovedVar}) {\nlinks[$2] := $1;\n\nreturn ?$1Key;\n}`

      keysPeekCondition = `${keysPeekCondition} else {\n$1 := $1Links[$2];\n};\n};`

      let valsCondition = `entry := links[$1];\n\nloop if (${checkRemoved} or hash == ROOT) return value else {\nentry := links[$1];\n};`

      let valsPeekCondition = `var $1 = links[$2];\n\nloop if (${checkSiblingRemoved} or $1Hash == ROOT) {\nlinks[$2] := $1;\n\nreturn $1Value;\n}`

      valsPeekCondition = `${valsPeekCondition} else {\n$1 := $1Links[$2];\n};`

      let entriesCondition = `entry := links[$1];\n\nloop switch (value) {\ncase (?someValue) if (${checkRemoved})`

      entriesCondition = `${entriesCondition} return ?(key, someValue) else {\nentry := links[$1];\n};\n\ncase (_) return null;\n};`

      let entriesPeekCondition = `var $1 = links[$2];\n\nloop switch ($1Value) {\ncase (?someValue) if (${checkSiblingRemoved})`

      entriesPeekCondition = `${entriesPeekCondition} {\nlinks[$2] := $1;\n\nreturn ?($1Key, someValue);\n}`

      entriesPeekCondition = `${entriesPeekCondition} else {\n$1 := $1Links[$2];\n};\n\ncase (_) return null;\n};`

      let entriesCurrentReplace = 'switch (value) { case (?someValue) ?(key, someValue); case (_) null }'

      if (!isFrom) {
        let placeLogicBody = getBody(newMethodBody, /let keyParam = switch/, { bodyStarted: true, bodyOnly: true })
        let iterObjectBody = getBody(newMethodBody, /let iter =/)

        newMethodBody = newMethodBody.replace(placeLogicBody, `var started = false;\nvar entry = root;\n\n${iterObjectBody};\n`)
      }

      if (isKeys) {
        newMethodBody = newMethodBody.replace(/entry := getSibling\(entry, (DEQ\w+)\);\s+if \(hash[^;]+;/g, keysCondition)

        newMethodBody = newMethodBody.replace(/entry := getSibling\(entry, (DEQ\w+)\);\s+iter;/g, iterCondition)

        newMethodBody = newMethodBody.replace(/let (deqPrev|deqNext) = getSibling\(entry, (DEQ\w+)\);\s+if \(deq[^;]+;/g, keysPeekCondition)

        newMethodBody = newMethodBody.replace(/mapEntry\(key, unwrap\(value\)\)/, '?key')
      }

      if (isVals) {
        newMethodBody = newMethodBody.replace(/entry := getSibling\(entry, (DEQ\w+)\);\s+if \(hash[^;]+;/g, valsCondition)

        newMethodBody = newMethodBody.replace(/entry := getSibling\(entry, (DEQ\w+)\);\s+iter;/g, iterCondition)

        newMethodBody = newMethodBody.replace(/let (deqPrev|deqNext) = getSibling\(entry, (DEQ\w+)\);\s+if \(deq[^;]+;/g, valsPeekCondition)

        newMethodBody = newMethodBody.replace(/if \(hash[^;]+mapEntry\(key, unwrap\(value\)\)/, 'value')
      }

      if (isEntries) {
        newMethodBody = newMethodBody.replace(/entry := getSibling\(entry, (DEQ\w+)\);\s+if \(hash[^;]+;/g, entriesCondition)

        newMethodBody = newMethodBody.replace(/entry := getSibling\(entry, (DEQ\w+)\);\s+iter;/g, iterCondition)

        newMethodBody = newMethodBody.replace(/let (deqPrev|deqNext) = getSibling\(entry, (DEQ\w+)\);\s+if \(deq[^;]+;/g, entriesPeekCondition)

        newMethodBody = newMethodBody.replace(/if \(hash[^;]+mapEntry\(key, unwrap\(value\)\)/, entriesCurrentReplace)
      }

      if (type === 'map') {
        newMethodBody = newMethodBody.replace(/\bT\b/g, () => isKeys ? 'K' : isVals ? 'V' : '(K, V)')
      };

      newMethodBody = newMethodBody.replace(/(var started = hash)/, '$1Var')

      newMethodBody = newMethodBody.replace(/if \(hash == ROOT or hash/, 'let hashVar = hash;\n\nif (hashVar == ROOT or hashVar')
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (/^(find|some|every|forEach)(Desc)?$/.test(methodName)) {
      let isFind = methodName.startsWith('find')
      let isSome = methodName.startsWith('some')
      let isEvery = methodName.startsWith('every')
      let isForEach = methodName.startsWith('forEach')

      newMethodBody = newMethodBody.replace(methodBodyOnly, methods.findHelper)

      newMethodBody = newMethodBody.replace(/entry := getSibling[^;]+;/, `entry := entry.${linksKey}[DEQ_NEXT];`)

      let acceptEntry = type === 'map' ? 'acceptEntry(key, someValue)' : 'acceptEntry(key)'

      let mapEntry = type === 'map' ? 'mapEntry(key, someValue)' : 'mapEntry(key)'

      let findData = type === 'map' ? '?(key, someValue)' : '?key'

      let condition = 'switch (value) {\ncase (?someValue) PLACEHOLDER\ncase (_) return null;\n};'

      if (type === 'set') condition = 'if (hashVar == ROOT) return null else PLACEHOLDER'

      if (isFind) {
        condition = condition.replace('PLACEHOLDER', `if ((${checkRemoved}) and ${acceptEntry}) {\nreturn ${findData};\n};\n`)

        newMethodBody = newMethodBody.replace(/if \(hash == ROOT[^;]+;/, condition)
      }

      if (isSome) {
        condition = condition.replace('PLACEHOLDER', `if ((${checkRemoved}) and ${acceptEntry}) {\nreturn true;\n};\n`)

        condition = condition.replace('return null', 'return false')

        newMethodBody = newMethodBody.replace(/if \(hash == ROOT[^;]+;/, condition)
      }

      if (isEvery) {
        condition = condition.replace('PLACEHOLDER', `if ((${checkRemoved}) and not ${acceptEntry}) {\nreturn false;\n};\n`)

        condition = condition.replace('return null', 'return true')

        newMethodBody = newMethodBody.replace(/if \(hash == ROOT[^;]+;/, condition)
      }

      if (isForEach) {
        condition = condition.replace('PLACEHOLDER', `if (${checkRemoved}) ${mapEntry};`)

        condition = condition.replace('return null', 'return')

        newMethodBody = newMethodBody.replace(/if \(hash == ROOT[^;]+;/, condition)
      }

      if (type === 'set') {
        newMethodBody = newMethodBody.replace(/\bhash\b/g, 'hashVar')

        newMethodBody = newMethodBody.replace(/(if \(hashVar == ROOT)/g, 'let hashVar = hash;\n\n$1')
      }
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (/^fromIter(Map)?(Desc)?$/.test(methodName)) {
      let isDesc = methodName.endsWith('Desc')
      let isMap = methodName.endsWith('Map') || methodName.endsWith('MapDesc')

      let insertMethodName = `${'putMove'}${isDesc ? 'Front' : ''}`

      newMethodBody = newMethodBody.replace(methodBodyOnly, methods.fromIterHelper)

      if (!isMap) {
        let switchBody = getBody(newMethodBody, /switch \(mapItem/)

        newMethodBody = newMethodBody.replace(switchBody, `{${methods[insertMethodName]}}`)
      }

      if (isMap) {
        newMethodBody = newMethodBody.replace(/ignore putHelper\([^;]+\);/, `{${methods[insertMethodName]}};\n`)
      }

      if (type === 'map') {
        let switchBody = getBody(newMethodBody, /switch \(valueParam/)
        let caseBody = getBody(switchBody, /case \(_/, { bodyOnly: true })

        newMethodBody = newMethodBody.replace(switchBody, caseBody)
      }

      newMethodBody = newMethodBody.replace(/switch \(valueParam\) {[^}]+valueParam }/, 'valueParam')

      newMethodBody = newMethodBody.replace(/\s+let (rootVar|rootLinksVar) =[^;]+;/g, '')

      newMethodBody = newMethodBody.replace('size[SIZE]', 'sizeVar')

      newMethodBody = newMethodBody.replace(/(getHash|areEqual)/g, '$1Var')

      let createRootParams = type === 'map' ? '<K, V>' : '<K>'

      let newMapReplace = `let rootVar = createRoot${createRootParams}(getNullKey());\nlet rootLinks = rootVar.${linksKey};`

      newMapReplace = `${newMapReplace}\nlet getHashVar = hashUtils.0;\nlet areEqualVar = hashUtils.1;\nvar sizeVar = 0:Nat32;`

      newMethodBody = newMethodBody.replace(/let map = new(<[^>]+>)?\(hashUtils\);/g, newMapReplace)

      newMethodBody = newMethodBody.replace(/\?\(key, value\)|\?key/g, '?item')

      newMethodBody = newMethodBody.replace(/(for \(item in iter\))/g, '$1 label loopBody')

      newMethodBody = newMethodBody.replace('return map;', '(rootVar, [var sizeVar]);')

      newMethodBody = newMethodBody.replace(/return[^;]+;/g, 'break loopBody;')

      newMethodBody = newMethodBody.replace(/keyParam|valueParam/g, (name) => type === 'set' ? 'item' : name === 'keyParam' ? 'item.0' : '?item.1')
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (/^toArray(Desc)?$/.test(methodName)) {
      newMethodBody = newMethodBody.replace(methodBodyOnly, methods.toArrayHelper)

      newMethodBody = newMethodBody.replace(/return/g, '')
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (/^toArrayMap(Desc)?$/.test(methodName)) {
      newMethodBody = newMethodBody.replace(methodBodyOnly, methods.toArrayMapHelper)

      newMethodBody = newMethodBody.replace(/entry := getSibling[^;]+;/, `entry := entry.${linksKey}[DEQ_NEXT];`)

      let iterationBody = getBody(newMethodBody, /switch \(mapEntry/)

      let [finalArray] = newMethodBody.match(/return tabulateArray[^;]+/)

      newMethodBody = newMethodBody.replace(/if \(hash == ROOT[^;]+;/, '')

      let iterationBodyReplace = `switch (value) {\ncase (?someValue) if (${checkRemoved}) ${iterationBody};\n\ncase (_) ${finalArray};\n};`

      if (type === 'set') iterationBodyReplace = `if (hash == ROOT) {\n${finalArray}\n} else if (${checkRemoved}) ${iterationBody};`

      newMethodBody = newMethodBody.replace(iterationBody, iterationBodyReplace)

      newMethodBody = newMethodBody.replace('unwrap(value)', 'someValue')

      if (type === 'set') {
        newMethodBody = newMethodBody.replace(/\bhash\b/g, 'hashVar')

        newMethodBody = newMethodBody.replace(/(if \(hashVar == ROOT)/g, 'let hashVar = hash;\n\n$1')
      }
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (/^hash(Int|Nat)(8|16|32|64)?$/.test(methodName)) {
      let [, hashMethod, keyValue] = methodBodyOnly.match(/(hashNat(?:32|64)Helper)\(([^;]+)\);/)

      newMethodBody = newMethodBody.replace(methodBodyOnly, methods[hashMethod])

      newMethodBody = newMethodBody.replace(/var hash = key;/, `var hash = ${keyValue};`)
    };

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (!/^(useHash|calcHash|hash\w+)$/.test(methodName)) {
      let entryVars = type === 'map' ? { key: 0, value: 1, hash: 2, links: 3 } : { key: 0, hash: 1, links: 2 }

      let keyAccess = (_, name, key) => `${name}.${entryVars[key.toLowerCase()]}`

      newMethodBody = newMethodBody.replace(/let \(\w+, (\w+, )?\w+, \w+\) = (entry|deqPrev|deqNext|leaf|place|item);/g, '')

      newMethodBody = newMethodBody.replace(/\b(entry|deqPrev|deqNext|leaf|place|item)(Key|Value|Hash|Links)\b/g, keyAccess)

      newMethodBody = newMethodBody.replace(/\b(key|value|hash|links)\b/g, (key) => `entry.${entryVars[key]}`)
    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    if (/(Desc|Front|After)$/.test(methodName)) newMethodBody = changeDirection(newMethodBody)

    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    let bodyVars = { root: 0, size: 1 }

    let hashUtilsVars = { getHash: 0, areEqual: 1, getNullKey: 2 }

    newMethodBody = newMethodBody.replace(/let \([^)]+\) = map;/g, '')

    newMethodBody = newMethodBody.replace(/\b(root|size)\b(?!<)/g, (name) => `map.${bodyVars[name]}`)

    newMethodBody = newMethodBody.replace(/\((getHash|_), (areEqual|_), (getNullKey|_)\)/g, 'hashUtils')

    newMethodBody = newMethodBody.replace(/\b(getHash|areEqual|getNullKey)\b/g, (name) => `hashUtils.${hashUtilsVars[name]}`)

    newMethodBody = newMethodBody.replace(/unwrap\(([^)]+)\)/g, 'switch ($1) { case (?value) value; case (_) trap("unreachable") }')

    newMethodBody = newMethodBody.replace(/return (.+\n *})$/, '$1')

    newMethodBody = newMethodBody.replace(/\b(\w+)Var\b/g, '$1')

    optimizedStruct = optimizedStruct.replace(methodBody, newMethodBody)
  }

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  optimizedStruct = optimizedStruct.replace(/\s+;/g, ';')

  optimizedStruct = optimizedStruct.replace(/\/\/;+/g, '//')

  optimizedStruct = optimizedStruct.replace(/(\/{2,})(\s+\/{2,})+/g, '$1')

  optimizedStruct = optimizedStruct.replace(/{;+/g, '{')

  optimizedStruct = optimizedStruct.replace(/;{2,}/g, ';')

  optimizedStruct = optimizedStruct.replace(/(\S) {2,}/g, '$1 ')

  optimizedStruct = optimizedStruct.replace(/\n[^\S\n]+(?=\n)/g, '\n')

  optimizedStruct = optimizedStruct.replace(/ +\n/g, '\n')

  optimizedStruct = optimizedStruct.replace(/\n{3,}/g, '\n\n')

  optimizedStruct = optimizedStruct.replace(/{\n{2,}/g, '{\n')

  optimizedStruct = optimizedStruct.replace(/\n{2,}( *})/g, '\n$1')

  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  let indent = 0
  let indentArea = true
  let prevChar = ''
  let indentedStruct = ''

  for (char of optimizedStruct) {
    if (indentArea && (char === '}' || char === ']' || char === ')')) indent -= 2

    if (indentArea && char === ' ') continue

    if (indentArea && char != ' ' && char != '\n') {
      if (indent > 0) indentedStruct += ' '.repeat(indent)

      indentArea = false
    }

    if (char === '\n' && (prevChar === '{' || prevChar === '[' || prevChar === '(')) indent += 2

    if (char === '\n') indentArea = true

    if (char != ' ' && char != '\n') prevChar = char

    indentedStruct += char
  }

  fs.writeFileSync(path, indentedStruct)
}
