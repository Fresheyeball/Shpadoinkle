
/*

Generate the reflexive and transitive closure of (<:) from JSFFI.hs

*/


// Read haskell file
const hsText = require('fs').readFileSync('./JSFFI.hs').toString();

const reTypename = "[\\[\\]\\w]+";
  // ^ nb This is very naive and misses cases such as 'Maybe Int'
const reInstance = new RegExp(
  `instance(\\s*{-\\s*gen\\s*-})?\\s+.*?(${reTypename})\\s+<:\\s+(${reTypename})\\s+where`,
  "g"
);

console.log('Searching for instances via', reInstance);

// Parse relevant instances into 'Array { sub :: String, sup :: String }'
const instances0 = (
  Array.from(hsText.matchAll(reInstance))
  .flatMap(match => {
    const isGenerated = !!match[1];
    const sub = match[2];
    const sup = match[3];
    console.log('Found', isGenerated ? 'generated' : 'non-generated', 'instance', sub, '<:', sup);
    if (isGenerated) return [];
    return [{ sub, sup }];
  })
);


// Set of type names
let typeNames = new Set(instances0.flatMap(inst => [inst.sub, inst.sup]));
console.log('Types:', Array.from(typeNames).join(', '));


/*

Initialize relation matrix

  mat[[t1, t2]] is truthy iff 't1 <: t2'

Value 'mat' has type 'Mat' in the following

  type TypeName = string
  type Pair a = [a, a]
  type Mat = Mapping (Pair TypeName) Entry

  type Entry =
    ( { kind: 'initial' }
    | { kind: 'reflexive' }
    | { kind: 'transitive', via: TypeName }
    | undefined
    )

*/
let mat = {};
for (const inst of instances0) {
  console.log('Initial', inst.sub, '<:', inst.sup);
  mat[[inst.sub, inst.sup]] = { kind: 'initial' };
}

console.log('Performing closure');

// Compute reflexive closure
for (const t of typeNames) {
  if (!mat[[t, t]]) {
    console.log('Reflexive', t, '<:', t);
    mat[[t, t]] = { kind: 'reflexive' };
  }
}

// Compute transitive closure
// Naive algorithm; good enough for now
{
  while (true) {
    let didWork = false;

    for (const t1 of typeNames)
    for (const t2 of typeNames)
    for (const t3 of typeNames)
    if (
      mat[[t1, t2]]
      && mat[[t2, t3]]
      && !mat[[t1, t3]]
    ) {
      console.log('Transitive', t1, '<:', t3, 'via', t3);
      mat[[t1, t3]] = { kind: 'transitive', via: t2 };
      didWork = true;
    }

    if (!didWork) break;
  }
}

// Emit
console.log('Closure implementation:\n');
{
  for (const t1 of typeNames)
  for (const t2 of typeNames)
  {
    const entry = mat[[t1, t2]];
    if (!entry) continue;
    switch (entry.kind) {
      case 'initial': break;  // dont re-emit existing (non-generated) instances
      case 'reflexive':
        const t = t1;  // == t2
        emitInstance(t, t, 'rxUp', 'rxDn');
        break;
      case 'transitive':
        const via = entry.via;
        emitInstance(t1, t2, `trUp @${via}`, `trDn @${via}`);
        break;
    }
  }

  function emitInstance (t1, t2, upcast, downcast) {
    console.log(
        `instance {-gen-} ${t1} <: ${t2} where { upcast = ${upcast}; downcast = ${downcast} }`
    );
  }
}

