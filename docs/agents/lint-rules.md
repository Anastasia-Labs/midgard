# Lint Rules

The demo workspace's ESLint config loads a local plugin,
`demo/scripts/lib/eslint-plugin-midgard/`, whose rules check owner rulings
that can be read from the syntax tree. `pnpm --dir demo run lint` runs them
with every other rule, and Midgard Node CI runs that command. Each message
ends with a `Fix:` that says what to write instead. The rules are syntactic:
they follow values through local constants and parameter defaults in one
file, not across files or through types.

## The Baseline

`demo/scripts/lib/eslint-plugin-midgard/baseline.json` lists the violations
each rule already had when it was introduced, by rule, then file (relative to
`demo/`), then the trimmed text of each reported line, with a reason for every
file entry. A listed violation does not fail the lint. A listed line that no
longer occurs does fail it, so fixing a violation and dropping its entry land
in one change: from `demo/`, run
`node scripts/lib/eslint-plugin-midgard/prune-baseline.mjs`, which removes
every stale site and adds none (`--check` only reports).

- Fix a new violation rather than baselining it. An entry is added by hand,
  with a reason that says why the site is correct or who must decide it; the
  plugin's test refuses an entry with no reason, for an unknown rule, or for a
  file that does not exist.
  [script: demo/scripts/lib/eslint-plugin-midgard.test.mjs]
- A new rule must ship fixtures under
  `demo/scripts/lib/eslint-plugin-midgard/fixtures/` with `// ruleid:` and
  `// ok:` cases; the test fails a rule without them, and fails when the
  reported lines differ from the `ruleid` lines.
  [script: demo/scripts/lib/eslint-plugin-midgard.test.mjs]

## Rules

- **Always pass a locale to `localeCompare`.** Without one the order follows
  the host's collation. Canonical JSON is pinned to `"en"`
  (`compareCanonicalJsonKeys`); the watcher's code-unit ordering is deliberate
  and must not be unified with it. The baselined sites are untriaged: several
  feed digests, so changing their collation is a consensus change and an owner
  call. [eslint: midgard/locale-compare-explicit-locale]
- **L1 completion must use `localUPLCEval: true`**, the literal, everywhere
  (see [transaction-finalization.md](transaction-finalization.md)). A
  property, shorthand, destructured binding or assignment with anything else
  is flagged, except between `MIDGARD_UNPUBLISHABLE_DIAGNOSTIC_BEGIN` and
  `_END` comments; an unbalanced marker is flagged too. Blind spot: a value
  set through a spread, a computed key or `Object.assign` is not seen.
  [eslint: midgard/local-uplc-eval]
- **Never attach a script inline in a fault-proof source tree**
  (`demo/midgard-fault-proofs/src/`, `demo/midgard-sdk/src/fraud-proof/`,
  `demo/midgard-watcher/src/fault-proofs/`): fault proofs deploy as reference
  scripts (owner ruling 2026-08-26). Blind spot: it flags `x.attach.<Kind>(...)`
  calls in those trees only, not a witness added another way or a builder
  outside them. [eslint: midgard/fault-proof-reference-scripts-only]
- **Never apply parameters outside the blueprint shape guard.**
  `applyParamsToScript` and `apply_params_to_script` are called only in
  `demo/midgard-sdk/src/fraud-proof/contracts/blueprint.ts`; everything else
  goes through `applyBlueprintParams` (#609). Renaming either on import or in
  a destructuring is flagged too. Blind spot: a call through a namespace
  re-export or a computed name passes.
  [eslint: midgard/apply-params-through-blueprint]
- **An exact fee must not leave a change output.** `setMinFee` plus Lucid's
  change output overpays by about 3k lovelace and breaks every on-chain
  `fee == penalty`. A `setMinFee` passes only when a completion in the same
  function, or one nested with it, visibly turns coin selection off (literal
  `coinSelection: false`, `exactFeeCompleteOptions(...)`, or a constant or
  spread holding one). Blind spot: it cannot follow a build into a completion
  in another function or a helper's default options (the baselined sites), and
  it does not check the explicit remainder output.
  [eslint: midgard/exact-fee-no-change-output]
- **Release an `overrideUTxOs` pin with `clearUTxOOverride()` on the same
  receiver**, after it in the same function or one nested with it. A pin
  persists for every later build on the instance. Blind spot: receivers match
  by source text and order by position, so an early `return` before the
  release, or a release on an alias, passes; builders that inherit someone
  else's pin are not checked. [eslint: midgard/scoped-utxo-override]
- **Never take a validity lower bound from the wall clock without a 30-second
  margin.** The value passed to `.validFrom(...)` or a `validFrom*` property
  is followed through constants, parameter defaults and call arguments to
  `Date.now()`, `new Date().getTime()` or a parameter named like the current
  time (`now`, `nowMs`, `currentTime`, ...); subtracting a constant of at
  least 30,000 ms passes. Blind spot: a subtrahend it cannot evaluate passes, a
  time from another module or a differently named parameter is not followed,
  and `Math.max(bound, now - backoff)` shape is not checked.
  [eslint: midgard/valid-from-wall-clock-margin]
- **Do not register test, demo, bench, e2e or acceptance commands in the
  operator binary** (`demo/midgard-node/src/index.ts`); they belong to
  `demo/midgard-node-tools`. Blind spot: it reads command names, so a
  test command with an operator-sounding name, or one registered from another
  module, passes. [eslint: midgard/node-cli-operator-commands-only]
- **Read, build and write Plutus Data without recursion in production
  source** (`src/` of midgard-core, -validation, -node, -sdk, -watcher and
  -fault-proofs). The carriers admit Data nested thousands of levels deep, and
  a recursive walk overflows the stack. Flagged: the recursive
  `@harmoniclabs/plutus-data` functions (`dataFromCbor`, `dataToCbor`, their
  `Obj` forms, `eqData`, `cloneData`, `dataFromJson`, `dataToJson`);
  `@harmoniclabs/plutus-machine` names other than `BnCEK`, `CEKConst`,
  `CEKError`, `ExBudget`, `PartialBuiltin` and `costModelV3ToBuiltinCosts`;
  `Cbor` from `@harmoniclabs/cbor`; `cborg`; and Lucid `Data.from` /
  `Data.to` in midgard-core and midgard-validation. Use
  `plutusDataFromCborIterative` and `encodeMidgardCekPlutusData` in
  midgard-validation, and
  `@al-ft/midgard-core/plutus-data-lucid-iterative` for Lucid-shaped values.
  The baselined Lucid encoders take repository-built values of bounded depth.
  Blind spot: `.toString()`, `.toJson()` and `.clone()` on a harmonic value,
  and a hand-written recursive walk, are not seen.
  [eslint: midgard/no-recursive-plutus-data]
- **Count L1 depth only in the heads module**
  (`demo/midgard-l1-follower/src/heads.ts`, plan §9): the tip is depth 1,
  `safe` is depth ≥ cd (liveness only), `final` is depth > k. Use `depth`,
  `heightAtDepth`, `isSafe`, `isFinal` or `levelAtDepth` from
  `@al-ft/midgard-l1-follower/heads`. Flagged: a `<`, `<=`, `>` or `>=` that
  reads `confirmationDepth`, `automaticRecoveryMaxDepth`, `securityParameter`
  (or a variant) directly or through a local constant, arithmetic or a call
  argument, together with anything not constant. Shape checks on parameters
  and constants, equality, and values returned by follower-package functions
  pass. Blind spot: a parameter under another name (`finalityDepth`) or from
  another module is not seen; block-mining loop counters are flagged (the
  baselined test drivers). [eslint: midgard/depth-through-heads]
