# L1 size-fit plans: shared primer

Every plan in this directory cites this primer and covers only what is
specific to its contract. Read this first.

## Scope

On the reproducible working-tree blueprint (built 2026-09-01 with the pinned
fork `v1.1.23+5adf783`, 567 validators, SHA-256 `597c3891…`), 51 distinct
compiled scripts have a raw body above the 16,384-byte Cardano transaction
limit and therefore cannot be published as reference scripts. Fifty of them
are planned here; `fraud_proofs/withdrawal_mistag/step_03` is being fixed
separately and is out of scope.

| Group                                       | Scripts | Raw size range (bytes) |
| ------------------------------------------- | ------: | ---------------------- |
| `validation_trace` CEK semantics            |       3 | 45,486 – 94,268        |
| `validation_trace` script-sources semantics |      20 | 20,004 – 115,590       |
| `validation_trace` resolve-inputs semantics |       6 | 28,023 – 72,039        |
| `validation_trace` phase-A semantics        |      10 | 16,762 – 28,066        |
| `validation_trace` value-and-mint semantics |       8 | 17,859 – 22,000        |
| `transition_trace` finals                   |       2 | 26,172 – 40,869        |
| `availability_challenge` (mint + spend)     |       1 | 19,927                 |

Five further validation-trace bodies sit between 16,193 and 16,332 raw bytes
and will fail once applied and wrapped. They are listed in the index and
should be handled by the plan of the group they belong to.

## The limits that matter

- **Transaction size.** `maxTxSize = 16,384` bytes on the complete signed
  transaction. A reference-script publication carries the raw script body
  plus parameters, the funding input, the script-ref output with min-Ada, the
  role-NFT mint, and one signature. Measured overheads in this repo:
  parameter application adds 72–73 bytes per script; the signed wrapper adds
  about 276 bytes (state-queue minting policy: 5,222 applied → 5,498 signed).
  The largest body proven to publish is the min-ADA tx yield at 15,522 raw.
  **Target raw body ≤ 15,000 bytes** so the margin survives regeneration.
- **Production admission.** `assertReferenceScriptRawBodiesFitL1EnvelopeV1`
  in `demo/midgard-sdk/src/reference-scripts.ts` rejects any raw body at or
  above 16,384 before funding; the completed signed transaction is the
  authoritative fit check.
- **Execution budget per transaction.** The shared emulator harness pins Van
  Rossem's `maxTxExMem = 16,500,000` and `maxTxExSteps = 10,000,000,000`
  (`tests/support/emulator/protocol-parameters.ts`). Every validator that
  runs in one transaction shares that budget. A split that runs several
  scripts in the same transaction must fit in aggregate; a split that chains
  transactions must fit per step.
- **Reference scripts do not count toward `maxTxSize`**, but Conway bounds
  the total reference-script bytes one transaction may reference
  (200 KiB, `maxRefScriptSizePerTx`) and prices them with a tiered fee
  (`minFeeRefScriptCostPerByte`, escalating per 25 KiB tier). A transaction
  referencing a 115 KB family split into eight yields still references
  115 KB. Plans must state the total referenced bytes per transaction and
  the resulting fee band.
- **Emulator honesty.** `publishPlainReferenceScriptUtxo` asserts a positive
  L1 byte margin unless the caller passes `oversized: true`. The
  transition-trace and validation-dispute suites currently pass it for the
  scripts planned here (the dispute suite also raises `maxTxSize` to 262,144
  for those publications). Each plan's done-criterion includes removing that
  flag for its scripts.

## Why these scripts are large

The validator files are thin (60–200 lines). Their size comes from the library
code Aiken monomorphises and inlines into each one:

| Library module                                                                           |   Lines | Pulled in by                                  |
| ---------------------------------------------------------------------------------------- | ------: | --------------------------------------------- |
| `lib/midgard/validation-machine-v1.ak`                                                   |  19,142 | every validation-trace semantic resolver      |
| `lib/midgard/cek-builtin-v1.ak`, `cek-machine-v1.ak`, `cek-proof-v1.ak`, `cek-data-*.ak` | ~11,000 | CEK semantics, execution selection            |
| `lib/midgard/script-sources-redeemer-normalization-v1.ak`                                |     879 | script-sources stage-one chain                |
| `lib/midgard/native-tx-*.ak`                                                             |  ~5,000 | phase-A, resolve-inputs, script-sources       |
| `lib/midgard/ledger-output-*.ak`                                                         |  ~3,700 | value-and-mint, resolve-inputs, output proofs |
| `lib/midgard/fraud-proofs/transition-trace/proof.ak`                                     |   2,169 | both transition-trace finals                  |
| `lib/midgard/availability-challenge.ak` + validator                                      |   2,328 | availability challenge                        |

Compiled size tracks reachable code after inlining, not source lines. A
`when` over a large sum type reaches every arm's decoder; a shared verifier
that branches on phase reaches every phase's semantics. The first job of
every plan is to establish which reachable functions dominate the body.

### Measuring what dominates

Never run `aiken` inside the repository checkout for probes; concurrent
builds corrupt `build/` and `plutus.json`. Copy the project first:

```bash
cp -r /home/gumbo/midgard-hub/midgard/onchain/aiken /tmp/size-probe-<group>
cd /tmp/size-probe-<group>
/home/gumbo/.aiken/versions/v1.1.23-org-5adf7837/bin/aiken build --env testnet
node -e 'const b=require("./plutus.json");for(const v of b.validators)if(/<pattern>/.test(v.title))console.log(v.title,Buffer.from(v.compiledCode,"hex").length)'
```

Then add throwaway probe validators under `validators/probe/` in the copy,
each calling one candidate sub-function with opaque redeemer inputs, rebuild,
and read their sizes. The difference between probes is the cost of the code
between them. Record the probe table in the plan. Delete the copy when done.

## Patterns, in order of preference

### 1. Prune reachable code

Cheapest and ABI-neutral. Look for: generic decoders reached through a
shared entry point when the resolver only needs one shape; `when` arms that
this resolver can never take because its phase or action is fixed; helper
modules imported for one constant; duplicated verification the prepare step
already performed. Prefer narrowing the library entry point (a
resolver-specific function) over editing the shared verifier's semantics.
Pruning must not change what the resolver proves.

### 2. Withdraw-zero yield split (single transaction)

The repository's established pattern; three precedents exist:

- **State queue.** `validators/state-queue.ak` keeps `Init`/`Deinit` in the
  minting policy and dispatches commit, unattested removal, unavailable
  removal, fraud removal, and merge to five rewarding validators in
  `validators/state-queue-yields.ak`. Each yield reads the unique mint
  redeemer with `utils.get_unique_mint_redeemer` and runs the arm's
  validation function unchanged.
- **Min-ADA step 02.** `validators/fraud-proofs/min-ada/step-02.ak` is a
  3,319-byte dispatcher whose transaction and UTxO branches live in
  `step-02-yields.ak` (15,522 and 6,571 bytes). The dispatcher calls
  `state_queue_yield.require_authenticated_zero_yield` with a role asset
  name; each yield recovers the dispatcher's datum and spend redeemer with
  `min_ada/yield.unique_dispatch` and re-derives the expected output state.
- **Script-sources stage one (RF-021).** A monolithic resolver was replaced
  by an envelope binder plus traversal normalizer, outer normalizer, two
  executors, and a settlement validator, chained as computation-thread steps
  with an envelope commitment binding every hop.

The handshake that makes a yield sound, all of which every plan must keep:

1. The dispatcher indexes one reference input, requires it to carry exactly
   one role NFT under `reference_script_auth_policy_id` with the arm's asset
   name, and requires an exact zero-lovelace withdrawal from that script
   hash with a unique withdraw redeemer
   (`require_authenticated_zero_yield`). Cross-arm substitution fails on the
   role name; script substitution fails on the withdrawal credential.
2. The yield locates the one dispatcher input in the transaction by payment
   credential and reads its inline datum and spend redeemer
   (`unique_dispatch`). Requiring a singleton prevents one withdrawal from
   discharging several threads.
3. The yield re-derives the expected continuation output state and checks it
   against the actual output datum, so the dispatcher cannot be satisfied by
   a yield that verified something else.
4. Parameters carry the dispatcher script hash into the yield and the yield
   role names into the dispatcher; nothing is trusted from the redeemer.

Cost model: each additional yield re-parses the spend redeemer and datum
from `Data`, so one transaction pays that parse once per yield. Plans that
split a body into N yields must budget N parses of the largest witness.

For semantic resolvers specifically, the outer contract is
`validation_semantic_v1.continue_winning`: the resolver proves
`semantic_transition_is_valid` and continues the thread to the award script.
The split shape is therefore: keep `main.spend` as a dispatcher that calls
`continue_winning` with `semantic_transition_is_valid = True` **only after**
`require_authenticated_zero_yield` for every required yield role, and move
the semantic predicate into yields. The award, evidence-hash, and phase
checks stay in the dispatcher because `continue_winning` already does them.

### 3. Multi-transaction chaining

When the aggregate execution budget of a single transaction cannot hold the
predicate, split it into ordered computation-thread steps with intermediate
state in the datum, as native-script-invalid's staged 29/33-signer route,
missing-native-script-UTxO's step-05→06→07 path, and the RF-021 stage-one
chain do. Each hop must commit the exact intermediate state (hash it into the
datum) and the next hop must check it; the last hop reaches the same terminal
the monolith reached. Chaining costs extra transactions inside the challenge
window, so a plan choosing it must state the added step count against the
§3.3 maturity margin and the 5,000-transaction cap (GOAL_SPEC C52).

### 4. Redesign

Only when the arm boundaries themselves are wrong. The availability
challenge is the one contract here whose validator is a hand-written
multi-arm mint/spend script rather than a thin wrapper; it should follow the
state-queue redesign directly.

## Ripple effects every plan must account for

- **Applied-parameter graph.** Semantic resolver hashes are parameters of the
  phase `prepare` validators (`validation_resolver_v1.select_semantic_resolver`
  checks the exact per-phase list and count: script-sources 29, phase-A
  script-preconditions 2, and so on). Changing a resolver's hash re-applies
  the prepare validator, which changes its hash, and so on up to the family's
  first step and the catalogue root. All 50 changes should land in one
  blueprint regeneration with one catalogue-root re-pin, not fifty.
- **Reference-script roles.** New yields need role names in
  `DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES`
  (`demo/midgard-sdk/src/reference-scripts.ts`), matching Aiken constants
  (compare `min_ada/yield.tx_role`), deployment-manifest entries, and the
  contract-deployment-info / inspection fixtures.
- **SDK contract wiring.** `demo/midgard-sdk/src/fraud-proof/contracts/`
  applies parameters by blueprint title; new validators need entries and the
  arity test (`zz605-semantic-resolver-arity.test.ts`) must see them.
- **Submit routing.** `demo/midgard-fault-proofs/src/validation-dispute/submit.ts`
  attaches resolver reference inputs by deployment entry; a yield needs its
  reference input and a zero withdrawal added to the transaction builder.
  Today only the CEK (3) and value-and-mint (11) resolvers have named
  deployment entries and submit routes; the remaining semantics are wired
  in `contracts.ts` only. Plans for those must add the entry, the route, and
  the funding-requirement row.
- **Watcher.** `validationTraceDispute` and `transitionTrace` are not
  installed in `demo/midgard-watcher`; the plans do not need to install
  them, but must not make installation harder (no operator-local inputs).

### Naming

Role names are Cardano asset names (at most 32 bytes) and must match
byte-for-byte between the Aiken constant, `REFERENCE_SCRIPT_AUTH_TOKEN_NAMES`
(`demo/midgard-sdk/src/reference-scripts.ts`) and
`DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES` (`midgard-core`). The
existing table is PascalCase `<Family><Arm><Kind>` with `<Kind>` one of
`Spend`, `Mint`, `Withdraw`, `Observer`, `Yield` (`StateQueueCommitYield`,
`V1FpMinAdaS02TxYield`, `V1FpTransitionTraceFinal4`,
`AvailabilityChallengeSpend`). The plans here use one scheme on top of it:

- **Validation-trace yields:** `V1Vt<Group><Arm>Yield` with group tags `Cek`,
  `Ss` (script-sources), `Ri` (resolve-inputs), `PhaseA`, `Vam`
  (value-and-mint) and `Lop` (the ledger-output-proof yields shared by
  script-sources and resolve-inputs). `V1Vt` abbreviates the existing
  `V1ValidationTrace` prefix because the full prefix leaves too few bytes for
  the arm (`V1ValidationTraceCekSelMatDataYield` would be 35). Chain hops
  that are consumed hash-checked (CEK context/core, RF-021 executors) carry
  no role and keep their blueprint titles as deployment-entry names.
- **Transition-trace yields:** `V1FpTtF<final><Arm>Yield`; the existing
  `V1FpTransitionTraceFinal<n>` roles stay on the dispatchers.
- **Availability challenge:** `AvailabilityChallenge<Arm>Yield` beside the
  existing `AvailabilityChallengeSpend` / `Mint`.
- **Human-readable keys** follow the existing style, e.g.
  `"V1 validation-trace script-sources redeemer-item-step yield"`.

Add a length assertion for every new constant (Aiken test on the constant,
`deployment-manifest-identity-v1.test.ts` on the table) so a 33-byte name
fails at build time rather than at mint time.

### Landing order

Everything lands in one blueprint regeneration with one catalogue-root
re-pin, but inside that change the pieces have a dependency order (each
plan's §10 is the authority; this is the merged view):

1. Shared libraries with no validator changes: the script-sources raw
   stage-frame library and semantic-yield handshake (non-output §4.1–4.2),
   the value-and-mint arm split (replay-asset §4a), the phase-A PA-CARRY /
   PA-UNDECODED functions (signature-between §4), the stage 7–12 prunes and
   descriptor-mode surface (stage-ten-match §4a–4b), `assemble_v1` and
   `final_yield_v1` (transition-trace accepted-transaction §4.1), the CEK
   chain module (cek-context §4.1), the `ledger-output-proof-v1` raw stage
   steps (output-proof-step §4.2). The `reference_script_auth_policy_id`
   semantic-parameter name is added to `contracts.ts` once here.
2. Shared yields whose parameters are dispatcher hashes are built **after**
   their dispatchers but the dispatchers depend only on the role constants:
   value-and-mint asset-fold; script-sources redeemer-item-step and the two
   observer yields; the eleven LOP and four descriptor yields (parameterised
   by both the script-sources and the resolve-inputs dispatchers, so those
   two groups are built together); transition-trace `output_summaries`
   (after `l2_open` / `projection`); the four CEK execution-selection
   yields; the five availability-challenge yields.
3. The RF-021 stage-one chain extension (stage-one-redeemer §4.2) before the
   CEK context chain, which consumes its item-step entry/return hops.
4. Prepare validators and family first steps re-apply from the new semantic
   hashes (`script_sources_v1`, `value_and_mint_v1`, `phase_a_*_v1`,
   `resolve_inputs_v1`, `cek_v1`, `dispute_v1`, `route_v1`), then the
   catalogue root, `Q13_CATALOGUE_ROOT` and the deployment-manifest identity.
5. Off-chain: roles and manifest tables, deployment entries and submit
   routes per group, stake registrations, funding rows, inspection fixtures.
6. Emulator: remove `oversized: true` and the raised `maxTxSize` per group
   once every script of that group publishes plainly.

## What every plan must contain

Use these headings in this order.

1. **Identity.** Blueprint title, file, raw size, applied parameters, phase
   and resolver index (or arm), role name today, deployment entry today.
2. **Why it is this size.** Probe table naming the dominating reachable
   functions and modules.
3. **Options considered.** Prune, yield split, chain, redesign, with the
   reason each was kept or rejected for this contract.
4. **Chosen design.** New validator list with responsibilities, parameters,
   role names, datum/redeemer ABI deltas, and the exact handshake. Include a
   security argument covering dispatch uniqueness, role authentication,
   cross-arm substitution, output-state re-derivation, and what an attacker
   gains if any yield is omitted.
5. **Size and budget projection.** Expected raw size per new script, total
   referenced bytes per transaction, fee band, and aggregate ExUnits; how the
   projection was measured.
6. **Off-chain work.** SDK contracts, reference-script roles, manifest,
   inspection fixtures, submit route, funding requirements, and any
   `midgard-core`/`validation` codec change. State which of these do not
   exist today for this contract.
7. **Emulator scenario tests.** The Lucid Evolution tests to add or change:
   publication fit for every new script without `oversized`, positive
   lifecycle through award, valid-block negative at the same evidence
   frontier, cancel/resume where the family supports it, and the maximum
   supported shape. Name the file and describe fixtures. State what exists
   today.
8. **Aiken tests.** Unit and property tests for the new yields, including
   substitution and omission negatives.
9. **Verification commands.** Exact commands, expected counts.
10. **Ordering and dependencies.** Which other plans share library entry
    points or parameters and must land together.
11. **Risks.** Budget overrun, ABI churn, spec conflicts.

Done criterion for every plan: raw body of every script in the family
≤ 15,000 bytes; the emulator publishes each without `oversized` and the
production admission helper accepts it; the family lifecycle passes under
the shared Van Rossem limits; the catalogue root is re-pinned once.
