# Minimum-Ada wrongful-rejection size and transition plan

The existing five spending steps and authenticated step-02 transaction/UTxO
withdrawals remain the deployment topology. No deployment parameter changes are
required. Accepted transaction and newly introduced post-UTxO underfunding retain
their existing predicates and predecessor non-membership requirement.

Step 01 additionally authenticates a forced source against the thread-bound
header and counted forced root. Only wrongful `OutputBelowMinAda` with the exact
nonnegative output index is admitted. The source's compact transaction binds
field 2; submitted full bytes are retained durably while the committed compact
source uses the invalid adjudication. Step 02 preserves the authenticated
withdrawal target, unique dispatcher handshake, and exact output selection. Its
transaction scan state carries the authenticated direction. Step 03 certifies
canonical output bytes through bounded continuations and applies the
same compiled minimum-Ada predicate, requiring sufficiency for wrongful rejection
and insufficiency for accepted-invalid evidence. Step 05 mints the permanent
proof; removal retains that proof. UTxO states never admit wrongful direction.

The preimplementation measurement scope was maximum 32,768-byte field, maximum
16,384-byte canonical output (including datum/reference-script and multiasset
shapes), descriptor carriage, and 64-node forced membership. The required
transition splits and their measured results follow. Evaluator and ledger limits
remain unchanged.

## Measured refinements

The first forced source branch enlarged step 01 to a 17,281-byte signed
publication. Moving the existing post-descriptor output-index equality into the
UTxO rewarding validator removed its duplicate decoder while retaining the
check before advancement. The registered topology and parameter ABI stay fixed.

A 1,200-asset valid output exceeded memory in the monolithic transaction-output
decoder. Step 02 now freezes a chunk-authenticated canonical output scan; step 03
selfloops over the existing ledger-output scanner, then freezes its exact byte
length and lovelace for the unchanged floor predicate. A batch of sixteen scanner
transitions exceeded memory; four transitions per transaction retain bounded
work. The scanner's subject is an accepted-shaped transaction anchor used only
for canonical byte verification; the separately authenticated min-Ada direction
controls the terminal predicate.

A selected output following 700 canonical siblings in a maximum 32,768-byte
field exceeded CPU in the unbounded item lookup. The transaction withdrawal now
certifies grammar and advances the existing authenticated field walker in
32-item batches, selflooping step 02 until the exact selected output is bound.
Both checkpoint hashes and completion flags persist in the thread datum. Durable
cursor successors are 2→2/3 and 3→3/4/5; cancellation remains valid at each loop.

## Verified ledger and checks

The standard fit ledger records 649 evaluated, signed publications and lifecycle
transactions. Its combined maximum case uses an exact 5,000-byte Cardano value
with 1,304 assets, a 16,384-byte output, a 32,768-byte field, and a 64-Branch forced
proof. It cancels after a submitted scanner checkpoint, reopens the serialized
artifact, and completes a fresh registered proof through minting and removal.
Another maximum field selects an output following 700 siblings. The post-UTxO
case carries a maximum output descriptor and 64-Branch membership and predecessor
exclusion proofs through the retained UTxO predicate, mint, and removal.

Maximum signed size is 15,872 bytes (512 bytes below the ledger limit and exactly
preserving the 512-byte publication reserve). Maximum memory is 9,863,640 units
(6,636,360 remaining); maximum CPU is 3,846,512,750 units (6,153,487,250 remaining).
The normal testnet blueprint SHA-256 is
`95e0ea6bb577e9a756138745946ccfa2cf20518974d25f8a90da42e5700fd7cd`.
The ledger digest is
`2984725e7c2c8b233b6193ebd889045fc215a349a2d797bdb0d431349297ec0a`.
Compiler: `aiken v1.1.23+5adf783`.

Verification uses the declared Node 22/pnpm 9 toolchain and the freshly built
normal testnet blueprint. From the worktree root:

```sh
MIN_ADA_FIT_LEDGER_PATH="$PWD/docs/fault-proofs/size-plans/min-ada-wrongful-rejection-v1-fit-ledger.json" MIDGARD_REAL_BLUEPRINT_PATH="$PWD/onchain/aiken/plutus.json" pnpm --dir demo/midgard-fault-proofs exec vitest run tests/min-ada-wrongful-rejection-lifecycle.test.ts tests/submit-init-emulator-min-ada-standalone.test.ts
MIDGARD_REAL_BLUEPRINT_PATH="$PWD/onchain/aiken/plutus.json" pnpm --dir demo/midgard-fault-proofs exec vitest run tests/min-ada-wrongful-rejection-fit-ledger.test.ts
pnpm --dir demo/midgard-sdk exec vitest run tests/min-ada.test.ts
```

Results: 12/12 lifecycle tests, 1/1 ledger verification, and 5/5 SDK codec tests.
The focused testnet Aiken family checks pass 31/31. The lifecycle suite includes
exact-floor and above-floor convictions, actual on-chain refusal of honest
underfunding, authenticated reason/index/root/direction substitutions, durable
artifact mutation rejection, installed replay admission, and both accepted
transaction and post-UTxO regressions. The legacy interactive min-Ada resolver is
a separate contract and is covered by its own deployment work.
