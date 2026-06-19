# Hub Oracle Nonce Preparation SDK Boundary Plan

## Implementation Status

Status: complete as of 2026-06-19.

Implementation evidence:
- `demo/midgard-sdk/src/hub-oracle.ts` now owns hub-oracle one-shot nonce marker
  datum validation/encoding and the deterministic nonce preparation transaction
  builder, exported through `demo/midgard-sdk/src/index.ts`.
- `demo/midgard-node/src/commands/prepare-hub-oracle-nonce.ts` keeps wallet
  discovery, completion, signing, submission, refetch, and JSON reporting in the
  node while using the SDK builder for transaction construction.
- Reference-script top-up selection now reserves the configured
  `HUB_ORACLE_ONE_SHOT_*` outref so deployment funding cannot consume the nonce
  before init.
- Live nonce preparation produced tx
  `fe9661a259f243d9055bbdae4e47ab1ae87100aa86e3aca66858c3ee69e7451a` outref
  `fe9661a259f243d9055bbdae4e47ab1ae87100aa86e3aca66858c3ee69e7451a#0`, which
  was then used by the successful replacement reference-script deployment and
  protocol init.

Verification evidence:
- `demo/midgard-sdk/tests/hub-oracle-nonce.test.ts` covers marker datum
  encoding, validation, and SDK boundary constraints that prevent wallet,
  provider, completion, signing, or submission side effects.
- `demo/midgard-node/tests/prepare-hub-oracle-nonce.test.ts` and
  `tests/reference-scripts.test.ts` cover node orchestration and reserved outref
  exclusion.
- Final live e2e acceptance passed from nonce preparation through clean final
  state as recorded in `.codex/e2e-reliability-fixes/plan.md`.

## Problem Statement

`demo/midgard-node/src/commands/prepare-hub-oracle-nonce.ts` currently owns the
whole hub-oracle one-shot nonce preparation flow. The command parses the CLI
amount, switches to the operator wallet, derives the wallet address, generates a
fresh marker, encodes that marker as a Plutus datum, builds a marked
`pay.ToAddressWithData` output, completes the transaction, signs and submits it,
then verifies that exactly one matching output is visible.

That mixes deterministic transaction construction with node-only operational
responsibilities. The SDK should own the reusable, pure boundary for the nonce
datum and unsigned transaction shape. The node should keep wallet selection,
fresh marker generation, completion, signing, submission, provider visibility
checks, CLI parsing, and CLI/JSON formatting.

This matters because the prepared output reference becomes
`HUB_ORACLE_ONE_SHOT_TX_HASH` and `HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX`, which
parameterize the real hub-oracle policy and are later consumed by canonical
protocol initialization. The boundary must stay strict: no fallback construction
path, no node-local duplicate builder, and no SDK-owned wallet/query/submission
behavior.

## Current-State Evidence

- `demo/midgard-node/src/commands/prepare-hub-oracle-nonce.ts:2` imports
  `Constr` and `Data as LucidData` only for local nonce datum construction.
- `demo/midgard-node/src/commands/prepare-hub-oracle-nonce.ts:9-10` defines the
  marker domain and default nonce lovelace amount in the node package.
- `demo/midgard-node/src/commands/prepare-hub-oracle-nonce.ts:36-45` parses the
  CLI amount and rejects non-positive values before any transaction work.
- `demo/midgard-node/src/commands/prepare-hub-oracle-nonce.ts:48-53` generates a
  fresh marker from
  `MidgardHubOracleOneShotNonceV1:${Date.now()}:${randomUUID()}` and encodes it
  as `LucidData.to(new Constr(0, [markerHex]))`.
- `demo/midgard-node/src/commands/prepare-hub-oracle-nonce.ts:98-130` switches
  to the operator wallet, derives the address, creates a `lucid.newTx()`, builds
  a `pay.ToAddressWithData` output, completes, signs, and submits.
- `demo/midgard-node/src/commands/prepare-hub-oracle-nonce.ts:131-161`
  refetches `lucid.utxosAt(address)` and requires exactly one visible output
  matching the submitted transaction hash, inline datum, and requested lovelace.
- `demo/midgard-node/src/index.ts:519-599` exposes
  `prepare-hub-oracle-one-shot-nonce`, preserving `--amount-lovelace`,
  `--dry-run`, `--json`, text output, and environment-variable output.
- `demo/midgard-sdk/src/hub-oracle.ts:35-88` is already the SDK module for
  hub-oracle constants, datum types, UTxO types, and initialization params.
- `demo/midgard-sdk/src/hub-oracle.ts:173-203` builds the existing hub-oracle
  init transaction fragment and consumes an explicit `oneShotNonceUTxO`.
- `demo/midgard-sdk/src/hub-oracle.ts:212-246` already defines and uses
  `HubOracleError`, so nonce validation/build failures can reuse that error
  family.
- `demo/midgard-sdk/src/initialization.ts:74-83` and
  `demo/midgard-sdk/src/initialization.ts:182-204` build canonical atomic
  initialization from explicit inputs and consume `oneShotNonceUTxO`.
- `demo/midgard-node/src/transactions/initialization.ts:413-450` only resolves
  the configured nonce UTxO from the operator wallet. It does not define how the
  nonce UTxO is created.
- `demo/midgard-node/src/services/midgard-contracts.ts:334-342`,
  `demo/midgard-node/src/services/midgard-contracts.ts:541-542`, and
  `demo/midgard-node/src/services/midgard-contracts.ts:1019-1030` show that the
  consensus-critical identity is the configured output reference, not a
  node-private marker string.
- `demo/midgard-sdk/src/index.ts:8` already exports `hub-oracle.ts`, so helpers
  placed there become part of the public SDK surface without a new export file.
- Current nearby tests are `demo/midgard-sdk/tests/fault-proof.test.ts`,
  `demo/midgard-sdk/tests/state-queue.test.ts`, and
  `demo/midgard-node/tests/initialization-emulator.test.ts`. There is no
  dedicated nonce-preparation test yet.
- `demo/midgard-node/tests/initialization-emulator.test.ts:170-188` covers
  `SDK.incompleteHubOracleInitTxProgram` in isolation, and
  `demo/midgard-node/tests/initialization-emulator.test.ts:190-283` asserts that
  the SDK atomic init builder uses explicit inputs and does not fetch wallet
  UTxOs.
- `demo/midgard-node/README.md:200-203` and
  `demo/midgard-node/README.md:350-356` document the CLI command and the copied
  `HUB_ORACLE_ONE_SHOT_*` values.

## Target Architecture

### Selected SDK Boundary

Add the nonce preparation builder to `demo/midgard-sdk/src/hub-oracle.ts`. This
is the narrowest boundary because the module already owns hub-oracle datums,
init parameters, hub-oracle transaction fragments, and `HubOracleError`.

The SDK should export one datum encoder and one incomplete transaction builder.
The implementation should be normal exported functions; the signatures below
define the public surface and expected return types.

```ts
export const HUB_ORACLE_ONE_SHOT_NONCE_DATUM_DOMAIN =
  "MidgardHubOracleOneShotNonceV1";

export type HubOracleOneShotNonceDatumParams = {
  readonly markerHex: string;
};

export type HubOracleOneShotNonceTxParams = {
  readonly address: Address;
  readonly amountLovelace: bigint;
  readonly markerHex: string;
};

export type IncompleteHubOracleOneShotNonceTx = {
  readonly txBuilder: TxBuilder;
  readonly inlineDatum: string;
};

export const makeHubOracleOneShotNonceDatum: (
  params: HubOracleOneShotNonceDatumParams,
) => Effect.Effect<string, HubOracleError>;

export const incompleteHubOracleOneShotNonceTxProgram: (
  lucid: LucidEvolution,
  params: HubOracleOneShotNonceTxParams,
) => Effect.Effect<IncompleteHubOracleOneShotNonceTx, HubOracleError>;
```

SDK responsibilities:

- Validate `markerHex` as non-empty, even-length hex bytes. `isHexString` from
  `demo/midgard-sdk/src/common.ts:35` is available for the character check, but
  the implementation must also reject odd-length input.
- Validate `amountLovelace > 0n` before transaction construction.
- Encode the datum byte-for-byte like the current node expression:
  `Data.to(new Constr(0, [markerHex]))`. Do not replace it with a bare
  `Data.Bytes()` datum unless a test proves identical CBOR, which it should not.
- Build exactly one payment output with
  `lucid.newTx().pay.ToAddressWithData(address, { kind: "inline", value:
inlineDatum }, { lovelace: amountLovelace })`.
- Return the `TxBuilder` and exact `inlineDatum` so the node can keep its strict
  post-submit visibility check.
- Avoid wallet access, marker generation, completion, signing, submission,
  confirmation waiting, provider refetching, CLI output, and environment
  handling.

Node responsibilities after the refactor:

- Keep `parseNonceLovelaceOption`,
  `inspectOperatorWalletForNonceProgram`, `PreparedHubOracleNonce`, and
  `OperatorWalletNonceReadiness` in
  `demo/midgard-node/src/commands/prepare-hub-oracle-nonce.ts`.
- Keep the CLI default `DEFAULT_NONCE_LOVELACE = 5_000_000n` in the node unless
  a separate SDK consumer needs a shared operational default. The amount is not
  a protocol datum constant.
- Replace the private `makeNonceDatum` helper with a private marker helper such
  as `makeHubOracleOneShotNonceMarkerHex`, using
  `SDK.HUB_ORACLE_ONE_SHOT_NONCE_DATUM_DOMAIN`, `Date.now()`, `randomUUID()`,
  and `Buffer.from(payload, "utf8").toString("hex")`.
- Call `SDK.incompleteHubOracleOneShotNonceTxProgram(lucid, { address,
amountLovelace, markerHex })`.
- Complete the returned builder in the node. If touching the completion call,
  follow `docs/agents/transaction-finalization.md` and use
  `.complete({ localUPLCEval: true })`; do not move completion into the SDK.
- Continue signing, submitting, refetching `lucid.utxosAt(address)`, requiring
  exactly one match, and returning the existing CLI/JSON result shape.

### Rejected Alternatives

- Do not move the full command into the SDK. Wallet selection, signing,
  submission, confirmation, provider visibility, and operator-facing output are
  node runtime responsibilities.
- Do not add a second node-local construction path as a fallback. The SDK helper
  should become the single nonce-output builder.
- Do not put the helper in a new SDK file unless `hub-oracle.ts` becomes
  unreasonably crowded during implementation. If a new file is chosen, it must
  be exported from `demo/midgard-sdk/src/index.ts` and the plan should be
  updated before implementation continues.
- Do not make the SDK generate entropy or depend on Node-only APIs such as
  `node:crypto` or `Buffer`. Browser and test consumers should be able to pass a
  deterministic `markerHex`.

## Phased Task Breakdown

1. Add SDK nonce constants and types in
   `demo/midgard-sdk/src/hub-oracle.ts`.

   - Add `Constr` to the Lucid imports if the encoder uses the same expression
     as the node.
   - Put `HUB_ORACLE_ONE_SHOT_NONCE_DATUM_DOMAIN` near
     `HUB_ORACLE_ASSET_NAME`.
   - Place the helper implementations after `HubOracleError`, or move
     `HubOracleError` earlier, so validation/build failures use the existing
     hub-oracle error type cleanly.

2. Implement `makeHubOracleOneShotNonceDatum`.

   - Reject empty, odd-length, or non-hex `markerHex` with `HubOracleError`.
   - Encode with constructor `0` and one bytes field.
   - Keep the function deterministic and side-effect free.

3. Implement `incompleteHubOracleOneShotNonceTxProgram`.

   - Validate amount and marker before calling `lucid.newTx()`.
   - Call `makeHubOracleOneShotNonceDatum` internally.
   - Build only the marked output and return `{ txBuilder, inlineDatum }`.
   - Map unexpected Lucid builder exceptions to `HubOracleError` if the
     implementation uses `Effect.try`.

4. Refactor
   `demo/midgard-node/src/commands/prepare-hub-oracle-nonce.ts`.

   - Add an SDK import consistent with nearby node files, preferably
     `import * as SDK from "@al-ft/midgard-sdk";`.
   - Remove `Constr` and `Data as LucidData` from the node command.
   - Generate only `markerHex` in the node command.
   - Replace the direct `lucid.newTx()` and `pay.ToAddressWithData` calls with
     the SDK helper result.
   - Preserve the existing error messages unless the SDK validation error needs
     a clearer wrapper for operator debugging.

5. Preserve CLI behavior in `demo/midgard-node/src/index.ts`.

   - Keep command name, options, defaults, dry-run behavior, JSON shape, and text
     output stable.
   - No README update is required if output and invocation stay unchanged.
     Update `demo/midgard-node/README.md` only if the command surface changes.

6. Add focused tests.

   - Add `demo/midgard-sdk/tests/hub-oracle-nonce.test.ts` for SDK datum and
     builder behavior.
   - Add `demo/midgard-node/tests/prepare-hub-oracle-nonce.test.ts` for node
     orchestration and result shape, using either an emulator Lucid service or a
     narrow fake `Lucid` service via `Effect.provideService`.
   - Extend `demo/midgard-node/tests/initialization-emulator.test.ts` only if
     the implementation changes initialization inputs. This boundary move should
     not require that.

7. Run focused verification before broader workspace checks.
   - Run the new SDK test first.
   - Run the new node command test next.
   - Run typecheck/build only after focused tests pass.
   - Do not run Aiken unless an implementation changes on-chain datum
     expectations or validator behavior.

## Acceptance Criteria

- `demo/midgard-sdk/src/hub-oracle.ts` exports
  `HUB_ORACLE_ONE_SHOT_NONCE_DATUM_DOMAIN`,
  `makeHubOracleOneShotNonceDatum`, and
  `incompleteHubOracleOneShotNonceTxProgram` through the existing
  `demo/midgard-sdk/src/index.ts` barrel.
- The SDK helper owns the nonce datum encoding and the
  `pay.ToAddressWithData` transaction shape.
- `demo/midgard-node/src/commands/prepare-hub-oracle-nonce.ts` no longer imports
  `Constr` or `Data as LucidData`.
- `demo/midgard-node/src/commands/prepare-hub-oracle-nonce.ts` no longer calls
  `lucid.newTx()` or `pay.ToAddressWithData` for nonce preparation.
- The node still switches to the operator wallet, derives the operator address,
  generates a fresh marker, completes, signs, submits, and verifies visibility.
- The post-submit check still requires exactly one visible UTxO matching
  `txHash`, `inlineDatum`, and `amountLovelace`.
- `prepare-hub-oracle-one-shot-nonce` keeps the same command name, flags,
  default amount, text output, JSON output, and
  `HUB_ORACLE_ONE_SHOT_*` semantics.
- Existing initialization builders continue to consume an explicit
  `oneShotNonceUTxO`; they do not learn how to create or discover one.
- The SDK nonce helper does not call `.wallet()`, `.getUtxos()`, `.utxosAt()`,
  `.utxosAtWithUnit()`, `.complete()`, `.sign`, `.submit()`, or `.awaitTx()`.
- No compatibility shim, alternate node-local builder, or demo-only fallback is
  introduced.

## Tests And Verification

Add SDK unit coverage in `demo/midgard-sdk/tests/hub-oracle-nonce.test.ts`:

- `makeHubOracleOneShotNonceDatum({ markerHex })` equals the current encoding
  produced by `Data.to(new Constr(0, [markerHex]))`.
- Invalid markers fail: empty string, odd-length hex, and non-hex text.
- Non-positive lovelace amounts fail before `lucid.newTx()` is called.
- `incompleteHubOracleOneShotNonceTxProgram` calls
  `pay.ToAddressWithData` once with the requested address, inline datum, and
  `{ lovelace: amountLovelace }`.
- A fake `lucid.wallet()` that throws is never called, proving the SDK builder
  has no wallet dependency.
- Fake `.complete`, `.sign`, `.submit`, `.awaitTx`, `.utxosAt`, and
  `.utxosAtWithUnit` methods that throw are never called.

Add node command coverage in
`demo/midgard-node/tests/prepare-hub-oracle-nonce.test.ts`:

- `parseNonceLovelaceOption` still accepts positive integers and rejects zero,
  negative values, whitespace-only values, and non-digits.
- `prepareHubOracleOneShotNonceProgram` still returns `txHash`, `outputIndex`,
  `outRef`, `address`, decimal `lovelace`, and `inlineDatum`.
- The returned inline datum matches the SDK nonce datum shape and includes a
  non-empty marker payload.
- The node verifies exactly one matching output and fails if the submitted
  transaction becomes visible with zero or multiple matching outputs.
- The node service test proves wallet switching, address derivation, completion,
  signing/submission, and provider refetching remain in the node.

Focused implementation checks:

```bash
cd "$(git rev-parse --show-toplevel)/demo/midgard-sdk"
pnpm exec vitest run tests/hub-oracle-nonce.test.ts --reporter=basic
pnpm run typecheck
```

```bash
cd "$(git rev-parse --show-toplevel)/demo/midgard-node"
NODE_ENV=emulator pnpm exec vitest run tests/prepare-hub-oracle-nonce.test.ts --reporter=basic --disable-console-intercept
pnpm run typecheck
```

Source-boundary assertions:

```bash
rg -n "ToAddressWithData|Data as LucidData|Constr|newTx\\(" "$(git rev-parse --show-toplevel)/demo/midgard-node"/src/commands/prepare-hub-oracle-nonce.ts
```

The command above should produce no output after the refactor.

```bash
rg -n "incompleteHubOracleOneShotNonceTxProgram|makeHubOracleOneShotNonceDatum|HUB_ORACLE_ONE_SHOT_NONCE_DATUM_DOMAIN" "$(git rev-parse --show-toplevel)/demo/midgard-sdk"/src/hub-oracle.ts "$(git rev-parse --show-toplevel)/demo/midgard-node"/src/commands/prepare-hub-oracle-nonce.ts
```

The command above should show the SDK definitions and the node call site.

Broader checks before merging the implementation:

```bash
cd "$(git rev-parse --show-toplevel)/demo"
pnpm run typecheck
pnpm run build
```

No Aiken verification is required for this boundary move unless implementation
changes on-chain datum expectations, validator parameters, or initialization
validator behavior.

## Migration And Operational Notes

Existing prepared nonce UTxOs remain valid. The refactor preserves the output
datum shape, the lovelace amount requested by the operator, and the configured
output-reference semantics. The canonical bootstrap identity remains
`HUB_ORACLE_ONE_SHOT_TX_HASH` plus `HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX`.

Operators should see no CLI change. The same command should still print the
same environment variable names for new deployments, and dry-run should remain
an operator-wallet readiness inspection that does not submit a transaction.

No local state reset, database migration, or on-chain redeploy is needed solely
for this SDK-boundary refactor. If a future implementation changes the datum
schema or validator expectations, treat that as a protocol-affecting change and
revisit deployment instructions separately.

Provider visibility lag belongs in the node after submission, not in the SDK
builder. The SDK helper should remain deterministic and should not poll or query
provider state.

The one-shot nonce should remain fresh deployment input. Do not add docs or code
that encourage nonce reuse, discovery of old nonce outputs, or fallback to any
available wallet UTxO.

## Risks And Open Questions

- Public helper names become part of the pre-launch SDK surface. Decision owner:
  SDK maintainers. Evidence needed: consistency with adjacent exported builder
  names in `hub-oracle.ts`, `initialization.ts`, and other SDK modules before
  implementation lands.
- The plan keeps `DEFAULT_NONCE_LOVELACE` as a node CLI default. Decision owner:
  SDK and node maintainers. Evidence needed to move it later: a second SDK
  consumer that needs the same operational default rather than an explicit
  amount.
- The marker domain constant moves to the SDK because it identifies the datum
  payload generated by SDK consumers, while entropy generation remains in the
  node. Decision owner: SDK maintainers. Evidence needed to reverse this:
  agreement that marker payload contents are purely CLI-local and should not be
  exposed as SDK API.
- The current datum is only used by the preparation command's visibility check.
  If future validators inspect the nonce datum, the helper becomes
  consensus-sensitive and needs schema review against Aiken before release.
- Node command tests may be easiest as emulator tests rather than pure unit
  tests because `prepareHubOracleOneShotNonceProgram` signs, submits, confirms,
  and refetches through Lucid. Decision owner: implementer. Evidence needed:
  whether Vitest ESM mocking can reliably isolate `@al-ft/midgard-sdk` and
  `handleSignSubmit` without making the test brittle.
