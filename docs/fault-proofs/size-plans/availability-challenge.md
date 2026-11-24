# Availability-challenge publication plan

Status: implemented, with the pooled DA committee bond and the
[operational actors](../availability-challenge-operations.md). Live release
acceptance is open.

The emulator profile is pinned to the mainnet protocol parameters in
[`mainnet-protocol-parameters.json`](../../../demo/midgard-node/tests/fixtures/mainnet-protocol-parameters.json).
All availability publication evidence uses signed transactions with a 512-byte
size reserve.

## Design boundary

A single availability validator carrying every mint and spend arm exceeds the
complete signed 16,384-byte transaction ceiling, so the arms are split.

Retain a mint/spend dispatcher with `AdvanceTranche`, `ConsumeCarrier`, and
`Coordinate` inline. Move each mint arm into a separate authenticated
zero-withdrawal validator. Challenge-record, tranche, publication, terminal and state-queue semantics are
unchanged by the split; only physical verification boundaries move.

| Mint arm | Role token                         | Manifest contract key                  |
| -------- | ---------------------------------- | -------------------------------------- |
| Open     | `AvailabilityChallengeOpenYield`   | `availabilityChallengeOpenWithdraw`    |
| Settle   | `AvailabilityChallengeSettleYield` | `availabilityChallengeSettleWithdraw`  |
| Close    | `AvailabilityChallengeCloseYield`  | `availabilityChallengeCloseWithdraw`   |
| Timeout  | `AvailabilityChallengeExpiryYield` | `availabilityChallengeTimeoutWithdraw` |

DA attestation mints no bond: Apply checks the pooled DA committee bond, and a
Timeout slashes the pool.

The Timeout yield takes the pool as a mandatory input, even at zero backing,
and checks the slash exactly. With `backing` the pool's lovelace above its
floor, `taken = min(da_bond_lovelace, backing)`,
`fee_part = min(da_slash_penalty_lovelace, taken)` and
`payout = taken − fee_part`. The pool output keeps its address, datum and NFT
and holds `taken` less lovelace. The transaction fee is exactly
`fee_part + c`, where `c` is the challenger's own fee contribution,
`0 ≤ c ≤ max_timeout_fee_lovelace`. The challenger receives one merged output
of `remaining − c + challenge_record_lovelace + payout`: a separate reward
output with `0 < payout <` min-UTxO would make the Timeout unbuildable, while
the merged output always carries at least the record lovelace. The builder sets
`c` to the part of the required ledger fee that `fee_part` does not cover, so
with a full pool `c = 0` and the fee is exactly the penalty.

The existing `AvailabilityChallengeSpend` and `AvailabilityChallengeMint` roles
remain on the dispatcher. Match role bytes between Aiken, SDK and core tables;
all asset names must fit 32 bytes.

## Alternatives and consequences

Keep `AdvanceTranche` inline because its transaction carries the large payload
chunk. Adding another withdrawal/reference input would change the measured
response geometry and charge an additional script across thousands of
publications. Re-measure the maximum chunk publication if this choice changes.
Pairing mint arms reduces deployed roles but increases referenced bytes per
action and obscures one-role-per-arm authentication. Chaining mint arms adds
response-window transactions without addressing the code-size cause.

The split adds four reference-script UTxOs and reward-account registrations.
Budget their min-Ada, registration deposits and publication fees separately from
challenger action funding. Include aggregate referenced bytes and reference-script
fees, not only the signed transaction size.

## Authentication and ABI

The dispatcher parameters include `reference_script_auth_policy_id`. Each yield
is applied with the availability policy id, hub-oracle policy id and availability
parameters. Build the dispatcher before yields so applied hashes do not cycle.

Each mint constructor carries `yield_to_ref_input_index` as its first field. The rewarding redeemer is fieldless.

The dispatcher chooses the role from its mint constructor and requires the exact
role NFT, script reference and unique zero withdrawal. Each yield opens the unique
mint redeemer for its applied availability policy, requires its own constructor,
and runs that arm's complete predicate. It rederives exact outputs, mint pairs,
queue status and accumulators. A missing, wrong-role, duplicate or substituted
yield must refuse. `Coordinate` continues to bind each declared spend to that
mint redeemer; carrier consumption stays bound to `AdvanceTranche`.

Follow [withdraw-zero delegation](../../agents/withdraw-zero-yielding.md).
Missing role publication or reward registration must fail readiness before an
operator/challenger depends on that action.

Source authorities: the [validator](../../../onchain/aiken/validators/availability-challenge.ak),
[availability library](../../../onchain/aiken/lib/midgard/availability-challenge.ak),
[SDK schemas/planners](../../../demo/midgard-sdk/src/availability-challenge.ts),
[attestation builder](../../../demo/midgard-sdk/src/da-attestation.ts), and
[node publication admission test](../../../demo/midgard-node/tests/availability-challenge-publication-admission.test.ts).

## Acceptance

Build the normal testnet blueprint through the pinned Aiken build workflow. Every
fully applied publication must fit the signed envelope and 512-byte reserve;
all executing scripts in each transaction share the Van Rossem budget. Preserve
the 20% execution basis. No oversized route, raised limit, disabled evaluator,
skipped publication scenario, or arity-only test can establish completion.

Exercise complete registered scenarios for:

- attestation to `Attested{commitment_hash}` status under a backed DA bond pool;
- open through ordered chunk/carrier publication, per-tranche settlement,
  terminal accumulation and close;
- no-response timeout with the exact pool slash, refund and queue removal;
- partial-response timeout and maximum-tranche settlement;
- invalid/cross-arm/omitted-role/reference substitutions and honest refusal;
- restart, repeated requests, already-submitted intent, rollback and concurrent
  actor recovery;
- maximum chunk and maximum tranche shapes, including 16-tranche opening with
  its 19 outputs, then publication/settlement through completion.

Run SDK schema/planner/attestation tests, core role/manifest identity, node contract
application/publication/initialization, and the watcher adapter and lifecycle
scenarios. Require nonzero collection and record exact results against the current
blueprint. Update the readiness checklist from those results.

The availability remedy is complete only when independently operated parties can challenge,
respond, recover and settle or time out under the authenticated deployment within
the configured window and transaction cap. [Release acceptance](../execution-plan.md)
remains the completion authority.

## Long-window scenarios

The lifecycle suite takes its response windows from the selected deployment
profile. The testing and emulator profiles carry windows of at most 880 s, and
the emulator lands one transaction per 20 s block, so the complete-response
scenario (301 chunks plus one settlement, 302 blocks) is skipped with the
derived reason in its title. The maximum-commitment scenario still opens all 16
tranches with 19 outputs, publishes one maximum chunk of tranche 0 with the
full-tranche proof and the tranche-1 partial, settles all 16 and times out with
queue removal.

Under the acceptance rule, a skipped publication scenario does not establish
completion. The complete-response fit, and with it the settlement of a completed
300-carrier tranche, is measured only with a long-window profile
(`preprod-public` or `mainnet`, 1 h small and 48 h full windows). Export the
profile the blueprint was built for:

```sh
pnpm --dir demo deployment:build preprod-public
# rebuild midgard-core, lucid-midgard, midgard-sdk and midgard-node, then:
NODE_ENV=emulator MIDGARD_DEPLOYMENT_PROFILE=preprod-public \
  MIDGARD_REAL_BLUEPRINT_PATH="$PWD/onchain/aiken/plutus.json" \
  pnpm --dir demo/midgard-node exec vitest run \
  tests/availability-challenge-lifecycle.test.ts
pnpm --dir demo deployment:build preprod-testing   # restore the selection
```

Reproduce the primary gates from the repository root (the testing profile skips
the complete-response scenario; see above):

```sh
pnpm --dir demo deployment:build preprod-testing
NODE_ENV=emulator MIDGARD_REAL_BLUEPRINT_PATH="$PWD/onchain/aiken/plutus.json" \
  pnpm --dir demo/midgard-node exec vitest run \
  tests/availability-challenge-publication-admission.test.ts \
  tests/availability-challenge-readiness.test.ts \
  tests/availability-challenge-lifecycle.test.ts \
  tests/mainnet-protocol-parameters.test.ts \
  tests/initialization-emulator.test.ts \
  tests/reference-script-publication-fit.test.ts
pnpm --dir demo/midgard-node run typecheck
```

Set `MIDGARD_AVAILABILITY_FIT_REPORT_DIR` to an existing output directory to retain
all lifecycle transaction measurements. Saved results do not replace running
these gates on the final release blueprint.
