# `mintDeclaredAssetLimit` V1 maximum-shape and size plan

- Frozen category ID: `0000002c`; central registration is intentionally left
  to the serial integrator.
- Exact rejection constructor: `MintDeclaredAssetLimit { policy_index }`.
- Logical topology: four family computation-thread stages after generic
  `Init`: bind coordinate, authenticate the exact field-5 policy item,
  resumably fold declared asset counts, and finalize the first crossing or a
  complete non-crossing contradiction.

## Consensus seam

This family is the narrow twin of
`validation-machine-v1.script_sources_begin_mint_policy`. At policy `i`, the
machine reads the canonical two-element policy item, the 28-byte policy id,
and the canonical asset-map header. It rejects with `E_ASSET_COUNT` exactly
when `previous_asset_count + declared_asset_count > 16,384`, before decoding
that policy's asset entries. A preceding policy contributes only after its
entire canonical, non-empty, non-zero, strictly ordered asset map has been
consumed, matching `script_sources_fold_mint_asset`.

The accepted direction therefore proves the first reachable crossing at the
bound `policy_index`. The forced-rejection direction completes the same fold
through the bound item and proves that it does not cross. A malformed earlier
item is never silently counted: it belongs to a decoding/canonicity family and
this family refuses it.

## Physical validators and carried state

1. `fraud_proofs/mint_declared_asset_limit/step_01.main.spend`
   parameters, in order: step-02 script hash, computation-thread policy id,
   hub-oracle script hash. It binds an accepted or forced native-V1 leaf and
   the exact policy coordinate. Wrongful rejection additionally binds only
   `MintDeclaredAssetLimit { policy_index }`.
2. `fraud_proofs/mint_declared_asset_limit/step_02.main.spend`
   parameters: step-03 hash, computation-thread policy id, field-certificate
   policy id. It authenticates field 5 and the exact policy item at the bound
   coordinate, reads the policy id and canonical map header, and commits their
   item identity. Certified fields first complete their envelope grammar using
   a constant-size checkpoint and same-script resumes.
3. `fraud_proofs/mint_declared_asset_limit/step_03.main.spend`
   parameters: step-04 hash, computation-thread policy id, field-certificate
   policy id. It resumes an authenticated field-5 walk. Its rule state is
   constant size: the walk checkpoint hash, the accumulated consumed-asset
   count, the previous policy key, and the machine's `MintFoldControlV1`
   cursor over the open item (active policy key, item byte cursor, assets
   remaining, per-policy asset ordinal, previous asset name). Each
   transaction spends a work budget of at most 192 units: opening a policy
   item costs 8 units, every consumed asset entry costs one. The walk
   checkpoint advances only when an item closes, so a policy item wider than
   one budget is re-read from the same authenticated walk position and
   consumed across as many transactions as it needs. The fold stops at the
   bound policy's begin header when it crosses (the exact first crossing is
   recorded) or when the bound policy is fully consumed (a complete
   non-crossing result is recorded).

   The intra-item cursor is what makes every field shape reachable: a prior
   policy inside the 32,768-byte field bound can carry roughly 8,000 asset
   entries, and one consumed entry costs about 45K memory and 14M CPU, so a
   fold that had to complete a policy item inside one transaction could not
   reach the bound coordinate past roughly 300 assets in any earlier policy.

4. `fraud_proofs/mint_declared_asset_limit/step_04.main.spend`
   parameters: permanent fraud-proof policy id, permanent token address, and
   computation-thread policy id. It imports only the family decision rule and
   Wave-0 terminal polarity helper, burns the thread token, and mints the
   permanent proof token. All four stages share canonical cancellation.

Item bytes and checkpoint bytes remain in redeemers; datums contain only
fixed-size identity/cursor/accumulator commitments. The target item is bound
by transaction id, field-5 positional commitment, policy index, policy id and
declared count. A checkpoint from another field, transaction, item, cursor,
or carriage cannot resume the thread, and a fold transaction that would
re-commit the identical state is refused.

The policy item's array head is read with the machine's own
`decode_definite_array_header_at`, so the twin admits exactly the two-element
heads `script_sources_begin_mint_policy` admits (a `98 02` spelling included)
rather than a stricter subset the machine would still count.

## Maximum mint frontier

The maximum authenticated field-5 preimage is 32,768 bytes. The measured
adversarial frontier is an exact 32,768-byte certified field holding a
1,000-asset first policy, sixty singleton policies, and a bound target whose
canonical map header declares exactly one asset more than the bound admits
(1,060 + 15,325 = 16,385): the smallest crossing. It exercises every fold
shape the budget admits — a full 192-asset budget spent inside one open
policy, a policy closing mid-transaction with singleton policies and the
crossing header following in the same budget — and it resumes the fold from
committed checkpoints both inside and between policy items. Tier-3 carriage
is three certified chunks plus one certificate.

Because a well-formed policy item carries at most about 8,200 asset entries
in 32,768 bytes, no well-formed transaction reaches an _actual_ total of
16,384 assets; the bound is only ever crossed by a declaration. The exact
boundary is therefore exercised as the machine decides it: a field whose
prior policy and bound header declare exactly 16,384 in total opens the bound
item as a non-crossing fold (the crossing claim is refused on chain, the
opened-item state is accepted), while the declaration one greater crosses.
The fold budget is 192 work units per transaction (one consumed asset entry
measures about 45K memory and 14M CPU; 256 units left a pure-entry
transaction within 2M memory of the limit) and may be reduced only if an
ordinary Van Rossem measurement requires it; protocol limits and
transaction-size settings are never raised.

The lifecycle exercises field-grammar self-loops, fold self-loops inside and
between policy items, restart from committed checkpoints, permanent mint and
mutation-leased target/descendant removal, cancellation from every physical
step, the honest accepted block and the honest forced rejection at their
terminal steps, and on-chain refusal of transaction-membership, forced-leaf,
carriage, transaction-anchor, grammar-checkpoint, walk-checkpoint, budget,
successor, reason-coordinate, subject-coordinate, direction and
out-of-range-coordinate substitutions.

## Publication, lifecycle, and reproducible ledger gate

The family publishes all four freshly applied scripts and records complete
signed transaction bytes, memory, CPU, and positive margins under the shared
Van Rossem parameters. Every builder uses
`.complete({ localUPLCEval: true })`; production lower validity bounds retain
the repository's 60-second backoff. Required ledger rows cover four reference
publications, accepted and forced starts, raw/certified carriage publication,
all grammar/fold self-loops, terminal mint, cancellation from every physical
stage, and mutation-leased target plus descendant removal. Reference
publication reliability requires signed bytes `<= 15,872`; hard limits remain
16,384 bytes, 16,500,000 memory, and 10,000,000,000 CPU.

The deterministic artifact is
`docs/fault-proofs/size-plans/mint-declared-asset-limit-v1-fit-ledger.json`.
Its focused test reconstructs every row from the fresh blueprint and asserts
deep equality, including the blueprint digest and ledger digest.

The final ledger contains 51 deterministic rows and digest
`a2215fa01414f7dbd2e73480d1a7051641f549f4aa4add3163742c050af4e7d2`, bound to
blueprint `d1ac61daef73a015ee617382b52bfa5cd0ca806e99ef10a0d1e99353c69f353c`
(aiken `v1.1.23+5adf783`). The applied step-01 publication is the tightest
script publication at 14,712 signed bytes (1,160 bytes inside the reliability
reserve); step 02 publishes at 11,664, step 03 at 11,231 and step 04 at
2,214. The maximum field's first two certified chunks land exactly at the
15,872-byte publication target; all lifecycle transactions retain at least
14,120 signed bytes of hard-limit margin. The heaviest execution by memory is
the fold that closes the 1,000-asset policy and opens fourteen singleton
policies in the same budget, at 9,367,944 memory (7,132,056 left); a full
192-entry fold inside one open policy costs 9,156,183 memory and
2,907,515,829 CPU; the heaviest execution by CPU is the terminal grammar
certification at 4,063,315,651 CPU (5,936,684,349 left).

## Package-owned production surface

Family production configuration contains infrastructure and immutable
references only: manifest, `blueprintJson`, `deploymentInfo`, `headerHash`,
Lucid/signer, authenticated retained-DA/raw-L1 sources, `decisionDigest`,
`stateQueueMutationLeaseCoordinator`, and reference scripts. The family owns
classification, evidence reconstruction, carriage/certificate publication,
all four dynamic actions, journal reads/appends, funding permits, intent-before-
submit, transaction-id reconciliation, permanent mint, and canonical removal.
No evidence, stage, submit, observe, or journal callback is accepted.

The family implementation exports
`ManifestBoundMintDeclaredAssetLimitWorkflowConfigV1`,
`ManifestBoundMintDeclaredAssetLimitWorkflowV1`,
`createManifestBoundMintDeclaredAssetLimitWorkflowV1`,
`executeManifestBoundMintDeclaredAssetLimitWorkflowV1`, and a standard
family runner surface named
`createMintDeclaredAssetLimitProductionWorkflowRunnerSurfaceV1`. It identifies
the four ordered applied scripts and
all computation-thread, permanent-proof, certificate, shared-witness, and
removal references without modifying protected central registries in this
slice.

The five shared witness roles are, in order of role name:
`computationThreadMint`, `fraudProofMint`, `phasMembershipWithdraw`,
`chunkedVerifyWithdraw`, and `pexcludesWithdraw`. The accepted classifier is a
family-local raw-envelope route and must be dispatched before strict canonical
block parsing; the forced wrongful-rejection classifier is the canonical
fallback when the raw accepted scan is empty.
