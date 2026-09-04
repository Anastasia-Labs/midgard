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
   constant size: policy cursor, next byte offset, previous policy key,
   accumulated completed-asset count, and a domain-separated checkpoint hash.
   Each transaction consumes at most 24 complete policy items. It stops only
   at the bound policy's begin header:
   either the exact first crossing is recorded, or that policy is fully
   consumed and a complete non-crossing result is recorded.
4. `fraud_proofs/mint_declared_asset_limit/step_04.main.spend`
   parameters: permanent fraud-proof policy id, permanent token address, and
   computation-thread policy id. It imports only the family decision rule and
   Wave-0 terminal polarity helper, burns the thread token, and mints the
   permanent proof token. All four stages share canonical cancellation.

Item bytes and checkpoint bytes remain in redeemers; datums contain only
fixed-size identity/cursor/accumulator commitments. The target item is bound
by transaction id, field-5 positional commitment, policy index, item length,
item commitment, policy id, and declared count. A checkpoint from another
field, transaction, item, cursor, or carriage cannot resume the thread.

## Maximum mint frontier

The maximum authenticated field-5 preimage is 32,768 bytes. The adversarial
frontier combines the greatest number of minimum-width completed canonical
policies that fit before a final target whose canonical map header declares
the first over-limit count. The adjacent honest frontier uses the identical
prefix and target coordinate with a declaration that leaves the total exactly
16,384. Tier-3 carriage is at most three 15,148-byte certified chunks plus one
certificate. The fold budget starts at 24 entries per transaction and may be
reduced only if an ordinary Van Rossem measurement requires it; protocol
limits and transaction-size settings are never raised.

The maximum lifecycle must exercise at least one field-grammar self-loop and
one declared-count fold self-loop, restart from both committed checkpoints, and
reach permanent mint and mutation-leased target/descendant removal. It must
also reject policy-index, item, previous-key, checkpoint, decision-digest, and
transaction substitutions, plus a declared-count mutation on otherwise
identical authenticated bytes.

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

The final ledger contains 28 deterministic rows and digest
`96545b49f17d0f836a7b988a1151f4a3cada70a82816ab581a7a3b95ff21d89f`.
The applied step-01 publication is the tightest script publication at 14,712
signed bytes (1,160 bytes inside the reliability reserve). The maximum field's
first two certified chunks land exactly at the 15,872-byte publication target;
all lifecycle transactions retain at least 14,324 signed bytes of hard-limit
margin. The largest measured execution is the second 24-policy fold at
8,516,927 memory and 3,866,869,067 CPU, leaving 7,983,073 memory and
6,133,130,933 CPU.

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
