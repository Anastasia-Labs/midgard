# Cardano ScriptContext / Aiken `Transaction` Invariants

This document records the invariants that Cardano scripts in this repository may safely rely on when inspecting Aiken's `cardano/transaction.Transaction`.

It is intentionally strict about scope:

- only ledger/runtime guarantees, or invariants enforced by Aiken's opaque on-chain types, are listed as safe assumptions
- if a property is not listed here, contracts and off-chain code should not treat it as guaranteed without an additional source
- where a rule only applies to `TxOut` values and not to `transaction.mint`, that distinction is called out explicitly

## Scope

1. Aiken's `Transaction` is not a byte-for-byte copy of the full transaction body. On-chain scripts cannot see bootstrap inputs, bootstrap outputs, or transaction metadata.
2. `transaction.redeemers` only contains script witnesses. Pubkey witnesses do not appear there.
3. `transaction.inputs` in script context are ledger-ordered, not builder-ordered. Off-chain code must not assume they follow the order of `.collectFrom(...)` or similar builder calls.

## Ordering Invariants

### Inputs

1. `transaction.inputs` is ordered lexicographically by `OutputReference` (`tx id`, then `output index`).
2. The input list order is the reference order used when reasoning about `Spend` purposes.

### Outputs

1. `transaction.outputs` preserves transaction-body order.
2. Output order is not lexicographically re-sorted by the ledger for script context.

### Withdrawals

1. `transaction.withdrawals` is ordered by ascending `Credential`.
2. For this ordering, script credentials compare lower than verification-key credentials.

### Redeemers

1. `transaction.redeemers` is ordered by ascending `ScriptPurpose`.
2. For the subset used by this repository, the effective purpose ordering is:
   - `Spend`
   - `Mint`
   - `Withdraw`
3. Within `Spend`, the `Spend(...)` entries that actually appear in `transaction.redeemers` are ordered lexicographically by the `OutputReference` / `TxOutRef` carried by those spend purposes.
4. Within `Mint`, ordering is lexicographic by policy ID / currency symbol.
5. Within `Withdraw`, ordering follows the ledger-ordered withdrawal credential list.
6. `transaction.redeemers` only contains script witnesses, so pubkey-spent inputs do not create `Spend(...)` entries there.
7. Therefore, when reading `transaction.redeemers` directly, the observable ordering fact is: `Spend` redeemers are sorted by the `TxOutRef` of the spent script inputs that have redeemers.

### Governance-Specific Ordered Fields

1. `transaction.votes` is ordered by ascending `Voter` and then ascending `GovernanceActionId`.
2. `Publish { at, .. }` refers to the 0-based index in `transaction.certificates`.
3. `Propose { at, .. }` refers to the 0-based index in `transaction.proposal_procedures`.

## `Value` Invariants

These apply to Aiken `Value` values exposed inside script context, including `Output.value` and `transaction.mint`.

1. A `Value` is a normalized nested map: `PolicyId -> AssetName -> Int`.
2. The outer policy map is ordered lexicographically by policy ID / currency symbol.
3. Each inner asset-name map is ordered lexicographically by token name.
4. Duplicate policy IDs cannot appear.
5. Duplicate asset names under one policy cannot appear.
6. Zero-quantity entries are not allowed in `Value`.
7. Ada is represented as policy ID `""` and asset name `""`.
8. Because `""` sorts before any non-empty bytestring, Ada is the first asset entry whenever a `Value` contains lovelace.


## `TxOut`-Specific Value Invariants

These apply to `transaction.outputs[*].value` and to resolved input/output values visible through `transaction.inputs[*].output.value`.

1. Output values cannot contain negative quantities.
2. Any asset that appears in an output value has strictly positive quantity.
3. All `Value`'s from tx outputs carry lovelace; in Aiken, Ada is therefore the first entry in a output `Value`.
4. Contracts may rely on "Ada first" only because:
   - outputs carry lovelace
   - Ada uses the empty policy ID and empty asset name
   - Aiken `Value` ordering is lexicographic

## Important Non-Guarantees

1. Do not generalize output-value positivity rules to `transaction.mint`. Mint values may contain negative quantities, and those negatives represent burns.
2. Do not assume every spent input has a redeemer entry. Only plutus script-spent inputs do.
3. Do not assume redeemer positions are stable under reordering of inputs, mint policies, or withdrawals in the underlying transaction construction. Witness indices are a function of ledger ordering, not builder call order.
4. Do not assume output order is sorted. It is preserved body order.

## Practical Summary

For this repository, the key facts to rely on are:

1. `txInputs` are lexicographically ordered by `txOutRef`.
2. `txOutputs` preserve tx body order.
3. `Value` is lexicographically ordered by policy ID, then token name.
4. Zero-quantity entries are absent from `Value`.
5. Token quantities in TxOut values are non-negative and non-zero; assets present in them have strictly positive quantity.
6. TxOut values always contain lovelace/ada as the first entry (policy ID of ada is `""` which lexicographically sorts first always).
7. `txInfoRedeemers` / `transaction.redeemers` must be interpreted in ledger order, with the repository-relevant subset `Spend < Mint < Withdraw < ...`.


