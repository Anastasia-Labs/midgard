# Balancing, Fees, and Coin Selection

Balancing is a correctness feature, not a convenience shortcut. It must produce
transactions that satisfy Phase B value preservation.

## Value Equation

Phase B enforces:

```text
inputs - fee + minted - burned = outputs
```

The builder must account for lovelace and multi-assets exactly. It must reject
negative asset quantities except where native mint maps explicitly represent
burning.

## Fee Policy

Current Phase A minimum fee is:

```text
fee >= minFeeA * native_cbor_length + minFeeB
```

The builder should obtain `minFeeA` and `minFeeB` from the provider or explicit
completion options. It should iterate until fee and transaction size converge.

Default max iterations should be small and explicit, for example 12, matching
the current transfer builder. Failure to converge is a hard error.

## Coin Selection

Initial deterministic selection:

- Prefer UTxOs that cover required non-lovelace assets.
- Prefer higher coverage of required asset quantity.
- Prefer lovelace coverage after token coverage.
- Break ties by canonical `TxOutRef` order.

This matches the current transfer intent without pretending to solve global
coin selection. Future strategies can be added behind explicit named options.

## Wallet Input Sources

Balancing records where selected-wallet candidate inputs came from:

- `provider`: fetched from the captured `MidgardProvider` snapshot.
- `instance-override`: provided through `LucidMidgard.overrideUTxOs(...)`
  before `newTx()`.
- `completion-preset`: provided through `complete({ presetWalletInputs })`.

Instance overrides and completion presets are local construction inputs only.
They do not rewrite Midgard node truth, hide missing-node-state failures, or
make a spend authoritative. Phase B/node validation can still reject a
transaction for missing inputs, double spends, tx-id conflicts, or script
failures.

Overrides are selected-wallet inputs. They are normalized before use, duplicate
outrefs are rejected, output bytes must decode as Midgard outputs, address
network must match the active config, and each input must belong to the
selected wallet payment key before it can be used for coin selection.

Builders snapshot the override generation at `newTx()` time. Replacing or
clearing overrides later cannot change an existing builder or in-flight
completion. `clearUTxOOverrides()` returns subsequent builders to provider
wallet input lookup.

## Change

Change must be explicit:

- Default change address is the selected wallet address.
- Caller may provide `changeAddress`.
- Change output is omitted only when the remaining value is exactly zero.
- Generated change is returned in completion metadata.

No asset may be burned implicitly by omission from change. Burning must be a
mint map entry with a negative quantity and required policy authorization.

## Insufficient Funds

Insufficient funds must include:

- Missing unit.
- Required quantity.
- Available quantity.
- Whether fee was included in the requirement.

Errors should be deterministic and serializable for CLI usage.

## Provider Race Conditions

UTxOs may be spent after fetch and before node validation. The builder can
locally validate against a snapshot, but Phase B may still reject with
`E_INPUT_NOT_FOUND` or `E_DOUBLE_SPEND`. Submission/status APIs must surface
that clearly.
