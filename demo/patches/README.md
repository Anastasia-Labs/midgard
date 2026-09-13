# Dependency patches

`@lucid-evolution__lucid@0.6.4.patch` fixes delayed-redeemer bootstrap for
transactions whose inputs fund an explicit fee. The unpatched builder assigns
the entire maximum transaction execution budget before the delayed redeemers
exist. That provisional fee exceeds the exact funding required by availability
challenge validators, so construction fails before actual evaluation.

For an explicitly supplied minimum fee only, bootstrap now uses provisional
zero execution units to construct the canonical redeemer context. The subsequent
resolved transaction still runs local UPLC evaluation and the existing fee and
execution checks. Automatic fee selection retains its conservative bootstrap.
Both published module formats are patched through pnpm's locked patch mechanism.
The external Lucid checkout and generated dependency files are not source
authorities for this repository change.

The same Lucid patch normalizes inline datum CBOR before calculating minimum ADA
for canonical outputs. Static canonical completion and delayed-redeemer replay
set this mode before constructing outputs, so the minimum ADA calculation uses
the datum encoding that will appear in the completed transaction.

The Lucid patch also selects collateral from an explicit wallet
`getCollateral()` method when supplied. Wallets without that method retain the
existing selection from wallet inputs; an explicitly empty collateral list does
not fall back to ordinary spendable inputs. The
`@lucid-evolution__core-types@0.3.0.patch` declares this optional wallet method,
and `@lucid-evolution__wallet@0.2.2.patch` exposes a CIP-30 wallet's collateral
through it. Both JavaScript module formats and both type declaration formats
are covered by their respective patches.

The production SDK lifecycle tests in
`midgard-node/tests/availability-challenge-sdk-lifecycle.test.ts` verify signed
transactions with nonzero measured execution, fee refusal, and mainnet transaction
limits. These tests use the canonical delayed-redeemer callbacks throughout.

The existing `blake2b@2.1.4.patch` remains independently managed.
