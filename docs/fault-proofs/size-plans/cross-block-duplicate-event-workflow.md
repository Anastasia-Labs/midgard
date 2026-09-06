# Cross-block duplicate event installed workflow

Preserve the two physical validators. Init freezes the live challenged header;
step 01 opens its exact counted event root and freezes the hub-authenticated
settlement policy, event domain and event key. Step 02 opens the same key in a
distinct currently live settlement NFT's counted root and mints the permanent
proof. Shared removal corrects the challenged live block. Deposit, withdrawal,
and forced-order domains are separate; there is no wrongful-rejection polarity
for this category.

Discovery must use complete authenticated live settlement-address scans, the
current authentic hub NFT/datum, and retained historical DA reconstructed under
the exact settlement NFT header hash. Public historical provider quorum bytes
are only preimages; the live settlement NFT and counted roots provide authority.
No caller-provided historical header, settlement liveness Boolean or descriptor
is a production authority. Capture and recovery refresh the settlement authority
and refuse spent, rolled-back, substituted, or unrelated NFTs.

Journal artifacts retain exact challenged/settled payload envelopes, event
coordinate, settlement outRef and authority digest. Reconstruct commitments on
JSON restart and compare the selected current L1 settlement before capture.
Use central cursor transitions for Init, step 01, step 02, proof token, and
removal, persisting signed intent before broadcast. Cancellation remains the
existing explicit two-stage path.

Verify real registered lifecycles and durable intent recovery for all applicable
event domains, honest distinct keys/domains and same-header refusals, settlement
and payload substitutions, live-NFT withdrawal/rollback refusal, and maximum
counted-root evidence. Record complete signed lifecycle/publication measurements
with the shared current-blueprint VanRossem writer.

Installed-runner verification covers all three event domains through real
settlement minting, cursor initiation, both steps, permanent proof minting and
removal, with signed intents recovered through JSON journal restarts. Both
cancellation positions, substituted key/proof/settlement witnesses, missing live
NFTs, honest distinct events and domains are exercised. The required witness
and family publications are measured alongside lifecycle transactions.

The initial 77-row ledger measures the maximum 64-branch MPF path with compact
source leaves. Its largest signed transaction is 11,407 bytes; peak execution
is 3,208,126 memory and 1,048,086,341 CPU. It does not establish maximum leaf
carriage: a withdrawal with a 12,000-byte inline datum and the same 64-branch
proof requires 22,373 transaction bytes and is refused by the 16,384-byte
transaction limit. The follow-up replaces whole-value carriage with an MPF
value-digest opening for this key-only duplicate predicate. The admitted raw DA
artifacts continue retaining both complete original values and counted roots.

Regenerate the checked-in ledger against the normal testnet blueprint:

```sh
MIDGARD_WRITE_FIT_LEDGER=1 pnpm --dir demo/midgard-fault-proofs exec vitest run tests/cross-block-installed-lifecycle.test.ts
pnpm --dir demo/midgard-fault-proofs exec vitest run tests/cross-block-workflow-fit-ledger.test.ts
```
