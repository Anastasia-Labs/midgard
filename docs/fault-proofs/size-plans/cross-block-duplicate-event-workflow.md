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

The complete-leaf carriage failed the maximum gate: a withdrawal with a
12,000-byte inline datum and 64-branch proof required 22,373 transaction bytes,
above the 16,384-byte limit. The fourth proof constructor therefore opens the
same MPF leaf using its Blake2b-256 value digest. Existing full-value constructors
retain their behavior. Both physical steps bind the domain, counted root,
positive count, exact key and strict MPF digest membership; step 02 additionally
binds the live settlement NFT and frozen key/domain. A substituted value digest
cannot satisfy the authenticated root. No value-semantic claim is needed by the
duplicate-identity predicate.

The installed preparation constructs digest openings from canonical retained
values. Artifacts retain complete original values and counted roots through JSON
restart. The SDK preserves Plutus Data map order while deriving the digest,
matching on-chain serialiseData. Real deposit and withdrawal installed scenarios
retain 16,384-byte datum payloads. Maximum direct openings cover 16,947–17,173-byte
source values plus 64 widest MPF branches in each domain; value size no longer
contributes to the transaction envelope. The final 77-row ledger's largest
signed transaction is 12,300 bytes (reference publication), leaving 3,572 bytes
below the reserved publication ceiling. Peak execution is 4,138,820 memory and
1,267,356,242 CPU. The normal testnet blueprint SHA-256 is
`8094a2fe8bff9945cb933e46849ec8401de1883f6d2c9babfa6a32880562d2d1`.

Verification includes four Aiken digest binding/refusal checks, seven SDK wire
checks, all six real installed/maximum lifecycle scenarios, and unchanged
full-value constructor emulator regressions. The publication and manifest
factories apply the rebuilt two-validator ABI and all required witness scripts.

Regenerate the checked-in ledger against the normal testnet blueprint:

```sh
MIDGARD_WRITE_FIT_LEDGER=1 pnpm --dir demo/midgard-fault-proofs exec vitest run tests/cross-block-installed-lifecycle.test.ts
pnpm --dir demo/midgard-fault-proofs exec vitest run tests/cross-block-workflow-fit-ledger.test.ts
```
