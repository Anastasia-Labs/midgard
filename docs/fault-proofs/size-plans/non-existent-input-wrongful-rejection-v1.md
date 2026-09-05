# Non-existent input wrongful rejection size and transition plan

The accepted direction retains its four registered steps and existing absence
checks. The forced direction contradicts exactly `InputNotFound` with spend
source kind zero and the reason's authenticated input index. A present input
must be shown in the ledger immediately before this forced event, rather than
merely in the header's previous-block ledger. An index outside the authenticated
spend-input collection is also an impossible reason and therefore contradicts
this rejection.

The four forced transitions are:

1. Bind the header, counted forced-source membership, exact rejected source,
   transaction identity, and reason index. Carry the event and trace commitments.
2. Open field zero through the existing inline/publication/certificate door and
   select the reason's index. Authenticate the event-to-step mapping. Carry an
   optional selected input, the mapped step index, and trace commitment.
3. Authenticate the mapped transition's index, event, phase, and schema; carry
   its pre-UTxO root and the optional input.
4. Verify exact-key ledger membership using its 32-byte value hash when the input exists, or the authenticated
   impossible-index contradiction otherwise, and mint the shared proof token.

Each transition carries at most one potentially maximum-depth MPF proof. Measure
64-level source, event, transition, and ledger proofs independently and together
with the maximum field-zero opening. Published or certified field carriage must
keep the spend-input preimage out of a transaction that already carries a large
mapping proof. No compiler or emulator limit override is permitted.

The four physical validators and deployment parameters remain unchanged. The
accepted state and later argument constructors stay at constructor zero; forced
variants occupy constructor one. Step one's source selector wraps its previous
accepted carriage. Cancellation remains available at every boundary.

Durable preparation retains authenticated source, event mapping, transition,
field material, and ledger witness bytes, then rechecks their commitments on
admission and restart. Complete replay derives ledger witnesses from retained
canonical predecessor state and committed event effects; it never trusts a
caller-supplied membership verdict. Registered Lucid lifecycle evidence must
include honest rejection refusal, source/index/root mutations, cancellation and
restart, proof mint, and real state removal. Measurements belong in the adjacent
fit ledger after execution; this document asserts no unmeasured fit result.

## Verified implementation and regeneration

The terminal uses the existing `mpf_proof_v1.has_value_hash` verifier. Presence
requires no output decoding and is independent of ledger-value length. Artifact
admission verifies the full retained value and derives the hash for submission.
The forced submitted preimage retains its original validity scalar; source
comparison re-adjudicates it to `TxIsInvalid`, matching canonical reconstruction.

The certified maximum is 819 fixed-width spend inputs, 32,763 field bytes. Each
of the four independent MPFs has 64 branch levels, with source/event/trace counted
root counters remaining one. The synthetic roots deliberately represent an
adversarial commitment; they are not claims that a one-leaf honest trie has a
64-level path. The registered lifecycle uses the real state queue and proof mint
and removes the challenged block. Separate admitted-DA replay tests reconstruct
an honest prior ledger and verify that an earlier forced spend makes a later
`InputNotFound` honest even when the input existed in the previous block.

The checked-in ledger is regenerated from complete signed emulator transactions:

```sh
export PATH=/home/gumbo/.local/share/pnpm:/home/gumbo/.nvm/versions/node/v22.22.2/bin:$PATH
export MIDGARD_REAL_BLUEPRINT_PATH="$PWD/onchain/aiken/plutus.json"
(cd onchain/aiken && flock /tmp/midgard-nip-aiken.lock aiken build --env testnet)
MIDGARD_WRITE_FIT_LEDGER=1 pnpm --dir demo/midgard-fault-proofs exec vitest run tests/non-existent-input-wrongful-rejection-lifecycle.test.ts
pnpm --dir demo/midgard-fault-proofs exec vitest run tests/non-existent-input-fit-ledger.test.ts
pnpm --dir demo/midgard-fault-proofs exec vitest run tests/submit-init-emulator-ledger-rules.test.ts -t non-existent-input
```

No limit override is used. The shared Van Rossem writer enforces positive byte,
memory, and CPU margins and the 512-byte reserve for reference publications.
The verifier binds the ledger digest and every margin to the current blueprint.

| Maximum forced stage | Signed bytes | Memory | CPU |
| --- | ---: | ---: | ---: |
| step-1 | 10,667 | 3,245,930 | 1,060,879,945 |
| step-2 | 10,172 | 3,546,953 | 1,083,028,234 |
| step-3 | 9,718 | 2,794,901 | 849,735,700 |
| step-4 | 9,790 | 3,814,759 | 1,148,767,225 |

Verification: five registered forced lifecycles (including raw honest refusal,
source/event/transition/value mutations, every cancellation boundary, and JSON
reloads); 21 focused replay/preparation/accepted-artifact tests; 18/18 Aiken
family selectors; accepted four-step lifecycle through real removal; TypeScript
and scoped ESLint. Installed workflow observation reads both accepted and forced
state constructors and resumes their matching stage route.
