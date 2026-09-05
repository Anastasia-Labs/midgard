# No-reference-input wrongful rejection size and transition plan

This extension retains the four registered accepted-direction validators and
all deployment parameters. The forced direction contradicts exactly
`InputNotFound { source_kind: 1, input_index }`, using the authenticated reason
index to select field one. A present reference input is proven under the ledger
root immediately before the forced event. The header's previous-block root
alone is insufficient: earlier events may consume or create the referenced
output. An index outside the authenticated reference-input collection also
contradicts this reason.

The four forced transitions bind: (1) the header and counted forced-source
membership; (2) field-one selection and event-to-step membership; (3) the exact
mapped transition, event, phase, schema and pre-state root; (4) exact-key ledger
membership using a 32-byte value hash, or the impossible-index contradiction,
followed by the shared proof mint. At most one maximum-depth MPF proof appears
in each transition. Test the full 819-item reference field with 64 branch levels
in all four MPFs; counted roots retain their authenticated claimed count.

Accepted states and arguments keep constructor zero. Forced variants occupy
constructor one. Only the first source argument acquires an explicit accepted
or forced wrapper; the accepted builder must change with it. Field one retains
its positional anchor even when field zero has identical item hashes.

Retained DA preserves submitted transaction bytes. Source verification derives
the rejected (`TxIsInvalid`) compact source without changing those bytes.
Durable artifact admission rechecks source, header, count, event, transition,
index and ledger commitments. Replay reconstructs committed prior-event effects
from authenticated predecessor ledger descriptors. Every cancellation boundary,
JSON restart, honest and mutation refusal, proof mint and real removal requires
registered Lucid coverage. The shared Van Rossem writer uses
`MIDGARD_WRITE_FIT_LEDGER=1` and the adjacent fixed ledger path; its verifier
must bind the current blueprint and positive margins. No fit is claimed before
measurement and no compiler, publication, or evaluator limit may be raised.

Measured with the pinned testnet blueprint: 95 signed transaction rows cover
four forced stages, field publication/certification, cancellation at all four
boundaries, proof mint and removal. The maximum field contains 819 references
(32,763 bytes), with 64-level proofs independently authenticating source,
event mapping, transition and ledger membership. The four maximum stages use
10,667 / 10,172 / 9,718 / 9,790 signed bytes and
3,245,930 / 3,551,456 / 2,794,901 / 3,819,583 memory units;
the maximum CPU is 1,150,168,070. Every recorded limit margin is positive.

Validation: 14 Aiken tests; five forced registered lifecycle scenarios; five
installed replay/admission scenarios; three accepted registered lifecycle
scenarios (including large field publication and honest refusal); one current
blueprint fit-ledger verifier. Forced lifecycle artifacts retain submitted-valid
full transaction bytes while authenticating the rejected committed source.

Regenerate from the repository root after the locked pinned testnet build:

```sh
PATH=/home/gumbo/.local/share/pnpm:/home/gumbo/.nvm/versions/node/v22.22.2/bin:$PATH \
MIDGARD_REAL_BLUEPRINT_PATH="$PWD/onchain/aiken/plutus.json" \
MIDGARD_WRITE_FIT_LEDGER=1 \
pnpm --dir demo/midgard-fault-proofs exec vitest run \
  tests/no-reference-input-wrongful-rejection-lifecycle.test.ts
```

Run `tests/no-reference-input-fit-ledger.test.ts` against that same blueprint
without the writer flag to verify its digest and all recorded margins.
