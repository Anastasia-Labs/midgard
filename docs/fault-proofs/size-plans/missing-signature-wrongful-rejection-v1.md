# Missing-signature wrongful rejection size plan

The accepted-invalid four-step absence walk remains unchanged. Step 01 adds a
forced dispatch constructor while retaining Cancel=0 and Continue=1. Its new
successor parameter authenticates the separate forced branch.

The forced branch has four transactions after Init:

1. Step 01 forwards the constant direction marker to `forced-step`.
2. `forced-step` authenticates the thread's header, counted forced-root
   membership, leaf source, rejected validity code and exact
   `RequiredSignerUnsigned { signer_index }` reason. It freezes the source
   coordinate, transaction ID, witness-set hash and signer coordinate.
3. `forced-signer` opens field 4 against that ID. It forwards the required hash
   at exactly the operator's coordinate. An out-of-range coordinate forwards
   None, proving that this reason names no required signer.
4. `forced-witness` opens field 7 against both anchored ID and witness-set hash,
   selects an authenticated witness, checks its key hash against the required
   signer and verifies its Ed25519 signature over the ID. It burns the thread
   and mints the permanent proof. For the impossible-coordinate arm it requires
   no opening and canonical witness index zero.

Each step is independently cancellable. Recovery fetches the current out-ref,
identifies the physical script and checks the entire canonical datum against
prepared authenticated evidence. Every submission passes the shared durable
pre-submit boundary before broadcast. The production planner accepts only a
CanonicalBlockEvidence and reconstructs verdict, coordinate, evidence and
membership internally; it compares all three retained source components to the
committed leaf before classification.

## Fit decision

The initial design separates binding, signer opening and witness verification:
no transaction opens both potentially maximal fields. Both fields use fixed
strides (30 bytes per signer envelope, 103 per witness envelope), so certified
carriage authenticates count arithmetically from mint-verified total length.
No universal witness scan or variable-width grammar phase is needed for the
existential signed-witness contradiction.

The maximum raw fields are 504 signers (15,123 bytes) and 146 witnesses (15,040
bytes). The maximum aggregate-bounded shapes keep the other relevant field at
one item: 1,088 signers (32,643 bytes) or 317 witnesses (32,654 bytes). These
include the required signer or witness at the final ordinal; earlier entries
are unrelated. The next entry would exceed the 32,768-byte aggregate field
budget once the other field and remaining empty fields are included.

The real Lucid lifecycle measures signed reference publication, all field chunk
publications, certification, Init, every forced step, permanent mint and removal.
All transactions satisfy the real 16,384-byte limit and the shared 20% execution
reserve. The adjacent raw-to-certified boundary uses the same authenticated
fixed-stride primitive as the maximum certified shapes. Exact measurements and
the blueprint digest are recorded in
`missing-signature-wrongful-rejection-v1-fit-ledger.json`.

## Verification

- The Aiken source binding selectors cover header/root/count/key/source/reason,
  polarity, input/output positions and handoff substitutions.
- Signer selectors cover the exact coordinate, impossible coordinates, forged
  hashes and false absence at an existing coordinate.
- Terminal selectors refuse present-but-forged signatures and missing openings.
- The Lucid lifecycle starts from registered Init, cancels every position,
  resumes by out-ref, mints the permanent proof and removes the block. An honest
  rejection reaches the genuine terminal validator with a forged signature and
  is refused, even when the prover fabricates off-chain signature evidence.
- Accepted-path Lucid lifecycle, negative and adversarial suites remain green.

- Installed prerequisite tests automatically publish and certify both aggregate
  maxima. Each publication/certificate capture is reconstructed and reconciled
  from its serialized recovery record before advancing; no certificate is
  supplied to the family submitter. The installed watcher resolves the mint
  reference from the authenticated deployment manifest.
- The journal test reloads retained evidence for stages 1, 5, 6 and 7 and
  captures the real installed transaction port. Header, transaction bytes and
  forced membership mutations are refused on admission.

Regenerate the shared Van Rossem ledger from measured submissions:

```sh
MIDGARD_WRITE_FIT_LEDGER=1 pnpm --dir demo/midgard-fault-proofs exec vitest run tests/missing-signature-wrongful-rejection-lifecycle.test.ts
pnpm --dir demo/midgard-fault-proofs exec vitest run tests/missing-signature-wrongful-rejection-fit-ledger.test.ts
```

The verifier hashes the current `realBlueprintPath`, reconstructs the shared
ledger digest and verifies all 144 named measurements. The lifecycle records
maximum shape names, publication and proof actions, cancellations and removals.
