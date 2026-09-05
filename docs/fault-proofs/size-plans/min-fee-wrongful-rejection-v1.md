# Min-fee wrongful rejection: transition and size sketch

The registered two-step chain retains the accepted-invalid path. Step 01 gains
an explicit accepted/forced source discriminator. Forced binding authenticates
the thread header, counted forced root and count, order key, native proof-source
triple, invalid verdict and exactly `FeeBelowMinimum`. The next datum retains
the canonical verdict subject beside compact transaction and header schedule.
Step 02 authenticates all nine field lengths using existing field doors and
compares the canonical size with the same non-negative header schedule. The
accepted direction requires fee < minimum; wrongful rejection requires fee >=
minimum. Equality is an honest transaction and therefore contradicts rejection.
Cancellation remains available at each live step.

State is constant size: subject (two integers plus version, transaction id,
forced order-key CBOR and nullable reason), compact transaction, fee, id and two
schedule integers. Forced binding carries one header and counted-root proof;
field data belongs only in terminal carriages or referenced publications. The
initial measurement candidates are inline, raw reference, certified reference,
maximum counted-root proof, all nine nonempty fields, and the largest fields
supported by each carriage. Every script/field/chunk publication, Init, bind,
terminal and removal must fit 16,384 signed bytes and 80% of the configured CPU
and memory maxima. If simultaneous authentication of nine lengths exceeds the
reserve, split forced length authentication into bounded continuation steps;
do not reduce supported source sizes or bypass evaluation.

This is a design sketch, not measured fit evidence. The fit ledger will record
actual lifecycle measurements before the family is marked complete.

## Measured implementation

The first all-nine populated terminal consumed 15,811,068 memory units, above
the 13,200,000 reserve limit. Its cost came from walking variable-width field
grammars before reading their byte lengths. The final forced branch uses the
existing `authenticated_committed_preimage` door and `bytearray.length`:
transaction id, anchored witness-set hash, positional field commitments, and
certificate/chunk bindings remain authenticated, while no item count or item
boundary is claimed. The accepted branch retains its existing field-view path.
No additional script, step, or mutable intermediate checkpoint is needed.

The real registered Lucid lifecycle now measures six complete shapes through
permanent mint and state-queue removal. Every family script publication,
carriage publication, certificate-script publication, certification, Init,
forced binding, terminal and removal-reference publication is captured. The
named entries in the sibling fit ledger comprise two family publications,
Init, binding, all field prerequisites, terminal, removal reference
publications, and removal.

- 358 inputs: largest fixed-stride field below the inline threshold, explicitly
  published by the durable driver to preserve envelope reserve.
- 378 inputs: largest raw field-0 shape, 15,123 preimage bytes.
- 379 inputs: adjacent certified shape, 15,163 preimage bytes.
- 819 inputs: maximum fixed-stride field-0 shape under the field door’s 32,768-byte cap,
  32,763 preimage bytes, authenticated across three chunks.
- All nine populated: two input fields and all seven other fields together,
  including four variable-width field grammars. This is the regression shape
  that rejected the original eager field-view implementation.
- Maximum MPF path: 64 widest Branch steps combined with the maximum certified
  field-0 shape. The synthetic proof reconstructs the exact challenged root;
  the malicious header's count is independently bound, as in the live rule.

The final ledger captures 119 transactions across these six shapes. Maximum
signed size is 15,872 bytes (the exact 512-byte publication reserve); maximum
memory is 3,430,167 and maximum CPU is 1,118,500,434, leaving 9,769,833 memory
and 6,881,499,566 CPU below the 80% limits. The largest applied family reference
publication is 14,903 bytes. No upper field size or proof depth was reduced to
obtain these margins.

Durable recovery stores canonical forced-source CBOR and submitted full
transaction bytes, header hash, selected detection id and forced position.
Admission reconstructs the operator-adjudicated invalid source, authenticates
header/count/root/MPF/source and exact reason, and re-derives all nine lengths
and economics. A serialize/reopen/live-bind/reopen/finalize/remove test proves
that the resumed evidence matches the actual step-02 datum. Trailing source
CBOR, source substitutions, altered identity and unexpected fields are refused.

## Consolidated evidence

The integrated testnet blueprint is
`9cd5a8b1f19c2cb5977f69062547e3be24617bf97470a598347d349bd927becc`.
The adjacent ledger uses the shared Van Rossem writer, with named rows and
recomputed byte, memory, CPU, and publication margins. Its test requires the
current blueprint digest and compiler version. The combined minFee and
invalidSignature family regression passed 63 tests on this tree.

Regenerate with `MIDGARD_WRITE_FIT_LEDGER=1` while running the complete
`tests/min-fee-wrongful-rejection-lifecycle.test.ts` against
`MIDGARD_REAL_BLUEPRINT_PATH`, then run the family fit-ledger test.
