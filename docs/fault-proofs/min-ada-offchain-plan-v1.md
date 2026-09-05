# Minimum-Ada fault — implementation reference

The registered `minAda` family uses five spending scripts and authenticated
step-02 transaction/UTxO withdrawals. It proves accepted transaction output
underfunding, newly introduced post-state UTxO underfunding, and wrongful forced
`OutputBelowMinAda` rejection for the exact authenticated output index.

Step 01 binds accepted, forced, or post-UTxO evidence. The transaction withdrawal
certifies output-field grammar and advances an authenticated indexed walk in
bounded batches. Step 03 scans the selected canonical output in bounded batches,
then applies the compiled minimum-Ada formula to its exact byte length and
lovelace. Accepted evidence requires underfunding; wrongful rejection requires
sufficiency, including the exact floor. The post-UTxO route authenticates the
committed descriptor, applies the same predicate, and proves predecessor
non-membership. Step 05 mints permanent evidence for registered-chain removal.

Source artifacts preserve submitted full transaction bytes and authenticate the
invalid adjudication committed by a forced leaf. Installed complete replay and
`createMinAdaWorkflowRunner` support both directions and resume step-02/03
selfloops from authenticated L1 checkpoints. Direct submit helpers complete their
bounded continuations when confirmation is enabled; without confirmation they
return the submitted continuation's `nextStepIndex` for later observation.

See [the size and transition plan](size-plans/min-ada-wrongful-rejection-v1.md)
and its standard fit ledger for measured maximum output, field, asset, descriptor,
and MPF carriages. Standalone registered lifecycles cover cancellation, restart,
permanent mint, and removal. The separate `submit-init-emulator-min-ada.test.ts`
exercises the legacy interactive validation-trace route and is not the standalone
family's acceptance surface.
