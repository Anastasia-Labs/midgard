# CEK publication exclusion binding

Status: CG1 input with an unresolved historical exclusion; not current proof-fit acceptance.

Documentation reduced: 2026-09-07. No measurement or identity was refreshed.

## Executable consumer

`demo/scripts/verify-canonical-v1-cg1-control-publication-fit.mjs` reads this
file from the Git index and checks the exact quotation in
`canonical-v1-cg1-control-publication-fit-v1.json`'s `exclusions[0].recordedIn`.
That executable dependency is the reason this small historical binding remains.
The gate's exclusion is not independently validated by repeating its quotation.

## Bound historical quotation

The gate requires this exact historical quotation:

```text
a4bfbd01e9a07dc2165a2cc8c4e00a4775b82fa754e3136c878d2666`, **162,145 bytes**
```

Its signed publication measured 162,660 bytes, exceeding 16,384 by 146,276.
These are the old exclusion's values, not current script-size claims.

## Superseding measurement and remaining decision

The same CG1 artifact's `openItems[0].remeasured` records the later 2026-08-23
publication: 5,471 applied bytes, 5,984 signed bytes, and a 10,400-byte margin,
with applied hash `0388dc27438025a6f20e9b6077c1d37b08e482cc5b53c1b7a7de9d28`.
That receipt bound blueprint SHA-256
`c55917de0f9b479046f601431ea3e088879b2ebfed3150dbe87dd98933b80f00`.
It already refutes the older oversized-body justification, but did not change
the runtime reference-script roster or the gate's exclusion. Reconcile that
roster and exclusion against the current deployment and fresh publication
measurements; retaining a matching quotation cannot close that work.

The retained publication driver is
`demo/midgard-node/tests/scratch-cg1-publication-fit.test.ts`. It measures
roster members, so it cannot establish an excluded role's fit merely by
passing. Current resolver and material-traversal decisions are in
[Publishable semantic resolvers](../../../fault-proofs/decisions/0003-publishable-semantic-resolvers.md).
The old monolithic-route diary and superseded pin chains have been removed.
