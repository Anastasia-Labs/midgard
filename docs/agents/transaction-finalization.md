# Transaction Finalization

For Cardano L1 transaction finalization with Lucid Evolution, always use local
UPLC evaluation: `.complete({ localUPLCEval: true })`, never
`.complete({ localUPLCEval: false })`. These options belong to the L1 builder;
Lucid Midgard L2 completion has its own API and validation semantics. Blind
spot: pre-existing uses are baselined with reasons, and a value set through a
spread or `Object.assign` is not seen ([lint-rules.md](lint-rules.md)).
[eslint: midgard/local-uplc-eval]

## Validity Windows

For off-chain SDK and node transaction builders, do not set `validFrom` exactly
at the current wall-clock time, `lucid.currentSlot()`, or an optimistic
Ogmios-tip-derived slot. Cardano node submit validation can lag the wall-clock
or tip estimate, so edge-triggered lower bounds can produce
`OutsideValidityInterval` races even when local slot evidence appears ready.
Blind spot: the lint follows the wall clock only within one file and does not
see `lucid.currentSlot()` or tip-derived slots ([lint-rules.md](lint-rules.md)).
[eslint: midgard/valid-from-wall-clock-margin]

- When transaction semantics allow a current-time lower bound, set `validFrom`
  at least 30 seconds before the current time; prefer 60 seconds for
  production/e2e paths unless a tighter protocol reason exists.
- When a protocol rule imposes a later lower bound, use
  `max(protocolLowerBound, currentTime - backoff)`. Never backdate before a
  smart-contract-required lower bound such as a scheduler shift boundary.
  [review]
- Recompute `validTo` from the chosen `validFrom` so max-validity-range
  constraints remain satisfied.
- If a submit error returns authoritative provider slot evidence such as
  `data.currentSlot`, recovery should wait against that slot delta rather than
  trusting an optimistic local tip estimate.

## Dependent Output Visibility

Exact transaction confirmation can precede provider output visibility. When the
next operation requires outputs from a node transaction, pass their resolved
`requiredOutputIndexes` to the submission helper. It checks those exact signed
outputs through the provider with a bounded retry; a fixed propagation sleep is
not a readiness check. Only declare outputs the next operation needs, since
unrelated recipients may already have spent theirs. A visibility timeout after
confirmation requires reconciliation of that transaction before rebuilding.
