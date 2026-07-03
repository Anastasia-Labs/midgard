# Transaction Finalization

For transaction finalization in this repository, always use local UPLC
evaluation.

- Use `.complete({ localUPLCEval: true })`.
- Never set `.complete({ localUPLCEval: false })`.

## Validity Windows

For off-chain SDK and node transaction builders, do not set `validFrom` exactly
at the current wall-clock time, `lucid.currentSlot()`, or an optimistic
Ogmios-tip-derived slot. Cardano node submit validation can lag the wall-clock
or tip estimate, so edge-triggered lower bounds can produce
`OutsideValidityInterval` races even when local slot evidence appears ready.

- When transaction semantics allow a current-time lower bound, set `validFrom`
  at least 30 seconds before the current time; prefer 60 seconds for
  production/e2e paths unless a tighter protocol reason exists.
- When a protocol rule imposes a later lower bound, use
  `max(protocolLowerBound, currentTime - backoff)`. Never backdate before a
  smart-contract-required lower bound such as a scheduler shift boundary.
- Recompute `validTo` from the chosen `validFrom` so max-validity-range
  constraints remain satisfied.
- If a submit error returns authoritative provider slot evidence such as
  `data.currentSlot`, recovery should wait against that slot delta rather than
  trusting an optimistic local tip estimate.
