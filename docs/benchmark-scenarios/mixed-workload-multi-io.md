# Mixed Workload Multi-IO Scenario

**Status:** Blocked
**Reschedule date:** 2026-07-16
**Blocking dependency:** Phase 1 mixed `mixed-multi-io` corpus template.

The Phase 5 scenario entrypoint exists as
`pnpm --dir demo/midgard-node run bench:l2:scenario:mixed-workload-multi-io`,
but it intentionally exits blocked until the corpus generator can produce a
deterministic mixed multi-input/multi-output template without `/utxos` lookups
during corpus construction.

When unblocked, set `STRESS_MIXED_MULTI_IO_READY=1` and provide a corpus whose
manifest identifies `mixed-multi-io`. The scenario should ramp separately from
the minimal-transfer ceiling and report the per-stage DB timestamp cost ratio
against the minimal-transfer baseline at the same offered rate.
