# Benchmark-gated runtime options

Status: Accepted defaults; changing them requires separate measured acceptance.

Recorded: 2026-09-07 from the delivered throughput design and current configuration.

## Decision

Keep `MPF_ENGINE=legacy` and `SPECULATIVE_COMMIT_BUILD=false` as defaults.
Architecture G and speculative commit building are explicit opt-ins; their
implementation or a short benchmark does not authorize a default change.

Architecture G requires final-build differential and crash/recovery coverage,
50k and retained-growth root/candidate gates, release-image verification, a
clean live lifecycle, and the complete soak on matching identities. The
[operator closure](../../benchmark-scenarios/phase-3-architecture-g-closure.md)
and [soak procedure](../../benchmark-scenarios/phase-3-architecture-g-soak.md)
provide the retained acceptance commands.

Speculative building does not authorize submitting children of unconfirmed L1
commits. The [one-hour gate](../../benchmark-scenarios/phase-4-pipelined-one-hour.md)
measures the current pipeline. Unconfirmed chaining needs a separate design for
rollback, journals, provider acceptance, and on-chain state linkage. Multi-block
merge likewise requires an explicit validator and recovery assessment.

DA capacity is a consensus and memory-admission boundary, not an environment
escape hatch. The retired 50k distribution fixture could not be regenerated
within the canonical payload bound, so its former timing results and commands
were removed. Any replacement must use the current payload format and actual
publication measurements; a fixed transaction count alone proves neither fit
nor latency. A future format change follows
[prelaunch replacement rules](prelaunch-format-replacement.md) or the relevant
shipped-version upgrade policy.

## Consequences

Acceptance must bind the actual runtime, corpus, applied deployment, topology,
and producer/consumer evidence. Failed capacity or recovery gates remain failed;
changing defaults, inflating limits, or shortening runs cannot discharge them.
