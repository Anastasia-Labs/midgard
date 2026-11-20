# Production L2 Principles

Midgard is intended to be a production-grade L2. Interpret prompts, design
choices, code changes, and review feedback through that lens.

## Decision Standard

Prefer options that maximize:

- Protocol correctness and state integrity
- Safety under adversarial conditions
- Determinism and reproducibility
- Operational reliability and failure recovery
- Auditability, traceability, and clear observability
- Explicit, auditable migrations and explicit configuration

## Priority Order

When tradeoffs are required, prioritize:

1. Correctness
2. Safety
3. Liveness
4. Performance
5. Convenience

## Non-Goals

Unless explicitly requested and isolated, do not introduce:

- Benchmark shortcuts that weaken correctness guarantees
- Silent data rewrites that hide integrity issues
- Demo-only behavior as default production behavior
- Throughput optimizations that compromise safety or liveness guarantees
- Backward compatibility for unlaunched runtime formats, local state files,
  scaffold APIs, previous in-repo shapes, legacy UTxO layouts, alias fields, or
  compatibility shims

## Temporary Code

Temporary or debug-only code, configuration, and instrumentation may be used
during testing and diagnosis, but must be removed or cleaned up before
finalizing work.

If a temporary workaround is unavoidable, it must be isolated, explicitly
documented, and unavailable as the default path.

## Benchmark and Demo Behavior

If a change improves benchmarks but weakens production guarantees, do not make
it the default. Keep strict behavior as the default and isolate non-production
behavior behind clearly named controls.
