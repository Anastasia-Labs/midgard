# Midgard Agent Directive

All prompts, design choices, and code changes in this repository must be interpreted through one primary lens:

- This codebase is intended to be a **production-grade L2**.

## Decision Standard

When making any implementation or operational decision, prefer options that maximize:

- Protocol correctness and state integrity
- Safety under adversarial conditions
- Determinism and reproducibility
- Operational reliability and failure recovery
- Auditability, traceability, and clear observability
- Backward-compatible migrations and explicit configuration

## Explicit Non-Goals (Unless Clearly Requested and Isolated)

- Benchmark shortcuts that weaken correctness guarantees
- Silent data rewrites that hide integrity issues
- Demo-only behavior becoming default production behavior
- Throughput optimizations that compromise safety or liveness guarantees

## No Shortcuts Policy

- Shortcuts are not allowed for default behavior.
- Do not cut corners to make code "work" quickly if it weakens correctness, safety, or operability.
- Implementations must follow the right approach for a production L2, even when it is more complex or slower to ship.
- If a temporary workaround is unavoidable, it must be clearly isolated, explicitly documented, and never the default path.

## Priority Order

When tradeoffs are required, prioritize:

1. Correctness
2. Safety
3. Liveness
4. Performance
5. Convenience

## Practical Rule

If a change improves benchmarks but weakens production-grade guarantees, do not make it the default. Keep strict behavior as default and isolate non-production behavior behind explicit, clearly named controls.
