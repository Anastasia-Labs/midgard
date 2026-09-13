# Domain documentation

Before changing protocol terminology or architecture, read the relevant terms
in [CONTEXT.md](../../CONTEXT.md) and decisions in
[docs/midgard/decisions](../midgard/decisions/). Fault-proof-specific decisions
live in [docs/fault-proofs/decisions](../fault-proofs/decisions/README.md).

Use the glossary's names when describing domain concepts. Record new decisions
in the existing decision directory for that concern; a separate `docs/adr/`
tree is unnecessary. Each ADR states context, decision, consequences, status,
and links to the implementation or normative specification. Keep exact format
rules in [component specifications](../spec/README.md).

When a proposed change contradicts an accepted decision, identify the decision
and explain why it should be superseded. Preserve its rationale and link it to
the replacement rather than silently rewriting the historical decision.
