---
name: setup-pstack
description: "Configure pstack roles for Codex and Claude, including model and effort overrides."
disable-model-invocation: true
---

# Setup pstack

Read [the host adapter](../poteto-mode/references/host-adapter.md) for dispatch and capability rules.

1. Read `.agents/pstack-models.json` at the repository root. It is the shared configuration for both hosts, loaded when a workflow starts rather than through an always-loaded rule. [review]
2. Inspect the active host's exposed models and the installed `codex exec --help` and `claude --help`. A CLI's presence proves installation, not model entitlement or authentication. Keep model and effort unset unless the user specifies supported values. [review]
3. Apply requested role changes. A single role is one object; a panel is a nonempty array. Providers are `native`, `codex`, and `claude`. `native` inherits the active host. An unset model uses that provider's configured default. Model IDs and effort are separate fields. [review]
4. For an unspecified setup, keep the shipped defaults: native workers, Codex and Claude panels. Explain that panels use each CLI's own default model. Ask only for unresolved user preferences, using the host's question tool when available. [review]
5. Validate before saving with `node .agents/skills/poteto-mode/scripts/dispatch.mjs --check-config <candidate.json>`. Save the complete validated configuration to `.agents/pstack-models.json`, preserving unrelated roles. A rerun with the same choices produces the same JSON. [review]
6. Show changed roles and any unavailable capabilities. Explicit model failures remain failures; resolve them with the user instead of silently substituting a model. [review]

Example role values:

```json
{
  "feature, refactoring": {"provider": "native"},
  "interrogate reviewers": [
    {"provider": "codex"},
    {"provider": "claude", "model": "opus", "effort": "high"}
  ]
}
```

The configuration changes pstack dispatch only. It does not edit global host settings, install software, log in, or create scheduled work. Midgard already has verification harnesses; use the routing table in [poteto-mode](../poteto-mode/SKILL.md) before generating another.
