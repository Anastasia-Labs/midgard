# Pstack for Midgard

This repository ports pstack's engineering workflows to Codex and Claude Code.
The shared source is `.agents/skills`; `.claude/skills` points to that same tree.
No plugin installation or global settings change is needed.

## Start a task

In Codex, start a new session in Midgard and type:

```text
$poteto-mode reproduce this bug, fix its root cause, and verify the change.
```

In Claude Code, use:

```text
/poteto-mode reproduce this bug, fix its root cause, and verify the change.
```

The entry point selects one of 23 playbooks and routes to the existing Midgard
skills for contracts, consensus review, tests, CI, commits, and acceptance.
You can invoke other skills directly, for example `$interrogate` in Codex or
`/interrogate` in Claude. Subordinate references can be read directly by path.
The mode is a conversation instruction; neither host needs a custom mode hook.

Use `$setup-pstack` or `/setup-pstack` to change role choices in
[the shared configuration](../../../.agents/pstack-models.json).
Workers inherit the current host. Arena, architect, and interrogate panels
have Codex and Claude seats. Models are unset by default, so each provider uses
its runtime default. Model IDs and effort are configured separately. The
configuration does not assert which models an account can access.

## Cross-provider review

Native delegation stays on the current host. Cross-provider design or review
uses the installed `codex` and `claude` CLIs with bounded read-only execution.
Both CLIs need working authentication through their existing setup. The parent
supplies sanitized instructions, source, and diff excerpts in a prompt file.
The parent neither logs in nor copies credentials. Write each seat's result to
a new path. An unavailable CLI, denied model, empty response,
timeout, or failed process remains a coverage gap. Cross-provider review does
not claim both providers ran when one failed. Writable work uses native agents
and the current permission policy.

Read [the host adapter](../../../.agents/skills/poteto-mode/references/host-adapter.md)
when changing dispatch, history access, or scheduling. It records the native
schemas, CLI restrictions, and missing-capability behavior. CLI flags were
checked against local Codex 0.160.0 and Claude Code 2.1.257 on 2026-10-03.

## What was adapted

All 49 bundled skills and all 23 playbooks are present. Engineering principles,
review rubrics, design prompts, benchmark guidance, and source-investigation
references retain their upstream substance. Entry points and playbooks use
Midgard's tools and authorization rules.

The port replaces model-slug defaults, custom agent registration, transcript
paths, cloud-only spawning, webhook UI assumptions, and the Bun script package.
PR state uses GitHub tools or `gh`; program coordination uses task-local records
and actual host agent tools. The optional upstream Benny automation templates
are not installed as services or scheduled jobs. No task is scheduled by setup.

Pstack simplification and comment review preserve protocol rationale and
required checks. The existing Midgard skills own verification requirements;
plans choose live and performance gates based on the changed behavior.

## Verify edits to the port

```bash
node scripts/ci/check-agent-skills.mjs
node scripts/preflight.mjs
```

## Provenance

Adapted from [Lauren Tan's pstack](https://github.com/cursor/plugins/tree/main/pstack),
version 0.15.6 at commit
`23e4138daa01c42d4969f7a5465f82704e64f798` (retrieved 2026-10-03).
The upstream MIT notice is preserved in [LICENSE](LICENSE).

Host references used while porting:

- [Codex configuration](https://learn.chatgpt.com/docs/config-file/config-reference).
- [Codex subagents](https://learn.chatgpt.com/docs/agent-configuration/subagents).
- [Claude skills](https://code.claude.com/docs/en/skills).
- [Claude CLI](https://code.claude.com/docs/en/cli-reference).
