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
The dispatcher neither logs in nor copies credentials.

Preview a Claude seat from a Codex session without calling a provider:

```bash
node .agents/skills/poteto-mode/scripts/dispatch.mjs \
  --role 'interrogate reviewers' --seat 1 --host codex \
  --workspace "$PWD" --prompt /tmp/review-brief.txt \
  --output /tmp/claude-verdict.txt --dry-run
```

Remove `--dry-run` to run it. The result file is created exclusively; choose a
new path for each seat. An unavailable CLI, denied model, empty response,
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
The Node dispatcher and TSV helper use built-in modules and ship negative tests.
PR state uses GitHub tools or `gh`; program coordination uses task-local records
and actual host agent tools. The optional upstream Benny automation templates
are not installed as services or scheduled jobs. No task is scheduled by setup.

Pstack simplification and comment review preserve protocol rationale and
required checks. The existing Midgard skills own verification requirements;
plans choose live and performance gates based on the changed behavior.

## Verify edits to the port

```bash
node scripts/ci/check-agent-skills.mjs
node --test .agents/skills/poteto-mode/scripts/dispatch.test.mjs
node --test .agents/skills/show-me-your-work/scripts/log.test.mjs
node .agents/skills/poteto-mode/scripts/dispatch.mjs \
  --check-config .agents/pstack-models.json
node scripts/preflight.mjs
```

The existing Agent Skills CI discovers the new helper tests without another
workflow. Provider invocation tests use subprocess fixtures to exercise literal
stdin, errors, limits, and output preservation without model access. They do not
prove account entitlement or live model behavior.

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

## Validation of this port

On 2026-10-03:

| Check | Result |
| --- | --- |
| `node scripts/preflight.mjs` | Passed all eight selected checks, including docs-site build and typecheck. |
| `node scripts/ci/check-agent-skills.mjs` | Passed; 64 skills and the Claude symlink checked. |
| `node --test .agents/skills/poteto-mode/scripts/dispatch.test.mjs .agents/skills/show-me-your-work/scripts/log.test.mjs` | Passed all 10 tests after the final adapter fix. |
| Dispatcher `--check-config .agents/pstack-models.json` | Passed. |
| Prettier `--check` on the configuration and four new helper/test files | Passed. |
| Prospective instruction/link validation with the repository validators | Passed on all new files, including untracked files that normal tracked-file checks omit. |
| YAML frontmatter, Codex interface fields, and invocation policies | Validated for all 49 new skills. |
| Installed Codex MCP inventory with adapter overrides | Verified every configured server disabled before inference. |
| `git diff --check` | Flagged the pre-existing `.gitignore` blank line at EOF; that user edit was preserved. |

The full preflight ran before the final MCP argument fix. Helper tests,
prospective instruction/link checks, configuration validation, and formatting
were checked after that fix. The later `--pre-push` slice judged committed HEAD
only and is not used as evidence for the uncommitted port. No live model call,
login, merge, deployment, or scheduled job was performed.
