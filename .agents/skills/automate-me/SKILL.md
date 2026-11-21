---
name: automate-me
description: "Create or revise a personal mode skill from the user\u2019s stated preferences and scoped working history."
disable-model-invocation: true
---

# Automate me

Read [the host adapter](../poteto-mode/references/host-adapter.md) before dispatching agents, selecting models, accessing history, or scheduling work. It owns these operations for every pstack skill.

1. Search `.agents/skills/*-mode/SKILL.md` for an existing mode. Preserve its path and unrelated preferences when revising it. New repository skills live at `.agents/skills/<handle>-mode/`; `.claude/skills` already reads that tree. [review]
2. Use stated preferences and task-scoped history through the adapter. Corroborate inferred habits across several examples. Treat one-off corrections as tentative. Ask only for preferences that cannot be inferred. [review]
3. Read [editing-agent-instructions](../editing-agent-instructions/SKILL.md) and any available skill-creation guidance. Cluster specific preferences by response style, autonomy, understanding, delegation, verification, or process. Add only sections the evidence warrants. [review]
4. Write the smallest operational skill. Reference existing owners rather than repeating their rules. Include Codex interface metadata. Preserve invocation policy for an existing skill; use normal discovery for a new skill unless the user requests explicit-only invocation. [review]
5. Apply [unslop](../unslop/SKILL.md), run the skill checker, and present the file for feedback. A personal mode's first evaluation is whether the user recognizes their intent. Commit or open a PR only within the requested delivery scope. [review]
