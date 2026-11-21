# Authoring a skill

Read [the host adapter](../references/host-adapter.md) before host-dependent operations.

1. Read [editing-agent-instructions](../../editing-agent-instructions/SKILL.md) and any available skill-creation guidance before editing.

2. Define trigger requests, completion criteria, current owners, and branches requiring supporting references. Preserve existing invocation policy.

3. Write to the shared .agents/skills tree with matching frontmatter and Codex interface metadata. Add portable helpers only when their reuse earns the cost.

4. Test each helper with a realistic negative case. Run the agent-skill and instruction checks; evaluate complex workflows against realistic bounded scenarios.

5. Report the skill paths, intended usage, exact validation, and untested host behavior.
