# PR description template

Delete any section that would be empty except "Not checked". If nothing was
left unchecked, say so in one line; that is itself a claim.

```markdown
## Summary

- <what changed, for whom, in one fact per bullet>
- <any change a reviewer would not expect from the title>

## Why

<one short paragraph, only if the Summary does not already make it obvious>

## Verification

- `<exact command>` in `<directory>`: <N> tests collected, <N> passed,
  exit <code> (<YYYY-MM-DD>)
- GitHub <workflow name> on `<short sha>`: <result>

Not checked:

- <check>: <reason> (for example: devnet journeys skip without
  `MIDGARD_WATCHER_JOURNEY_RUN_DIR`; no devnet was up)

Pre-existing failures seen:

- <test>: <failure>, also red on <baseline run or base branch>

Closes #<NNN>
```

Rules this template carries are in [../SKILL.md](../SKILL.md).
