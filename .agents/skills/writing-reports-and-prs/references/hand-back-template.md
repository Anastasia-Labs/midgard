# Hand-back template

For what an agent returns to the orchestrator or owner that dispatched it.
Keep every section, and write "none" where it is empty: a missing section
reads as "not considered".

```markdown
## Outcome

<one or two sentences: done, partly done (what is left), or stopped (why)>

## Files changed

- /absolute/path/one.ts: <what changed> (<N> lines)
- /absolute/path/two.ak: <what changed>

Not committed / committed as <sha> (say which; say "not pushed" if so).

## Checked

- `<exact command>` in `<directory>`: <N> collected, <N> passed,
  <N> skipped, exit <code>, <YYYY-MM-DD>, <quiet | concurrent with X>
- Acceptance criterion "<quoted>": met, by <evidence line above or file:line>

## Not checked

- <check or criterion>: <why> (could not look: missing tool, environment,
  time; or deliberately out of scope)

## Pre-existing reds

- <test>: <failure>; pre-existing because <baseline run, base branch run>

## Rulings and instructions received

- <ruling, quoted or cited>: applied as <what you did>

## Could not verify (left out)

- <fact>: <what you tried>

## Open questions

- <question>: <why it needs the owner; the options you see>
```

Rules this template carries are in [../SKILL.md](../SKILL.md).
