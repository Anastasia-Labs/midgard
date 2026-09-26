---
name: writing-reports-and-prs
description: Writes Midgard PR descriptions, commit message bodies, and the report an agent hands back to whoever dispatched it, so that each one separates claims from evidence and states what was not checked. Use when opening or editing a pull request, writing a commit message that says something was verified, handing a report back to an orchestrator or the owner, posting a verification summary on an issue, or about to write "green", "passes", "verified" or "tested".
---

# Writing reports and PRs

A PR description, a commit body and a hand-back report are read by someone who
will act on them without re-running your work. Each is an **evidence ledger**:
every claim that something works points at the run that showed it, and every
place you did not look is written down as such.

Nothing checks the prose. CI runs the code, not the description; the only
automated check on any of this text is the attribution refusal in the
`committing-safely` script, and it sees only commits made through it. Every
rule below is `[review]` unless it says otherwise, and you are the review.

## The evidence ledger

Every PR description, commit body that claims verification, and hand-back
report carries these five things. They are rules; each is `[review]`.

1. **What was checked, as evidence lines.** One line per run: the exact
   command, where it ran, the collected count, the exit code, and the date.
   `vitest run tests/foo.test.ts` in `demo/midgard-fault-proofs` —
   `Tests 42 passed (42)`, exit 0, 2026-09-25. "Suite green" is a claim, not
   an evidence line.
2. **What was not checked, and why.** Name each required or obviously
   relevant check you did not run and the reason: no devnet on this box, the
   Postgres suites need 5433, the full suite takes 45 minutes and only the
   touched files ran. The reader can then decide whether that gap matters;
   silence decides for them. [AGENTS.md](../../../AGENTS.md) "Always-On
   Rules" asks you to report exactly what ran and says a smoke test does not
   replace a required acceptance check.
3. **"Could not look" apart from "looked and it passed".** A skipped test, a
   `skipIf` without its environment variable, a tool that was missing, a
   check that timed out: each is _could not look_, and goes under "Not
   checked", never inside a pass count. A run reporting
   `203 passed | 108 skipped` passed 203 tests and did not look at 108.
4. **Measurements carry their date** and, where it varies, the machine state
   (quiet, or concurrent with another suite). Timings, test counts, line
   counts and sizes all go stale.
5. **Claims apart from evidence.** Write what the change does as a claim
   ("the watcher now refuses X"), then point at the evidence line or test
   that shows it. A claim with no evidence behind it is labelled as untested,
   not left to read as verified.

### Green needs a count

A green claim names the command, the number of tests collected and the exit
code. A run that collected zero tests is not a pass: a bare
`aiken check -m <module>` and a vitest `-t` that matches nothing both exit 0
having run nothing. Read the collected count before you write "passed". The
traps and the guarded runner are in
[writing-tests §5 and §8](../writing-tests/SKILL.md#5-keep-it-able-to-fail).

A red that is not yours is still reported: name it, say why you believe it
pre-exists (the baseline run, or the same failure on the base branch), and
how you checked. `[review]`

## Pull request descriptions

Shape (see [references/pr-template.md](references/pr-template.md)): `##
Summary` bullets of what changed and for whom, an optional `## Why` when the
motivation is not obvious from the summary, then `## Verification` with the
evidence ledger and a "Not checked" list. The Summary/Verification shape is
what this repository's best merged PRs already use (#466, #468; read with
`gh pr view <n> -R Anastasia-Labs/midgard`, 2026-09-25). `[review]`

- **Title**: the same imperative-sentence style as a commit subject
  (committing-safely rule 9, below). `[review]`
- **Size tracks the diff.** A nine-file type change needs a paragraph, not a
  page; a 14,000-character body on a small diff hides its one important line.
  `[review]`
- **Mention every change a reviewer would not expect** from the title, such
  as an unrelated config or lockfile edit that rode along. `[review]`
- **GitHub checks are evidence too.** Cite the workflow, its result and the
  commit it ran on; a PR with no checks attached says so. `[review]`
- **Base branch.** For the Goal program, `docs/exec-plans/GOAL_SPEC.md` §4.4
  says the base is exactly `tx-validation` and there is one long-lived Goal
  PR for the program's lifetime (lines 591–597; release checklist line 1426),
  and the owner has confirmed `tx-validation` for it. That is the Goal
  program's rule, not a rule for every PR. Outside it, no document fixes the
  base: open PRs as of 2026-09-25 target `tx-validation`, stacked `fp/*`
  branches and a feature branch. Ask whoever requested the PR. `[review]`
- **Opening, editing, commenting on or merging a PR** happens only when you
  were asked to. Pushing is covered by committing-safely rule 10. `[review]`

## Commit messages

The style is measured once, in
[committing-safely rule 9](../committing-safely/SKILL.md#rules): imperative
subject, no period, no `type:` prefix, a body that says why the change exists
and what was wrong before, issue links as `(#NNN)` or `Closes #NNN`. This
skill adds one thing: a body that says something was verified carries an
evidence line, or points to where the evidence lines live (the issue comment
or PR). `[review]`

## Hand-back reports

A report to an orchestrator or the owner is the same ledger, with more
structure, because the reader decides from it whether to commit, dispatch
more work or ask the owner. Use
[references/hand-back-template.md](references/hand-back-template.md). It
separates: what changed (files), what was checked (evidence lines), what was
not checked, rulings or instructions received and how each was applied, facts
you could not verify, and open questions. Never report work as done that you
did not do, and never fold an unanswered question into a default choice
without saying so. `[review]`

## No attribution

No tool attribution anywhere: no co-author trailers naming a
tool, no "Generated with" lines, no mention in PR titles, descriptions,
comments or report text meant for the repository. For commits made through
`commit-paths` this is `[script: .agents/skills/committing-safely/scripts/commit-paths.mjs]`
(committing-safely rule 8); blind spot: it matches a fixed list of tool names.
Plain `git commit`, PR text and issue comments:
`[review]`.

## Before you send

Read your text as the reviewer who will not re-run anything. Every "passes",
"green", "verified" or "works" has an evidence line with a count and exit
code; every environment you lacked appears under "Not checked"; every number
has a date. The completion criterion: no sentence claims more than an
evidence line shows.

For a real before-and-after rewrite (commit `43d36379f` and PR #456), read
[references/examples.md](references/examples.md) when you want to see what
the rules change in practice.
