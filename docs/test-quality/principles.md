# Test-quality principles

Status: Active
Last reviewed: 2026-09-07 (source-review corrections and attribution)

These are repository review rules informed by testing literature. They do not
establish that every candidate in the [audit](../test-quality-audit.md) is a
defect. Read the actual assertion, fixture, production path, and CI consumer
before changing it. A proposed cleanup must preserve the ability to detect the
regression the test was written for.

## Sources and limits

- Meszaros, [xUnit Test Patterns](https://www.informit.com/store/xunit-test-patterns-refactoring-test-code-9780321504807),
  supplies vocabulary for fragile tests, obscure fixtures, duplication, and
  slow tests. A smell prompts investigation; it is not proof that a test is
  redundant.
- [Software Engineering at Google, unit testing](https://abseil.io/resources/swe-book/html/ch12.html)
  discusses public interfaces, observable outcomes, readability, and avoiding
  brittle coupling. Its [test-doubles chapter](https://abseil.io/resources/swe-book/html/ch13.html)
  also describes appropriate interaction testing; call assertions are not
  categorically invalid.
- Freeman and Pryce's [Growing Object-Oriented Software](https://growing-object-oriented-software.com/toc.html)
  recommends mocking interfaces the application owns. It does **not** ban mocks
  of owned collaborators. Wrap third-party APIs in an owned boundary where
  appropriate and separately test that adapter against the real dependency.
- [Zhang and Mesbah, FSE 2015](https://people.ece.ubc.ca/amesbah/www/resources/papers/fse15.pdf)
  reports a correlation between assertions and effectiveness in its studied
  Java suites. It does not prove that adding assertions improves every suite,
  or that a weak assertion is worse than no assertion.
- [Inozemtseva and Holmes, ICSE 2014](https://cs.uwaterloo.ca/~rtholmes/papers/icse_2014_inozemtseva.pdf)
  finds limited correlation between coverage and effectiveness after accounting
  for suite size. Coverage identifies unexecuted code; it does not establish
  that a test detects a particular regression.

## Review rules

**R1 — Identify a regression the test can detect.** Literal self-comparison is
usually redundant. Comparing production output with an independently stated
expected value, or checking consistency between distinct output fields, can
catch regressions even when the current implementation guarantees the property.
Do not label those assertions tautologies without tracing both sides.
→ TQ-01, TQ-08, TQ-09, TQ-10, TQ-17, TQ-18

**R2 — Test at the relevant observable boundary.** Prefer returned state,
persisted effects, and public APIs over source layout. An interaction, its
count, or its order can itself be the contract, such as exactly-once submission
or no lease acquisition after refusal. → TQ-11, TQ-14

**R3 — Match the oracle to the claim.** If a test names a specific validator or
refusal, distinguish it from fixture or unrelated failures. Pair relevant
negative scenarios with successful fixture controls. A broad refusal assertion
can still prove a broad refusal contract; it does not prove a particular guard.
→ TQ-04, TQ-05, TQ-06, TQ-07, TQ-17, TQ-18

**R4 — Prefer structured refusal codes where they are the API.** Preserve tests
of diagnostic wording when that wording is an intentional user-facing contract.
Regex length alone does not determine whether a matcher is discriminating.
→ TQ-04, TQ-05

**R5 — State why each pin exists.** Wire vectors, public export sets, deployment
identities, and reproducible measurements can all justify pins. A changing hash
is not automatically a bad test: it may be the change a release reviewer needs
to see. Apply the [retention policy](../DOCUMENTATION_POLICY.md), preserve live
verifier inputs, and do not weaken an exact measurement gate without reviewing
its acceptance contract. → TQ-12, TQ-13, TQ-15

**R6 — Share constants without erasing independent expectations.** Derive
incidental sizes from declared inputs; preserve an independent check of
normative constants and supported feature sets. Deriving both actual and
expected from the same buggy definition does not test that definition.
→ TQ-01, TQ-12

**R7 — Make required execution fail closed.** Missing prerequisites must not
silently satisfy required coverage. Explicit optional lanes need a runnable
command and visible skip reason. Keep useful opt-in tests when their
infrastructure is unavailable; report the resulting coverage limit.
→ TQ-02

**R8 — Share setup where it preserves readable scenarios.** Consolidate actual
duplicates while retaining independent inputs, effects, and family-specific
negatives. A shared helper does not force callers to invoke it; verify the
required scenario set separately. → TQ-15, TQ-16

**R9 — Compare measured cost with unique coverage.** A timeout is an upper
allowance, not measured runtime. A saved ledger is not a substitute for the
live producer that measures it. Keep semantic controls alongside cost readings
and preserve their verifier consumers when moving benchmarks. → TQ-03, TQ-15

**R10 — Keep test behavior representative.** Remove test-only branches that
bypass the production decision being claimed. Dependency injection, controlled
clocks, read-only diagnostics, and test-support constructors are not inherently
bypasses. Review reachability and effects before banning names or `NODE_ENV`.
→ TQ-14

**R11 — Control time at the right layer.** Use deterministic clocks for unit
logic. Real scheduling, transport deadlines, and process behavior also need
integration checks using real time with bounded waits and appropriate margins.
Separate performance measurements from correctness claims. → TQ-10

**R12 — Guard demonstrated regressions proportionately.** Prefer focused
checks that fail for the intended defect and accept legitimate counterexamples.
Neither a blanket syntax ban nor a raw coverage target proves test quality.
→ TQ-19

## Before deleting coverage

Record the current consumer, the claimed behavior, and what will catch its
regression after the change. Preserve generated cross-language vectors,
parameter-application emulator scenarios, release evidence checks, and tests of
external or persistence boundaries. A passing suite before and after deletion
cannot by itself prove that coverage was preserved.

Counts in the audit are dated observations, not deletion quotas or current
inventories. Task-specific source corrections take precedence over an older
audit recommendation.
