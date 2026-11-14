# Test-quality requirements

**Status:** Proposed replacement\
**Source review:** 2026-09-09

## Purpose

The test suite exists to detect meaningful violations of supported contracts and make legitimate changes safer. It does not exist to maximize test count, assertion count, snapshots, coverage percentages, or apparent activity.

A test must earn its place. Its author must demonstrate the behavior it protects, the incorrect behavior it rejects, the trustworthiness of its oracle, and the value it adds to the suite. A reviewer does not have to prove that a questionable test is worthless before rejecting it.

These requirements apply to existing tests, new tests, generated tests, and AI-written tests. An existing test is not grandfathered in merely because it passes.

## 1. Require a meaningful contract and a concrete failure mode

Every test must identify a supported behavior and a credible way that behavior could be wrong. Name the behavior, not the method being exercised.

“Rejects an expired request without persisting it” is a claim. “Tests validation,” “covers this function,” and “increases coverage” are not adequate claims.

The author must be able to explain what incorrect production behavior would make the test fail and why that behavior matters. A theoretical change to an irrelevant implementation detail is not sufficient justification.

Do not add standalone tests of language semantics, mocking-library configuration, or framework behavior the application does not implement. Do not mechanically generate one test per getter, setter, method, or line.

Short production code is not exempt from testing; unimportant assertions are not made valuable by surrounding them with complicated fixtures.

## 2. Prohibit tautological and circular oracles

Tautological assertions are prohibited. Comparing a captured value with itself through a reflexive matcher constrains no production behavior and must be removed.

```javascript
// Rejected: the assertion cannot distinguish correct from incorrect output.
const actual = calculateFee(input);
expect(actual).toEqual(actual);
```

Aliasing the value, copying it into an expected object, or hiding the comparison behind a helper does not make it independent.

An oracle is the rule used to decide whether a result is correct. Its verdict must not depend on assuming that the behavior under test is already correct. Do not use the same calculation under test to establish the expected answer, or copy its algorithm and branches as the only check.

Use independently specified examples, reviewed compatibility baselines, normative vectors, a simpler independently designed reference model, or meaningful mathematical properties. State which claim the oracle supports.

Do not derive a normative expected constant or required feature set from the very production definition being checked. Shared setup constants must not erase independent expectations.

## 3. Demonstrate that the test detects its claimed defect

Every new or materially changed behavioral test must have failure evidence: it fails against a credible contract-violating version of the exercised production path and passes against the correct version.

For a regression fix, reproduce the original faulty behavior or temporarily revert the fix. For new behavior or stronger coverage, use a targeted mutation or controlled fault. Restore the code and verify the passing result. Parameterized cases may share evidence where it genuinely establishes the same claimed protection; distinct claims need their own evidence.

The failure must be caused by the intended behavioral difference. A compilation error, broken fixture, missing dependency, changed expected value, or unrelated forced crash is not evidence that the intended behavior is protected.

One detected fault establishes sensitivity to that fault, not completeness. Do not invent low-value tests merely to kill mutants unrelated to supported contracts.

For non-behavioral gates, demonstrate the corresponding rejection: for example, an invalid release artifact must fail the artifact verifier.

## 4. Make assertions as discriminating as the contract requires

For a concrete deterministic example with a specified result, assert that result. A presence, truthiness, type, or generic-success check is not a substitute for checking the required value, contents, or effect.

For collections, specify the required contents and the relevant rules for multiplicity, ordering, and cardinality. For state changes, verify the resulting state. For protocols, verify the required fields and representations. Use tolerances only where the contract warrants them, with justified bounds.

A property test must reject realistic wrong implementations. Sortedness alone does not establish preservation of the input elements. A total equaling the sum of its reported components does not establish that the components are correct. A round trip alone does not establish compatibility with a specified wire format.

Add the complementary checks necessary for the claim. Multiple assertions are required when a single behavior has multiple essential postconditions; padding the test with unrelated assertions is prohibited.

## 5. Require causal, controlled negative tests

A negative test must establish that the relevant invalid condition causes the required refusal, not merely that something fails.

Start from a demonstrated valid scenario. Introduce the targeted invalid condition while keeping unrelated validity requirements satisfied. Verify the specified error category, refusal code, or observable rejection behavior. Where several failures deliberately share one public error, establish causality through controlled inputs and targeted failure evidence rather than weakening the contract.

For refusal contracts that prohibit side effects, verify their absence after the operation has reached the relevant completion or synchronization point. Returning an error does not satisfy a contract that also requires no persistence, submission, or state change.

Pair accept and reject cases so an always-rejecting implementation cannot satisfy the tested rule. Test compound-invalid inputs separately when error precedence is itself a requirement.

An exception from fixture construction or a downstream service must not satisfy a test of a different validator.

## 6. Exercise the real decision being claimed

A test claiming to verify a production decision must execute that decision. Do not replace the validator, calculation, state transition, or other behavior under test with a stub and then assert the configured response.

Test-only branches that bypass the claimed behavior are prohibited. Production code must not recognize a test environment and silently take an easier route through the very behavior being verified.

Dependency injection may control a clock, transport, data source, or scheduler. It must not remove the decision the test claims to establish. Fixture builders may prepare state, but a test claiming to verify creation or loading must use the real creation or loading path.

A test of orchestration with a stubbed dependency verifies orchestration only. It must not be presented as evidence that the dependency or its integration works.

## 7. Protect behavior, not incidental implementation structure

Assert through the supported boundary of the unit or system being tested. That boundary can be a domain function, component API, persistence interface, protocol, or application entry point; it need not be a full end-to-end interface.

Behavior-preserving changes to private helpers, internal object layout, incidental call sequences, or implementation strategy must not require behavioral test rewrites.

An interaction assertion requires a behavioral reason. Preventing duplicate submissions is a reason to check a submission count. Repeating the current sequence of internal helper calls is not.

Do not use source-text searches as proof of runtime behavior. Static architecture checks, type-level compatibility checks, and export checks must state their actual static contract and be reported as that category.

When a refactor breaks a test without changing its contract, repair the test’s coupling instead of automatically copying the new implementation into its expectations.

## 8. Use test doubles without creating a fictional system

Use real deterministic in-process dependencies by default. Use doubles where control, cost, isolation from external infrastructure, or deliberate fault injection requires them.

Every double must have an explicit role. Do not mock simple values or replace every collaborator merely because a mocking framework makes it easy.

Do not rely on invented third-party behavior. Model the application’s boundary and verify its adapter against the actual dependency or an authoritative conformance environment. Fakes carrying semantic behavior need conformance checks for the behavior the tests rely on.

Mock-only tests cannot establish persistence, transaction semantics, serialization compatibility, network behavior, or external-service integration. Claims about those boundaries require tests that cross the relevant real boundary.

## 9. Select scenarios systematically, not cosmetically

Derive the required scenario set from the contract and its failure modes, not by copying the implementation’s branches into a test table.

Cover the relevant valid and invalid input classes, boundary values, state transitions, and failure outcomes. For discrete limits, examine the limit and neighboring values. Include empty, singleton, duplicate, overflow, retry, cancellation, replay, and ordering cases where they are part of the supported risk model.

Do not generate dozens of examples that add no meaningful distinction while omitting a critical boundary. Conversely, cases sharing a helper are not interchangeable when they exercise different transitions or failure modes.

Property-based tests must execute meaningful, non-vacuous cases. Excessive filtering, empty generated scenario sets, and implications whose preconditions are never exercised must not produce a successful required check. Record reproduction information for failures and retain minimized regressions.

## 10. Keep tests readable enough to audit

A reviewer must be able to identify the important input, action, and expected outcome without reconstructing a second application.

Use small, explicit scenarios with the relevant values visible. Share mechanical setup; do not hide the decisive condition or expectation in a general-purpose fixture factory.

Conditional assertions that adapt to the observed output are prohibited. Do not catch and swallow unexpected failures. Await or return asynchronous work so the test runner observes its result. A test must not finish before its verification runs.

Use named data-driven cases instead of duplicated scripts or assertion loops that obscure which scenario failed. Test generators and shared verification helpers are test infrastructure: review their own correctness and ensure they actually execute the required cases.

Failure output must identify the scenario, expected behavior, and observed mismatch. Logging output for a person to inspect is not an automated oracle.

## 11. Make results trustworthy and execution reproducible

Correctness tests must not depend on previous tests, leaked state, a developer’s workstation, or an uncontrolled local environment. Create and clean up the resources each scenario needs.

Control time and randomness for unit logic. Use synchronization and bounded waits for real asynchronous behavior. Do not use an arbitrary sleep as evidence that an event occurred or could no longer occur. Real scheduling and transport requirements must also be checked at an integration boundary that exercises them.

A fail-then-pass retry is not evidence of a clean test run. Preserve the first failure and investigate whether the cause is the test, infrastructure, or an actual product race.

Quarantine requires an owner, a repair deadline, and an explicit coverage-gap record. Quarantined tests do not satisfy required verification, and safety- or integrity-critical claims need reliable replacement coverage before release.

## 12. Treat snapshots, vectors, and pins as reviewed contracts

Every retained snapshot or pin must identify the compatibility, deployment, diagnostic, performance, or reproducibility requirement it protects.

An unexplained snapshot of internal state is not a substitute for a behavioral oracle. Keep the asserted surface bounded and reviewable. Preserve representation details when the representation is the actual contract, such as specified wire bytes.

Generated baselines must have provenance and explicit review. A baseline copied from current output can freeze approved behavior; it cannot independently prove that behavior satisfies a separate specification.

Do not update expected files merely to make a failing run green. The change must be justified against the contract, with semantic checks that continue to establish correctness. Preserve supported external verifier inputs and consumers when reorganizing them.

## 13. Match test scope and cost to the evidence required

Use the smallest test scope that can establish the claim faithfully. Keep focused tests for decision logic and boundary cases; retain integration tests for integration risks and end-to-end tests for critical assembled workflows.

Do not exercise an entire application repeatedly when a focused test provides the same protection. Do not replace a necessary integration test with mocks merely to make it fast.

Evaluate measured runtime, setup cost, failure diagnosis, maintenance, and additional protection. Timeout allowances are not runtime measurements.

Performance claims require a defined workload, environment, measurement method, and acceptance criterion. A printed duration is not a performance gate. A saved measurement is historical evidence, not a new execution of the measuring producer. Keep correctness controls with the workload so doing less or incorrect work cannot count as an optimization.

## 14. Required verification must run and fail closed

Required CI jobs must fail or block when dependencies are missing, fixtures cannot load, expected scenarios are not discovered, or verification does not finish. A skip, early return, empty case list, or successful shell wrapper must not impersonate passed coverage.

Separate passed, failed, skipped, quarantined, and not-run results. An optional experiment may remain optional, but it cannot satisfy a merge or release requirement. A test invoked by nobody provides no ongoing protection.

Required tests must have a reproducible command and an enforced execution point. Critical scenario families must remain discoverable even when their cases are generated through shared infrastructure.

The presence of tests in a repository is not evidence that CI ran them.

## 15. Retain evidence, remove waste, and prohibit metric gaming

Test count, assertion count, coverage percentage, and mutation score are diagnostics, not acceptance arguments. A high score does not excuse a weak oracle, irrelevant fault model, fictional dependency, or missing critical scenario.

Review uncovered critical paths and meaningful surviving mutants. Resolve them with better tests or a concrete demonstration that the proposed obligation is not part of the supported contract. Do not add brittle checks solely to satisfy a number.

Remove tautologies and filler assertions. Rewrite a weak test that protects an important scenario rather than deleting the scenario. Consolidate repeated coverage when it contributes no meaningful additional protection, diagnostic value, or independent boundary evidence.

Before deleting nontrivial coverage, identify the protected contract, existing consumer, and replacement protection, or demonstrate why the contract is no longer supported. A green suite before and after deletion does not establish that detection capability was preserved.

There are no deletion quotas and no retention waivers for zero-information assertions.

## Review gate

A test change is not ready to merge until the reviewer can answer the following from the test and its review evidence. A concise record can cover a coherent test group; duplicative paperwork is not required.

```text
Contract: What supported behavior is protected?
Failure mode: What credible wrong behavior must be rejected?
Oracle: What independently justifies the expected result?
Execution: Which production path and relevant boundary are exercised?
Sensitivity: What faulty version failed, and at which intended check?
Value: What useful protection does this add or replace?
CI: Where does this run, and what happens if it cannot run?
```

A missing or unconvincing answer is a reason to reject or revise the test. “It passes,” “it covers lines,” “the generator wrote it,” and “it might catch something” are not substitutes.

**A passing test is useful only when its passing result is credible evidence against a meaningful failure.**
