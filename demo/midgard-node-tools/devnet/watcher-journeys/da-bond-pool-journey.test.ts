import "node:crypto";
import "vitest";
import "./da-bond-pool-journey.js";
import "./da-bond-pool-journey.faults.js";
import "./da-bond-pool-journey.fake-port.js";
import "./da-bond-pool-journey.meta.js";

import { describe, expect, it } from "vitest";

import { fakePort } from "./da-bond-pool-journey.fake-port.js";
import {
  CHALLENGER_REMAINING,
  type Faults,
  PARAMS,
} from "./da-bond-pool-journey.faults.js";
import {
  DA_BOND_POOL_JOURNEY_CHRONOLOGY,
  DaBondPoolJourneyFailure,
  type DaBondPoolJourneyPort,
  planDaBondPoolSlash,
  renderDaBondPoolJourneyReport,
  runDaBondPoolJourney,
  tryRunDaBondPoolJourney,
} from "./da-bond-pool-journey.js";
import { failedAssertions, META, stage } from "./da-bond-pool-journey.meta.js";

describe("pooled DA bond journey driver", () => {
  it("walks all six steps in the append-safe order and reports every transaction id", async () => {
    const { port, issued, calls } = fakePort();
    const record = await runDaBondPoolJourney(port);

    expect(record.status).toBe("passed");
    expect(record.stages.map(({ step }) => step)).toEqual([
      ...DA_BOND_POOL_JOURNEY_CHRONOLOGY,
    ]);
    expect(record.stages.map(({ step }) => step)).toEqual([1, 3, 4, 5, 2, 6]);
    for (const outcome of record.stages) {
      expect(outcome.status).toBe("passed");
      expect(outcome.finishedAt).toBeDefined();
      expect(Object.keys(outcome.txIds).length).toBeGreaterThan(0);
    }
    expect(failedAssertions(record)).toEqual([]);
    expect(
      record.stages.flatMap(({ assertions }) =>
        assertions.filter(({ ok }) => ok === "not-observable"),
      ),
    ).toEqual([]);

    // Every landed transaction is in the ledger exactly once.
    const ledgerTxIds = record.stages.flatMap(({ txIds }) =>
      Object.values(txIds),
    );
    expect([...ledgerTxIds].sort()).toEqual([...issued].sort());

    // B1 is withheld, the others are served; the withdrawing probe block is
    // committed while Withdrawing and attested after the Cancel.
    expect(calls.filter((call) => call.startsWith("commit"))).toEqual([
      "commit B1 withhold",
      "commit B2 serve",
      "commit B3 serve",
      "commit B4 serve",
    ]);
    expect(Object.keys(stage(record, 6).txIds)).toEqual([
      "begin withdraw #1",
      "commit B4",
      "cancel withdraw",
      "apply B4",
      "begin withdraw #2",
      "complete withdraw",
    ]);

    const report = renderDaBondPoolJourneyReport(record, META);
    for (const txId of issued) expect(report).toContain(txId);
    expect(report).toContain("# Pooled DA bond journey (emulator)");
    expect(report).toContain("not devnet evidence");
    const headings = [...report.matchAll(/^## Step (\d)/gm)].map(
      (match) => match[1],
    );
    expect(headings).toEqual(["1", "2", "3", "4", "5", "6"]);
    expect(report).not.toContain("| FAIL |");

    const live = renderDaBondPoolJourneyReport(record, {
      ...META,
      adapter: "live-devnet",
    });
    expect(live).toContain("Observed on the process devnet");
    expect(live).not.toContain("not devnet evidence");
  });

  it("slashes one full bond at a full pool: fee = penalty, payout = da_bond - penalty", async () => {
    for (const backing of [
      PARAMS.daBond,
      PARAMS.daBond + 1n,
      2n * PARAMS.daBond,
    ])
      expect(
        planDaBondPoolSlash({
          daBond: PARAMS.daBond,
          penalty: PARAMS.penalty,
          backing,
        }),
      ).toEqual({
        taken: PARAMS.daBond,
        feePart: PARAMS.penalty,
        payout: PARAMS.daBond - PARAMS.penalty,
      });
    // Below one bond the penalty fills first (spec §5).
    expect(
      planDaBondPoolSlash({
        daBond: PARAMS.daBond,
        penalty: PARAMS.penalty,
        backing: PARAMS.penalty + 7n,
      }),
    ).toEqual({
      taken: PARAMS.penalty + 7n,
      feePart: PARAMS.penalty,
      payout: 7n,
    });
    expect(
      planDaBondPoolSlash({
        daBond: PARAMS.daBond,
        penalty: PARAMS.penalty,
        backing: 3n,
      }),
    ).toEqual({ taken: 3n, feePart: 3n, payout: 0n });

    const initialBacking = PARAMS.daBond + 20_000_000n;
    const { port } = fakePort({}, initialBacking);
    const record = await runDaBondPoolJourney(port);
    const slashed = stage(record, 3);
    const timeout = slashed.observations.find(
      (observation) => observation.kind === "timeout",
    );
    expect(timeout?.kind === "timeout" && timeout.value).toMatchObject({
      fee: PARAMS.penalty,
      poolBefore: PARAMS.floor + initialBacking,
      poolAfter: PARAMS.floor + initialBacking - PARAMS.daBond,
      challengerOutputLovelace:
        CHALLENGER_REMAINING +
        PARAMS.challengeRecordLovelace +
        PARAMS.daBond -
        PARAMS.penalty,
    });
    expect(
      slashed.assertions.find(({ name }) => name.startsWith("full pool"))?.ok,
    ).toBe(true);
  });

  it("removes the block separately when the Timeout leaves it in the queue", async () => {
    const { port, calls } = fakePort({ timeoutLeavesBlock: true });
    const record = await runDaBondPoolJourney(port);
    expect(record.status).toBe("passed");
    expect(calls).toContain("remove header-B1");
    expect(Object.keys(stage(record, 3).txIds)).toContain("remove B1 [0]");
  });

  it("marks what the adapter cannot read as not-observable, never as passed", async () => {
    const { port } = fakePort({ minimalObservability: true });
    const record = await runDaBondPoolJourney(port);
    expect(record.status).toBe("passed");
    const unobserved = record.stages.flatMap(({ step, assertions }) =>
      assertions
        .filter(({ ok }) => ok === "not-observable")
        .map(({ name }) => `${step}: ${name}`),
    );
    expect(unobserved).toEqual(
      expect.arrayContaining([
        "3: D3: challenger output = remaining - c + challenge_record_lovelace + payout",
        "3: D3: exactly one challenger output",
        "3: committee pool monitor emits da_bond_pool_backing_short",
        "4: committee pool monitor emits no transition while the pool stays short",
        "5: committee pool monitor emits da_bond_pool_backing_restored",
        "3: P16: alerts after Timeout: committee reasons from the da-committee-node /readyz, events from its stderr",
        "3: P27: the committee node reported B1's challenge unavailable and never acted on it",
        "4: P16: alerts while short: committee reasons from the da-committee-node /readyz, events from its stderr",
        "5: P16: alerts after top-up: committee reasons from the da-committee-node /readyz, events from its stderr",
        "6: P16: alerts after cancel: committee reasons from the da-committee-node /readyz, events from its stderr",
        "6: P16: alerts after complete: committee reasons from the da-committee-node /readyz, events from its stderr",
        "5: P18: top-up submitted by the real da-bond CLI",
        "6: P18: begin withdraw #1 submitted by the real da-bond CLI",
        "6: P18: cancel withdraw submitted by the real da-bond CLI",
        "6: P18: complete withdraw submitted by the real da-bond CLI",
      ]),
    );
    expect(renderDaBondPoolJourneyReport(record, META)).toContain(
      "| not-observable |",
    );
  });

  it("records the P16 committee process view and the P18 CLI chains, and all of it passes on an honest port", async () => {
    const { port } = fakePort();
    const record = await runDaBondPoolJourney(port, {
      requireProcessEvidence: true,
    });
    expect(record.status).toBe("passed");
    const processChecks = record.stages.flatMap(({ step, assertions }) =>
      assertions
        .filter(
          ({ name }) => name.startsWith("P16: ") || name.startsWith("P18: "),
        )
        .map(({ name, ok }) => ({ step, name, ok })),
    );
    expect(processChecks.every(({ ok }) => ok === true)).toBe(true);
    // P16 at step 1 and step 6 on each fresh node start, and at steps 3 to 6
    // (both directions); P18 for the top-up and every withdraw step.
    expect([
      ...new Set(
        processChecks
          .filter(({ name }) => name.startsWith("P16: "))
          .map(({ step }) => step),
      ),
    ]).toEqual([1, 3, 4, 5, 6]);
    const cliLabels = new Set(
      processChecks
        .filter(({ name }) => name.startsWith("P18: "))
        .map(({ step, name }) => `${step}: ${name.split(": ")[1]}`),
    );
    expect([...cliLabels]).toEqual([
      "5: top-up",
      "6: begin withdraw #1",
      "6: cancel withdraw",
      "6: begin withdraw #2",
      "6: complete withdraw",
    ]);
    // Each CLI process lands in the ledger with its command line and exit code.
    const report = renderDaBondPoolJourneyReport(record, META);
    expect(report).toContain(
      "exit 0: node dist/index.js da-bond top-up --manifest",
    );
    expect(report).toContain("exit 0: node dist/index.js da-bond assemble");
    expect(report).toContain("exit 0: node dist/index.js da-bond status");
    expect(report).toContain(stage(record, 5).txIds["top-up"]);
  });

  it("fails step 5 when the top-up process prints the pre-transaction pool status (P15/P18)", async () => {
    const { record } = await tryRunDaBondPoolJourney(
      fakePort({ cliStaleStatus: "top-up" }).port,
    );
    expect(record.failure?.step).toBe(5);
    expect(failedAssertions(record)).toEqual(
      expect.arrayContaining([
        {
          step: 5,
          name: "P18: top-up: same-output status is the post-transaction pool",
        },
        {
          step: 5,
          name: "P18: top-up: same-output status matches the transaction",
        },
        {
          step: 5,
          name: "P18: top-up: da-bond status after reads the same pool",
        },
      ]),
    );
    expect(
      failedAssertions(record).every(({ name }) =>
        name.startsWith("P18: top-up: "),
      ),
    ).toBe(true);
  });

  it("fails step 6 when the CompleteWithdraw assemble prints the old pool", async () => {
    const { record } = await tryRunDaBondPoolJourney(
      fakePort({ cliStaleStatus: "complete" }).port,
    );
    expect(record.failure?.step).toBe(6);
    expect(failedAssertions(record)).toContainEqual({
      step: 6,
      name: "P18: complete withdraw: same-output status is the post-transaction pool",
    });
  });

  it("fails step 6 when the CancelWithdraw assemble process exits non-zero", async () => {
    const { record } = await tryRunDaBondPoolJourney(
      fakePort({ cliExitNonZero: "cancel" }).port,
    );
    expect(record.failure?.step).toBe(6);
    expect(failedAssertions(record)).toEqual(
      expect.arrayContaining([
        { step: 6, name: "P18: cancel withdraw: every process exits 0" },
        {
          step: 6,
          name: "P18: cancel withdraw: stdout names the landed txHash",
        },
      ]),
    );
  });

  it("fails step 3 when the committee node reports ready while its pool reason is raised (P16)", async () => {
    const { record } = await tryRunDaBondPoolJourney(
      fakePort({ readyzReadyWhileRaised: true }).port,
    );
    expect(record.failure?.step).toBe(3);
    expect(failedAssertions(record)).toEqual([
      {
        step: 3,
        name: "P16: alerts after Timeout: committee reasons from the da-committee-node /readyz, events from its stderr",
      },
    ]);
  });

  it("fails on missing process evidence only when it is required", async () => {
    for (const [faults, step, name] of [
      [
        { committeeInProcess: true },
        1,
        "P16: alerts before commit: committee reasons from the da-committee-node /readyz, events from its stderr",
      ],
      [
        { cliInProcess: true },
        5,
        "P18: top-up submitted by the real da-bond CLI",
      ],
      [
        { responderNotRead: true },
        3,
        "P27: the committee node reported B1's challenge unavailable and never acted on it",
      ],
    ] as const) {
      const lenient = await runDaBondPoolJourney(fakePort(faults).port);
      expect(lenient.status).toBe("passed");
      expect(
        stage(lenient, step).assertions.find(
          (assertion) => assertion.name === name,
        )?.ok,
      ).toBe("not-observable");

      const { record } = await tryRunDaBondPoolJourney(fakePort(faults).port, {
        requireProcessEvidence: true,
      });
      expect(record.failure?.step).toBe(step);
      expect(failedAssertions(record)).toEqual([{ step, name }]);
    }
  });

  it("fails step 3 on an under-reported challenger output and still returns the ledger", async () => {
    const { port } = fakePort({ underReportChallengerOutput: true });
    const { record, error } = await tryRunDaBondPoolJourney(port);

    expect(error).toBeInstanceOf(Error);
    expect(record.status).toBe("failed");
    expect(record.failure?.step).toBe(3);
    expect(failedAssertions(record)).toEqual([
      {
        step: 3,
        name: "D3: challenger output = remaining - c + challenge_record_lovelace + payout",
      },
    ]);
    expect(record.stages.map(({ step, status }) => [step, status])).toEqual([
      [1, "passed"],
      [3, "failed"],
    ]);
    expect(stage(record, 3).txIds["timeout B1"]).toBeDefined();

    const report = renderDaBondPoolJourneyReport(record, META);
    expect(report).toContain("FAILED at step 3");
    expect(report).toContain("| FAIL | D3: challenger output");
    expect(report).toContain(stage(record, 3).txIds["timeout B1"]);
    expect(report.match(/Not reached\./g)).toHaveLength(4);

    await expect(
      runDaBondPoolJourney(
        fakePort({ underReportChallengerOutput: true }).port,
      ),
    ).rejects.toSatisfy(
      (failure: unknown) =>
        failure instanceof DaBondPoolJourneyFailure &&
        failure.record.failure?.step === 3,
    );
  });

  it("records a failed step's whole cause chain and rethrows the original error", async () => {
    const { port } = fakePort();
    const failure = new Error("Header submission e1bb is unresolved", {
      cause: Object.assign(new Error("RejectTx"), {
        data: { validationError: "ValueNotConserved" },
      }),
    });
    const { record, error } = await tryRunDaBondPoolJourney({
      ...port,
      beforeStep: async () => {
        throw failure;
      },
    });
    expect(error).toBe(failure);
    expect(record.failure?.step).toBe(1);
    expect(record.failure?.message).toMatch(
      /^Header submission e1bb is unresolved <- RejectTx .*ValueNotConserved/u,
    );
    expect(stage(record, 1).error).toBe(record.failure?.message);
  });

  it("fails step 4 when Apply is not refused while the pool is short", async () => {
    const { record } = await tryRunDaBondPoolJourney(
      fakePort({ attestAppliesWhileShort: true }).port,
    );
    expect(record.failure?.step).toBe(4);
    expect(failedAssertions(record)).toEqual([
      { step: 4, name: "Apply of B2 is refused with pool-under-backed" },
    ]);
    expect(stage(record, 4).txIds["unexpected apply B2"]).toBeDefined();
  });

  it("fails step 5 when the resumed Apply lands after the attestation timeout", async () => {
    const { record } = await tryRunDaBondPoolJourney(
      fakePort({ lateSecondApply: true }).port,
    );
    expect(record.failure?.step).toBe(5);
    expect(failedAssertions(record)).toEqual([
      {
        step: 5,
        name: "Apply of B2 lands within the attestation timeout",
      },
    ]);
  });

  it("fails step 6 when CompleteWithdraw draws a different amount", async () => {
    const { record } = await tryRunDaBondPoolJourney(
      fakePort({ completeWithdrawOffBy: 1n }).port,
    );
    expect(record.failure?.step).toBe(6);
    // The CLI's own status readout shows the same wrong draw.
    expect(failedAssertions(record)).toEqual([
      {
        step: 6,
        name: "P18: complete withdraw: same-output status matches the transaction",
      },
      {
        step: 6,
        name: "complete: pool lovelace decreased by exactly the amount",
      },
    ]);
    expect(stage(record, 6).txIds["complete withdraw"]).toBeDefined();
  });

  it("records every check of every step on an honest port, so a deleted check shows", async () => {
    const record = await runDaBondPoolJourney(fakePort().port, {
      requireProcessEvidence: true,
    });
    const p18 = (label: string) =>
      [
        "command lines are the da-bond CLI",
        "every process exits 0",
        "stdout names the landed txHash",
        "txHash confirmed on chain",
        "same-output status is the post-transaction pool",
        "same-output status matches the transaction",
        "da-bond status after reads the same pool",
        "CLI status agrees with the adapter's chain read",
      ].map((check) => `P18: ${label}: ${check}`);
    const p16 = (label: string) =>
      `P16: ${label}: committee reasons from the da-committee-node /readyz, events from its stderr`;
    const begin = (label: string) => [
      `${label}: unlock_at = validity upper bound + withdraw delay`,
      ...p18(label),
      `${label}: pool is Withdrawing{unlock_at}`,
      `${label}: pool value unchanged`,
      "watcher withdrawing alert fires",
      "committee reports a withdrawing readiness reason",
      "committee pool monitor emits da_bond_pool_withdrawing",
      p16(`alerts after ${label}`),
    ];
    expect(
      record.stages.map(({ step, assertions }) => [
        step,
        assertions.map(({ name }) => name),
      ]),
    ).toEqual([
      [
        1,
        [
          "da_bond = penalty + reward with reward > 0",
          "pool is Bonded",
          "pool backing is its lovelace above the floor",
          "pool backs at least one bond",
          "pool backs fewer than two bonds, so the step-3 slash leaves it short for step 4",
          "no watcher pool alert",
          "no committee pool readiness reason",
          "committee pool monitor emits no event on its first read",
          p16("alerts before commit"),
          "Apply of B1 lands",
          "Apply of B1 lands within the attestation timeout",
          "pool unchanged by Apply",
          "pool unchanged by Apply: same pool UTxO",
          "B1 is Attested",
        ],
      ],
      [
        3,
        [
          "B1 is Challenged",
          "Timeout spends the observed pool",
          "pool_out = pool_in - taken, taken = min(da_bond, backing)",
          "fee = fee_part + c with 0 <= c <= max_timeout_fee",
          "full pool: fee = penalty (c = 0)",
          "D3: challenger output = remaining - c + challenge_record_lovelace + payout",
          "D3: exactly one challenger output",
          "pool output keeps its datum and NFT",
          "pool read after the Timeout matches its output",
          "watcher under-backed alert fires",
          "committee reports a backing-short readiness reason",
          "committee pool monitor emits da_bond_pool_backing_short",
          p16("alerts after Timeout"),
          "P27: the committee node reported B1's challenge unavailable and never acted on it",
          "B1 is removed from the queue",
        ],
      ],
      [
        4,
        [
          "the slash left the pool short of one bond",
          "Apply of B2 is refused with pool-under-backed",
          "watcher under-backed alert fires",
          "committee reports a backing-short readiness reason",
          "committee pool monitor emits no transition while the pool stays short",
          p16("alerts while short"),
          "B2 stays Unattested",
        ],
      ],
      [
        5,
        [
          ...p18("top-up"),
          "top-up adds exactly its amount",
          "pool backs a bond again",
          "watcher under-backed alert clears",
          "committee backing-short readiness reason clears",
          "committee pool monitor emits da_bond_pool_backing_restored",
          p16("alerts after top-up"),
          "Apply of B2 lands",
          "Apply of B2 lands within the attestation timeout",
          "pool unchanged by Apply",
          "pool unchanged by Apply: same pool UTxO",
          "B2 is Attested",
        ],
      ],
      [
        2,
        [
          "Apply of B3 lands",
          "Apply of B3 lands within the attestation timeout",
          "B3 is Attested",
          "B3 is Challenged",
          "the committee answered the challenge",
          "pool untouched: no slash",
          "pool untouched: no slash: same pool UTxO",
          "B3 is Published (or has since merged)",
        ],
      ],
      [
        6,
        [
          "pool is Bonded",
          "no committee pool readiness reason",
          "committee pool monitor emits no event on its first read",
          p16("alerts before the withdrawal cycle"),
          ...begin("begin withdraw #1"),
          "Apply of B4 is refused with pool-withdrawing",
          ...p18("cancel withdraw"),
          "cancel: pool is Bonded again",
          "cancel: pool value unchanged",
          "watcher withdrawing alert clears",
          "committee withdrawing readiness reason clears",
          "committee pool monitor emits da_bond_pool_bonded",
          p16("alerts after cancel"),
          "Apply of B4 lands",
          "Apply of B4 lands within the attestation timeout",
          "B4 is Attested",
          ...begin("begin withdraw #2"),
          "0 < withdraw amount <= backing",
          ...p18("complete withdraw"),
          "complete: pool lovelace decreased by exactly the amount",
          "complete: pool is Bonded",
          "watcher withdrawing alert clears",
          "watcher under-backed alert matches the remaining backing",
          "committee withdrawing readiness reason clears",
          "committee backing-short reason matches the remaining backing",
          "committee pool monitor emits da_bond_pool_bonded",
          p16("alerts after complete"),
        ],
      ],
    ]);
  });

  it("fails the exact check each misbehaving port breaks", async () => {
    const p16 = (label: string) =>
      `P16: ${label}: committee reasons from the da-committee-node /readyz, events from its stderr`;
    const cases: readonly (readonly [
      string,
      Faults,
      number,
      readonly string[],
    ])[] = [
      // P16: the stderr half of the committee process view.
      [
        "a committee process view without its stderr events",
        { processWithoutEvents: true },
        1,
        [p16("alerts before commit")],
      ],
      [
        "no backing-short event after the Timeout",
        { dropEvent: "da_bond_pool_backing_short" },
        3,
        ["committee pool monitor emits da_bond_pool_backing_short"],
      ],
      [
        "no backing-restored event after the top-up",
        { dropEvent: "da_bond_pool_backing_restored" },
        5,
        ["committee pool monitor emits da_bond_pool_backing_restored"],
      ],
      [
        "no withdrawing event after the BeginWithdraw",
        { dropEvent: "da_bond_pool_withdrawing" },
        6,
        ["committee pool monitor emits da_bond_pool_withdrawing"],
      ],
      [
        "no bonded event after the CancelWithdraw",
        { dropEvent: "da_bond_pool_bonded" },
        6,
        ["committee pool monitor emits da_bond_pool_bonded"],
      ],
      [
        "a pool event while the pool stays short",
        { spuriousEventWhileShort: true },
        4,
        [
          "committee pool monitor emits no transition while the pool stays short",
        ],
      ],
      // P27: the node's own /readyz body and its restart.
      [
        "a 503 that names no pool reason while the pool is short",
        { readyzNoReasonWhileShort: true },
        3,
        ["committee reports a backing-short readiness reason"],
      ],
      [
        "a stale backing-short reason after the top-up",
        { staleReasonAfter: "top-up" },
        5,
        ["committee backing-short readiness reason clears"],
      ],
      [
        "a stale withdrawing reason after the CancelWithdraw",
        { staleReasonAfter: "cancel" },
        6,
        ["committee withdrawing readiness reason clears"],
      ],
      [
        "reported pool reasons that are not the /readyz body's",
        { reasonsOffBody: true },
        3,
        [p16("alerts after Timeout")],
      ],
      // P27: the payload-free node reports B1 unavailable and never answers.
      [
        "a node that never reported B1's challenge unavailable",
        { responderOnB1: "silent" },
        3,
        [
          "P27: the committee node reported B1's challenge unavailable and never acted on it",
        ],
      ],
      [
        "a node that acted on B1's challenge",
        { responderOnB1: "publishes" },
        3,
        [
          "P27: the committee node reported B1's challenge unavailable and never acted on it",
        ],
      ],
      [
        "a node that reports B1's challenge answered",
        { responderOnB1: "confirmed" },
        3,
        [
          "P27: the committee node reported B1's challenge unavailable and never acted on it",
        ],
      ],
      [
        "a node whose action on B1's challenge failed in execution",
        { responderOnB1: "executionFailed" },
        3,
        [
          "P27: the committee node reported B1's challenge unavailable and never acted on it",
        ],
      ],
      [
        "an event from before the restart reported after it",
        { eventCarriedAcrossRestart: true },
        6,
        [
          "committee pool monitor emits no event on its first read",
          p16("alerts before the withdrawal cycle"),
        ],
      ],
      // Step 3: the slash arithmetic and the Timeout's shape.
      [
        "a pool output that is not pool_in - taken",
        { timeoutPoolAfterOffBy: 1n },
        3,
        ["pool_out = pool_in - taken, taken = min(da_bond, backing)"],
      ],
      [
        "c above max_timeout_fee",
        { timeoutChallengerFee: PARAMS.maxTimeoutFee + 1n },
        3,
        [
          "fee = fee_part + c with 0 <= c <= max_timeout_fee",
          "full pool: fee = penalty (c = 0)",
        ],
      ],
      [
        "a fee below fee_part",
        { timeoutChallengerFee: -1n },
        3,
        [
          "fee = fee_part + c with 0 <= c <= max_timeout_fee",
          "full pool: fee = penalty (c = 0)",
        ],
      ],
      [
        "a fee above the penalty on a full pool",
        { timeoutChallengerFee: 1n },
        3,
        ["full pool: fee = penalty (c = 0)"],
      ],
      [
        "a Timeout that spends another pool",
        { timeoutReportsOtherPool: 7n },
        3,
        ["Timeout spends the observed pool"],
      ],
      [
        "two challenger outputs",
        { challengerOutputCount: 2 },
        3,
        ["D3: exactly one challenger output"],
      ],
      [
        "a pool read after the Timeout that differs from its output",
        { poolDriftAfterTimeout: 1n },
        3,
        ["pool read after the Timeout matches its output"],
      ],
      // Steps 1, 2, 4 and 5.
      [
        "a backing that is not lovelace - floor",
        { misreportFirstBacking: true },
        1,
        ["pool backing is its lovelace above the floor"],
      ],
      [
        "an Apply that spends the pool",
        { applyMovesPool: "outref" },
        1,
        ["pool unchanged by Apply: same pool UTxO"],
      ],
      [
        "an Apply that moves pool value",
        { applyMovesPool: "value" },
        1,
        ["pool unchanged by Apply"],
      ],
      [
        "a served challenge nobody answers",
        { noResponses: true },
        2,
        ["the committee answered the challenge"],
      ],
      [
        "a pool that is full again before step 4",
        { refillAfterTimeout: true },
        4,
        ["the slash left the pool short of one bond"],
      ],
      [
        "a watcher that stops flagging the short pool",
        { watcherQuietWhileShort: true },
        4,
        ["watcher under-backed alert fires"],
      ],
      [
        "a top-up that adds a different amount",
        { topUpOffBy: 1n, cliInProcess: true },
        5,
        ["top-up adds exactly its amount"],
      ],
      // Step 6: the withdrawal cycle.
      [
        "an unlock_at below validity upper bound + delay",
        { beginUnlockAtEarly: true },
        6,
        [
          "begin withdraw #1: unlock_at = validity upper bound + withdraw delay",
        ],
      ],
      [
        "a reported unlock_at the pool does not hold",
        { beginReportsUnlockAtOffBy: 1, cliInProcess: true },
        6,
        ["begin withdraw #1: pool is Withdrawing{unlock_at}"],
      ],
      [
        "a CancelWithdraw that moves value",
        { cancelValueOffBy: 1n, cliInProcess: true },
        6,
        ["cancel: pool value unchanged"],
      ],
      [
        "a CancelWithdraw that leaves the pool Withdrawing",
        { cancelLeavesWithdrawing: true, cliInProcess: true },
        6,
        ["cancel: pool is Bonded again"],
      ],
      [
        "alerts that miss the short pool a CompleteWithdraw leaves",
        { alertsMissShortAfterComplete: true },
        6,
        [
          "watcher under-backed alert matches the remaining backing",
          "committee backing-short reason matches the remaining backing",
        ],
      ],
      [
        "a watcher and a committee that miss Withdrawing",
        { watcherMissesWithdrawing: true, committeeMissesWithdrawing: true },
        6,
        [
          "watcher withdrawing alert fires",
          "committee reports a withdrawing readiness reason",
        ],
      ],
      [
        "a watcher that keeps flagging Withdrawing after the CancelWithdraw",
        { watcherKeepsWithdrawingAfter: "cancel" },
        6,
        ["watcher withdrawing alert clears"],
      ],
      [
        "a watcher that keeps flagging Withdrawing after the CompleteWithdraw",
        { watcherKeepsWithdrawingAfter: "complete" },
        6,
        ["watcher withdrawing alert clears"],
      ],
      [
        "a stale withdrawing reason after the CompleteWithdraw",
        { staleReasonAfter: "complete" },
        6,
        [
          "committee withdrawing readiness reason clears",
          "committee backing-short reason matches the remaining backing",
        ],
      ],
      [
        "a BeginWithdraw that moves value",
        { beginValueOffBy: 1n, cliInProcess: true },
        6,
        ["begin withdraw #1: pool value unchanged"],
      ],
      [
        "a CompleteWithdraw that leaves the pool Withdrawing",
        { completeLeavesWithdrawing: true, cliInProcess: true },
        6,
        [
          "complete: pool is Bonded",
          "watcher withdrawing alert clears",
          "committee withdrawing readiness reason clears",
          "committee pool monitor emits da_bond_pool_bonded",
        ],
      ],
      [
        "a CancelWithdraw that keeps the unlock_at",
        { cancelKeepsUnlockAt: true, cliInProcess: true },
        6,
        ["cancel: pool is Bonded again"],
      ],
      [
        "a pool read before CompleteWithdraw with no backing",
        { emptyPoolBeforeComplete: true },
        6,
        ["0 < withdraw amount <= backing"],
      ],
      [
        "a pool that is Withdrawing when the withdrawal cycle starts",
        { withdrawingBeforeStep6: true },
        6,
        ["pool is Bonded"],
      ],
      // Refusals and the watcher and committee views of steps 1, 3, 4 and 5.
      [
        "an Apply refused for another reason while the pool is short",
        { refusalReason: "l1 submitter preflight failed" },
        4,
        ["Apply of B2 is refused with pool-under-backed"],
      ],
      [
        "an Apply refused while the pool backs a bond",
        { refuseWhileBacked: true },
        1,
        ["Apply of B1 lands"],
      ],
      [
        "a watcher silent on the Timeout that left the pool short",
        { watcherQuietAfterTimeout: true },
        3,
        ["watcher under-backed alert fires"],
      ],
      [
        "a committee that drops its backing-short reason while the pool stays short",
        { committeeQuietWhileShort: true },
        4,
        ["committee reports a backing-short readiness reason"],
      ],
      [
        "a watcher still flagging after the top-up",
        { watcherStuckShort: true },
        5,
        ["watcher under-backed alert clears"],
      ],
      [
        "a top-up that leaves the pool short of a bond",
        { topUpOffBy: -1n, cliInProcess: true },
        5,
        ["top-up adds exactly its amount", "pool backs a bond again"],
      ],
      [
        "a top-up that makes the pool Withdrawing",
        { topUpFlipsState: true, cliInProcess: true },
        5,
        ["top-up adds exactly its amount"],
      ],
      [
        "a watcher alert on the full pool",
        { watcherAlertOnFirstRead: true },
        1,
        ["no watcher pool alert"],
      ],
      [
        "a committee pool reason on the full pool",
        { reasonOnFirstRead: true },
        1,
        ["no committee pool readiness reason"],
      ],
      [
        "a /readyz body that is not JSON",
        { readyzMalformed: "not-json" },
        1,
        [p16("alerts before commit")],
      ],
      [
        "a /readyz that answers 200 with ready=false",
        { readyzMalformed: "ok-not-ready" },
        1,
        [p16("alerts before commit")],
      ],
      [
        "an Apply that makes the pool Withdrawing",
        { applyMovesPool: "state" },
        1,
        ["pool unchanged by Apply"],
      ],
      [
        "a Timeout pool output without its datum or NFT",
        { poolDatumLost: true },
        3,
        ["pool output keeps its datum and NFT"],
      ],
      [
        "a Timeout that leaves the pool Withdrawing",
        { timeoutFlipsState: true },
        3,
        ["pool read after the Timeout matches its output"],
      ],
      [
        "a challenger output below payout + record when remaining is not reported",
        {
          minimalObservability: true,
          cliInProcess: true,
          challengerOutputShortBy: CHALLENGER_REMAINING + 1n,
        },
        3,
        ["challenger output >= payout + challenge_record_lovelace"],
      ],
      // Step 1's preconditions.
      [
        "a penalty that leaves no reward",
        { penaltyAtBond: true },
        1,
        ["da_bond = penalty + reward with reward > 0"],
      ],
      [
        "a pool that starts Withdrawing",
        { startWithdrawing: true },
        1,
        ["pool is Bonded"],
      ],
      [
        "a pool that backs less than one bond",
        { initialBacking: PARAMS.daBond - 1n },
        1,
        ["pool backs at least one bond"],
      ],
      [
        "a pool that backs two bonds",
        { initialBacking: 2n * PARAMS.daBond },
        1,
        [
          "pool backs fewer than two bonds, so the step-3 slash leaves it short for step 4",
        ],
      ],
      // A block-status port that lies at each require.
      ...(
        [
          ["header-B1", "Attested", "Unattested", 1, "B1 is Attested"],
          ["header-B1", "Challenged", "Attested", 3, "B1 is Challenged"],
          [
            "header-B1",
            "removed",
            "Challenged",
            3,
            "B1 is removed from the queue",
          ],
          ["header-B2", "Unattested", "Attested", 4, "B2 stays Unattested"],
          ["header-B2", "Attested", "Unattested", 5, "B2 is Attested"],
          ["header-B3", "Attested", "Unattested", 2, "B3 is Attested"],
          ["header-B3", "Challenged", "Attested", 2, "B3 is Challenged"],
          [
            "header-B3",
            "Published",
            "Challenged",
            2,
            "B3 is Published (or has since merged)",
          ],
          ["header-B4", "Attested", "Unattested", 6, "B4 is Attested"],
        ] as const
      ).map(
        ([header, actual, reported, step, name]) =>
          [
            `a block status that reports ${header} ${reported} while it is ${actual}`,
            { blockStatusLie: [header, actual, reported] },
            step,
            [name],
          ] as const,
      ),
    ];
    for (const [description, faults, step, names] of cases) {
      const { record } = await tryRunDaBondPoolJourney(fakePort(faults).port, {
        requireProcessEvidence: faults.cliInProcess !== true,
      });
      expect({ description, step: record.failure?.step }, description).toEqual({
        description,
        step,
      });
      expect(failedAssertions(record), description).toEqual(
        names.map((name) => ({ step, name })),
      );
    }
  });

  it("runs the end-of-step hook inside each step, after its body, and fails that step when it throws", async () => {
    const honest = fakePort();
    const record = await runDaBondPoolJourney(honest.port);
    expect(record.status).toBe("passed");
    const hooks = honest.calls.filter((call) => / step \d$/u.test(call));
    expect(hooks).toEqual(
      [1, 3, 4, 5, 2, 6].flatMap((step) => [
        `before step ${step}`,
        `after step ${step}`,
      ]),
    );
    // The step-6 stop comes after the step's last action.
    const last = honest.calls.lastIndexOf("after step 6");
    expect(last).toBe(honest.calls.length - 1);

    const { record: failed, error } = await tryRunDaBondPoolJourney(
      fakePort({ afterStepFails: 6 }).port,
      { requireProcessEvidence: true },
    );
    expect(error).toBeInstanceOf(Error);
    expect(failed.failure).toEqual({
      step: 6,
      message: "committee node did not exit 0 after step 6",
    });
    expect(stage(failed, 6).status).toBe("failed");
    expect(failedAssertions(failed)).toEqual([]);
  });

  /**
   * A fake chain as an earlier run left it after step 5: that run stopped at
   * the start of step 2. Returns the fake and the B2 it committed.
   */
  const stoppedAfterStep5 = async (faults: Faults = {}) => {
    const fake = fakePort(faults);
    const earlier = await tryRunDaBondPoolJourney({
      ...fake.port,
      beforeStep: async (step) => {
        if (step === 2) throw new Error("stopped before step 2");
        await fake.port.beforeStep!(step);
      },
    });
    expect(earlier.record.stages.map(({ step }) => step)).toEqual([
      1, 3, 4, 5, 2,
    ]);
    expect(earlier.record.failure?.step).toBe(2);
    const b2 = stage(earlier.record, 4).observations.find(
      (observation) => observation.label === "B2 header hash",
    );
    if (b2?.kind !== "value" || typeof b2.value !== "string")
      throw new Error("the earlier run recorded no B2 header hash");
    return {
      fake,
      b2: { label: "B2", headerHash: b2.value, committedAt: 0 },
    };
  };

  it("resumes after step 5: runs steps 2 and 6 only, keeps their checks, and reports a smoke", async () => {
    const { fake, b2 } = await stoppedAfterStep5();
    const calls: string[] = [];
    const port: DaBondPoolJourneyPort = {
      ...fake.port,
      params: async () => {
        calls.push("params");
        return fake.port.params();
      },
      beforeStep: async (step) => {
        calls.push(`before step ${step}`);
        await fake.port.beforeStep!(step);
      },
      resumeBeforeStep: async (step) => {
        calls.push(`resume before step ${step}`);
      },
      commitBlock: async (intent) => {
        calls.push(`commit ${intent.label}`);
        return fake.port.commitBlock(intent);
      },
    };
    const record = await runDaBondPoolJourney(port, {
      resume: { afterStep: 5, b2 },
      requireProcessEvidence: true,
    });
    expect(record.status).toBe("passed");
    expect(record.chronology).toEqual([2, 6]);
    expect(record.stages.map(({ step }) => step)).toEqual([2, 6]);
    expect(record.resumedAfterStep).toBe(5);
    expect(record.params).toEqual(PARAMS);
    // Parameters first, then the resume hook in place of step 2's.
    expect(calls.slice(0, 3)).toEqual([
      "params",
      "resume before step 2",
      "commit B3",
    ]);
    expect(calls).not.toContain("before step 2");
    expect(calls).toContain("before step 6");
    expect(stage(record, 2).assertions.length).toBeGreaterThan(0);
    expect(failedAssertions(record)).toEqual([]);

    const report = renderDaBondPoolJourneyReport(record, {
      ...META,
      adapter: "live-devnet",
    });
    expect(report).toContain(
      "RESUMED after step 5 (smoke, not journey evidence)",
    );
    expect(report).not.toContain("Observed on the process devnet");
    expect(report).toContain("| Run order | step 2 -> step 6 |");
    expect(report.match(/Not run\./gu)).toHaveLength(4);
  });

  it("fails a resumed step 2 at the check its port breaks", async () => {
    const { fake, b2 } = await stoppedAfterStep5({ noResponses: true });
    const { record } = await tryRunDaBondPoolJourney(fake.port, {
      resume: { afterStep: 5, b2 },
    });
    expect(record.status).toBe("failed");
    expect(record.failure?.step).toBe(2);
    expect(failedAssertions(record)).toEqual([
      { step: 2, name: "the committee answered the challenge" },
    ]);
  });

  it("times each stage and each long wait through the injected stage timer", async () => {
    const timed: string[] = [];
    const record = await runDaBondPoolJourney(fakePort().port, {
      stageTimer: async (name, action) => {
        timed.push(name);
        return action();
      },
    });
    expect(record.status).toBe("passed");
    expect(timed.filter((name) => name.includes(" step "))).toHaveLength(6);
    expect(timed).toContain("da-bond-pool: B1 response deadline");
    expect(timed).toContain("da-bond-pool: unlock_at");
  });
});
