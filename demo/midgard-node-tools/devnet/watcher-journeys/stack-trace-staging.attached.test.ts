import * as SDK from "@al-ft/midgard-sdk";
import { paymentCredentialOf } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import {
  createPublishedWorkflowDeploymentAccounts,
  publishWorkflowDeployment,
} from "midgard-node/tests/helpers/published-workflow-deployment";
import { DEPOSIT_BLOCK_INTERVAL_MS } from "midgard-watcher/tests/support/published-deposit-trace";
import { preparePredecessorDeposits } from "midgard-watcher/tests/support/published-deposit-trace.predecessor-deposits";
import { afterEach, expect, it, vi } from "vitest";

import { stageAttachedTransitionTrace } from "./stack-trace-staging.js";
import {
  stackUserEventFloorOffsetMs,
  stackUserEventGate,
} from "./stack-user-event-floor.js";

afterEach(() => {
  vi.restoreAllMocks();
});

/** An attached stack whose committee is never reached in these cases. */
const attachedStack = {
  attest: async () => {
    throw new Error("The trace reached attestation");
  },
  shiftWorkMs:
    Number(SDK.MAX_EVENT_VALIDITY_RANGE_LENGTH_MS) +
    SDK.EVENT_WAIT_DURATION_MS +
    DEPOSIT_BLOCK_INTERVAL_MS +
    61_000,
  beforeFaultCommit: async () => {},
  beforeTraceDeposit: async () => {},
};

// On an attached stack the trace reads the tail's ledger inside operator-2's
// shift. The replay must already stand at that tail before the trace
// onboards operator-2, so the read inside the shift only follows the blocks
// appended since.
it("reads the attached stack's tail from the replay before onboarding operator-2", async () => {
  const deployment = await publishWorkflowDeployment({
    accounts: createPublishedWorkflowDeploymentAccounts(),
  });
  vi.spyOn(Date, "now").mockImplementation(() => deployment.emulator.now());
  const events: string[] = [];
  const onStage = (name: string) => {
    events.push(name);
  };
  const onboarded = () => events.includes("operator registration");

  // A tail the replay has no ledger for is refused before any onboarding.
  await expect(
    stageAttachedTransitionTrace(deployment, {
      attachedStack,
      onStage,
      liveTail: {
        ledgerAt: async (headerHash) => {
          events.push(`replay read ${headerHash}`);
          return undefined;
        },
      },
    }),
  ).rejects.toThrow(/has no independently replayed ledger/u);
  expect(onboarded()).toBe(false);

  // The replay answers until operator-2 is onboarded, then stops, so
  // staging ends at its first read inside the shift.
  events.length = 0;
  await expect(
    stageAttachedTransitionTrace(deployment, {
      attachedStack,
      onStage,
      liveTail: {
        ledgerAt: async (headerHash) => {
          events.push(`replay read ${headerHash}`);
          return onboarded() ? undefined : [];
        },
      },
    }),
  ).rejects.toThrow(/has no independently replayed ledger/u);
  const reads = events.filter((event) => event.startsWith("replay read "));
  expect(events[0]).toMatch(/^replay read /u);
  expect(events.indexOf("operator registration")).toBeGreaterThan(0);
  // The read inside the shift is of the same tail the replay was brought to.
  expect(reads).toHaveLength(2);
  expect(reads[1]).toBe(reads[0]);
}, 1_800_000);

/** The deployment's user-event timing, as the SDK compiles it. */
const timing = {
  eventTtlMs: SDK.USER_EVENT_TX_TTL_MS,
  eventWaitMs: SDK.EVENT_WAIT_DURATION_MS,
  negligenceTimeoutMs: Number(SDK.USER_EVENTS_NEGLIGENCE_TIMEOUT_MS),
  graceMs: Number(SDK.NEW_SHIFT_INACTIVITY_GRACE_PERIOD_MS),
  shiftDurationMs: Number(SDK.SHIFT_DURATION_MS),
};

// Only the fault's block delivers the trace deposit, so on a stack it waits
// for operator-2's publication floor as every family's own event does. With
// a deposit due after the tail already published, nothing else holds it.
it("builds the trace deposit no earlier than operator-2's floor when a predecessor deposit already exists", async () => {
  const deployment = await publishWorkflowDeployment({
    accounts: createPublishedWorkflowDeploymentAccounts(),
  });
  const { chain, emulator, operatorLucid } = deployment;
  vi.spyOn(Date, "now").mockImplementation(() => emulator.now());
  const operatorVkey = paymentCredentialOf(
    await operatorLucid.wallet().address(),
  ).hash;
  // A deposit after the tail, already due when operator-2's shift starts:
  // the honest predecessor delivers it, and the trace publishes no other.
  const { priorDeposits } = await preparePredecessorDeposits({
    deployment,
    lucid: operatorLucid,
    operatorVkey,
    onStage: () => {},
  });
  expect(priorDeposits).toHaveLength(1);
  const dueMs = priorDeposits[0]!.metadata.inclusionTime;
  await chain.awaitLedgerTime(dueMs + 1_000);
  // Operator-2's shift starts as late as it can: when the trace first reads
  // the scheduler, after onboarding.
  let shiftStartMs: number | undefined;
  const stages: string[] = [];
  const tailsRead: bigint[] = [];
  const gate = stackUserEventGate({
    readAppointment: async () => ({
      operator: operatorVkey,
      startTime: BigInt((shiftStartMs ??= chain.now())),
    }),
    timing,
    commitAllowanceMs: 190_000,
    readUndeliveredEvent: (tailEndTimeMs) => {
      tailsRead.push(tailEndTimeMs);
      return Effect.runPromise(
        SDK.fetchNeglectedUserEventProgram(
          operatorLucid,
          deployment.contracts,
          tailEndTimeMs,
        ),
      );
    },
    now: () => chain.now(),
    wallClock: () => Date.now(),
    awaitLedgerTime: async (timeMs) => {
      await chain.awaitLedgerTime(timeMs);
    },
    onStage: (name) => {
      stages.push(name);
    },
  });
  let builtMs: number | undefined;
  await expect(
    stageAttachedTransitionTrace(deployment, {
      attachedStack: {
        ...attachedStack,
        beforeTraceDeposit: (operator, tailEndTimeMs) =>
          gate.beforeEvent(operator, BigInt(tailEndTimeMs)),
      },
      onStage: (name) => {
        stages.push(name);
        if (name !== "deposit publication") return;
        builtMs = chain.now();
        throw new Error("Stopped once the trace deposit was built");
      },
      liveTail: { ledgerAt: async () => [] },
    }),
  ).rejects.toThrow(/Stopped once the trace deposit was built/u);
  expect(stages).not.toContain("honest predecessor deposit publication");
  expect(shiftStartMs).toBeDefined();
  const floorMs = shiftStartMs! + stackUserEventFloorOffsetMs(timing);
  expect(builtMs).toBeGreaterThanOrEqual(floorMs);
  // Another's undelivered event is read past the predecessor deposit, which
  // the honest predecessor delivers before the fault.
  expect(tailsRead).toEqual([BigInt(dueMs)]);
}, 1_800_000);
