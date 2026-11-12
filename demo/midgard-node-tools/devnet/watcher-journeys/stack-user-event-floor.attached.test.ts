import { existsSync } from "node:fs";
import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import {
  createPublishedWorkflowDeploymentAccounts,
  publishWorkflowDeployment,
} from "midgard-node/tests/helpers/published-workflow-deployment";
import { afterEach, expect, it, vi } from "vitest";

import { publishJourneyDeposit } from "./history-event-staging.js";
import {
  stackUserEventFloorOffsetMs,
  stackUserEventGate,
} from "./stack-user-event-floor.js";
import type { JourneyFaultPreparationInput } from "./staging.decode-journey-retained-block.js";

afterEach(() => {
  vi.restoreAllMocks();
});

const OPERATOR_2 = "02".repeat(28);

/** The deployment's user-event timing, as the SDK compiles it. */
const timing = {
  eventTtlMs: SDK.USER_EVENT_TX_TTL_MS,
  eventWaitMs: SDK.EVENT_WAIT_DURATION_MS,
  negligenceTimeoutMs: Number(SDK.USER_EVENTS_NEGLIGENCE_TIMEOUT_MS),
  graceMs: Number(SDK.NEW_SHIFT_INACTIVITY_GRACE_PERIOD_MS),
  shiftDurationMs: Number(SDK.SHIFT_DURATION_MS),
};

// On an attached stack operator-2's node is stopped while the drill stages
// its family, so a user event the drill publishes early in operator-2's
// shift falls due while the shift still runs, and another operator may
// strike operator-2 for it before the fault's late commit. Here the drill's
// own deposit, built through the same staging path, waits for the floor.
it("publishes an attached family's deposit no earlier than operator-2's floor, and refuses one behind an undelivered event", async () => {
  const accounts = createPublishedWorkflowDeploymentAccounts();
  const deployment = await publishWorkflowDeployment({ accounts });
  const { chain, emulator, operatorLucid } = deployment;
  // The SDK dates each event's validity from the wall clock, as a live
  // node's clock runs ahead of its ledger; here both are the emulator's.
  vi.spyOn(Date, "now").mockImplementation(() => emulator.now());
  const floorMs = stackUserEventFloorOffsetMs(timing);
  expect(floorMs).toBeGreaterThan(0);
  const readUndeliveredEvent = (tailEndTimeMs: bigint) =>
    Effect.runPromise(
      SDK.fetchNeglectedUserEventProgram(
        operatorLucid,
        deployment.contracts,
        tailEndTimeMs,
      ),
    );
  const stagingIn = async (shiftStartMs: number) => {
    const stages: string[] = [];
    const gate = stackUserEventGate({
      readAppointment: async () => ({
        operator: OPERATOR_2,
        startTime: BigInt(shiftStartMs),
      }),
      // Every case stages inside a shift that has not ended.
      holdOperator2: () =>
        Promise.reject(new Error("This case never holds operator-2")),
      refresh: () =>
        Promise.reject(new Error("This case never advances the scheduler")),
      timing,
      // A commit's allowance at twin cadence.
      commitAllowanceMs: 190_000,
      readUndeliveredEvent,
      now: () => chain.now(),
      wallClock: () => Date.now(),
      awaitLedgerTime: async (timeMs) => {
        await chain.awaitLedgerTime(timeMs);
      },
      onStage: (name) => {
        stages.push(name);
      },
    });
    const directory = await mkdtemp(join(tmpdir(), "stack-user-event-floor-"));
    const input = {
      context: {
        deployment,
        provider: {
          getUtxosWithUnit: (address: string, unit: string) =>
            operatorLucid.utxosAtWithUnit(address, unit),
          submitTx: (cbor: string) => emulator.submitTx(cbor),
          awaitTx: (txHash: string) => emulator.awaitTx(txHash),
        },
      },
      directory,
      onStage: (name: string) => {
        stages.push(name);
      },
      ledgerOwnerSeedPhrase: accounts.operator.seedPhrase,
      // No block is committed: every event is after the tail.
      predecessor: { header: { endTime: 0n } },
      beforeUserEvent: () => gate.beforeEvent(OPERATOR_2, 0n),
    } as unknown as JourneyFaultPreparationInput;
    return { input, stages, directory };
  };

  // Operator-2's shift starts once the deposit history's protection has
  // passed, so that nothing but the floor holds the drill's deposit back.
  const history = SDK.eventHistoryDeploymentFromContracts(
    SDK.requireEventHistoryContracts(deployment.contracts).deposit,
  );
  const protectedUntilMs = SDK.authenticateHistoryNodes(
    await operatorLucid.utxosAt(history.address),
    history,
  ).reduce(
    (latest, { node }) => Math.max(latest, Number(node.protected_until)),
    0,
  );
  await chain.awaitLedgerTime(protectedUntilMs + 120_000);
  const shiftStartMs = chain.now();
  const first = await stagingIn(shiftStartMs);
  try {
    await publishJourneyDeposit(first.input);
  } finally {
    await rm(first.directory, { recursive: true, force: true });
  }
  // The deposit as the scheduler would read it for a strike: due too late
  // in operator-2's shift for anyone to cite it.
  const deposit = await readUndeliveredEvent(0n);
  expect(deposit?.kind).toBe("Deposit");
  expect(
    SDK.computeInactivityThreshold({
      shiftStartMs: BigInt(shiftStartMs),
      stateQueueTailEndTimeMs: 0n,
      neglectedEvent: deposit!,
    }),
  ).toMatchObject({
    kind: "unsatisfiable",
    reason: "threshold-not-before-shift-end",
  });
  // It was built only once the floor had passed.
  expect(
    first.stages.findIndex((stage) =>
      stage.startsWith(
        `awaiting ${(shiftStartMs + floorMs).toString()} in operator-2's shift`,
      ),
    ),
  ).toBe(0);
  expect(first.stages).toContain("deposit publication");

  // In operator-2's next shift that deposit is still undelivered, and due
  // early enough for a strike before the fault's commit could land: the
  // drill refuses before it builds another event.
  const second = await stagingIn(chain.now());
  try {
    await expect(publishJourneyDeposit(second.input)).rejects.toThrow(
      /undelivered Deposit .*refusing before the drill publishes its user events/u,
    );
    expect(existsSync(join(second.directory, "deposit.json"))).toBe(false);
  } finally {
    await rm(second.directory, { recursive: true, force: true });
  }
}, 1_800_000);
