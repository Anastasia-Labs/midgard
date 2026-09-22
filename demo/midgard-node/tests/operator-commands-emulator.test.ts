/**
 * The node-side operator programs (status report, retirement, bond recovery,
 * takeover planning and strike submission, forced retirement, duplicate
 * selection) against the real compiled validators in the emulator.
 *
 * The SDK builders themselves are covered by `operator-exit-emulator.test.ts`
 * and `operator-inactivity-emulator.test.ts`; this file covers the layer the
 * CLI verbs and the watchdog call: local refusals with printable reasons, the
 * funding preflight, witness derivation from a live snapshot, and the
 * scheduler route each retirement takes.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { generateSeedPhrase, Lucid } from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";
import { describe, expect, it } from "vitest";

import {
  type OperatorEconomics,
  OperatorExitRefusal,
  recoverOperatorBondProgram,
  retireOperatorProgram,
  selectDuplicateRegistration,
} from "../src/transactions/operators/exit.js";
import { OperatorFundingShortfall } from "../src/transactions/operators/funding-preflight.js";
import { deriveOperatorStatusReport } from "../src/transactions/operators/status.js";
import {
  planTakeoverProgram,
  submitInactivityStrikeProgram,
} from "../src/transactions/operators/takeover.js";
import {
  advanceEmulatorPastUnixTime,
  appointFirstSchedulerOperator,
  fetchInactivityDirectorySnapshot,
  initOperatorInactivityFixture,
  type OperatorInactivityFixture,
  strikeOperatorToMaxStrikes,
} from "./helpers/operator-inactivity.js";

const ECONOMICS: OperatorEconomics = {
  requiredBondLovelace: 900_000_000n,
  slashingPenaltyLovelace: 500_000_000n,
  inactivitySlashingPenaltyLovelace: 100_000_000n,
};

const WATCHDOG = { enabled: true, patienceMs: 120_000 } as const;

const UNKNOWN_KEY = "ab".repeat(28);

const report = async (
  fixture: OperatorInactivityFixture,
  operatorKeyHash: string,
) =>
  deriveOperatorStatusReport(await fetchInactivityDirectorySnapshot(fixture), {
    operatorKeyHash,
    watchdog: WATCHDOG,
    nowMs: BigInt(fixture.emulator.now()),
  });

const retiredBondOf = async (
  fixture: OperatorInactivityFixture,
  operatorKeyHash: string,
): Promise<bigint | null> => {
  const snapshot = await fetchInactivityDirectorySnapshot(fixture);
  const node = SDK.findNodeByKey(snapshot.retired, operatorKeyHash);
  return node === undefined ? null : (node.utxo.assets["lovelace"] ?? 0n);
};

const expectRefusal = async <A, E>(effect: Effect.Effect<A, E>): Promise<E> => {
  const outcome = await Effect.runPromise(Effect.either(effect));
  if (Either.isRight(outcome)) {
    throw new Error("Expected the program to refuse");
  }
  return outcome.left;
};

describe("operator status report", () => {
  it("describes the scheduled operator, an idle operator, and an unknown key", async () => {
    const fixture = await initOperatorInactivityFixture(2);
    const appointed = await appointFirstSchedulerOperator(fixture);
    const idle = fixture.operators.find(
      ({ keyHash }) => keyHash !== appointed.operatorKeyHash,
    )!;

    const scheduled = await report(fixture, appointed.operatorKeyHash);
    expect(scheduled.state).toBe("active");
    expect(scheduled.duplicate).toBe(false);
    expect(scheduled.bondLovelace).toBe("900000000");
    expect(scheduled.inactivityStrikes).toBe(0);
    expect(scheduled.maxInactivityStrikes).toBe(5);
    expect(scheduled.scheduler.holdsShift).toBe(true);
    expect(scheduled.scheduler.currentOperator).toBe(appointed.operatorKeyHash);
    expect(scheduled.scheduler.shiftStartTime).toBe(
      appointed.startTime.toString(),
    );
    expect(scheduled.inactivity).not.toBeNull();
    expect(scheduled.inactivity!.blocked).toBeNull();
    expect(scheduled.inactivity!.strikesExhausted).toBe(false);
    expect(BigInt(scheduled.inactivity!.nextTakeoverTime!)).toBe(
      BigInt(scheduled.inactivity!.thresholdTime!) + 1n,
    );
    expect(scheduled.watchdog).toMatchObject({
      enabled: true,
      patienceMs: 120_000,
      lastTakeoverTxHash: null,
    });

    const idleReport = await report(fixture, idle.keyHash);
    expect(idleReport.state).toBe("active");
    expect(idleReport.scheduler.holdsShift).toBe(false);
    expect(idleReport.scheduler.currentOperator).toBe(
      appointed.operatorKeyHash,
    );

    const unknown = await report(fixture, UNKNOWN_KEY);
    expect(unknown.state).toBe("none");
    expect(unknown.memberships).toEqual([]);
    expect(unknown.bondLovelace).toBeNull();
    expect(unknown.bondRecoverable).toBe(false);
  }, 600_000);
});

describe("voluntary retirement and bond recovery", () => {
  it("retires the idle operator without touching the scheduler, rewinds when the scheduled operator leaves last, and recovers both bonds", async () => {
    const fixture = await initOperatorInactivityFixture(2);
    const appointed = await appointFirstSchedulerOperator(fixture);
    const idle = fixture.operators.find(
      ({ keyHash }) => keyHash !== appointed.operatorKeyHash,
    )!;

    const idleLucid = await fixture.lucidFor(idle.keyHash);
    const idleRetirement = await Effect.runPromise(
      retireOperatorProgram(
        idleLucid,
        fixture.contracts,
        fixture.referenceScriptsAddress,
        {
          operatorKeyHash: idle.keyHash,
          mode: "voluntary",
          economics: ECONOMICS,
        },
      ),
    );
    expect(idleRetirement.schedulerRoute).toBe("OperatorIsInactive");
    expect(idleRetirement.retiredBondLovelace).toBe(900_000_000n);
    expect(await retiredBondOf(fixture, idle.keyHash)).toBe(900_000_000n);
    expect((await report(fixture, idle.keyHash)).state).toBe("retired");

    // Retiring again is refused locally with a printable reason.
    const again = await expectRefusal(
      retireOperatorProgram(
        idleLucid,
        fixture.contracts,
        fixture.referenceScriptsAddress,
        {
          operatorKeyHash: idle.keyHash,
          mode: "voluntary",
          economics: ECONOMICS,
        },
      ),
    );
    expect(again).toBeInstanceOf(OperatorExitRefusal);
    expect((again as Error).message).toMatch(/retired, not active/);

    const scheduledLucid = await fixture.lucidFor(appointed.operatorKeyHash);
    const lastRetirement = await Effect.runPromise(
      retireOperatorProgram(
        scheduledLucid,
        fixture.contracts,
        fixture.referenceScriptsAddress,
        {
          operatorKeyHash: appointed.operatorKeyHash,
          mode: "voluntary",
          economics: ECONOMICS,
        },
      ),
    );
    expect(lastRetirement.schedulerRoute).toBe("Rewind");
    const afterLast = await fetchInactivityDirectorySnapshot(fixture);
    expect(afterLast.scheduler.datum).toBe("NoActiveOperators");
    expect(
      (await report(fixture, appointed.operatorKeyHash)).inactivity,
    ).toBeNull();

    for (const operator of [idle.keyHash, appointed.operatorKeyHash]) {
      const before = await report(fixture, operator);
      expect(before.bondRecoverable).toBe(true);
      const recovery = await Effect.runPromise(
        recoverOperatorBondProgram(
          await fixture.lucidFor(operator),
          fixture.contracts,
          fixture.referenceScriptsAddress,
          { operatorKeyHash: operator },
        ),
      );
      expect(recovery.bondLovelace).toBe(900_000_000n);
      expect(await retiredBondOf(fixture, operator)).toBeNull();
      expect((await report(fixture, operator)).state).toBe("none");
    }

    const nothingLeft = await expectRefusal(
      recoverOperatorBondProgram(
        idleLucid,
        fixture.contracts,
        fixture.referenceScriptsAddress,
        { operatorKeyHash: idle.keyHash },
      ),
    );
    expect(nothingLeft).toBeInstanceOf(OperatorExitRefusal);
    expect((nothingLeft as Error).message).toMatch(/no retired node/);
  }, 600_000);

  it("refuses to build when the submitting wallet cannot fund the fee", async () => {
    const fixture = await initOperatorInactivityFixture(1);
    const operator = fixture.operators[0]!;
    const emptyWallet = await Lucid(fixture.emulator, "Custom");
    emptyWallet.selectWallet.fromSeed(generateSeedPhrase());
    const refusal = await expectRefusal(
      retireOperatorProgram(
        emptyWallet,
        fixture.contracts,
        fixture.referenceScriptsAddress,
        {
          operatorKeyHash: operator.keyHash,
          mode: "voluntary",
          economics: ECONOMICS,
        },
      ),
    );
    expect(refusal).toBeInstanceOf(OperatorFundingShortfall);
    expect((refusal as Error).message).toMatch(/ADA/);
  }, 600_000);
});

describe("takeover planning, strike, and forced retirement", () => {
  it("plans not-yet before the threshold, strikes after it, and force-retires at the strike limit", async () => {
    const fixture = await initOperatorInactivityFixture(2);
    const appointed = await appointFirstSchedulerOperator(fixture);
    const successor = fixture.operators.find(
      ({ keyHash }) => keyHash !== appointed.operatorKeyHash,
    )!;
    const successorLucid = await fixture.lucidFor(successor.keyHash);

    const early = await Effect.runPromise(
      planTakeoverProgram(successorLucid, fixture.contracts),
    );
    expect(early.plan.kind).toBe("not-yet");
    if (early.plan.kind !== "not-yet") {
      throw new Error("unreachable");
    }
    expect(early.plan.currentOperator).toBe(appointed.operatorKeyHash);

    advanceEmulatorPastUnixTime(fixture.emulator, early.plan.thresholdMs);
    const due = await Effect.runPromise(
      planTakeoverProgram(successorLucid, fixture.contracts),
    );
    expect(due.plan.kind).toBe("ready");
    if (due.plan.kind !== "ready") {
      throw new Error("unreachable");
    }
    expect(due.plan.newOperatorKey).toBe(successor.keyHash);
    const strike = await Effect.runPromise(
      submitInactivityStrikeProgram(
        successorLucid,
        fixture.contracts,
        fixture.referenceScriptsAddress,
        { ...due, plan: due.plan },
      ),
    );
    expect(strike.skippedOperator).toBe(appointed.operatorKeyHash);
    expect(strike.newOperator).toBe(successor.keyHash);
    expect(strike.struckInactivityStrikes).toBe(1n);
    const struck = await report(fixture, appointed.operatorKeyHash);
    expect(struck.inactivityStrikes).toBe(1);
    expect(struck.scheduler.holdsShift).toBe(false);
    expect(
      (await report(fixture, successor.keyHash)).scheduler.holdsShift,
    ).toBe(true);

    // Forced retirement is refused locally below the strike limit.
    const tooEarly = await expectRefusal(
      retireOperatorProgram(
        successorLucid,
        fixture.contracts,
        fixture.referenceScriptsAddress,
        {
          mode: "forced-inactivity",
          snapshot: await fetchInactivityDirectorySnapshot(fixture),
          operatorKeyHash: appointed.operatorKeyHash,
          economics: ECONOMICS,
        },
      ),
    );
    expect(tooEarly).toBeInstanceOf(OperatorExitRefusal);
    expect((tooEarly as Error).message).toMatch(/forced retirement needs 5/);

    const exhausted = await strikeOperatorToMaxStrikes(
      fixture,
      appointed.operatorKeyHash,
    );
    expect(exhausted.inactivityStrikes).toBe(SDK.MAX_INACTIVITY_STRIKES);
    const atLimit = await report(fixture, appointed.operatorKeyHash);
    expect(atLimit.inactivityStrikes).toBe(5);

    // Past the cap the voluntary verb is refused before anything is built.
    const appointedLucid = await fixture.lucidFor(appointed.operatorKeyHash);
    const capped = await Effect.runPromise(
      retireOperatorProgram(
        appointedLucid,
        fixture.contracts,
        fixture.referenceScriptsAddress,
        {
          mode: "voluntary",
          snapshot: await fetchInactivityDirectorySnapshot(fixture),
          operatorKeyHash: appointed.operatorKeyHash,
          economics: ECONOMICS,
        },
      ),
    ).catch((error: unknown) => error);
    expect((capped as Error).message).toMatch(/can only be force-retired/);

    // Anyone may submit; the successor's wallet does, paying nothing net.
    const forced = await Effect.runPromise(
      retireOperatorProgram(
        successorLucid,
        fixture.contracts,
        fixture.referenceScriptsAddress,
        {
          mode: "forced-inactivity",
          snapshot: await fetchInactivityDirectorySnapshot(fixture),
          operatorKeyHash: appointed.operatorKeyHash,
          economics: ECONOMICS,
        },
      ),
    );
    expect(forced.mode).toBe("forced-inactivity");
    expect(forced.retiredBondLovelace).toBe(800_000_000n);
    expect(await retiredBondOf(fixture, appointed.operatorKeyHash)).toBe(
      800_000_000n,
    );
    const retired = await report(fixture, appointed.operatorKeyHash);
    expect(retired.state).toBe("retired");
    expect(retired.bondLovelace).toBe("800000000");
    expect(retired.bondRecoverable).toBe(true);

    // The partially slashed bond comes back to the operator in full.
    const recovery = await Effect.runPromise(
      recoverOperatorBondProgram(
        await fixture.lucidFor(appointed.operatorKeyHash),
        fixture.contracts,
        fixture.referenceScriptsAddress,
        { operatorKeyHash: appointed.operatorKeyHash },
      ),
    );
    expect(recovery.bondLovelace).toBe(800_000_000n);
    expect((await report(fixture, appointed.operatorKeyHash)).state).toBe(
      "none",
    );
  }, 600_000);
});

describe("selectDuplicateRegistration", () => {
  const node = (
    key: string,
    data: unknown,
    lovelace = 900_000_000n,
  ): SDK.NodeWithDatum => ({
    utxo: {
      txHash: "00".repeat(32),
      outputIndex: 0,
      address: "addr_test1",
      assets: { lovelace },
    },
    datum: {
      key: { Key: { key } },
      next: "Empty",
      data: data as SDK.LinkedListNodeView["data"],
    },
    assetName: key,
  });
  const operator = "11".repeat(28);
  const registeredNode = (activationKey: string) =>
    node(activationKey, SDK.encodeRegisteredOperatorDatumValue(operator));
  const hubOracle = {
    utxo: node("", null).utxo,
  } as unknown as SDK.OperatorDirectorySnapshot["hubOracle"];

  it("prefers an active membership as the proof", () => {
    const selection = selectDuplicateRegistration(
      {
        registered: [registeredNode("0000000000000001")],
        active: [node(operator, null)],
        retired: [],
        hubOracle,
      },
      operator,
    );
    expect(selection?.proof.kind).toBe("active");
    expect(SDK.nodeKeyHex(selection!.removed.datum.key)).toBe(
      "0000000000000001",
    );
  });

  it("removes the later of two registrations and proves it by the earlier one", () => {
    const selection = selectDuplicateRegistration(
      {
        registered: [
          registeredNode("0000000000000001"),
          registeredNode("0000000000000009"),
        ],
        active: [],
        retired: [],
        hubOracle,
      },
      operator,
    );
    expect(selection?.proof.kind).toBe("registered");
    expect(SDK.nodeKeyHex(selection!.removed.datum.key)).toBe(
      "0000000000000009",
    );
    expect(SDK.nodeKeyHex(selection!.proof.node.datum.key)).toBe(
      "0000000000000001",
    );
  });

  it("finds nothing to slash for a single honest registration", () => {
    expect(
      selectDuplicateRegistration(
        {
          registered: [registeredNode("0000000000000001")],
          active: [],
          retired: [],
          hubOracle,
        },
        operator,
      ),
    ).toBeNull();
  });
});
