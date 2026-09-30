import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { activateOperatorProgram } from "../src/transactions/register-active-operator.js";
import {
  addActivatedOperatorAbove,
  advanceShiftToPredecessor,
  requireActiveOperator,
} from "./operator-exit-emulator.advance-shift-to-predecessor.js";
import {
  BOND_LOVELACE,
  initOperatorExitFixture,
  PAST_SHIFT_END_SLOTS,
  runProgram,
} from "./operator-exit-emulator.build-operator-exit-snapshot.js";
import {
  appointFirstOperator,
  buildRetireTx,
  fetchDirectorySnapshot,
  keyHashOf,
  registerAndActivate,
  retireOperator,
} from "./operator-exit-emulator.build-retire-tx.js";
import { forceRegisterOperator } from "./operator-exit-emulator.force-activate-operator.js";
import {
  addActivatedOperator,
  newFundedWallet,
} from "./operator-exit-emulator.operator-exit-emulator.js";

describe("operator exit scheduler synchronisation", () => {
  it("hands the shift to the preceding operator when the scheduled operator retires", async () => {
    const fixture = await initOperatorExitFixture();
    const { lucid, contracts, emulator } = fixture;
    await registerAndActivate({
      operatorLucid: lucid,
      referenceScriptsLucid: fixture.referenceScriptsLucid,
      contracts,
      emulator,
    });
    const second = await addActivatedOperator(fixture);

    const wallets = new Map<string, LucidEvolution>([
      [fixture.operatorKeyHash, lucid],
      [second.operatorKeyHash, second.lucid],
    ]);
    const snapshot = await fetchDirectorySnapshot(lucid, contracts);
    // The active list is ascending by operator key and the schedule runs the
    // other way, so only its last member can be appointed first.
    const tail = SDK.findTailNode(snapshot.active);
    const tailKeyHash =
      tail === undefined ? null : SDK.nodeKeyHex(tail.datum.key);
    if (tailKeyHash === null) {
      throw new Error("Expected two active operators");
    }
    const otherKeyHash = [...wallets.keys()].find((key) => key !== tailKeyHash);
    const tailLucid = wallets.get(tailKeyHash);
    if (otherKeyHash === undefined || tailLucid === undefined) {
      throw new Error("Expected to know both operator wallets");
    }

    await appointFirstOperator({
      fixture,
      operatorLucid: tailLucid,
      operatorKeyHash: tailKeyHash,
    });
    const scheduled = await fetchDirectorySnapshot(lucid, contracts);
    expect(requireActiveOperator(scheduled.scheduler.datum).operator).toEqual(
      tailKeyHash,
    );

    const { layout } = await buildRetireTx({
      fixture,
      submitterLucid: tailLucid,
      operatorKeyHash: tailKeyHash,
      mode: "voluntary",
    });
    expect(layout.schedulerSync.kind).toEqual("SchedulerIsAdvancing");
    await retireOperator({
      fixture,
      submitterLucid: tailLucid,
      operatorKeyHash: tailKeyHash,
      mode: "voluntary",
    });

    const afterRetire = await fetchDirectorySnapshot(lucid, contracts);
    expect(SDK.findNodeByKey(afterRetire.active, tailKeyHash)).toBeUndefined();
    expect(SDK.findNodeByKey(afterRetire.retired, tailKeyHash)).toBeDefined();
    // `GoToNextDueToOperatorRemoval`: the shift moves to the removed node's
    // anchor, which is the remaining operator.
    expect(requireActiveOperator(afterRetire.scheduler.datum).operator).toEqual(
      otherKeyHash,
    );
    expect(
      SDK.deriveOperatorStatus(
        afterRetire,
        otherKeyHash,
        BigInt(emulator.now()),
        {
          maxInactivityStrikes: 5n,
        },
      ).holdsShift,
    ).toBe(true);
  }, 300_000);

  it("leaves the scheduler with no operators when the only active operator retires", async () => {
    const fixture = await initOperatorExitFixture();
    const { lucid, contracts, operatorKeyHash, emulator } = fixture;
    await registerAndActivate({
      operatorLucid: lucid,
      referenceScriptsLucid: fixture.referenceScriptsLucid,
      contracts,
      emulator,
    });
    await appointFirstOperator({
      fixture,
      operatorLucid: lucid,
      operatorKeyHash,
    });
    expect(
      requireActiveOperator(
        (await fetchDirectorySnapshot(lucid, contracts)).scheduler.datum,
      ).operator,
    ).toEqual(operatorKeyHash);

    const { layout } = await buildRetireTx({
      fixture,
      submitterLucid: lucid,
      operatorKeyHash,
      mode: "voluntary",
    });
    expect(layout.schedulerSync.kind).toEqual("SchedulerIsAdvancing");
    await retireOperator({
      fixture,
      submitterLucid: lucid,
      operatorKeyHash,
      mode: "voluntary",
    });

    // `RewindDueToOperatorRemoval` with the removed node as the list's only
    // member: the scheduler falls back to naming nobody.
    const afterRetire = await fetchDirectorySnapshot(lucid, contracts);
    expect(afterRetire.scheduler.datum).toEqual("NoActiveOperators");
    expect(
      SDK.findNodeByKey(afterRetire.active, operatorKeyHash),
    ).toBeUndefined();
    expect(
      SDK.findNodeByKey(afterRetire.retired, operatorKeyHash),
    ).toBeDefined();
  }, 300_000);

  it("rewinds the shift onto the surviving tail when the head retires during its own shift", async () => {
    const fixture = await initOperatorExitFixture();
    const { lucid, contracts, operatorKeyHash, emulator } = fixture;
    await registerAndActivate({
      operatorLucid: lucid,
      referenceScriptsLucid: fixture.referenceScriptsLucid,
      contracts,
      emulator,
    });
    // The second key sorts after the first, so the first node is the head and
    // the second node's insertion rewrote the head in the same transaction.
    const second = await addActivatedOperatorAbove(fixture, operatorKeyHash);
    const wallets = new Map<string, LucidEvolution>([
      [operatorKeyHash, lucid],
      [second.operatorKeyHash, second.lucid],
    ]);
    const active = (await fetchDirectorySnapshot(lucid, contracts)).active;
    const headNode = SDK.findNodeByKey(active, operatorKeyHash);
    const tailNode = SDK.findTailNode(active);
    expect(SDK.nodeKeyHex(tailNode?.datum.key ?? "Empty")).toEqual(
      second.operatorKeyHash,
    );
    expect(headNode?.utxo.txHash).toEqual(tailNode?.utxo.txHash);

    // Only the tail may be appointed first; the shift reaches the head by
    // the ordinary end-of-shift advance.
    await appointFirstOperator({
      fixture,
      operatorLucid: second.lucid,
      operatorKeyHash: second.operatorKeyHash,
    });
    const shiftStart = requireActiveOperator(
      (await fetchDirectorySnapshot(lucid, contracts)).scheduler.datum,
    ).start_time;
    emulator.awaitSlot(PAST_SHIFT_END_SLOTS);
    const incoming = await advanceShiftToPredecessor({
      fixture,
      wallets,
      scheduledKeyHash: second.operatorKeyHash,
      shiftStart,
    });
    expect(incoming).toEqual(operatorKeyHash);

    // The head's anchor is the root, so its retirement takes the
    // `RewindDueToOperatorRemoval` route and the tail inherits the shift.
    const { layout } = await buildRetireTx({
      fixture,
      submitterLucid: lucid,
      operatorKeyHash,
      mode: "voluntary",
    });
    expect(layout.schedulerSync.kind).toEqual("SchedulerIsAdvancing");
    await retireOperator({
      fixture,
      submitterLucid: lucid,
      operatorKeyHash,
      mode: "voluntary",
    });
    const afterRetire = await fetchDirectorySnapshot(lucid, contracts);
    expect(requireActiveOperator(afterRetire.scheduler.datum).operator).toEqual(
      second.operatorKeyHash,
    );
    expect(
      SDK.findNodeByKey(afterRetire.active, operatorKeyHash),
    ).toBeUndefined();
    expect(
      SDK.findNodeByKey(afterRetire.retired, operatorKeyHash),
    ).toBeDefined();
  }, 300_000);

  it("refuses to rewind the shift while a dangling registration could activate, until that registration activates", async () => {
    const fixture = await initOperatorExitFixture();
    const { lucid, contracts, operatorKeyHash, emulator } = fixture;
    await registerAndActivate({
      operatorLucid: lucid,
      referenceScriptsLucid: fixture.referenceScriptsLucid,
      contracts,
      emulator,
    });
    await appointFirstOperator({
      fixture,
      operatorLucid: lucid,
      operatorKeyHash,
    });

    // A stranger registers and its activation time passes without activation.
    const stranger = await newFundedWallet(fixture, 4_000_000_000n);
    const strangerKeyHash = await keyHashOf(stranger);
    await forceRegisterOperator({
      fixture,
      operatorLucid: stranger,
      operatorKeyHash: strangerKeyHash,
    });
    emulator.awaitSlot(180);

    // The only active operator holds the shift, so its retirement would
    // rewind the scheduler, which must prove that nobody can activate.
    await expect(
      buildRetireTx({
        fixture,
        submitterLucid: lucid,
        operatorKeyHash,
        mode: "voluntary",
      }),
    ).rejects.toThrow(
      /already eligible to activate, so the scheduler cannot rewind/,
    );
    const blocked = await fetchDirectorySnapshot(lucid, contracts);
    expect(SDK.findNodeByKey(blocked.active, operatorKeyHash)).toBeDefined();

    // Activating the stranger clears the block: the retiring operator is no
    // longer the last member and the shift goes to its neighbour.
    await runProgram(
      activateOperatorProgram(
        stranger,
        contracts,
        BOND_LOVELACE,
        fixture.referenceScriptsLucid,
      ),
    );
    await retireOperator({
      fixture,
      submitterLucid: lucid,
      operatorKeyHash,
      mode: "voluntary",
    });
    const afterRetire = await fetchDirectorySnapshot(lucid, contracts);
    expect(requireActiveOperator(afterRetire.scheduler.datum).operator).toEqual(
      strangerKeyHash,
    );
    expect(
      SDK.findNodeByKey(afterRetire.retired, operatorKeyHash),
    ).toBeDefined();
  }, 300_000);
});
