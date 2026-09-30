import "./operator-exit-emulator.operator-exit-scheduler-synchronisation.js";

import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  advanceShiftToPredecessor,
  requireActiveOperator,
} from "./operator-exit-emulator.advance-shift-to-predecessor.js";
import {
  BOND_LOVELACE,
  initOperatorExitFixture,
  PAST_SHIFT_END_SLOTS,
  SLASHING_PENALTY_LOVELACE,
} from "./operator-exit-emulator.build-operator-exit-snapshot.js";
import {
  appointFirstOperator,
  fetchDirectorySnapshot,
  keyHashOf,
  registerAndActivate,
  retireOperator,
  walletLovelace,
} from "./operator-exit-emulator.build-retire-tx.js";
import {
  expectOnChainRefusal,
  forceActivateOperator,
  forceRegisterOperator,
  slashDuplicateOperator,
} from "./operator-exit-emulator.force-activate-operator.js";
import {
  addActivatedOperator,
  newFundedWallet,
} from "./operator-exit-emulator.operator-exit-emulator.js";

describe("duplicate operator slashing", () => {
  it("slashes duplicate registrations of an active and then a retired operator", async () => {
    const fixture = await initOperatorExitFixture();
    const { lucid, contracts, operatorKeyHash, emulator } = fixture;

    const first = await forceRegisterOperator({
      fixture,
      operatorLucid: lucid,
      operatorKeyHash,
    });
    emulator.awaitSlot(3);
    const second = await forceRegisterOperator({
      fixture,
      operatorLucid: lucid,
      operatorKeyHash,
    });
    emulator.awaitSlot(3);
    const third = await forceRegisterOperator({
      fixture,
      operatorLucid: lucid,
      operatorKeyHash,
    });
    emulator.awaitSlot(200);

    const registered = await fetchDirectorySnapshot(lucid, contracts);
    expect(
      SDK.findOperatorDirectoryOccupancies(registered, operatorKeyHash),
    ).toHaveLength(3);
    expect(
      SDK.deriveOperatorStatus(
        registered,
        operatorKeyHash,
        BigInt(emulator.now()),
        {
          maxInactivityStrikes: 5n,
        },
      ).duplicate,
    ).toBe(true);

    await forceActivateOperator({
      fixture,
      operatorLucid: lucid,
      operatorKeyHash,
      registeredNodeKey: first.nodeKey,
    });

    const slasher = await newFundedWallet(fixture, 3_000_000_000n);
    const activeSnapshot = await fetchDirectorySnapshot(lucid, contracts);
    const activeNode = SDK.findNodeByKey(
      activeSnapshot.active,
      operatorKeyHash,
    );
    if (activeNode === undefined) {
      throw new Error("Expected the operator to be active");
    }
    const balanceBeforeActiveSlash = await walletLovelace(slasher);
    const activeSlash = await slashDuplicateOperator({
      fixture,
      submitterLucid: slasher,
      operatorKeyHash,
      removedRegisteredNodeKey: second.nodeKey,
      duplicateProof: {
        kind: "active",
        node: activeNode,
        hubOracleRefInput: activeSnapshot.hubOracle.utxo,
      },
    });
    expect(activeSlash.fee).toEqual(SLASHING_PENALTY_LOVELACE);
    expect((await walletLovelace(slasher)) - balanceBeforeActiveSlash).toEqual(
      BOND_LOVELACE - SLASHING_PENALTY_LOVELACE,
    );
    expect(
      SDK.findNodeByKey(
        (await fetchDirectorySnapshot(lucid, contracts)).registered,
        second.nodeKey,
      ),
    ).toBeUndefined();

    await retireOperator({
      fixture,
      submitterLucid: lucid,
      operatorKeyHash,
      mode: "voluntary",
    });
    const retiredSnapshot = await fetchDirectorySnapshot(lucid, contracts);
    const retiredNode = SDK.findNodeByKey(
      retiredSnapshot.retired,
      operatorKeyHash,
    );
    if (retiredNode === undefined) {
      throw new Error("Expected the operator to be retired");
    }
    const balanceBeforeRetiredSlash = await walletLovelace(slasher);
    const retiredSlash = await slashDuplicateOperator({
      fixture,
      submitterLucid: slasher,
      operatorKeyHash,
      removedRegisteredNodeKey: third.nodeKey,
      duplicateProof: { kind: "retired", node: retiredNode },
    });
    expect(retiredSlash.fee).toEqual(SLASHING_PENALTY_LOVELACE);
    expect((await walletLovelace(slasher)) - balanceBeforeRetiredSlash).toEqual(
      BOND_LOVELACE - SLASHING_PENALTY_LOVELACE,
    );

    const finalSnapshot = await fetchDirectorySnapshot(lucid, contracts);
    expect(
      SDK.findOperatorDirectoryOccupancies(finalSnapshot, operatorKeyHash),
    ).toHaveLength(1);
  }, 300_000);

  it("slashes a duplicate registration proved by another registration, and refuses a non-duplicate or a wrong fee", async () => {
    const fixture = await initOperatorExitFixture();
    const { lucid, contracts, operatorKeyHash, emulator } = fixture;

    const first = await forceRegisterOperator({
      fixture,
      operatorLucid: lucid,
      operatorKeyHash,
    });
    emulator.awaitSlot(3);
    const second = await forceRegisterOperator({
      fixture,
      operatorLucid: lucid,
      operatorKeyHash,
    });

    const slasher = await newFundedWallet(fixture, 3_000_000_000n);
    const snapshot = await fetchDirectorySnapshot(lucid, contracts);
    const proofNode = SDK.findNodeByKey(snapshot.registered, first.nodeKey);
    if (proofNode === undefined) {
      throw new Error("Expected the first registration to still be present");
    }

    // Same penalty, wrong amount: the fee is what the validator reads, so this
    // must be refused on chain rather than by the builder.
    const wrongFeeRefusal = await expectOnChainRefusal(() =>
      slashDuplicateOperator({
        fixture,
        submitterLucid: slasher,
        operatorKeyHash,
        removedRegisteredNodeKey: second.nodeKey,
        duplicateProof: { kind: "registered", node: proofNode },
        overrides: { slashingPenaltyLovelace: SLASHING_PENALTY_LOVELACE / 2n },
      }),
    );
    expect(wrongFeeRefusal).toContain("duplicate-operator slashing tx");

    const balanceBefore = await walletLovelace(slasher);
    const slash = await slashDuplicateOperator({
      fixture,
      submitterLucid: slasher,
      operatorKeyHash,
      removedRegisteredNodeKey: second.nodeKey,
      duplicateProof: { kind: "registered", node: proofNode },
    });
    expect(slash.fee).toEqual(SLASHING_PENALTY_LOVELACE);
    expect((await walletLovelace(slasher)) - balanceBefore).toEqual(
      BOND_LOVELACE - SLASHING_PENALTY_LOVELACE,
    );

    // The surviving registration cannot prove its own duplication: the proof
    // is a reference input and the removed node is an input, and the ledger
    // requires the two sets to be disjoint. The emulator applies that ledger
    // rule at submission, so the transaction never reaches the validator.
    const loneNode = SDK.findNodeByKey(
      (await fetchDirectorySnapshot(lucid, contracts)).registered,
      first.nodeKey,
    );
    if (loneNode === undefined) {
      throw new Error("Expected the first registration to survive the slash");
    }
    await expect(
      slashDuplicateOperator({
        fixture,
        submitterLucid: slasher,
        operatorKeyHash,
        removedRegisteredNodeKey: first.nodeKey,
        duplicateProof: { kind: "registered", node: loneNode },
      }),
    ).rejects.toThrow(/ReferenceInputsNotDisjointFromInputs/);

    // The surviving registration is not a duplicate of anything: a proof node
    // that belongs to a different operator does not make it one.
    const otherLucid = await newFundedWallet(fixture, 3_000_000_000n);
    const otherKeyHash = await keyHashOf(otherLucid);
    const other = await forceRegisterOperator({
      fixture,
      operatorLucid: otherLucid,
      operatorKeyHash: otherKeyHash,
    });
    const afterSlash = await fetchDirectorySnapshot(lucid, contracts);
    const foreignProof = SDK.findNodeByKey(
      afterSlash.registered,
      other.nodeKey,
    );
    if (
      foreignProof === undefined ||
      SDK.findNodeByKey(afterSlash.registered, first.nodeKey) === undefined
    ) {
      throw new Error("Expected both remaining registrations to be present");
    }
    const nonDuplicateRefusal = await expectOnChainRefusal(() =>
      slashDuplicateOperator({
        fixture,
        submitterLucid: slasher,
        operatorKeyHash,
        removedRegisteredNodeKey: first.nodeKey,
        duplicateProof: { kind: "registered", node: foreignProof },
      }),
    );
    expect(nonDuplicateRefusal).toContain("duplicate-operator slashing tx");
    expect(
      SDK.findNodeByKey(
        (await fetchDirectorySnapshot(lucid, contracts)).registered,
        first.nodeKey,
      ),
    ).toBeDefined();
  }, 300_000);

  it("does not let a dangling duplicate registration block the end-of-shift refresh", async () => {
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

    const activeSnapshot = await fetchDirectorySnapshot(lucid, contracts);
    const tail = SDK.findTailNode(activeSnapshot.active);
    const tailKeyHash =
      tail === undefined ? null : SDK.nodeKeyHex(tail.datum.key);
    if (tailKeyHash === null) {
      throw new Error("Expected two active operators");
    }
    const tailLucid = wallets.get(tailKeyHash);
    if (tailLucid === undefined) {
      throw new Error("Expected to know the tail operator wallet");
    }
    await appointFirstOperator({
      fixture,
      operatorLucid: tailLucid,
      operatorKeyHash: tailKeyHash,
    });
    const shiftStart = requireActiveOperator(
      (await fetchDirectorySnapshot(lucid, contracts)).scheduler.datum,
    ).start_time;

    // A third key registers twice and never activates: exactly the dangling
    // state `SlashDuplicateOperator` exists for.
    const stranger = await newFundedWallet(fixture, 4_000_000_000n);
    const strangerKeyHash = await keyHashOf(stranger);
    await forceRegisterOperator({
      fixture,
      operatorLucid: stranger,
      operatorKeyHash: strangerKeyHash,
    });
    emulator.awaitSlot(3);
    await forceRegisterOperator({
      fixture,
      operatorLucid: stranger,
      operatorKeyHash: strangerKeyHash,
    });

    // Past the end of the shift, the ordinary advance must still go through:
    // `GoToNextDueToEndOfShift` never reads the registered list.
    emulator.awaitSlot(PAST_SHIFT_END_SLOTS);
    const beforeAdvance = await fetchDirectorySnapshot(lucid, contracts);
    expect(
      SDK.findOperatorDirectoryOccupancies(beforeAdvance, strangerKeyHash),
    ).toHaveLength(2);
    const nextOperator = await advanceShiftToPredecessor({
      fixture,
      wallets,
      scheduledKeyHash: tailKeyHash,
      shiftStart,
    });

    const afterAdvance = await fetchDirectorySnapshot(lucid, contracts);
    expect(
      requireActiveOperator(afterAdvance.scheduler.datum).operator,
    ).toEqual(nextOperator);
  }, 300_000);
});

// ---------------------------------------------------------------------------
// Pure status queries (no chain)
// ---------------------------------------------------------------------------

export const OPERATOR_A = "aa".repeat(28);

export const OPERATOR_B = "bb".repeat(28);
