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
  FRAUD_PROVER_REWARD_LOVELACE,
  initOperatorExitFixture,
  PAST_SHIFT_END_SLOTS,
  SLASHING_PENALTY_LOVELACE,
} from "./operator-exit-emulator.build-operator-exit-snapshot.js";
import {
  appointFirstOperator,
  bountyAddress,
  bountyAddressUtxos,
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
  slashingPenaltyFeePlan,
} from "./operator-exit-emulator.force-activate-operator.js";
import {
  addActivatedOperator,
  newFundedWallet,
} from "./operator-exit-emulator.operator-exit-emulator.js";

const bountyLovelace = async (
  lucid: LucidEvolution,
  keyHash: string,
): Promise<bigint> =>
  (await bountyAddressUtxos(lucid, keyHash)).reduce(
    (total, utxo) => total + (utxo.assets["lovelace"] ?? 0n),
    0n,
  );

/**
 * The bond is split exactly: the penalty as the fee and the bounty to the
 * claimant's enterprise address. Nothing reaches the submitter's own wallet.
 */
const expectExactBountySplit = async ({
  slasher,
  claimantKeyHash,
  fee,
  walletBefore,
  bountyBefore,
}: {
  readonly slasher: LucidEvolution;
  readonly claimantKeyHash: string;
  readonly fee: bigint;
  readonly walletBefore: bigint;
  readonly bountyBefore: bigint;
}): Promise<void> => {
  expect(fee).toEqual(SLASHING_PENALTY_LOVELACE);
  expect(BOND_LOVELACE).toEqual(
    SLASHING_PENALTY_LOVELACE + FRAUD_PROVER_REWARD_LOVELACE,
  );
  expect(
    (await bountyLovelace(slasher, claimantKeyHash)) - bountyBefore,
  ).toEqual(FRAUD_PROVER_REWARD_LOVELACE);
  expect((await walletLovelace(slasher)) - walletBefore).toEqual(0n);
};

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
    const slasherKeyHash = await keyHashOf(slasher);
    const activeSnapshot = await fetchDirectorySnapshot(lucid, contracts);
    const activeNode = SDK.findNodeByKey(
      activeSnapshot.active,
      operatorKeyHash,
    );
    if (activeNode === undefined) {
      throw new Error("Expected the operator to be active");
    }
    const balanceBeforeActiveSlash = await walletLovelace(slasher);
    const bountyBeforeActiveSlash = await bountyLovelace(
      slasher,
      slasherKeyHash,
    );
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
    await expectExactBountySplit({
      slasher,
      claimantKeyHash: slasherKeyHash,
      fee: activeSlash.fee,
      walletBefore: balanceBeforeActiveSlash,
      bountyBefore: bountyBeforeActiveSlash,
    });
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
    const bountyBeforeRetiredSlash = await bountyLovelace(
      slasher,
      slasherKeyHash,
    );
    const retiredSlash = await slashDuplicateOperator({
      fixture,
      submitterLucid: slasher,
      operatorKeyHash,
      removedRegisteredNodeKey: third.nodeKey,
      duplicateProof: { kind: "retired", node: retiredNode },
    });
    await expectExactBountySplit({
      slasher,
      claimantKeyHash: slasherKeyHash,
      fee: retiredSlash.fee,
      walletBefore: balanceBeforeRetiredSlash,
      bountyBefore: bountyBeforeRetiredSlash,
    });

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

    const slasherKeyHash = await keyHashOf(slasher);
    const balanceBefore = await walletLovelace(slasher);
    const bountyBefore = await bountyLovelace(slasher, slasherKeyHash);
    const slash = await slashDuplicateOperator({
      fixture,
      submitterLucid: slasher,
      operatorKeyHash,
      removedRegisteredNodeKey: second.nodeKey,
      duplicateProof: { kind: "registered", node: proofNode },
    });
    await expectExactBountySplit({
      slasher,
      claimantKeyHash: slasherKeyHash,
      fee: slash.fee,
      walletBefore: balanceBefore,
      bountyBefore,
    });

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

  it("pays the duplicate-registration bounty only to a claimant that signs, and only the exact reward", async () => {
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
    const slasherKeyHash = await keyHashOf(slasher);
    const otherKeyHash = await keyHashOf(
      await newFundedWallet(fixture, 3_000_000_000n),
    );
    const proofNode = SDK.findNodeByKey(
      (await fetchDirectorySnapshot(lucid, contracts)).registered,
      first.nodeKey,
    );
    if (proofNode === undefined) {
      throw new Error("Expected the first registration to be present");
    }
    const attempt = (overrides: Partial<SDK.SlashDuplicateOperatorTxConfig>) =>
      slashDuplicateOperator({
        fixture,
        submitterLucid: slasher,
        operatorKeyHash,
        removedRegisteredNodeKey: second.nodeKey,
        duplicateProof: { kind: "registered", node: proofNode },
        overrides,
      });

    // Naming another key as the claimant makes it a required signer, so a
    // submitter without that key cannot take the bounty for it.
    await expect(
      attempt({ bountyClaimantKeyHash: otherKeyHash }),
    ).rejects.toThrow(`Missing vkey witness. Key hash: ${otherKeyHash}`);

    // The named claimant signs, but the reward goes to another key: the
    // ledger accepts the witnesses and the registered-operators policy, the
    // only mint, refuses the routing.
    const redirected = await expectOnChainRefusal(async () =>
      attempt({
        fraudProverRewardLovelace: 0n,
        exactFeePlan: await slashingPenaltyFeePlan({
          submitterLucid: slasher,
          remainderAddress: bountyAddress(slasher, otherKeyHash),
          remainderLovelace: FRAUD_PROVER_REWARD_LOVELACE,
          declaredOutputCount: 1,
        }),
      }),
    );
    expect(redirected).toContain("duplicate-operator slashing tx");
    expect(redirected).toContain("Mint[0]");

    // One lovelace either way: the validator reads the compiled reward, so the
    // builder's own figure cannot move it. The registered-operators policy is
    // the only mint, so `Mint[0]` is its refusal; the traceless blueprint names
    // no check, and the Aiken bounty tests pin each one.
    for (const fraudProverRewardLovelace of [
      FRAUD_PROVER_REWARD_LOVELACE - 1n,
      FRAUD_PROVER_REWARD_LOVELACE + 1n,
    ]) {
      const refusal = await expectOnChainRefusal(() =>
        attempt({ fraudProverRewardLovelace }),
      );
      expect(refusal).toContain("duplicate-operator slashing tx");
      expect(refusal).toContain("Mint[0]");
    }
    expect(await bountyLovelace(slasher, slasherKeyHash)).toEqual(0n);
    expect(await bountyLovelace(slasher, otherKeyHash)).toEqual(0n);

    const walletBefore = await walletLovelace(slasher);
    const slash = await attempt({});
    await expectExactBountySplit({
      slasher,
      claimantKeyHash: slasherKeyHash,
      fee: slash.fee,
      walletBefore,
      bountyBefore: 0n,
    });
    const [bounty, ...others] = await bountyAddressUtxos(
      slasher,
      slasherKeyHash,
    );
    expect(others).toHaveLength(0);
    expect(bounty?.assets).toEqual({ lovelace: FRAUD_PROVER_REWARD_LOVELACE });
    expect(bounty?.datum ?? null).toBeNull();
    expect(bounty?.datumHash ?? null).toBeNull();
    expect(bounty?.scriptRef ?? null).toBeNull();
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
