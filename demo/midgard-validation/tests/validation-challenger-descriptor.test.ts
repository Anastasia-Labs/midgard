import {
  buildMidgardValidationTraceTree,
  hashMidgardValidationMachineState,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  type MidgardValidationTraceDescriptor,
  MidgardValidationTraceFrontier,
  verifyMidgardValidationTraceProof,
} from "@al-ft/midgard-core";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { Effect } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import {
  buildChallengerDescriptorV1,
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  decideValidationChallengeV1,
  openValidationTraceV1,
  type ValidationMachineReplayInput,
  type ValidationTraceLeavesV1,
  validationTraceStateHashV1,
} from "../src/index.js";
import { plutusV3SpendInput } from "./helpers/plutus-spend-input.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  makeNativeTx,
  makeOutput,
  outRefFromByte,
  outRefFromTxId,
} from "./validation-fixtures.js";
import {
  buildAcceptingUnitProgram,
  context,
} from "./validation-machine.validate-boundary-abi-and-collect-auxiliary-kinds.js";

const MAX_STEP_COUNT = 0xffff_ffff;

/** An accepted PlutusV3 spend: field 8 carries one redeemer. */
const plutusSpendInput = () => plutusV3SpendInput(buildAcceptingUnitProgram());

/** An accepted transaction with no scripts: field 8 is empty. */
const nativeSpendInput = async (): Promise<ValidationMachineReplayInput> => {
  const spent = outRefFromByte(0x11);
  const output = makeOutput(FUNDED_OUTPUT_LOVELACE);
  const transaction = makeNativeTx({
    version: 1n,
    spendInputs: [spent],
    outputs: [output],
  });
  const expectedLedgerOps = [
    { type: "delete" as const, key: spent },
    buildValidationMachineLedgerInsertOp({
      key: outRefFromTxId(transaction.txId),
      outputCbor: output,
    }),
  ];
  const ledgerMutationSteps = await buildValidationMachineLedgerMutationSteps({
    initialEntries: [{ outRef: spent, output }],
    operations: expectedLedgerOps,
  });
  return {
    l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
    ...context,
    transactionId: transaction.txId,
    canonicalTransactionCbor: transaction.txCbor,
    priorUtxosRoot: ledgerMutationSteps[0]!.preRoot.toString("hex"),
    postUtxosRoot: ledgerMutationSteps.at(-1)!.postRoot.toString("hex"),
    ledgerWitnessEntries: [{ outRef: spent, output }],
    expectedLedgerOps,
    ledgerMutationSteps,
    expectedVerdict: "accepted",
    expectedRejectionCode: null,
  };
};

/** The challenger's states at step count `n`, materialised: h_min(i, T). */
const materialisedChallengerStates = (
  leaves: ValidationTraceLeavesV1,
  stepCount: number,
): Buffer[] =>
  Array.from({ length: stepCount + 1 }, (_, index) =>
    validationTraceStateHashV1(leaves, Math.min(index, leaves.stepCount)),
  );

const verifies = (
  descriptor: MidgardValidationTraceDescriptor,
  challenger: ReturnType<typeof buildChallengerDescriptorV1>,
  index: number,
): boolean =>
  verifyMidgardValidationTraceProof({
    descriptor,
    proof: challenger.proof(index),
  });

describe("challenger descriptor", { timeout: 120_000 }, () => {
  let leaves: ValidationTraceLeavesV1;
  let nativeLeaves: ValidationTraceLeavesV1;

  beforeAll(async () => {
    leaves = await Effect.runPromise(
      openValidationTraceV1(await plutusSpendInput(), {
        revealStates: [0, 5],
      }),
    );
    nativeLeaves = await Effect.runPromise(
      openValidationTraceV1(await nativeSpendInput()),
    );
  });

  it("opens the honest trace with its state hashes and revealed states", () => {
    expect(leaves.requiresRealDescriptor).toBe(true);
    expect(nativeLeaves.requiresRealDescriptor).toBe(false);
    expect(leaves.stepCount).toBe(leaves.descriptor.stepCount);
    expect(leaves.stateHashes.length).toBe((leaves.stepCount + 1) * 32);
    for (const index of [0, 5, leaves.stepCount]) {
      expect(
        hashMidgardValidationMachineState(leaves.states.get(index)!),
      ).toEqual(validationTraceStateHashV1(leaves, index));
    }
    expect([...leaves.states.keys()].sort((a, b) => a - b)).toEqual([
      0,
      5,
      leaves.stepCount,
    ]);
    const tree = buildMidgardValidationTraceTree(
      materialisedChallengerStates(leaves, leaves.stepCount),
      leaves.descriptor.verdict,
      leaves.descriptor.rejectionCodeHash,
    );
    expect(tree.descriptor).toEqual(leaves.descriptor);
  });

  it("matches the materialised tree when truncating or extending the honest trace", () => {
    const honest = leaves.stepCount;
    for (const stepCount of [
      1,
      2,
      honest - 3,
      honest - 1,
      honest,
      honest + 1,
      honest + 5,
      2 * honest + 3,
    ]) {
      const challenger = buildChallengerDescriptorV1(leaves, stepCount);
      const tree = buildMidgardValidationTraceTree(
        materialisedChallengerStates(leaves, stepCount),
        leaves.descriptor.verdict,
        leaves.descriptor.rejectionCodeHash,
      );
      expect(challenger.descriptor).toEqual(tree.descriptor);
      expect(challenger.lastHonestIndex).toBe(Math.min(stepCount, honest));
      for (let index = 0; index <= stepCount; index += 1) {
        expect(challenger.proof(index)).toEqual(tree.proofs[index]);
      }
    }
    expect(buildChallengerDescriptorV1(leaves, honest).descriptor).toEqual(
      leaves.descriptor,
    );
  });

  it("pads lazily to the maximum step count in under a second", () => {
    const started = performance.now();
    const challenger = buildChallengerDescriptorV1(leaves, MAX_STEP_COUNT);
    const { descriptor } = challenger;
    // Walk two bisection paths to the end, verifying every midpoint.
    for (const goRight of [false, true]) {
      let low = 0;
      let high = MAX_STEP_COUNT;
      while (high - low > 1) {
        const middle = Math.floor((low + high) / 2);
        expect(verifies(descriptor, challenger, middle)).toBe(true);
        if (goRight) low = middle;
        else high = middle;
      }
    }
    for (const index of [
      0,
      1,
      leaves.stepCount - 1,
      leaves.stepCount,
      leaves.stepCount + 1,
      MAX_STEP_COUNT - 1,
      MAX_STEP_COUNT,
    ]) {
      expect(verifies(descriptor, challenger, index)).toBe(true);
    }
    const elapsed = performance.now() - started;
    expect(elapsed).toBeLessThan(1_000);

    expect(descriptor.stepCount).toBe(MAX_STEP_COUNT);
    expect(descriptor.initialStateHash).toEqual(
      validationTraceStateHashV1(leaves, 0),
    );
    expect(descriptor.terminalStateHash).toEqual(
      validationTraceStateHashV1(leaves, leaves.stepCount),
    );
    expect(challenger.stateHash(MAX_STEP_COUNT)).toEqual(
      validationTraceStateHashV1(leaves, leaves.stepCount),
    );
    expect(challenger.honestIndex(MAX_STEP_COUNT)).toBe(leaves.stepCount);
    const frontier = new MidgardValidationTraceFrontier();
    for (let index = 0; index <= leaves.stepCount; index += 1) {
      frontier.appendState(validationTraceStateHashV1(leaves, index));
    }
    frontier.appendRun(
      validationTraceStateHashV1(leaves, leaves.stepCount),
      MAX_STEP_COUNT - leaves.stepCount,
    );
    expect(descriptor.traceRoot).toEqual(frontier.root());
  });

  it("refuses step counts and indices outside the descriptor range", () => {
    expect(() => buildChallengerDescriptorV1(leaves, 0)).toThrow();
    expect(() =>
      buildChallengerDescriptorV1(leaves, MAX_STEP_COUNT + 1),
    ).toThrow();
    const challenger = buildChallengerDescriptorV1(leaves, 10);
    expect(() => challenger.proof(11)).toThrow();
    expect(() => challenger.proof(-1)).toThrow();
  });

  describe("decision", () => {
    const honestTerminal = (stepCount: number) =>
      validationTraceStateHashV1(leaves, Math.min(stepCount, leaves.stepCount));

    it("agrees with the honest descriptor", () => {
      expect(decideValidationChallengeV1(leaves, leaves.descriptor)).toEqual({
        kind: "agree",
      });
    });

    it("never opens a game on a trace-root-only difference", () => {
      for (const traceRoot of [Buffer.alloc(32, 0x5a), Buffer.alloc(32)]) {
        expect(
          decideValidationChallengeV1(leaves, {
            ...leaves.descriptor,
            traceRoot,
          }),
        ).toEqual({ kind: "agree" });
      }
      expect(
        decideValidationChallengeV1(leaves, {
          ...leaves.descriptor,
          traceRoot: Buffer.alloc(32, 0x5a),
          initialStateHash: Buffer.alloc(32, 0x5b),
        }),
      ).toEqual({ kind: "agree" });
    });

    it("answers an equal terminal hash at the wrong step count with the honest state", () => {
      for (const stepCount of [
        1,
        leaves.stepCount - 1,
        leaves.stepCount + 1,
        MAX_STEP_COUNT,
      ]) {
        expect(
          decideValidationChallengeV1(leaves, {
            ...leaves.descriptor,
            stepCount,
            terminalStateHash: honestTerminal(stepCount),
          }),
        ).toEqual({
          kind: "terminalIncoherent",
          terminalStateIndex: Math.min(stepCount, leaves.stepCount),
        });
      }
    });

    it("bisects a different terminal hash with the challenger's trace at the operator's count", () => {
      for (const stepCount of [
        1,
        leaves.stepCount - 2,
        leaves.stepCount,
        leaves.stepCount + 9,
      ]) {
        const decision = decideValidationChallengeV1(leaves, {
          ...leaves.descriptor,
          stepCount,
          traceRoot: Buffer.alloc(32, 0x33),
          terminalStateHash: Buffer.alloc(32, 0x44),
        });
        expect(decision.kind).toBe("bisection");
        if (decision.kind !== "bisection") continue;
        expect(decision.challenger.descriptor).toEqual(
          buildChallengerDescriptorV1(leaves, stepCount).descriptor,
        );
      }
    });

    it("leaves a normal non-accepting descriptor to the descriptor check", () => {
      expect(
        decideValidationChallengeV1(leaves, {
          ...leaves.descriptor,
          verdict: "rejected",
          rejectionCodeHash: Buffer.alloc(32, 0x21),
        }),
      ).toEqual({ kind: "descriptorVerdict" });
      expect(
        decideValidationChallengeV1(leaves, {
          ...leaves.descriptor,
          rejectionCodeHash: Buffer.alloc(32, 0x21),
        }),
      ).toEqual({ kind: "descriptorVerdict" });
      expect(leaves.descriptor.rejectionCodeHash).toEqual(
        MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
      );
    });

    it("opens no game on a transaction that keeps the placeholder", () => {
      expect(
        decideValidationChallengeV1(nativeLeaves, nativeLeaves.descriptor),
      ).toEqual({ kind: "agree" });
      expect(
        decideValidationChallengeV1(nativeLeaves, {
          ...nativeLeaves.descriptor,
          traceRoot: Buffer.alloc(32, 0x33),
          terminalStateHash: Buffer.alloc(32, 0x44),
        }),
      ).toEqual({ kind: "placeholderTarget" });
    });

    it("bisects a real-descriptor transaction that committed a one-step placeholder", () => {
      const decision = decideValidationChallengeV1(leaves, {
        ...leaves.descriptor,
        stepCount: 1,
        traceRoot: Buffer.alloc(32, 0x01),
        initialStateHash: Buffer.alloc(32, 0x02),
        terminalStateHash: Buffer.alloc(32, 0x03),
      });
      expect(decision.kind).toBe("bisection");
      if (decision.kind !== "bisection") return;
      expect(decision.challenger.descriptor.stepCount).toBe(1);
      expect(decision.challenger.lastHonestIndex).toBe(1);
    });

    it("does not dispute a malformed descriptor by a game", () => {
      for (const malformed of [
        { ...leaves.descriptor, schemaVersion: 99 },
        { ...leaves.descriptor, machineVersion: 99 },
        { ...leaves.descriptor, stepCount: 0 },
        { ...leaves.descriptor, stepCount: MAX_STEP_COUNT + 1 },
        { ...leaves.descriptor, stepCount: 1.5 },
      ]) {
        expect(
          decideValidationChallengeV1(
            leaves,
            malformed as unknown as MidgardValidationTraceDescriptor,
          ),
        ).toEqual({
          kind: "malformed",
        });
      }
    });
  });
});
