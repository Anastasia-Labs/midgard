import { readFileSync } from "node:fs";
import { resolve } from "node:path";

import {
  buildMidgardValidationTraceTree,
  encodeCbor,
  hashMidgardValidationMachineStateV1,
  hashMidgardValidationRejectionCodeV1,
  MIDGARD_CONSENSUS_PROFILE_V1,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  type MidgardValidationVerdictName,
} from "@al-ft/midgard-core";
import { parseExactAikenDataCbor } from "@al-ft/midgard-fault-proofs";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildDeterministicValidationMachineTrace,
  buildValidationDisputeEvidenceBundleV1,
  buildValidationMachineLedgerInsertOpV1,
  buildValidationMachineLedgerMutationSteps,
  type DeterministicValidationMachineTrace,
  RejectCodes,
} from "../src/index.js";
import {
  FUNDED_OUTPUT_LOVELACE_V1,
  makeNativeTx,
  makeOutput,
  outRefFromByte,
  outRefFromTxId,
} from "./validation-fixtures.js";

const blueprint = JSON.parse(
  readFileSync(
    resolve(process.cwd(), "../../onchain/aiken/plutus.json"),
    "utf8",
  ),
) as unknown;

const baseContext = {
  consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
  eventKeyCbor: encodeCbor([2n, Buffer.alloc(32, 0x73)]),
  sourceKind: "forced" as const,
  blockEndTimeMs: 1_750_000_000_000,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  blockSlot: 100n,
};

const falsifyTerminalVerdict = (
  trace: DeterministicValidationMachineTrace,
  verdict: Exclude<MidgardValidationVerdictName, "pending">,
): DeterministicValidationMachineTrace => {
  const rejectionCode = verdict === "accepted" ? null : RejectCodes.EmptyInputs;
  const rejectionCodeHash =
    rejectionCode === null
      ? MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH
      : hashMidgardValidationRejectionCodeV1(rejectionCode);
  const states = trace.states.map((state, index) =>
    index === trace.states.length - 1
      ? {
          ...state,
          verdict,
          rejectionCodeHash,
        }
      : state,
  );
  const tree = buildMidgardValidationTraceTree(
    states.map(hashMidgardValidationMachineStateV1),
    verdict,
    rejectionCodeHash,
  );
  return {
    ...trace,
    states,
    tree,
    verdict,
    rejectionCode,
  };
};

const buildAcceptedForcedTrace =
  async (): Promise<DeterministicValidationMachineTrace> => {
    const spent = outRefFromByte(0x31);
    // RE-AUTHORED, NOT SUPPRESSED (#618 ruling 1): this trace has to be
    // genuinely accepted, and since the ValueAndMint output-descriptor step
    // began convicting under-funded outputs with `E_MIN_ADA` a 10-lovelace
    // output can no longer be. The rejected counterpart below keeps its
    // 10-lovelace output deliberately: it is convicted at `inputSets`, well
    // before ValueAndMint, and that is the rejection this fixture is about.
    const output = makeOutput(FUNDED_OUTPUT_LOVELACE_V1);
    const transaction = makeNativeTx({
      version: 1n,
      spendInputs: [spent],
      outputs: [output],
    });
    const expectedLedgerOps = [
      { type: "delete" as const, key: spent },
      buildValidationMachineLedgerInsertOpV1({
        key: outRefFromTxId(transaction.txId),
        outputCbor: output,
      }),
    ];
    const ledgerMutationSteps = await buildValidationMachineLedgerMutationSteps(
      {
        initialEntries: [{ outRef: spent, output }],
        operations: expectedLedgerOps,
      },
    );
    return await Effect.runPromise(
      buildDeterministicValidationMachineTrace({
        ...baseContext,
        transactionId: transaction.txId,
        canonicalTransactionCbor: transaction.txCbor,
        priorUtxosRoot: ledgerMutationSteps[0]!.preRoot.toString("hex"),
        postUtxosRoot: ledgerMutationSteps.at(-1)!.postRoot.toString("hex"),
        ledgerWitnessEntries: [{ outRef: spent, output }],
        expectedLedgerOps,
        ledgerMutationSteps,
        expectedVerdict: "accepted",
        expectedRejectionCode: null,
      }),
    );
  };

const buildRejectedForcedTrace =
  async (): Promise<DeterministicValidationMachineTrace> => {
    const transaction = makeNativeTx({
      version: 1n,
      spendInputs: [],
      outputs: [makeOutput(10n)],
    });
    return await Effect.runPromise(
      buildDeterministicValidationMachineTrace({
        ...baseContext,
        transactionId: transaction.txId,
        canonicalTransactionCbor: transaction.txCbor,
        priorUtxosRoot: "00".repeat(32),
        postUtxosRoot: "00".repeat(32),
        ledgerWitnessEntries: [],
        expectedLedgerOps: [],
        ledgerMutationSteps: [],
        expectedVerdict: "rejected",
        expectedRejectionCode: RejectCodes.EmptyInputs,
      }),
    );
  };

const expectExactBundle = (
  bundle: ReturnType<typeof buildValidationDisputeEvidenceBundleV1>,
): void => {
  expect(bundle.finalDispute.turn).toEqual({ type: "readyForOneStep" });
  expect(bundle.finalDispute.highIndex).toBe(bundle.finalDispute.lowIndex + 1);
  expect(bundle.moves.length).toBeGreaterThan(0);
  expect(
    bundle.moves.every(
      (move) =>
        move.proofCbor.length < 16 * 1024 &&
        move.disputeAfterCbor.length < 16 * 1024,
    ),
  ).toBe(true);
  parseExactAikenDataCbor({
    blueprint,
    definitionName:
      "midgard/validation_resolution_v1/ValidationBoundaryEvidenceV1",
    cbor: bundle.boundaryEvidenceCbor.toString("hex"),
    maxBytes: 16 * 1024 - 1,
  });
  parseExactAikenDataCbor({
    blueprint,
    definitionName: "midgard/validation_machine_v1/ValidationOneStepWitnessV1",
    cbor: bundle.oneStepArgument.transitionCbor.toString("hex"),
    maxBytes: 16 * 1024 - 1,
  });
  parseExactAikenDataCbor({
    blueprint,
    definitionName:
      "midgard/validation_machine_v1/ValidationAuxiliaryWitnessV1",
    cbor: bundle.oneStepArgument.auxiliaryCbor.toString("hex"),
    maxBytes: 16 * 1024 - 1,
  });
  // R5 item 1 retired the last direct resolvers, so
  // `ValidationOneStepEvidenceV1` is no longer any validator's redeemer
  // surface and the blueprint carries no definition for it: the evidence is
  // the hashed `(transition, auxiliary)` preimage, pinned below its envelope.
  expect(bundle.oneStepArgument.evidenceCbor.length).toBeLessThan(16 * 1024);
  expect(bundle.oneStepArgument.resolverIndex).toBeGreaterThanOrEqual(0);
  expect(bundle.oneStepArgument.resolverIndex).toBeLessThan(14);
  // Every index is a prepare + semantic family now: the semantic resolver
  // index is always selected (never null).
  expect(bundle.oneStepArgument.semanticResolverIndex).toBeGreaterThanOrEqual(
    0,
  );
};

describe("validation dispute evidence construction", () => {
  it("challenges a valid forced transaction falsely classified as a no-op", async () => {
    const challengerTrace = await buildAcceptedForcedTrace();
    const operatorTrace = falsifyTerminalVerdict(challengerTrace, "rejected");
    const bundle = buildValidationDisputeEvidenceBundleV1({
      operatorTrace,
      challengerTrace,
      currentTime: 1_750_000_001_000,
    });

    expectExactBundle(bundle);
    expect(bundle.finalDispute.highIndex).toBe(
      challengerTrace.states.length - 1,
    );
    expect(bundle.oneStepArgument).toMatchObject({
      resolverIndex: 13,
      semanticResolverIndex: 7,
    });
  });

  it("challenges an invalid forced no-op falsely classified as effectful", async () => {
    const challengerTrace = await buildRejectedForcedTrace();
    const operatorTrace = falsifyTerminalVerdict(challengerTrace, "accepted");
    const bundle = buildValidationDisputeEvidenceBundleV1({
      operatorTrace,
      challengerTrace,
      currentTime: 1_750_000_001_000,
    });

    expectExactBundle(bundle);
    expect(bundle.finalDispute.highIndex).toBe(
      challengerTrace.states.length - 1,
    );
    expect(bundle.oneStepArgument).toMatchObject({
      resolverIndex: 3,
      semanticResolverIndex: 0,
    });
  });

  it("fails closed when the two trace descriptors do not conflict", async () => {
    const trace = await buildAcceptedForcedTrace();
    expect(() =>
      buildValidationDisputeEvidenceBundleV1({
        operatorTrace: trace,
        challengerTrace: trace,
        currentTime: 1_750_000_001_000,
      }),
    ).toThrow(/cannot be disputed/u);
  });
});
