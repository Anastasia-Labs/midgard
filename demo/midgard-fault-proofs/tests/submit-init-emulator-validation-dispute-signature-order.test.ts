import {
  buildMidgardValidationTraceTree,
  encodeCbor,
  hashMidgardSignerLeaf,
  hashMidgardValidationMachineState,
  hashMidgardValidationWorkWitness,
} from "@al-ft/midgard-core";
import {
  decodeMidgardNativeByteListPreimage,
  decodeSingleCbor,
} from "@al-ft/midgard-core/codec";
import { validationTraceDescriptorDataFromCore } from "@al-ft/midgard-sdk";
import { buildValidationDisputeEvidenceBundle } from "@al-ft/midgard-validation";
import { CML } from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  buildForgedOperatorSuccessorValidationDisputeFixture,
  runForcedValidationDisputeScenario,
} from "./support/submit-init-emulator-shared.js";

const measure = (measurement: {
  readonly completeSignedBytes: number;
  readonly executionMemory: bigint;
  readonly executionSteps: bigint;
}): void => {
  expect(measurement.completeSignedBytes).toBeLessThanOrEqual(16_384);
  expect(measurement.executionMemory).toBeLessThanOrEqual(13_200_000n);
  expect(measurement.executionSteps).toBeLessThanOrEqual(8_000_000_000n);
  if (process.env.MIDGARD_PRINT_PROOF_FIT === "1")
    console.info(
      JSON.stringify(measurement, (_, value: unknown) =>
        typeof value === "bigint" ? value.toString() : value,
      ),
    );
};

it("proves an incorrect signature minimum successor through interactive resolution with two valid witnesses", async () => {
  const result = await runForcedValidationDisputeScenario(
    (input) =>
      buildForgedOperatorSuccessorValidationDisputeFixture({
        ...input,
        disputedPhase: "signatures",
        addressWitnessCount: 2,
      }),
    { onSubmittedTransaction: measure },
  );
  expect(result.awardResult?.txHash).toHaveLength(64);
  expect(result.removal?.transactions.length).toBeGreaterThan(0);
}, 600_000);

it("refuses another authentic first signature witness against an honest interactive trace", async () => {
  const failure = await expectOnchainRefusal(() =>
    runForcedValidationDisputeScenario(
      async (input) => {
        const fixture =
          await buildForgedOperatorSuccessorValidationDisputeFixture({
            ...input,
            disputedPhase: "signatures",
            addressWitnessCount: 2,
            dishonestChallenger: true,
          });
        const honest = fixture.operatorTrace;
        const low = fixture.disputedLowIndex;
        const adjacent = honest.witnesses[low]!;
        if (adjacent.auxiliary?.kind !== "transactionFieldChunk")
          throw new Error("signature minimum must carry its authentic field");
        const items = decodeMidgardNativeByteListPreimage(
          adjacent.auxiliary.fieldPreimage,
        );
        const alternativeIndex = adjacent.auxiliary.itemIndex === 0 ? 1 : 0;
        const witnessBytes = items[alternativeIndex]!;
        const address = decodeSingleCbor(witnessBytes) as readonly Uint8Array[];
        const signerHash = Buffer.from(
          CML.PublicKey.from_bytes(address[0]!).hash().to_raw_bytes(),
        );
        const alternativeControl = [
          ...(decodeSingleCbor(adjacent.cbor) as readonly unknown[]),
        ];
        alternativeControl[6] = 2n;
        alternativeControl[8] = 1n;
        alternativeControl[10] = Buffer.concat([
          signerHash,
          witnessBytes,
          encodeCbor(BigInt(alternativeIndex)),
        ]);
        alternativeControl[11] = signerHash;
        alternativeControl[12] = 1n;
        alternativeControl[13] = [[0n, hashMidgardSignerLeaf(signerHash)]];
        const alternativeCbor = encodeCbor(alternativeControl);
        const post = {
          ...honest.states[low + 1]!,
          workRoot: hashMidgardValidationWorkWitness({
            phase: "signatures",
            programCounter: honest.states[low]!.programCounter + 1,
            witnessCbor: alternativeCbor,
          }),
        };
        const states = honest.states.map((state, index) =>
          index <= low
            ? state
            : index === low + 1
              ? post
              : { ...state, workRoot: Buffer.alloc(32, 0x7e) },
        );
        const witnesses = [...honest.witnesses];
        witnesses[low] = {
          ...adjacent,
          auxiliary: { ...adjacent.auxiliary, itemIndex: alternativeIndex },
        };
        witnesses[low + 1] = { ...witnesses[low + 1]!, cbor: alternativeCbor };
        const challengerTrace = {
          ...honest,
          states,
          witnesses,
          tree: buildMidgardValidationTraceTree(
            states.map(hashMidgardValidationMachineState),
            honest.verdict,
            states.at(-1)!.rejectionCodeHash,
          ),
        };
        return {
          ...fixture,
          challengerTrace,
          challengerDescriptor: validationTraceDescriptorDataFromCore(
            challengerTrace.tree.descriptor,
          ),
          evidence: buildValidationDisputeEvidenceBundle({
            operatorTrace: honest,
            challengerTrace,
            currentTime: input.now + 2_000,
          }),
        };
      },
      { onSubmittedTransaction: measure },
    ),
  );
  expect(failure).toMatch(/semantic-resolution/u);
}, 600_000);

it.each([
  { count: 124, ordinal: 123 },
  { count: 318, ordinal: 0 },
  { count: 318, ordinal: 317 },
])(
  "resolves the authentic $count-witness signature at ordinal $ordinal within all transaction budgets",
  async ({ count, ordinal }) => {
    const result = await runForcedValidationDisputeScenario(
      (input) =>
        buildForgedOperatorSuccessorValidationDisputeFixture({
          ...input,
          disputedPhase: "signatures",
          addressWitnessCount: count,
          disputedMatchOrdinal: ordinal,
        }),
      {
        onSubmittedTransaction: measure,
        signatureItemMaximum: count === 318,
        stopAfter: "semantic-resolution",
      },
    );
    expect(result.semanticResult).toBeDefined();
  },
  600_000,
);
