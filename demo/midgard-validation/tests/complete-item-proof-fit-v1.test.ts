import { readFileSync } from "node:fs";
import { resolve } from "node:path";

import {
  buildMidgardBoundedItemV1,
  commitMidgardBoundedItemV1,
  MIDGARD_CONSENSUS_PROFILE_V1,
  midgardBoundedItemChunkCountV1,
} from "@al-ft/midgard-core";
import { encodeMidgardTxOutput } from "@al-ft/midgard-core/codec";
import {
  MIDGARD_CONSENSUS_LIMITS_V1,
  MIDGARD_V1_ENVELOPE_MEASUREMENTS,
} from "@al-ft/midgard-core/consensus-profile-v1";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { deriveValidationProofItemPublicationV1 } from "@al-ft/midgard-sdk";
import { CML, Constr, Data } from "@lucid-evolution/lucid";

import { parseExactAikenDataCbor } from "../../midgard-fault-proofs/src/aiken-blueprint-data.js";
import {
  encodeValidationSemanticResolutionRedeemerV1,
  selectValidationCompleteItemCarriageV1,
  validationOneStepEvidenceHashV1,
} from "../../midgard-fault-proofs/src/validation-dispute/submit.js";
import {
  buildDeterministicValidationMachineTrace,
  buildValidationMachineLedgerInsertOpV1,
  buildValidationMachineLedgerMutationSteps,
  buildValidationOneStepArgumentV1,
  type DeterministicValidationMachineTrace,
  type ValidationMachineWorkWitness,
} from "../src/index.js";
import {
  makeNativeTx,
  makeOutput,
  outRefFromByte,
  outRefFromTxId,
  TEST_ADDRESS_BYTES,
} from "./validation-fixtures.js";

const validationBlueprintPath =
  process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
  resolve(process.cwd(), "../../onchain/aiken/plutus.json");
const validationDisputeBlueprint = JSON.parse(
  readFileSync(validationBlueprintPath, "utf8"),
) as unknown;

const traceContext = {
  consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
  eventKeyCbor: Buffer.from("d8799f4100ff", "hex"),
  sourceKind: "normal" as const,
  blockEndTimeMs: 1_750_000_000_000,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  blockSlot: 100n,
};

const encodeBoundedBytesItem = (payload: Buffer): Buffer => {
  if (payload.length < 24) {
    return Buffer.concat([Buffer.from([0x40 + payload.length]), payload]);
  }
  if (payload.length <= 0xff) {
    return Buffer.concat([Buffer.from([0x58, payload.length]), payload]);
  }
  throw new Error("bounded datum chunk must stay below 256 bytes");
};

/**
 * Canonical Plutus-data filler: a single definite byte string when it fits 64
 * bytes, otherwise an indefinite list of 64-byte-bounded byte strings, which
 * matches Aiken's `cbor.serialise` list convention.
 */
const canonicalDatumFiller = (payloadBytes: number): Buffer => {
  if (payloadBytes <= 64) {
    return encodeBoundedBytesItem(Buffer.alloc(payloadBytes, 0xa5));
  }
  const items: Buffer[] = [];
  let remaining = payloadBytes;
  while (remaining > 0) {
    const take = Math.min(remaining, 64);
    items.push(encodeBoundedBytesItem(Buffer.alloc(take, 0xa5)));
    remaining -= take;
  }
  return Buffer.concat([Buffer.from([0x9f]), ...items, Buffer.from([0xff])]);
};

/**
 * Builds a canonical output item whose exact encoded length equals
 * `targetItemBytes` by sizing an inline-datum payload to close the gap.
 */
const makeExactSizeOutputItem = (targetItemBytes: number): Buffer => {
  const probe = (payloadBytes: number): Buffer =>
    encodeMidgardTxOutput({
      address: TEST_ADDRESS_BYTES,
      value: { lovelace: 10n, assets: new Map() },
      datum: { kind: "inline", cbor: canonicalDatumFiller(payloadBytes) },
    });
  let payload = Math.max(0, targetItemBytes - probe(0).length);
  for (let attempt = 0; attempt < 12; attempt += 1) {
    const candidate = probe(payload);
    const delta = targetItemBytes - candidate.length;
    if (delta === 0) {
      return candidate;
    }
    payload += delta;
    if (payload < 0) {
      throw new Error(
        `target item of ${targetItemBytes.toString()} bytes is below the minimum output framing`,
      );
    }
  }
  throw new Error(
    `could not converge on an exact ${targetItemBytes.toString()}-byte output item`,
  );
};

const buildTraceWithOutputs = async (
  outputs: readonly Buffer[],
): Promise<DeterministicValidationMachineTrace> => {
  const spent = outRefFromByte(0x11);
  const spentOutput = makeOutput(10n);
  const transaction = makeNativeTx({
    version: 1n,
    spendInputs: [spent],
    outputs,
  });
  const expectedLedgerOps = [
    { type: "delete" as const, key: spent },
    ...outputs.map((output, index) =>
      buildValidationMachineLedgerInsertOpV1({
        key: outRefFromTxId(transaction.txId, BigInt(index)),
        outputCbor: output,
      }),
    ),
  ];
  const ledgerMutationSteps = await buildValidationMachineLedgerMutationSteps({
    initialEntries: [{ outRef: spent, output: spentOutput }],
    operations: expectedLedgerOps,
  });
  return Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      ...traceContext,
      transactionId: transaction.txId,
      canonicalTransactionCbor: transaction.txCbor,
      priorUtxosRoot: ledgerMutationSteps[0]!.preRoot.toString("hex"),
      postUtxosRoot: ledgerMutationSteps.at(-1)!.postRoot.toString("hex"),
      ledgerWitnessEntries: [{ outRef: spent, output: spentOutput }],
      expectedLedgerOps,
      ledgerMutationSteps,
      expectedVerdict: "accepted",
      expectedRejectionCode: null,
    }),
  );
};

const findFieldItemStep = (
  trace: DeterministicValidationMachineTrace,
  itemBytes: number,
  phase: "canonicalDecode" | "scriptSources" = "canonicalDecode",
): {
  readonly stateIndex: number;
  readonly witness: ValidationMachineWorkWitness;
} => {
  for (let index = 0; index < trace.witnesses.length; index += 1) {
    const witness = trace.witnesses[index]!;
    if (
      witness.phase === phase &&
      witness.auxiliary?.kind === "transactionFieldItem" &&
      witness.auxiliary.itemCbor.length === itemBytes
    ) {
      return { stateIndex: index, witness };
    }
  }
  throw new Error(
    `trace has no ${phase} complete-item witness of ${itemBytes.toString()} bytes`,
  );
};

/**
 * The deployed `canonical_decode_item_semantic_v1` spend ABI takes the
 * complete item as two exact `Verify` fields (`collection_proof`,
 * `item_cbor`). The production submit path constructs this shape inside
 * `makeSemanticResolutionRedeemer`; this mirror keeps the measured redeemer
 * byte-identical to the deployed ABI without a live transaction context.
 */
const encodeDirectCompleteItemVerifyRedeemer = ({
  transitionCbor,
  auxiliaryCbor,
  inputIndex,
  outputIndex,
}: {
  readonly transitionCbor: Buffer;
  readonly auxiliaryCbor: Buffer;
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
}): Buffer => {
  const transition = Data.from(transitionCbor.toString("hex"));
  const auxiliary = Data.from(auxiliaryCbor.toString("hex"));
  if (
    !(auxiliary instanceof Constr) ||
    auxiliary.index !== 30 ||
    auxiliary.fields.length !== 2
  ) {
    throw new Error(
      "direct complete-item redeemer requires a TransactionFieldItemWitness auxiliary",
    );
  }
  return Buffer.from(
    Data.to(
      new Constr(1, [
        new Constr(0, [
          inputIndex,
          outputIndex,
          transition,
          auxiliary.fields[0]!,
          auxiliary.fields[1]!,
        ]),
      ]),
    ),
    "hex",
  );
};

describe("complete-item proof fit V1", () => {
  it("keeps the complete-item witness through the exact publication threshold and falls back only above it", async () => {
    const threshold =
      MIDGARD_CONSENSUS_LIMITS_V1.maxSinglePublicationCompleteItemBytes;
    const atThreshold = makeExactSizeOutputItem(threshold);
    const aboveThreshold = makeExactSizeOutputItem(threshold + 1);
    expect(atThreshold.length).toBe(threshold);
    expect(aboveThreshold.length).toBe(threshold + 1);

    const fittingTrace = await buildTraceWithOutputs([atThreshold]);
    const fitting = findFieldItemStep(fittingTrace, threshold);
    expect(fitting.witness.auxiliary?.kind).toBe("transactionFieldItem");

    const chunkedTrace = await buildTraceWithOutputs([aboveThreshold]);
    const chunkWitnesses = chunkedTrace.witnesses.filter(
      (witness) =>
        witness.auxiliary?.kind === "transactionFieldChunk" &&
        witness.auxiliary.collectionProof.itemLength === threshold + 1,
    );
    expect(chunkWitnesses.length).toBe(
      midgardBoundedItemChunkCountV1(threshold + 1),
    );
    const oversizedCanonicalDecodeComplete = chunkedTrace.witnesses.some(
      (witness) =>
        witness.phase === "canonicalDecode" &&
        witness.auxiliary?.kind === "transactionFieldItem" &&
        witness.auxiliary.itemCbor.length === threshold + 1,
    );
    expect(oversizedCanonicalDecodeComplete).toBe(false);
    // The scriptSources stage-4 fold intentionally has no bounded fallback:
    // it always authenticates the complete output item, so its maximum
    // provable shape is measured separately below.
    const stageFourComplete = findFieldItemStep(
      chunkedTrace,
      threshold + 1,
      "scriptSources",
    );
    expect(stageFourComplete.witness.phase).toBe("scriptSources");
  }, 120_000);

  it("measures the exact one-step evidence frontier of the fallback-free scriptSources output fold", async () => {
    // A maximum 16,384-byte ledger output is legal transaction content, but
    // its complete-item one-step argument must stay strictly below the
    // 16,384-byte L1 envelope. Measure the exact frontier.
    const maxOutput = MIDGARD_CONSENSUS_LIMITS_V1.maxLedgerOutputPreimageBytes;
    const maxTrace = await buildTraceWithOutputs([
      makeExactSizeOutputItem(maxOutput),
    ]);
    const maxStep = findFieldItemStep(maxTrace, maxOutput, "scriptSources");
    expect(() =>
      buildValidationOneStepArgumentV1({
        trace: maxTrace,
        stateIndex: maxStep.stateIndex,
      }),
    ).toThrow(/exceeds the strict L1 preimage envelope/u);

    const probeArgumentBytes = async (
      itemBytes: number,
    ): Promise<{
      readonly auxiliaryBytes: number;
      readonly evidenceBytes: number;
    } | null> => {
      const trace = await buildTraceWithOutputs([
        makeExactSizeOutputItem(itemBytes),
      ]);
      const step = findFieldItemStep(trace, itemBytes, "scriptSources");
      try {
        const argument = buildValidationOneStepArgumentV1({
          trace,
          stateIndex: step.stateIndex,
        });
        return {
          auxiliaryBytes: argument.auxiliaryCbor.length,
          evidenceBytes: argument.evidenceCbor.length,
        };
      } catch {
        return null;
      }
    };

    // The auxiliary carries the item as Plutus-data bytes (64-byte chunk
    // framing), so evidence overhead grows with the item. Locate the exact
    // largest provable output by bisection between a measured fit and a
    // measured failure.
    let low = 14_000;
    let lowMeasurement = await probeArgumentBytes(low);
    expect(lowMeasurement).not.toBeNull();
    let high = 14_900;
    expect(await probeArgumentBytes(high)).toBeNull();
    while (high - low > 1) {
      const middle = Math.floor((low + high) / 2);
      const measurement = await probeArgumentBytes(middle);
      if (measurement === null) {
        high = middle;
      } else {
        low = middle;
        lowMeasurement = measurement;
      }
    }
    const largestProvable = low;
    expect(await probeArgumentBytes(largestProvable + 1)).toBeNull();
    expect(lowMeasurement!.evidenceBytes).toBeLessThan(16 * 1024);
    expect(largestProvable).toBeLessThan(maxOutput);
    // Pin the measured frontier so silent producer changes re-open this row.
    expect(largestProvable).toBe(14_774);
    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      console.info(
        JSON.stringify({
          scriptSourcesStageFourCompleteOutputFrontierV1: {
            maxLedgerOutputPreimageBytes: maxOutput,
            largestProvableCompleteOutputBytes: largestProvable,
            largestProvableAuxiliaryBytes: lowMeasurement!.auxiliaryBytes,
            largestProvableEvidenceBytes: lowMeasurement!.evidenceBytes,
            evidenceEnvelopeBytes: 16 * 1024 - 1,
          },
        }),
      );
    }
  }, 240_000);

  it("encodes ABI-exact direct and reference complete-item proof redeemers for the maximum shapes", async () => {
    const directMax =
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemBytes;
    const item = makeExactSizeOutputItem(directMax);
    const trace = await buildTraceWithOutputs([item]);
    const { stateIndex } = findFieldItemStep(trace, directMax);
    const oneStepArgument = buildValidationOneStepArgumentV1({
      trace,
      stateIndex,
    });
    expect(oneStepArgument.resolverIndex).toBe(0);
    expect(oneStepArgument.semanticResolverIndex).toBe(1);
    expect(selectValidationCompleteItemCarriageV1(directMax)).toBe("direct");

    const directRedeemer = encodeDirectCompleteItemVerifyRedeemer({
      transitionCbor: oneStepArgument.transitionCbor,
      auxiliaryCbor: oneStepArgument.auxiliaryCbor,
      inputIndex: 1n,
      outputIndex: 0n,
    });
    const referenceRedeemer = encodeValidationSemanticResolutionRedeemerV1({
      oneStepArgument,
      inputIndex: 1n,
      outputIndex: 0n,
      proofItemReferenceInputIndex: 0n,
    });
    for (const redeemer of [directRedeemer, referenceRedeemer]) {
      parseExactAikenDataCbor({
        blueprint: validationDisputeBlueprint,
        definitionName:
          "fraud_proofs/validation_trace/canonical_decode_item_semantic_v1/SpendRedeemer",
        cbor: redeemer.toString("hex"),
        maxBytes: 16 * 1024 - 1,
      });
    }
    expect(directRedeemer.length).toBeGreaterThan(directMax);
    expect(directRedeemer.length).toBeLessThan(16 * 1024);
    expect(referenceRedeemer.length).toBeLessThan(2_048);
    expect(
      validationOneStepEvidenceHashV1({
        transitionCbor: oneStepArgument.transitionCbor,
        auxiliaryCbor: oneStepArgument.auxiliaryCbor,
      }),
    ).toMatch(/^[0-9a-f]{64}$/u);

    const bounded = buildMidgardBoundedItemV1({
      fieldIndex: 2,
      itemIndex: 0,
      bytes: item,
    });
    expect(
      commitMidgardBoundedItemV1({
        fieldIndex: 2,
        itemIndex: 0,
        totalLength: item.length,
        frontier: bounded.frontier,
      }).toString("hex"),
    ).toBe(bounded.commitment.toString("hex"));
  }, 120_000);

  it("measures that oversized items overflow every single-publication transaction, not just the item bound", () => {
    // §3.2: proof-fit decisions measure the actual publication transaction.
    // Construct the complete signed Conway publication for oversized shapes
    // and record the exact overshoot that necessitates bounded fallbacks.
    const collectionProof = {
      version: 1n,
      field_index: 2n,
      item_count: 16_384n,
      item_index: 16_383n,
      item_length: 0n,
      item_commitment: "11".repeat(32),
      frontier: Array.from({ length: 14 }, (_, height) => ({
        height: BigInt(height),
        hash: "22".repeat(32),
      })),
      siblings: Array.from({ length: 14 }, () => "33".repeat(32)),
    };
    const measurePublicationTransaction = (itemBytes: number): number => {
      const publication = deriveValidationProofItemPublicationV1({
        transactionId: "44".repeat(32),
        transactionCommitment: "55".repeat(32),
        collectionProof: {
          ...collectionProof,
          item_length: BigInt(itemBytes),
        },
        itemCbor: "a5".repeat(itemBytes),
      });
      const signingKey = CML.PrivateKey.from_normal_bytes(
        Buffer.alloc(32, 4),
      );
      const paymentKeyHash = signingKey.to_public().hash();
      const address = CML.Address.from_raw_bytes(
        Buffer.concat([
          Buffer.from([0x60]),
          Buffer.from(paymentKeyHash.to_raw_bytes()),
        ]),
      );
      const scriptAddress = CML.Address.from_raw_bytes(
        Buffer.concat([Buffer.from([0x70]), Buffer.alloc(28, 0x66)]),
      );
      const inputs = CML.TransactionInputList.new();
      inputs.add(
        CML.TransactionInput.new(
          CML.TransactionHash.from_raw_bytes(Buffer.alloc(32, 1)),
          0n,
        ),
      );
      const outputs = CML.TransactionOutputList.new();
      outputs.add(
        CML.TransactionOutput.new(
          scriptAddress,
          CML.Value.from_coin(70_000_000n),
          CML.DatumOption.new_datum(
            CML.PlutusData.from_cbor_hex(publication.datumCbor),
          ),
          undefined,
        ),
      );
      outputs.add(
        CML.TransactionOutput.new(
          address,
          CML.Value.from_coin(1_000_000_000n),
          undefined,
          undefined,
        ),
      );
      const body = CML.TransactionBody.new(inputs, outputs, 1_000_000n);
      const witnessSet = CML.TransactionWitnessSet.new();
      const vkeys = CML.VkeywitnessList.new();
      vkeys.add(
        CML.Vkeywitness.new(
          signingKey.to_public(),
          signingKey.sign(Buffer.alloc(32, 5)),
        ),
      );
      witnessSet.set_vkeywitnesses(vkeys);
      return CML.Transaction.new(body, witnessSet, true, undefined)
        .to_cbor_bytes().length;
    };

    const maxOutputItem = measurePublicationTransaction(
      MIDGARD_CONSENSUS_LIMITS_V1.maxLedgerOutputPreimageBytes,
    );
    expect(maxOutputItem).toBeGreaterThan(16 * 1024);
    const maxAggregateItem = measurePublicationTransaction(
      MIDGARD_CONSENSUS_LIMITS_V1.maxTransactionAggregateFieldBytes,
    );
    expect(maxAggregateItem).toBeGreaterThan(16 * 1024);
    // The exact publication-threshold shape still fits the same framing.
    const thresholdItem = measurePublicationTransaction(
      MIDGARD_CONSENSUS_LIMITS_V1.maxSinglePublicationCompleteItemBytes,
    );
    expect(thresholdItem).toBeLessThanOrEqual(16 * 1024);
    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      console.info(
        JSON.stringify({
          oversizedCompletePublicationOvershootV1: {
            maxLedgerOutputPreimageBytes:
              MIDGARD_CONSENSUS_LIMITS_V1.maxLedgerOutputPreimageBytes,
            maxLedgerOutputPublicationTransactionBytes: maxOutputItem,
            maxAggregateFieldItemBytes:
              MIDGARD_CONSENSUS_LIMITS_V1.maxTransactionAggregateFieldBytes,
            maxAggregatePublicationTransactionBytes: maxAggregateItem,
            thresholdItemBytes:
              MIDGARD_CONSENSUS_LIMITS_V1.maxSinglePublicationCompleteItemBytes,
            thresholdPublicationTransactionBytes: thresholdItem,
            maxTxSizeBytes: 16 * 1024,
          },
        }),
      );
    }
  });
});
