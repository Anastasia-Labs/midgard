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
import { deriveValidationProofItemPublicationV1 } from "@al-ft/midgard-sdk";
import { CML, Constr, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

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
    // canonicalDecode carries the complete item bytes; the scriptSources
    // stage-4 fold carries only the authenticated collection-proof tuple
    // (C21-STAGE4 Option A), so the item size lives on the proof there.
    if (
      witness.phase === phase &&
      ((witness.auxiliary?.kind === "transactionFieldItem" &&
        witness.auxiliary.itemCbor.length === itemBytes) ||
        (phase === "scriptSources" &&
          witness.auxiliary?.kind === "transactionRedeemerItemBegin" &&
          witness.auxiliary.collectionProof.itemLength === itemBytes))
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
    // The scriptSources stage-4 fold needs no bounded fallback at any size:
    // it folds the authenticated collection-proof tuple without a byte
    // reveal (C21-STAGE4 Option A), so even the above-threshold output has
    // a proof-only stage-4 witness.
    const stageFourComplete = findFieldItemStep(
      chunkedTrace,
      threshold + 1,
      "scriptSources",
    );
    expect(stageFourComplete.witness.phase).toBe("scriptSources");
    expect(stageFourComplete.witness.auxiliary?.kind).toBe(
      "transactionRedeemerItemBegin",
    );
  }, 120_000);

  it("builds output-size-independent stage-4 one-step evidence up to the 16,384-byte ledger maximum", async () => {
    // C21-STAGE4-GAP closure (Option A). Before the fix, the stage-4 fold
    // revealed the complete output bytes, its evidence crossed the
    // 16,384-byte L1 envelope at a measured 14,774-byte single-output best
    // case (frontier falling with output count), and the deployed
    // direct-only carriage bounded it near 8,769 bytes — so a dishonest
    // operator could finalize an invalid block by forging exactly that fold
    // step for a legal large output. Now the fold consumes only the
    // authenticated collection-proof tuple, so the one-step argument must
    // build at every admissible output size with O(1) evidence.
    const maxOutput = MIDGARD_CONSENSUS_LIMITS_V1.maxLedgerOutputPreimageBytes;

    const probeArgumentBytes = async (
      itemBytes: number,
    ): Promise<{
      readonly auxiliaryBytes: number;
      readonly evidenceBytes: number;
    }> => {
      const trace = await buildTraceWithOutputs([
        makeExactSizeOutputItem(itemBytes),
      ]);
      const step = findFieldItemStep(trace, itemBytes, "scriptSources");
      expect(step.witness.auxiliary?.kind).toBe("transactionRedeemerItemBegin");
      const argument = buildValidationOneStepArgumentV1({
        trace,
        stateIndex: step.stateIndex,
      });
      return {
        auxiliaryBytes: argument.auxiliaryCbor.length,
        evidenceBytes: argument.evidenceCbor.length,
      };
    };

    // The exact maximum admissible output — previously unprovable — must
    // now build, and the old 14,774 frontier as well as a small output must
    // produce byte-identical auxiliary sizes up to the CBOR integer-width
    // difference of item_length (<= 8 bytes).
    const atMaximum = await probeArgumentBytes(maxOutput);
    const atOldFrontier = await probeArgumentBytes(14_774);
    const small = await probeArgumentBytes(256);
    expect(atMaximum.evidenceBytes).toBeLessThan(2_048);
    expect(
      Math.abs(atMaximum.auxiliaryBytes - small.auxiliaryBytes),
    ).toBeLessThanOrEqual(8);
    expect(
      Math.abs(atMaximum.auxiliaryBytes - atOldFrontier.auxiliaryBytes),
    ).toBeLessThanOrEqual(8);
    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      console.info(
        JSON.stringify({
          scriptSourcesStageFourProofOnlyEvidenceV1: {
            maxLedgerOutputPreimageBytes: maxOutput,
            maximumOutputAuxiliaryBytes: atMaximum.auxiliaryBytes,
            maximumOutputEvidenceBytes: atMaximum.evidenceBytes,
            smallOutputAuxiliaryBytes: small.auxiliaryBytes,
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
      const itemCbor = "a5".repeat(itemBytes);
      const item = buildMidgardBoundedItemV1({
        fieldIndex: 2,
        itemIndex: 16_383,
        bytes: Buffer.from(itemCbor, "hex"),
      });
      const publication = deriveValidationProofItemPublicationV1({
        transactionId: "44".repeat(32),
        transactionCommitment: "55".repeat(32),
        collectionProof: {
          ...collectionProof,
          item_length: BigInt(item.bytes.length),
          item_commitment: item.commitment.toString("hex"),
        },
        itemCbor,
      });
      const signingKey = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 4));
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
      return CML.Transaction.new(
        body,
        witnessSet,
        true,
        undefined,
      ).to_cbor_bytes().length;
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
