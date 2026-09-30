import {
  asArray,
  asBytes,
  decodeSingleCbor,
  hashMidgardValidationMachineState,
} from "@al-ft/midgard-core";
import { planMidgardFieldCarriage } from "@al-ft/midgard-core/codec/native-tx-carriage";
import {
  type MidgardFieldCarriage,
  selectMidgardFieldCarriageTier,
} from "@al-ft/midgard-core/codec/native-tx-field-access";
import {
  deriveCanonicalDecodeItemStageData,
  validationOneStepEvidenceHash,
} from "@al-ft/midgard-fault-proofs";
import {
  assertMidgardFieldCarriageResolvesAtDoor,
  AuthenticatedCanonicalDecodeItemDatum,
  ObservedCanonicalDecodeItemDatum,
  PreparedCanonicalDecodeItemDatum,
  PreparedValidationResolutionDatum,
  type PreparedValidationResolutionDatum as PreparedValidationResolutionDatumData,
  resolveMidgardFieldCarriageAgainstReferenceInputs,
  validationMachineStateDataFromCore,
  ValidationOneStepWitness,
  type ValidationOneStepWitness as ValidationOneStepWitnessData,
  VerifiedCanonicalDecodeItemDatum,
} from "@al-ft/midgard-sdk";
import { Constr, Data, type UTxO } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  buildValidationOneStepArgument,
  encodeValidationOneStepWitnessCbor,
  type ValidationMachineFieldCarriageResolver,
} from "../src/index.js";
import {
  buildTraceWithOutputs,
  findFieldTwoCompleteItemStep,
  NO_AUXILIARY_WITNESS_CBOR,
  OUTPUT_FIELD_INDEX,
  outputsForFieldTwoPreimageBytes,
  SIGNER_HASH,
} from "./complete-item-carriage-tiers-emulator.outputs-for-field-two-preimage-bytes.js";
import {
  publishCarriage,
  publishReferenceScript,
  setupEmulator,
  submitAndAwait,
} from "./complete-item-carriage-tiers-emulator.publish-carriage.js";
import {
  submitStage,
  type TierJourney,
} from "./complete-item-carriage-tiers-emulator.submit-stage.js";

export const runTierJourney = async (
  fieldPreimageBytes: number,
  options: {
    readonly withholdCarriageAtDoor?: boolean;
    /**
     * Override the field's items. Only the publication-maximum row uses this,
     * because that row's whole claim is about **one** complete item at the cap
     * rather than about a byte count reached however is convenient.
     */
    readonly outputs?: readonly Buffer[];
  } = {},
): Promise<TierJourney> => {
  const outputs =
    options.outputs ?? outputsForFieldTwoPreimageBytes(fieldPreimageBytes);
  const trace = await buildTraceWithOutputs(outputs);
  const { stateIndex, fieldPreimage } = findFieldTwoCompleteItemStep(
    trace,
    fieldPreimageBytes,
  );
  const harness = await setupEmulator();
  const stages =
    harness.contracts.validationTraceDispute.canonicalDecodeItemStages;
  const semanticContract =
    harness.contracts.validationTraceDispute.semanticResolvers[1];
  if (semanticContract === undefined) {
    throw new Error("canonical-decode item semantic resolver is missing");
  }

  // The observe validator arrives by `readFrom`, exactly as a real dispute step
  // sources it. It is also what makes this leg's indices non-degenerate: a
  // non-carriage entry sorts into the same reference-input list, so a resolver
  // that counted only carriage UTxOs would be right here and wrong on L1.
  const observeScriptReference = await publishReferenceScript(
    harness,
    stages.observe.spendingScript,
  );

  const preState = validationMachineStateDataFromCore(
    trace.states[stateIndex]!,
  );
  const plan = planMidgardFieldCarriage({
    owner: Buffer.from(SIGNER_HASH, "hex"),
    txId: Buffer.from(preState.transaction_id, "hex"),
    fieldIndex: OUTPUT_FIELD_INDEX,
    preimage: fieldPreimage,
  });
  expect(plan.tier).toBe(selectMidgardFieldCarriageTier(fieldPreimage.length));

  // The §8.6 mint re-derives the field commitment from the disputed
  // transaction's own compact structures, so they come off the step's own work
  // witness rather than being invented for the certification. The transition is
  // encoded on its own here — it carries no carriage and therefore needs no
  // resolver, which is what lets the certification be built *before* the
  // carriage exists to resolve against.
  const transitionCbor = encodeValidationOneStepWitnessCbor({
    witness: trace.witnesses[stateIndex]!,
    claimedSuccessor: trace.states[stateIndex + 1]!,
  });
  const transition = Data.from(
    transitionCbor.toString("hex"),
    ValidationOneStepWitness,
  ) as ValidationOneStepWitnessData;
  const control = asArray(
    decodeSingleCbor(Buffer.from(transition.work_witness_cbor, "hex")),
    "canonical_decode_item.control",
  );
  if (control.length !== 9) {
    throw new Error("canonical-decode control must carry nine fields");
  }
  const compactCbor = asBytes(
    control[0],
    "canonical_decode_item.compact",
  ).toString("hex");
  const witnessSetCompactCbor = asBytes(
    control[1],
    "canonical_decode_item.witness_set",
  ).toString("hex");

  const published = await publishCarriage({
    harness,
    plan,
    compactCbor,
    witnessSetCompactCbor,
  });

  // **The door transaction's reference-input set**, in the order the builder
  // hands it to the ledger. Both the resolver and the guard sort it canonically
  // themselves, which is the property that makes the indices ledger-truth
  // rather than builder-order.
  const doorReferenceInputs: readonly UTxO[] = [
    observeScriptReference,
    ...published.chunkUtxos,
    ...(published.certificateUtxo === undefined
      ? []
      : [published.certificateUtxo]),
  ];
  const resolveAgainst = (
    referenceInputs: readonly UTxO[],
  ): MidgardFieldCarriage =>
    resolveMidgardFieldCarriageAgainstReferenceInputs({
      plan,
      referenceInputs,
      certificatePolicyId: published.certificatePolicyId,
    });
  const resolveFieldCarriage: ValidationMachineFieldCarriageResolver = ({
    fieldIndex,
    fieldPreimage: preimage,
  }) => {
    if (fieldIndex !== plan.fieldIndex || !preimage.equals(fieldPreimage)) {
      throw new Error(
        "carriage resolver was asked about a field this journey did not plan",
      );
    }
    return resolveAgainst(doorReferenceInputs);
  };

  const argument = buildValidationOneStepArgument({
    trace,
    stateIndex,
    resolveFieldCarriage,
  });
  expect(argument.resolverIndex).toBe(0);
  expect(argument.semanticResolverIndex).toBe(1);
  const auxiliary = Data.from(argument.auxiliaryCbor.toString("hex"));
  if (
    !(auxiliary instanceof Constr) ||
    auxiliary.index !== 30 ||
    auxiliary.fields.length !== 1
  ) {
    throw new Error("complete-item auxiliary witness has an unexpected shape");
  }
  const carriageData = auxiliary.fields[0]!;
  const committedCarriage = resolveAgainst(doorReferenceInputs);

  // The guard, over the material the door is about to be handed. It is the
  // production call site's own check (`submitValidationDisputeSemanticResolution`
  // calls it immediately before this stage), run here against real ledger UTxOs.
  assertMidgardFieldCarriageResolvesAtDoor({
    carriage: committedCarriage,
    plan,
    doorReferenceInputs,
    certificatePolicyId: published.certificatePolicyId,
    label: `field ${OUTPUT_FIELD_INDEX.toString()}`,
  });

  // Option B (#620, #617 wave regeneration): the canonical-decode resolver
  // commits to the transition ALONE. The auxiliary hashed into `evidence_hash`
  // is `NoAuxiliaryWitness` whatever carriage the auxiliary witness names,
  // because the carriage is dereferenced — and content-checked — only at the
  // observe stage's §8.8 door. `submit.ts` computes exactly this for
  // `resolverIndex === 0`; this file states it as the same constant the #621
  // adversarial matrix uses, so the two harnesses cannot drift apart silently.
  const evidenceHash = validationOneStepEvidenceHash({
    transitionCbor: argument.transitionCbor,
    auxiliaryCbor: NO_AUXILIARY_WITNESS_CBOR,
  });
  const claimedSuccessorHash = hashMidgardValidationMachineState(
    trace.states[stateIndex + 1]!,
  ).toString("hex");
  const preparedThreadDatum = Data.to(
    {
      fraud_prover: SIGNER_HASH,
      data: {
        version: 1n,
        resolution: {
          version: 1n,
          pre_state: preState,
          operator_successor_hash: claimedSuccessorHash,
          challenger_successor_hash: claimedSuccessorHash,
        },
        evidence_hash: evidenceHash,
      },
    },
    PreparedValidationResolutionDatum,
  );
  const preparedResolution = (
    Data.from(
      preparedThreadDatum,
      PreparedValidationResolutionDatum,
    ) as PreparedValidationResolutionDatumData
  ).data;
  if (preparedResolution === null) {
    throw new Error("prepared thread datum is missing its state");
  }
  const stageData = deriveCanonicalDecodeItemStageData({
    preparedResolution,
    transition,
    fieldPreimage: fieldPreimage.toString("hex"),
  });
  const authenticatedDatum = Data.to(
    { fraud_prover: SIGNER_HASH, data: stageData.authenticated },
    AuthenticatedCanonicalDecodeItemDatum,
  );
  const preparedDatum = Data.to(
    { fraud_prover: SIGNER_HASH, data: stageData.prepared },
    PreparedCanonicalDecodeItemDatum,
  );
  const observedDatum = Data.to(
    { fraud_prover: SIGNER_HASH, data: stageData.observed },
    ObservedCanonicalDecodeItemDatum,
  );
  const verifiedDatum = Data.to(
    { fraud_prover: SIGNER_HASH, data: stageData.verified },
    VerifiedCanonicalDecodeItemDatum,
  );

  // Hand the seeded computation-thread token to the semantic resolver with the
  // PrepareSelected datum the chain starts from.
  const threadSeed = (await harness.lucid.wallet().getUtxos()).find(
    (utxo) => utxo.assets[harness.threadUnit] === 1n,
  );
  if (threadSeed === undefined) {
    throw new Error("thread token seed was not found");
  }
  const seedUnsigned = await harness.lucid
    .newTx()
    .collectFrom([threadSeed])
    .pay.ToContract(
      semanticContract.spendingScriptAddress,
      { kind: "inline", value: preparedThreadDatum },
      { lovelace: 80_000_000n, [harness.threadUnit]: 1n },
    )
    .complete();
  const seed = await submitAndAwait(harness, seedUnsigned);
  const threadUtxo = (
    await harness.lucid.utxosAt(semanticContract.spendingScriptAddress)
  ).find(
    (utxo) =>
      utxo.txHash === seed.txHash && utxo.assets[harness.threadUnit] === 1n,
  );
  if (threadUtxo === undefined) {
    throw new Error("seeded thread UTxO was not found");
  }

  const authenticate = await submitStage({
    harness,
    inputUtxo: threadUtxo,
    inputContract: semanticContract,
    outputContract: stages.source,
    outputDatum: authenticatedDatum,
    label: "canonical item authentication",
    // Option B (#620): `Verify(input_index, output_index, transition)`. The
    // retired four-field form appended the carriage here; it is now a refusal
    // (pinned as such by `complete-item-route-adversarial-emulator.test.ts`),
    // and the carriage reaches the chain only at the observe door below.
    encode: ({ inputIndex, outputIndex }) =>
      Data.to(
        new Constr(1, [
          new Constr(0, [
            inputIndex,
            outputIndex,
            Data.from(argument.transitionCbor.toString("hex")),
          ]),
        ]),
      ),
  });
  const source = await submitStage({
    harness,
    inputUtxo: authenticate.nextThreadUtxo,
    inputContract: stages.source,
    outputContract: stages.observe,
    outputDatum: preparedDatum,
    label: "canonical item source binding",
    encode: ({ inputIndex, outputIndex }) =>
      Data.to(new Constr(1, [new Constr(0, [inputIndex, outputIndex])])),
  });
  const observe = await submitStage({
    harness,
    inputUtxo: source.nextThreadUtxo,
    inputContract: stages.observe,
    outputContract: stages.proof,
    outputDatum: observedDatum,
    label: "canonical item observation",
    scriptReference: observeScriptReference,
    // `withholdCarriageAtDoor` submits the *same* committed redeemer with the
    // carriage left out of the reference-input set. It is the falsifier for
    // every green row above: if the door did not really dereference these
    // indices, withholding what they name would change nothing.
    carriageReferences: options.withholdCarriageAtDoor
      ? []
      : [
          ...published.chunkUtxos,
          ...(published.certificateUtxo === undefined
            ? []
            : [published.certificateUtxo]),
        ],
    // §592's `Observe(input_index, output_index, carriage)`.
    encode: ({ inputIndex, outputIndex }) =>
      Data.to(
        new Constr(1, [new Constr(0, [inputIndex, outputIndex, carriageData])]),
      ),
  });
  const proof = await submitStage({
    harness,
    inputUtxo: observe.nextThreadUtxo,
    inputContract: stages.proof,
    outputContract: stages.settlement,
    outputDatum: verifiedDatum,
    label: "canonical item proof verification",
    encode: ({ inputIndex, outputIndex }) =>
      Data.to(new Constr(1, [new Constr(0, [inputIndex, outputIndex])])),
  });

  return {
    tier: committedCarriage.carriage,
    preimageBytes: fieldPreimage.length,
    committedCarriage,
    auxiliaryBytes: argument.auxiliaryCbor.length,
    doorReferenceInputs,
    published,
    plan,
    observedDatum,
    observedOnLedger: observe.nextThreadUtxo.datum ?? "",
    observation: {
      itemCount: stageData.observed.observation.item_count,
      itemLength: stageData.observed.observation.item_length,
    },
    stageBytes: {
      authenticate: authenticate.signedBytes,
      source: source.signedBytes,
      observe: observe.signedBytes,
      proof: proof.signedBytes,
    },
    reobserve: resolveAgainst,
  };
};
