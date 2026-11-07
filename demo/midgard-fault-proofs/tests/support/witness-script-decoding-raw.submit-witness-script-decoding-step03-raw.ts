import {
  type FieldOpening,
  type NativeTxWitnessSetCompact,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  type WitnessScriptDecodingScanState,
  WitnessScriptDecodingStep03DatumSchema,
  WitnessScriptDecodingStep03RedeemerSchema,
  WitnessScriptDecodingStep04DatumSchema,
  WitnessScriptDecodingStep04RedeemerSchema,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../../src/linear-fault-family.js";
import { submitLinearFaultFinalize } from "../../src/linear-fault-finalize.js";
import { submitLinearFaultContinue } from "../../src/linear-fault-submit.js";
import type { ResolvedProverSigner } from "../../src/runtime.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";
import type { FaultProofWitnessReferenceScripts } from "../../src/witness-reference-scripts.js";
import type { WitnessScriptDecodingContracts } from "../../src/witness-script-decoding/contracts.js";
import type { WitnessScriptDecodingScanArgs } from "../../src/witness-script-decoding/submit-step-03.js";
import { FAMILY } from "./witness-script-decoding-raw.over-bound-field-carriage-plan.js";

/**
 * Step 03 with the redeemer arguments, successor state and successor script
 * carried exactly as given: a substituted chunk, control, frame, budget,
 * checkpoint or successor is refused by the applied validator.
 */
export const submitWitnessScriptDecodingStep03Raw = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  args,
  nextState,
  nextStepIndex,
  referenceScriptUtxo,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: WitnessScriptDecodingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly args: WitnessScriptDecodingScanArgs;
  readonly nextState: WitnessScriptDecodingScanState;
  readonly nextStepIndex: 0 | 1 | 2 | 3;
  readonly referenceScriptUtxo: UTxO;
}) => {
  const stepIndex = 2;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextState } as never,
    (nextStepIndex === 3
      ? WitnessScriptDecodingStep04DatumSchema
      : WitnessScriptDecodingStep03DatumSchema) as never,
  );
  const nextStep = contracts.steps[nextStepIndex];
  const outputMatches = computationThreadOutputPredicate({
    address: nextStep.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${FAMILY} raw step 03`);
    const inputIndex = requireInputIndex(ctx, threadUtxo, FAMILY);
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${FAMILY} raw step 03`,
    );
    return Data.to(
      {
        Continue: [
          { input_index: inputIndex, output_index: outputIndex, ...args },
        ],
      } as never,
      WitnessScriptDecodingStep03RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[stepIndex].spendingScript,
    stepRole: `${FAMILY} raw step 03`,
    nextAddress: nextStep.spendingScriptAddress,
    nextDatum,
    redeemer,
    awaitConfirmation: true,
  });
  if (outputIndex === undefined) throw new Error(`${FAMILY}: raw layout`);
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};

/**
 * Step 04 without the off-chain closure guard: an honest verdict's closed
 * state reaches `terminal_contradiction_v1` and is refused there.
 */
export const submitWitnessScriptDecodingStep04Raw = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  referenceScriptUtxo,
  witnessReferenceScripts,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: WitnessScriptDecodingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
}) => {
  const stepIndex = 3;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  requireLinearFaultStepState<WitnessScriptDecodingScanState>({
    threadUtxo,
    signer,
    schema: WitnessScriptDecodingStep04DatumSchema as never,
    family: FAMILY,
    stepIndex,
  });
  return await submitLinearFaultFinalize({
    lucid,
    family: FAMILY,
    stepIndex,
    step: contracts.steps[stepIndex],
    computationThread: contracts.computationThread,
    fraudProof: contracts.fraudProof,
    signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: WitnessScriptDecodingStep04RedeemerSchema,
    buildFamilyArgs: (layout) => ({
      input_index: layout.inputIndex,
      output_index: layout.outputIndex,
      fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
    }),
    referenceScriptUtxo,
    witnessReferenceScripts,
    awaitConfirmation: true,
  });
};

// ---------------------------------------------------------------------------
// Opening mutations (field 6 openings are `WitnessFieldOpening`)
// ---------------------------------------------------------------------------

const witnessOpeningOf = (opening: FieldOpening) => {
  if (!("WitnessFieldOpening" in opening))
    throw new Error(`${FAMILY}: witness opening expected`);
  return opening.WitnessFieldOpening;
};

/** Patch a certified carriage's reference-input coordinates. */
export const mutateWitnessCertifiedCarriage = (
  opening: FieldOpening,
  patch: (carriage: {
    cert_ref_input_index: bigint;
    chunk_ref_input_indices: bigint[];
  }) => {
    cert_ref_input_index: bigint;
    chunk_ref_input_indices: bigint[];
  },
): FieldOpening => {
  const witness = witnessOpeningOf(opening);
  if (!("Certified" in witness.carriage))
    throw new Error(`${FAMILY}: certified carriage expected`);
  return {
    WitnessFieldOpening: {
      ...witness,
      carriage: {
        Certified: patch({
          cert_ref_input_index: witness.carriage.Certified.cert_ref_input_index,
          chunk_ref_input_indices: [
            ...witness.carriage.Certified.chunk_ref_input_indices,
          ],
        }),
      },
    },
  };
};

/** Point a published (RawUtxo) carriage at a different reference input. */
export const mutateWitnessRawUtxoCarriage = (
  opening: FieldOpening,
  offset: bigint,
): FieldOpening => {
  const witness = witnessOpeningOf(opening);
  if (!("RawUtxo" in witness.carriage))
    throw new Error(`${FAMILY}: raw-utxo carriage expected`);
  return {
    WitnessFieldOpening: {
      ...witness,
      carriage: {
        RawUtxo: {
          ref_input_index: witness.carriage.RawUtxo.ref_input_index + offset,
        },
      },
    },
  };
};

/** Replace the compact transaction bytes the opening is anchored to. */
export const mutateWitnessCompactSource = (
  opening: FieldOpening,
  nativeTxCompactCbor: string,
): FieldOpening => ({
  WitnessFieldOpening: {
    ...witnessOpeningOf(opening),
    native_tx_compact_cbor: nativeTxCompactCbor,
  },
});

/** Replace the compact witness set the opening claims for the anchor. */
export const mutateWitnessSet = (
  opening: FieldOpening,
  witnessSet: NativeTxWitnessSetCompact,
): FieldOpening => ({
  WitnessFieldOpening: {
    ...witnessOpeningOf(opening),
    witness_set: witnessSet,
  },
});
