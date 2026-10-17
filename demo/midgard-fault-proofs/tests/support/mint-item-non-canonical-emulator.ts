import {
  type FieldOpening,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  requireLinearFaultReferenceScript,
  requireLinearFaultThreadUtxo,
} from "../../src/linear-fault-family.js";
import { submitLinearFaultFinalize } from "../../src/linear-fault-finalize.js";
import { submitLinearFaultContinue } from "../../src/linear-fault-submit.js";
import type { MintItemNonCanonicalContracts } from "../../src/mint-item-non-canonical/contracts.js";
import {
  type MintItemEvidence,
  mintItemScanControlData,
} from "../../src/mint-item-non-canonical/mint-item-non-canonical.js";
import {
  MintItemStep02DatumSchema,
  MintItemStep02RedeemerSchema,
  MintItemStep03DatumSchema,
  MintItemStep03RedeemerSchema,
  MintItemStep04RedeemerSchema,
} from "../../src/mint-item-non-canonical/schemas.js";
import type { ResolvedProverSigner } from "../../src/runtime.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";
import type { FaultProofWitnessReferenceScripts } from "../../src/witness-reference-scripts.js";

const FAMILY = "mint-item-non-canonical";
export type Common = Readonly<{
  lucid: LucidEvolution;
  contracts: MintItemNonCanonicalContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  referenceScriptUtxo: UTxO;
}>;

/**
 * A raw continuation: the exact datum, redeemer and successor the test asks
 * for reach the validator, so every substitution is refused on chain rather
 * than by an off-chain builder guard.
 */
const continueRaw = async ({
  common,
  stepIndex,
  nextAddress,
  nextDatum,
  redeemerSchema,
  args,
  carriageUtxos = [],
  extraReferenceInputs = [],
}: {
  readonly common: Common;
  readonly stepIndex: number;
  readonly nextAddress: string;
  readonly nextDatum: string;
  readonly redeemerSchema: unknown;
  readonly args: (
    inputIndex: bigint,
    outputIndex: bigint,
  ) => Record<string, unknown>;
  readonly carriageUtxos?: readonly UTxO[];
  readonly extraReferenceInputs?: readonly UTxO[];
}) => {
  const { lucid, contracts, categoryId, signer, threadOutRef } = common;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef,
  });
  const role = `raw step ${(stepIndex + 1).toString().padStart(2, "0")}`;
  const stepReference = requireLinearFaultReferenceScript({
    utxo: common.referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex]!.spendingScriptHash,
    family: FAMILY,
    stepIndex,
  });
  const outputMatches = computationThreadOutputPredicate({
    address: nextAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, role);
    const inputIndex = requireInputIndex(ctx, threadUtxo, role);
    outputIndex = requireUniqueOutputIndex(ctx.outputs, outputMatches, role);
    return Data.to(
      { Continue: [args(inputIndex, outputIndex)] } as never,
      redeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  signer.selectWallet(lucid);
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[stepIndex]!.spendingScript,
    stepRole: role,
    nextAddress,
    nextDatum,
    redeemer,
    carriageUtxos,
    extraReferenceInputs,
    awaitConfirmation: true,
  });
  if (outputIndex === undefined) throw new Error(`${role}: no layout`);
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};

const datumOf = (common: Common, data: unknown, schema: unknown) =>
  Data.to(
    { fraud_prover: common.signer.paymentKeyHash, data } as never,
    schema as never,
  );

export type MintScanStateData = ReturnType<typeof scanStateOf>;

/** The step-03/04 thread state at trace position `controlIndex` with the given outcome. */
export const scanStateOf = (
  evidence: MintItemEvidence,
  controlIndex: number,
  outcome: bigint,
) => ({
  subject: evidence.subject,
  item_index: BigInt(evidence.itemIndex),
  item_length: BigInt(evidence.itemLength),
  item_hash: evidence.itemHash,
  chunk_hashes: evidence.chunkHashes,
  control: mintItemScanControlData(evidence.scanControls[controlIndex]!),
  outcome,
});

export const submitMintStep02Raw = async ({
  opening,
  nextState,
  nextStepIndex = 2,
  carriageUtxos,
  extraReferenceInputs,
  ...common
}: Common & {
  readonly opening: FieldOpening;
  readonly nextState: unknown;
  readonly nextStepIndex?: 1 | 2 | 3;
  readonly carriageUtxos?: readonly UTxO[];
  readonly extraReferenceInputs?: readonly UTxO[];
}) =>
  await continueRaw({
    common,
    stepIndex: 1,
    nextAddress: common.contracts.steps[nextStepIndex].spendingScriptAddress,
    nextDatum: datumOf(
      common,
      nextState,
      nextStepIndex === 1
        ? MintItemStep02DatumSchema
        : MintItemStep03DatumSchema,
    ),
    redeemerSchema: MintItemStep02RedeemerSchema,
    args: (input_index, output_index) => ({
      input_index,
      output_index,
      opening,
    }),
    carriageUtxos,
    extraReferenceInputs,
  });

/** Step 03 with the window, successor and next checkpoint supplied verbatim. */
export const submitMintStep03Raw = async ({
  window,
  nextState,
  nextStepIndex,
  ...common
}: Common & {
  readonly window: Buffer;
  readonly nextState: MintScanStateData;
  /** 2 keeps the scan self-loop, 3 hands over to step 04. */
  readonly nextStepIndex: 2 | 3;
}) =>
  await continueRaw({
    common,
    stepIndex: 2,
    nextAddress: common.contracts.steps[nextStepIndex].spendingScriptAddress,
    nextDatum: datumOf(common, nextState, MintItemStep03DatumSchema),
    redeemerSchema: MintItemStep03RedeemerSchema,
    args: (input_index, output_index) => ({
      input_index,
      output_index,
      window: window.toString("hex"),
    }),
  });

/** Step 04 without the off-chain contradiction guard, so an honest terminal reaches the validator. */
export const submitMintStep04Raw = async ({
  witnessReferenceScripts,
  ...common
}: Common & {
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
}) => {
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid: common.lucid,
    contracts: common.contracts,
    categoryId: common.categoryId,
    family: FAMILY,
    stepIndex: 3,
    threadOutRef: common.threadOutRef,
  });
  return await submitLinearFaultFinalize({
    lucid: common.lucid,
    family: FAMILY,
    stepIndex: 3,
    step: common.contracts.steps[3],
    computationThread: common.contracts.computationThread,
    fraudProof: common.contracts.fraudProof,
    signer: common.signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: MintItemStep04RedeemerSchema,
    buildFamilyArgs: ({
      inputIndex,
      outputIndex,
      fraudProofMintRedeemerIndex,
    }) => ({
      input_index: inputIndex,
      output_index: outputIndex,
      fraud_proof_mint_redeemer_index: fraudProofMintRedeemerIndex,
    }),
    referenceScriptUtxo: common.referenceScriptUtxo,
    witnessReferenceScripts,
    awaitConfirmation: true,
  });
};
