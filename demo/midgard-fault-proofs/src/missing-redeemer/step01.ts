/** `missingRedeemer` step-01: the accepted door authenticates the native
 * transaction's inclusion; the forced door authenticates the forced leaf
 * under the challenged header. Both pay the bound source to `bindEvent`. */
import { computeHash28 } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";

import type { DirectScriptMaterial } from "../direct-script-derivation/material.js";
import {
  requireLinearFaultReferenceScript,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { submitLinearFaultContinue } from "../linear-fault-submit.js";
import { submitNativeTxBinding } from "../native-tx-binding.js";
import type { ResolvedProverSigner } from "../runtime.js";
import {
  requireInitialStepDatum,
  type SubmitStep01TxInclusion,
} from "../step-support.js";
import type { TransitionTraceReconstruction } from "../transition-trace/reconstruct.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import {
  MISSING_REDEEMER_CATEGORY_LABEL,
  type MissingRedeemerContracts,
} from "./contracts.js";

type Step01Common = {
  readonly lucid: LucidEvolution;
  readonly contracts: MissingRedeemerContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly material: DirectScriptMaterial;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
};

const boundSourceDatum = (
  material: DirectScriptMaterial,
  signer: ResolvedProverSigner,
) =>
  Data.to(
    { fraud_prover: signer.paymentKeyHash, data: material.sourceBound },
    SDK.DirectBindEventDatum,
  );

const initialThread = async (common: Step01Common) => {
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid: common.lucid,
    contracts: common.contracts,
    categoryId: common.categoryId,
    family: MISSING_REDEEMER_CATEGORY_LABEL,
    stepIndex: 0,
    threadOutRef: common.threadOutRef,
  });
  requireInitialStepDatum({ threadUtxo, signer: common.signer });
  const headerHash = computeHash28(
    SDK.encodeHeaderCbor(common.material.sourceBound.header),
  ).toString("hex");
  if (threadToken.fraudulentHeaderHash !== headerHash)
    throw new Error(
      `${MISSING_REDEEMER_CATEGORY_LABEL}: thread disputes another header`,
    );
  return { threadUtxo, threadToken };
};

export const submitMissingRedeemerStep01Accepted = async ({
  blueprint,
  network,
  stateQueueBlockOutRef,
  txInclusion,
  witnessReferenceScripts,
  ...common
}: Step01Common & {
  readonly blueprint: unknown;
  readonly network: Network;
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
}) => {
  const { material, signer } = common;
  if (
    material.bound.subject.source_kind !== SDK.PROOF_THREAD_SOURCE_KIND_ACCEPTED
  )
    throw new Error(
      `${MISSING_REDEEMER_CATEGORY_LABEL}: subject is not accepted`,
    );
  if (txInclusion.nativeTxId !== material.bound.subject.transaction_id)
    throw new Error(
      `${MISSING_REDEEMER_CATEGORY_LABEL}: inclusion names another transaction`,
    );
  const { threadUtxo, threadToken } = await initialThread(common);
  return await submitNativeTxBinding({
    lucid: common.lucid,
    blueprint,
    network,
    contracts: common.contracts,
    signer,
    stepIndex: 0,
    threadUtxo,
    threadToken,
    stateQueueBlockOutRef,
    txInclusion,
    nextDatum: boundSourceDatum(material, signer),
    spendRedeemerSchema: SDK.MissingRedeemerStep01Redeemer,
    wrapInclusionCarriage: (inclusion) => ({
      source: { AcceptedSource: { inclusion } },
    }),
    referenceScriptUtxo: common.referenceScriptUtxo,
    witnessReferenceScripts,
    preSubmitBoundary: common.preSubmitBoundary,
    awaitConfirmation: common.awaitConfirmation ?? true,
  });
};

export const submitMissingRedeemerStep01Forced = async ({
  reconstruction,
  ...common
}: Step01Common & {
  /** The challenged block whose forced root carries the leaf. */
  readonly reconstruction: TransitionTraceReconstruction;
}) => {
  const { material, signer, contracts } = common;
  const eventKey = material.eventKey;
  if (
    material.bound.subject.source_kind !==
      SDK.PROOF_THREAD_SOURCE_KIND_FORCED ||
    !("ForcedTransactionEventKey" in eventKey)
  )
    throw new Error(
      `${MISSING_REDEEMER_CATEGORY_LABEL}: subject is not forced`,
    );
  const { threadUtxo, threadToken } = await initialThread(common);
  signer.selectWallet(common.lucid);
  const stepReference = requireLinearFaultReferenceScript({
    utxo: common.referenceScriptUtxo,
    expectedScriptHash: contracts.steps[0].spendingScriptHash,
    family: MISSING_REDEEMER_CATEGORY_LABEL,
    stepIndex: 0,
  });
  const header = material.sourceBound.header;
  const membership = await buildForcedTransactionLeafMembershipProof({
    reconstruction,
    eventKey,
  });
  const next = contracts.steps[1]!;
  const nextDatum = boundSourceDatum(material, signer);
  let outputIndex: bigint | undefined;
  const label = `${MISSING_REDEEMER_CATEGORY_LABEL} forced step-01`;
  const redeemer = ((ctx) => {
    SDK.requireOwnSpendPurpose(ctx, threadUtxo, label);
    const inputIndex = SDK.requireInputIndex(ctx, threadUtxo, label);
    outputIndex = SDK.requireUniqueOutputIndex(
      ctx.outputs,
      computationThreadOutputPredicate({
        address: next.spendingScriptAddress,
        datum: nextDatum,
        unit: threadToken.unit,
      }),
      label,
    );
    return Data.to(
      {
        Continue: [
          {
            source: {
              ForcedSource: {
                input_index: inputIndex,
                output_index: outputIndex,
                header,
                membership,
              },
            },
          },
        ],
      },
      SDK.MissingRedeemerStep01Redeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinue({
    lucid: common.lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[0].spendingScript,
    stepRole: label,
    nextAddress: next.spendingScriptAddress,
    nextDatum,
    redeemer,
    preSubmitBoundary: common.preSubmitBoundary,
    awaitConfirmation: common.awaitConfirmation ?? true,
  });
  if (outputIndex === undefined)
    throw new Error(`${label}: completed output index absent`);
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};
