import {
  NetworkIdForcedScanDatum,
  NetworkIdForcedScanSpendRedeemerSchema,
  NetworkIdStep02DatumSchema,
  type NetworkIdStep02State,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import {
  faultProofFieldOpening,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.js";
import { submitLinearFaultContinue } from "../linear-fault-submit.js";
import { fetchUtxoByOutRef, outRefLabel, parseOutRef } from "../runtime.js";
import { requireComputationThreadToken } from "../step-support.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import { NETWORK_ID_CATEGORY_LABEL } from "./contracts.js";
import {
  encodeNetworkIdForcedScanGrammarCheckpoint,
  encodeNetworkIdForcedScanWalkCheckpoint,
  networkIdForcedScanPriorGrammar,
  networkIdForcedScanPriorWalk,
  type NetworkIdForcedScanStep,
} from "./forced-scan-plan.js";
import { networkIdSubmitError } from "./submit-common.js";
import {
  requireForcedScanContract,
  requireScanState,
  STEP_LABEL,
  type SubmitNetworkIdForcedScanParams,
  type SubmitNetworkIdForcedScanResult,
  successorScanState,
} from "./submit-network-id-forced-scan.require-scan-state.js";
import { networkIdWrongfulRejectionCloses } from "./wrongful-rejection.js";

/**
 * Submits exactly one planned `forced_scan` action. The successor address and
 * datum are the validator's own for every action but the completing
 * `Advance`, which writes step 02's terminal state.
 */
export const submitNetworkIdForcedScanAction = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  prepared,
  outputsOpeningPlan,
  scan,
  step,
  referenceScriptUtxo,
  carriageUtxos,
  certificateUtxos,
  network,
  preSubmitBoundary,
  awaitConfirmation = true,
}: SubmitNetworkIdForcedScanParams & {
  readonly step: NetworkIdForcedScanStep;
}): Promise<SubmitNetworkIdForcedScanResult> => {
  const forcedScan = requireForcedScanContract(contracts);
  if (prepared.expectedNetworkId !== contracts.expectedNetworkId) {
    throw networkIdSubmitError("forced evidence targets another deployment");
  }
  if (!networkIdWrongfulRejectionCloses(prepared.evidence)) {
    throw networkIdSubmitError(
      "retained evidence does not contradict NetworkIdMismatch; the rejection was honest",
    );
  }
  if (
    outputsOpeningPlan.fieldIndex !== 2 ||
    outputsOpeningPlan.nativeTxId !== prepared.badTxId ||
    outputsOpeningPlan.nativeTxCompactCbor !== prepared.nativeTxCompactCbor ||
    outputsOpeningPlan.itemCount !== prepared.outputsItemCbors.length
  ) {
    throw networkIdSubmitError(
      "forced outputs opening is not field 2 of the authenticated forced transaction",
    );
  }
  if (scan.outputCount !== outputsOpeningPlan.itemCount) {
    throw networkIdSubmitError(
      "forced scan plan was planned against a different outputs field",
    );
  }
  if (step.kind === "open" && scan.requiresGrammar) {
    throw networkIdSubmitError(
      "certified carriage cannot be opened directly; its item count is provisional until the grammar terminal",
    );
  }
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: `${STEP_LABEL} computation-thread UTxO`,
  });
  if (threadUtxo.address !== forcedScan.spendingScriptAddress) {
    throw networkIdSubmitError(
      `thread UTxO ${outRefLabel(threadUtxo)} is not locked at the forced outputs scan.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId,
    categoryLabel: NETWORK_ID_CATEGORY_LABEL,
  });
  if (threadToken.fraudulentHeaderHash !== prepared.headerHash) {
    throw networkIdSubmitError(
      `thread ${outRefLabel(threadUtxo)} disputes header ${threadToken.fraudulentHeaderHash}, not the prepared ${prepared.headerHash}.`,
    );
  }
  if (threadUtxo.datum == null) {
    throw networkIdSubmitError(
      `thread UTxO ${outRefLabel(threadUtxo)} has no inline datum.`,
    );
  }
  const inputDatum = Data.from(threadUtxo.datum, NetworkIdForcedScanDatum);
  if (inputDatum.fraud_prover !== signer.paymentKeyHash) {
    throw networkIdSubmitError(
      `forced-scan thread names fraud prover ${inputDatum.fraud_prover}, not signing wallet ${signer.paymentKeyHash}.`,
    );
  }
  if (inputDatum.data === null) {
    throw networkIdSubmitError("forced-scan thread carries an empty state.");
  }
  requireScanState({ state: inputDatum.data, step, scan, prepared });
  if (
    referenceScriptUtxo.scriptRef == null ||
    validatorToScriptHash(referenceScriptUtxo.scriptRef) !==
      forcedScan.spendingScriptHash
  ) {
    throw networkIdSubmitError(
      `reference script at ${outRefLabel(referenceScriptUtxo)} is not the forced scan validator ${forcedScan.spendingScriptHash}.`,
    );
  }
  signer.selectWallet(lucid);
  const carriage =
    carriageUtxos ??
    (outputsOpeningPlan.plan.publications.length === 0
      ? []
      : await resolveFaultProofFieldCarriagePublications({
          lucid,
          publisherAddress: signer.address,
          planned: outputsOpeningPlan,
        }));
  if (carriage === undefined) {
    throw networkIdSubmitError(
      "forced outputs field carriage is not observable at the publisher address",
    );
  }
  const resolvedNetwork = network ?? lucid.config().network;
  const certificates =
    certificateUtxos ??
    (outputsOpeningPlan.plan.tier !== "Certified"
      ? []
      : await (async () => {
          if (resolvedNetwork === undefined) {
            throw networkIdSubmitError(
              "certified forced outputs carriage requires the Cardano network to resolve its §8.6 certificate",
            );
          }
          const certificate = await resolveFaultProofFieldPreimageCertificate({
            lucid,
            network: resolvedNetwork,
            planned: outputsOpeningPlan,
            certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
          });
          if (certificate === undefined) {
            throw networkIdSubmitError(
              "certified forced outputs carriage has no observable §8.6 certificate",
            );
          }
          return [certificate];
        })());
  const nextScanState = successorScanState({ scan, step, prepared });
  const step02State: NetworkIdStep02State | null =
    nextScanState === null
      ? {
          bad_tx_id: prepared.badTxId,
          committed_tx_network_id: prepared.evidence.committedNetworkId,
          expected_network_id: prepared.expectedNetworkId,
          fault: "ForcedNetworkIdMismatch",
          post_utxo: null,
          forced_source_key: prepared.subject.source_key,
        }
      : null;
  const nextDatum =
    step02State === null
      ? Data.to(
          { fraud_prover: signer.paymentKeyHash, data: nextScanState } as never,
          NetworkIdForcedScanDatum as never,
        )
      : Data.to(
          { fraud_prover: signer.paymentKeyHash, data: step02State } as never,
          NetworkIdStep02DatumSchema as never,
        );
  const nextStep = step02State === null ? forcedScan : contracts.steps[1];
  const outputMatches = computationThreadOutputPredicate({
    address: nextStep.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const referenceInputs = [...carriage, referenceScriptUtxo, ...certificates];
  const opening = faultProofFieldOpening({
    planned: outputsOpeningPlan,
    referenceInputs,
    certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
    label: `${STEP_LABEL} outputs`,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, STEP_LABEL);
    const inputIndex = requireInputIndex(ctx, threadUtxo, STEP_LABEL);
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${STEP_LABEL} ${step.kind} output`,
    );
    const common = {
      input_index: inputIndex,
      output_index: outputIndex,
      opening,
    };
    const action =
      step.kind === "open"
        ? { Open: common }
        : step.kind === "startGrammar"
          ? { StartGrammar: { ...common, item_budget: step.itemBudget } }
          : step.kind === "resumeGrammar"
            ? {
                ResumeGrammar: {
                  ...common,
                  checkpoint_bytes: encodeNetworkIdForcedScanGrammarCheckpoint(
                    networkIdForcedScanPriorGrammar(scan, step),
                  ).toString("hex"),
                  item_budget: step.itemBudget,
                },
              }
            : step.kind === "finishGrammar"
              ? {
                  FinishGrammar: {
                    ...common,
                    checkpoint_bytes:
                      encodeNetworkIdForcedScanGrammarCheckpoint(
                        networkIdForcedScanPriorGrammar(scan, step),
                      ).toString("hex"),
                  },
                }
              : {
                  Advance: {
                    ...common,
                    checkpoint_bytes: encodeNetworkIdForcedScanWalkCheckpoint(
                      networkIdForcedScanPriorWalk(scan, step.ordinal),
                    ).toString("hex"),
                    item_budget: step.itemBudget,
                  },
                };
    return Data.to(
      { Continue: [action] } as never,
      NetworkIdForcedScanSpendRedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference: referenceScriptUtxo,
    stepScript: forcedScan.spendingScript,
    stepRole: `${STEP_LABEL} ${step.kind}`,
    nextAddress: nextStep.spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos: carriage,
    extraReferenceInputs: certificates,
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined) {
    throw networkIdSubmitError(`forced scan ${step.kind} layout unresolved`);
  }
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    step,
    nextScanState,
    step02State,
  };
};

const actionSubmitter =
  (kind: NetworkIdForcedScanStep["kind"]) =>
  async (
    params: SubmitNetworkIdForcedScanParams & {
      readonly step: NetworkIdForcedScanStep;
    },
  ): Promise<SubmitNetworkIdForcedScanResult> => {
    if (params.step.kind !== kind) {
      throw networkIdSubmitError(
        `forced scan ${kind} builder was handed a planned ${params.step.kind} action`,
      );
    }
    return await submitNetworkIdForcedScanAction(params);
  };

/** Tier-1/2 whole-view opening: `Ready` becomes `Scanning` at item zero. */
export const submitNetworkIdForcedScanOpen = actionSubmitter("open");

/** Tier-3 envelope certification: `Ready` becomes `Grammar`. */
export const submitNetworkIdForcedScanStartGrammar =
  actionSubmitter("startGrammar");

/** One further certified envelope batch; `Grammar` stays `Grammar`. */
export const submitNetworkIdForcedScanResumeGrammar =
  actionSubmitter("resumeGrammar");

/** Terminal certification: `Grammar` becomes `Scanning` at item zero. */
export const submitNetworkIdForcedScanFinishGrammar =
  actionSubmitter("finishGrammar");

/** One semantic batch; the completing one writes step 02's terminal state. */
export const submitNetworkIdForcedScanAdvance = actionSubmitter("advance");

export type DriveNetworkIdForcedScanResult = {
  readonly results: readonly SubmitNetworkIdForcedScanResult[];
  /** Out-ref of the step-02 thread the completing batch produced. */
  readonly step02ThreadOutRef: string;
  readonly step02State: NetworkIdStep02State;
};
