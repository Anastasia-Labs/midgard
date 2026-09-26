import {
  NetworkIdForcedScanDatumSchema,
  type NetworkIdForcedScanState,
  NetworkIdForcedStepDatum,
  NetworkIdForcedStepSpendRedeemerSchema,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import { submitLinearFaultContinue } from "../linear-fault-submit.js";
import {
  fetchUtxoByOutRef,
  outRefLabel,
  parseOutRef,
  type ResolvedProverSigner,
} from "../runtime.js";
import { requireComputationThreadToken } from "../step-support.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import {
  NETWORK_ID_CATEGORY_LABEL,
  type NetworkIdContracts,
} from "./contracts.js";
import { networkIdSubmitError } from "./submit-common.js";
import {
  networkIdWrongfulRejectionCloses,
  type PreparedNetworkIdWrongfulRejection,
} from "./wrongful-rejection.js";

const STEP_LABEL = "network-id forced step";

/**
 * Spends the thread parked at the forced door by step 01 and binds the
 * authenticated forced leaf: header identity, counted-root membership, the
 * exact `NetworkIdMismatch` verdict, and the transaction's own network id are
 * all re-derived on chain, so the state that reaches the scan carries nothing
 * the prover asserted.
 *
 * The bind is now the scan's opening move rather than step 02's: it hands the
 * thread to `forced_scan` in the `Ready` state, because the body's own network
 * id alone does not close a wrongful rejection — every output still has to be
 * walked before the mint may be claimed.
 */
export const submitNetworkIdForcedBind = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  prepared,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: NetworkIdContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly prepared: PreparedNetworkIdWrongfulRejection;
  /** Published forced-step reference script; mandatory. */
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const forcedStep = contracts.forcedStep;
  if (forcedStep === undefined)
    throw networkIdSubmitError("forced binding step is not deployed");
  if (prepared.expectedNetworkId !== contracts.expectedNetworkId)
    throw networkIdSubmitError("forced evidence targets another deployment");
  if (prepared.forcedSource.direction !== 1n)
    throw networkIdSubmitError("forced binding requires direction 1");
  if (!networkIdWrongfulRejectionCloses(prepared.evidence))
    throw networkIdSubmitError(
      "retained evidence does not contradict NetworkIdMismatch; the rejection was honest",
    );
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: `${STEP_LABEL} computation-thread UTxO`,
  });
  if (threadUtxo.address !== forcedStep.spendingScriptAddress)
    throw networkIdSubmitError(
      `thread UTxO ${outRefLabel(threadUtxo)} is not locked at the forced step.`,
    );
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId,
    categoryLabel: NETWORK_ID_CATEGORY_LABEL,
  });
  if (threadToken.fraudulentHeaderHash !== prepared.headerHash)
    throw networkIdSubmitError(
      `thread ${outRefLabel(threadUtxo)} disputes header ${threadToken.fraudulentHeaderHash}, not the prepared ${prepared.headerHash}.`,
    );
  if (threadUtxo.datum == null)
    throw networkIdSubmitError(
      `thread UTxO ${outRefLabel(threadUtxo)} has no inline datum.`,
    );
  const inputDatum = Data.from(threadUtxo.datum, NetworkIdForcedStepDatum);
  if (inputDatum.fraud_prover !== signer.paymentKeyHash)
    throw networkIdSubmitError(
      `forced-step thread names fraud prover ${inputDatum.fraud_prover}, not signing wallet ${signer.paymentKeyHash}.`,
    );
  if (inputDatum.data !== "ForcedNetworkIdMismatch")
    throw networkIdSubmitError(
      "forced-step thread does not carry the ForcedNetworkIdMismatch handoff marker.",
    );
  if (
    referenceScriptUtxo.scriptRef == null ||
    validatorToScriptHash(referenceScriptUtxo.scriptRef) !==
      forcedStep.spendingScriptHash
  )
    throw networkIdSubmitError(
      `reference script at ${outRefLabel(referenceScriptUtxo)} is not the forced step validator ${forcedStep.spendingScriptHash}.`,
    );
  const forcedScan = contracts.forcedScan;
  if (forcedScan === undefined)
    throw networkIdSubmitError(
      "forced outputs scan is not deployed; the forced binding has nowhere to hand the thread",
    );
  signer.selectWallet(lucid);
  const state: NetworkIdForcedScanState = {
    Ready: {
      bound: {
        bad_tx_id: prepared.badTxId,
        committed_tx_network_id: prepared.evidence.committedNetworkId,
        expected_network_id: prepared.expectedNetworkId,
        forced_source_key: prepared.subject.source_key,
      },
    },
  };
  const datum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: state } as never,
    NetworkIdForcedScanDatumSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: forcedScan.spendingScriptAddress,
    datum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, STEP_LABEL);
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${STEP_LABEL} output`,
    );
    return Data.to(
      {
        Continue: [
          {
            input_index: requireInputIndex(ctx, threadUtxo, STEP_LABEL),
            output_index: outputIndex,
            ...prepared.forcedSource,
          },
        ],
      } as never,
      NetworkIdForcedStepSpendRedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference: referenceScriptUtxo,
    stepScript: forcedStep.spendingScript,
    stepRole: STEP_LABEL,
    nextAddress: forcedScan.spendingScriptAddress,
    nextDatum: datum,
    redeemer,
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw networkIdSubmitError("forced binding layout unresolved");
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    state,
  };
};
