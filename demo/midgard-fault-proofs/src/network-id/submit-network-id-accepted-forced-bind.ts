import {
  isAnyNetworkIdMismatch,
  NetworkIdForcedStepDatum,
  NetworkIdForcedStepSpendRedeemerSchema,
  NetworkIdStep02Datum,
  type NetworkIdStep02State,
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
import type { PreparedNetworkIdAcceptedForced } from "./accepted-forced.js";
import {
  NETWORK_ID_CATEGORY_LABEL,
  type NetworkIdContracts,
} from "./contracts.js";
import { networkIdSubmitError } from "./submit-common.js";
import { sameFault } from "./submit-network-id-step-02.same-fault.js";

const STEP_LABEL = "network-id accepted forced step";

/**
 * Spends the thread step 01 parked at the forced door with an accepted forced
 * leaf's claim. The door re-derives the header identity, the counted-root
 * membership, the `ForcedTxValid` verdict and the committed body network, and
 * hands step 02 the claim under the forced root key.
 */
export const submitNetworkIdAcceptedForcedBind = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  prepared,
  referenceScriptUtxo,
  unsafeSkipLocalViolationCheckForTest = false,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: NetworkIdContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly prepared: PreparedNetworkIdAcceptedForced;
  /** Published forced-step reference script; mandatory. */
  readonly referenceScriptUtxo: UTxO;
  /** Test-only: submit an honest claim so the on-chain refusal is observed. */
  readonly unsafeSkipLocalViolationCheckForTest?: boolean;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const forcedStep = contracts.forcedStep;
  if (forcedStep === undefined)
    throw networkIdSubmitError("forced binding step is not deployed");
  if (prepared.expectedNetworkId !== contracts.expectedNetworkId)
    throw networkIdSubmitError("forced evidence targets another deployment");
  if (prepared.forcedSource.direction !== 0n)
    throw networkIdSubmitError("accepted forced binding requires direction 0");
  const claimed =
    prepared.faultClaim.kind === "transaction-network"
      ? {
          committedNetworkId: prepared.committedNetworkId,
          outputNetworkIds: [],
        }
      : {
          committedNetworkId: 255n,
          outputNetworkIds: [
            prepared.outputNetworkIds[
              Number(prepared.faultClaim.outputIndex)
            ] ?? prepared.expectedNetworkId,
          ],
        };
  if (
    !unsafeSkipLocalViolationCheckForTest &&
    !isAnyNetworkIdMismatch({
      ...claimed,
      expectedNetworkId: prepared.expectedNetworkId,
    })
  )
    throw networkIdSubmitError(
      "the accepted forced transaction carries no foreign network at the claim",
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
  if (inputDatum.data === null || !sameFault(inputDatum.data, prepared.fault))
    throw networkIdSubmitError(
      "forced-step thread does not carry the prepared accepted claim.",
    );
  if (
    referenceScriptUtxo.scriptRef == null ||
    validatorToScriptHash(referenceScriptUtxo.scriptRef) !==
      forcedStep.spendingScriptHash
  )
    throw networkIdSubmitError(
      `reference script at ${outRefLabel(referenceScriptUtxo)} is not the forced step validator ${forcedStep.spendingScriptHash}.`,
    );
  signer.selectWallet(lucid);
  const step02 = contracts.steps[1];
  const state: NetworkIdStep02State = {
    bad_tx_id: prepared.badTxId,
    committed_tx_network_id: prepared.committedNetworkId,
    expected_network_id: prepared.expectedNetworkId,
    fault: prepared.fault,
    forced_source_key: prepared.subject.source_key,
  };
  const datum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: state },
    NetworkIdStep02Datum,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: step02.spendingScriptAddress,
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
    nextAddress: step02.spendingScriptAddress,
    nextDatum: datum,
    redeemer,
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw networkIdSubmitError("accepted forced binding layout unresolved");
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    state,
  };
};
