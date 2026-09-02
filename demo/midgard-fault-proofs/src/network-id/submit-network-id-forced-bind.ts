import {
  NetworkIdForcedStepSpendRedeemerSchema,
  NetworkIdStep02DatumSchema,
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
} from "@lucid-evolution/lucid";

import { submitLinearFaultContinueV1 } from "../linear-fault-submit-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { NetworkIdContractsV1 } from "./contracts-v1.js";
import type { PreparedNetworkIdWrongfulRejectionV1 } from "./wrongful-rejection-v1.js";

export const submitNetworkIdForcedBindV1 = async ({
  lucid,
  contracts,
  signer,
  threadUtxo,
  threadUnit,
  prepared,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: NetworkIdContractsV1;
  readonly signer: ResolvedProverSigner;
  readonly threadUtxo: UTxO;
  readonly threadUnit: string;
  readonly prepared: PreparedNetworkIdWrongfulRejectionV1;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  const forcedStep = contracts.forcedStep;
  if (forcedStep === undefined)
    throw new Error("networkId: forced binding step is not deployed");
  if (
    referenceScriptUtxo.scriptRef?.script !== forcedStep.spendingScript.script
  )
    throw new Error("networkId: forced binding reference script changed");
  const state: NetworkIdStep02State = {
    bad_tx_id: prepared.badTxId,
    committed_tx_network_id: prepared.evidence.committedNetworkId,
    expected_network_id: prepared.expectedNetworkId,
    fault: "ForcedNetworkIdMismatch",
    post_utxo: null,
    forced_source_key: prepared.subject.source_key,
  };
  const datum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: state } as never,
    NetworkIdStep02DatumSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[1].spendingScriptAddress,
    datum,
    unit: threadUnit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "network-id forced bind");
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "network-id forced bind output",
    );
    return Data.to(
      {
        Continue: [
          {
            input_index: requireInputIndex(
              ctx,
              threadUtxo,
              "network-id forced bind",
            ),
            output_index: outputIndex,
            ...prepared.forcedSource,
          },
        ],
      } as never,
      NetworkIdForcedStepSpendRedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinueV1({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit,
    stepReference: referenceScriptUtxo,
    stepScript: forcedStep.spendingScript,
    stepRole: "network-id forced binding step",
    nextAddress: contracts.steps[1].spendingScriptAddress,
    nextDatum: datum,
    redeemer,
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error("networkId: forced binding layout unresolved");
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    state,
  };
};
