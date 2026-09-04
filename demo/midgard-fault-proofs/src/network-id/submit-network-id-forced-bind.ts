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

import { submitLinearFaultContinue } from "../linear-fault-submit.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { NetworkIdContracts } from "./contracts.js";
import type { PreparedNetworkIdWrongfulRejection } from "./wrongful-rejection.js";

export const submitNetworkIdForcedBind = async ({
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
  readonly contracts: NetworkIdContracts;
  readonly signer: ResolvedProverSigner;
  readonly threadUtxo: UTxO;
  readonly threadUnit: string;
  readonly prepared: PreparedNetworkIdWrongfulRejection;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
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
  const txHash = await submitLinearFaultContinue({
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
