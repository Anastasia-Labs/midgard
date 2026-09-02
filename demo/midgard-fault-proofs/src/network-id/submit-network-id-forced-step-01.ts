import {
  FraudProofComputationThreadStepDatum,
  NetworkIdStep01SpendRedeemerSchema,
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
import { requireInitialStepDatum } from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import type { NetworkIdContractsV1 } from "./contracts-v1.js";
import {
  requireNetworkIdReferenceScriptV1,
  requireNetworkIdThreadUtxoV1,
} from "./submit-common-v1.js";
import type { PreparedNetworkIdWrongfulRejectionV1 } from "./wrongful-rejection-v1.js";

export const submitNetworkIdForcedStep01V1 = async ({
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
  readonly contracts: NetworkIdContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly prepared: PreparedNetworkIdWrongfulRejectionV1;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  if (prepared.expectedNetworkId !== contracts.expectedNetworkId)
    throw new Error("networkId: forced evidence targets another deployment");
  const { threadUtxo, threadToken } = await requireNetworkIdThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    stepIndex: 0,
    threadOutRef,
  });
  requireInitialStepDatum({ threadUtxo, signer });
  signer.selectWallet(lucid);
  const stepReference = requireNetworkIdReferenceScriptV1({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[0].spendingScriptHash,
    stepIndex: 0,
  });
  const forcedStep = contracts.forcedStep;
  if (forcedStep === undefined)
    throw new Error("networkId: forced binding step is not deployed");
  const datum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: null },
    FraudProofComputationThreadStepDatum,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: forcedStep.spendingScriptAddress,
    datum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "network-id forced step-01");
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "network-id forced step-01 output",
    );
    return Data.to(
      {
        Continue: [
          {
            tx_inclusion: null,
            post_utxo_membership: null,
            forced_source: {
              input_index: requireInputIndex(
                ctx,
                threadUtxo,
                "network-id forced step-01",
              ),
              output_index: outputIndex,
            },
            fault: "ForcedNetworkIdMismatch" as const,
          },
        ],
      } as never,
      NetworkIdStep01SpendRedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinueV1({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[0].spendingScript,
    stepRole: "network-id step-01 forced",
    nextAddress: forcedStep.spendingScriptAddress,
    nextDatum: datum,
    redeemer,
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error("networkId: forced step-01 layout unresolved");
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
  };
};
