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

import { submitLinearFaultContinue } from "../linear-fault-submit.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { requireInitialStepDatum } from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { NetworkIdContracts } from "./contracts.js";
import {
  requireNetworkIdReferenceScript,
  requireNetworkIdThreadUtxo,
} from "./submit-common.js";
import type { PreparedNetworkIdWrongfulRejection } from "./wrongful-rejection.js";

export const submitNetworkIdForcedStep01 = async ({
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
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  if (prepared.expectedNetworkId !== contracts.expectedNetworkId)
    throw new Error("networkId: forced evidence targets another deployment");
  const { threadUtxo, threadToken } = await requireNetworkIdThreadUtxo({
    lucid,
    contracts,
    categoryId,
    stepIndex: 0,
    threadOutRef,
  });
  requireInitialStepDatum({ threadUtxo, signer });
  signer.selectWallet(lucid);
  const stepReference = requireNetworkIdReferenceScript({
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
  const txHash = await submitLinearFaultContinue({
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
