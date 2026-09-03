import {
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
} from "../linear-fault-family-v1.js";
import { submitLinearFaultContinue } from "../linear-fault-submit-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { requireInitialStepDatum } from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary-v1.js";
import type { TransactionOutputNonCanonicalContracts } from "./contracts-v1.js";
import {
  TransactionOutputStep01RedeemerSchema,
  TransactionOutputStep02DatumSchema,
} from "./schemas-v1.js";
import {
  classifyTransactionOutputFinding,
  type TransactionOutputFinding,
} from "./transaction-output-non-canonical-v1.js";

export const submitTransactionOutputNonCanonicalStep01Forced = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  finding,
  forcedSource,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: TransactionOutputNonCanonicalContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly finding: TransactionOutputFinding;
  readonly forcedSource: Readonly<Record<string, unknown>>;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const exact = classifyTransactionOutputFinding(finding);
  const stepIndex = 0;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "transaction-output-non-canonical",
    stepIndex,
    threadOutRef,
  });
  requireInitialStepDatum({ threadUtxo, signer });
  signer.selectWallet(lucid);
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[0].spendingScriptHash,
    family: "transaction-output-non-canonical",
    stepIndex,
  });
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        subject: exact.subject,
        output_index: BigInt(exact.itemIndex),
      },
    } as never,
    TransactionOutputStep02DatumSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[1].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "transaction-output-non-canonical forced step-01",
    );
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      "transaction-output-non-canonical",
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "transaction-output-non-canonical forced output",
    );
    return Data.to(
      {
        Continue: [
          {
            source: {
              ForcedSource: {
                ...forcedSource,
                input_index: inputIndex,
                output_index: outputIndex,
              },
            },
            output_index: BigInt(exact.itemIndex),
          },
        ],
      } as never,
      TransactionOutputStep01RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[0].spendingScript,
    stepRole: "transaction-output-non-canonical step-01 forced",
    nextAddress: contracts.steps[1].spendingScriptAddress,
    nextDatum,
    redeemer,
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error(
      "transaction-output-non-canonical: forced layout unresolved",
    );
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};
