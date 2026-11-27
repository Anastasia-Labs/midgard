import {
  forcedLeafMembershipV1,
  type ForcedTransactionSourceMembershipProof,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk/core";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  type LinearFaultContracts,
  requireLinearFaultReferenceScript,
  requireLinearFaultThreadUtxo,
} from "../../../src/linear-fault-family.js";
import { buildLinearFaultContinue } from "../../../src/linear-fault-submit.js";
import type { ResolvedProverSigner } from "../../../src/runtime.js";
import { computationThreadOutputPredicate } from "../../../src/tx-layout.js";
import { buildAndSubmitTransaction } from "../submit-steps.js";

/**
 * A forced-source step 01 carrying exactly the caller's next datum and
 * family coordinate, with none of the production builder's off-chain
 * classification, so a refusal comes from the step 01 validator.
 */
export const submitRawForcedStep01 = async ({
  lucid,
  family,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  nextDatum,
  forcedSource,
  coordinate,
  redeemerSchema,
  referenceScriptUtxo,
}: {
  readonly lucid: LucidEvolution;
  readonly family: string;
  readonly contracts: LinearFaultContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly nextDatum: string;
  readonly forcedSource: Readonly<Record<string, unknown>>;
  /** The family's coordinate field, e.g. `{ input_index: 1n }`. */
  readonly coordinate: Readonly<Record<string, bigint>>;
  readonly redeemerSchema: unknown;
  readonly referenceScriptUtxo: UTxO;
}) => {
  const stepIndex = 0;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family,
    stepIndex,
    threadOutRef,
  });
  signer.selectWallet(lucid);
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex]!.spendingScriptHash,
    family,
    stepIndex,
  });
  const nextStep = contracts.steps[1]!;
  const outputMatches = computationThreadOutputPredicate({
    address: nextStep.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  const role = `${family} raw forced step-01`;
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, role);
    const inputIndex = requireInputIndex(ctx, threadUtxo, role);
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${role} output`,
    );
    return Data.to(
      {
        Continue: [
          {
            source: {
              ForcedSource: {
                ...forcedSource,
                membership: forcedLeafMembershipV1(
                  forcedSource.membership as ForcedTransactionSourceMembershipProof,
                ),
                input_index: inputIndex,
                output_index: outputIndex,
              },
            },
            ...coordinate,
          },
        ],
      } as never,
      redeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await buildAndSubmitTransaction(buildLinearFaultContinue, {
    lucid,
    signer,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[stepIndex]!.spendingScript,
    stepRole: role,
    nextAddress: nextStep.spendingScriptAddress,
    nextDatum,
    redeemer,
    awaitConfirmation: true,
  });
  if (outputIndex === undefined)
    throw new Error(`${family}: raw forced step-01 layout unresolved`);
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};
