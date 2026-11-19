import {
  forcedLeafMembershipV1,
  type ForcedTransactionSourceMembershipProof,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  verdictSubjectIsCanonical,
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
} from "../linear-fault-family.js";
import { buildLinearFaultContinue } from "../linear-fault-submit.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { requireInitialStepDatum } from "../step-support.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { MintItemNonCanonicalContracts } from "./contracts.js";
import {
  classifyMintItemFinding,
  mintItemClaimIsUnreachable,
  type MintItemFinding,
  mintItemUnreachableClaimState,
} from "./mint-item-non-canonical.js";
import {
  MintItemStep01RedeemerSchema,
  MintItemStep02DatumSchema,
  MintItemStep04DatumSchema,
} from "./schemas.js";

export const buildMintItemNonCanonicalStep01Forced = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  finding,
  forcedSource,
  referenceScriptUtxo,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MintItemNonCanonicalContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly finding: MintItemFinding;
  readonly forcedSource: Readonly<Record<string, unknown>>;
  readonly referenceScriptUtxo: UTxO;
}) => {
  // No machine step emits `MintItemMalformed`, so a forced rejection with it
  // goes straight to the terminal; every other forced subject scans an item.
  const unreachable = mintItemClaimIsUnreachable(finding.subject);
  if (unreachable && !verdictSubjectIsCanonical(finding.subject))
    throw new Error(
      "mint-item-non-canonical: verdict subject is not canonical",
    );
  const exact = unreachable ? finding : classifyMintItemFinding(finding);
  const nextStepIndex = unreachable ? 3 : 1;
  const stepIndex = 0;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "mint-item-non-canonical",
    stepIndex,
    threadOutRef,
  });
  requireInitialStepDatum({ threadUtxo, signer });
  signer.selectWallet(lucid);
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[0].spendingScriptHash,
    family: "mint-item-non-canonical",
    stepIndex,
  });
  const nextDatum = unreachable
    ? Data.to(
        {
          fraud_prover: signer.paymentKeyHash,
          data: mintItemUnreachableClaimState(exact.subject),
        } as never,
        MintItemStep04DatumSchema as never,
      )
    : Data.to(
        {
          fraud_prover: signer.paymentKeyHash,
          data: {
            subject: exact.subject,
            item_index: BigInt(exact.itemIndex),
            field_cursor: null,
          },
        } as never,
        MintItemStep02DatumSchema as never,
      );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[nextStepIndex].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "mint-item-non-canonical forced step-01",
    );
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      "mint-item-non-canonical",
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "mint-item-non-canonical forced output",
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
            item_index: BigInt(exact.itemIndex),
          },
        ],
      } as never,
      MintItemStep01RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const transaction = await buildLinearFaultContinue({
    lucid,
    signer,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[0].spendingScript,
    stepRole: "mint-item-non-canonical step-01 forced",
    nextAddress: contracts.steps[nextStepIndex].spendingScriptAddress,
    nextDatum,
    redeemer,
  });
  if (outputIndex === undefined)
    throw new Error("mint-item-non-canonical: forced layout unresolved");
  return {
    transaction,
    nextThreadOutRef: `${transaction.txHash}#${outputIndex.toString()}`,
  };
};
