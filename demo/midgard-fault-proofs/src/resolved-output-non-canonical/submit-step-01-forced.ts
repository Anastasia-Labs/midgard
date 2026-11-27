import { forcedLeafMembershipV1 } from "@al-ft/midgard-sdk/core";
import {
  type ForcedInclusionTxV1,
  type Header,
  type OutputReference,
  RejectionReason,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  type RootMembershipProof,
} from "@al-ft/midgard-sdk/core";
import { proofThreadDirectionOfForcedLeaf } from "@al-ft/midgard-sdk/fraud-proof";
import { PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION } from "@al-ft/midgard-sdk/fraud-proof";
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
import type { ResolvedOutputNonCanonicalContracts } from "./contracts.js";
import {
  classifyResolvedOutputNonCanonicalFinding,
  type ResolvedOutputAbsentInputEvidence,
  type ResolvedOutputEvidence,
  resolvedOutputEvidenceIsAbsent,
} from "./resolved-output-non-canonical.js";
import {
  ResolvedOutputStep01RedeemerSchema,
  ResolvedOutputStep02DatumSchema,
} from "./schemas.js";

/**
 * The forced leaf as step 01 authenticates it: the challenged header, the
 * leaf's membership under its counted forced-transactions root, and the
 * direction the subject claims (always wrongful rejection for this family's
 * forced door, since an accepted forced transaction carries no typed reason).
 */
export type ResolvedOutputForcedSource = Readonly<{
  header: Header;
  membership: RootMembershipProof<OutputReference, ForcedInclusionTxV1>;
}>;

/**
 * Off-chain pre-check of the forced door. The validator repeats every
 * binding; this only refuses to sign a transaction the chain would refuse.
 */
export const requireResolvedOutputForcedSource = (
  finding: ResolvedOutputEvidence | ResolvedOutputAbsentInputEvidence,
  source: ResolvedOutputForcedSource,
): ResolvedOutputForcedSource => {
  const fail = (message: string): never => {
    throw new Error(`resolved-output-non-canonical: ${message}`);
  };
  if (
    finding.subject.direction !== PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION ||
    proofThreadDirectionOfForcedLeaf(source.membership.value) !==
      PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION
  )
    return fail("forced source direction is not a wrongful rejection");
  const leaf = source.membership.value;
  if (leaf.tx_id !== finding.subject.transaction_id)
    return fail("forced leaf transaction differs from the subject");
  if (leaf.verdict === "ForcedTxValid")
    return fail("forced leaf carries no typed rejection reason");
  if (
    finding.subject.rejection_reason === null ||
    Data.to(leaf.verdict.ForcedTxInvalid.reason, RejectionReason) !==
      Data.to(finding.subject.rejection_reason, RejectionReason)
  )
    return fail("forced leaf reason differs from the bound subject");
  if (source.header.prevUtxosRoot !== finding.resolved.priorRoot)
    return fail("challenged header prior root differs from the evidence");
  return source;
};

export const buildResolvedOutputNonCanonicalStep01Forced = async ({
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
  readonly contracts: ResolvedOutputNonCanonicalContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /** Absent-input evidence binds a coordinate that names no input. */
  readonly finding: ResolvedOutputEvidence | ResolvedOutputAbsentInputEvidence;
  readonly forcedSource: ResolvedOutputForcedSource;
  readonly referenceScriptUtxo: UTxO;
}) => {
  // Absent-input evidence was classified when it was prepared; its
  // coordinate may name no input field, which no present finding binds.
  if (!resolvedOutputEvidenceIsAbsent(finding))
    classifyResolvedOutputNonCanonicalFinding(finding);
  requireResolvedOutputForcedSource(finding, forcedSource);
  const stepIndex = 0;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "resolved-output-non-canonical",
    stepIndex,
    threadOutRef,
  });
  requireInitialStepDatum({ threadUtxo, signer });
  signer.selectWallet(lucid);
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[0].spendingScriptHash,
    family: "resolved-output-non-canonical",
    stepIndex,
  });
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        subject: finding.subject,
        source_kind: BigInt(finding.coordinate.sourceKind),
        input_index: BigInt(finding.coordinate.inputIndex),
        prior_root: finding.resolved.priorRoot,
      },
    } as never,
    ResolvedOutputStep02DatumSchema as never,
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
      "resolved-output-non-canonical forced step-01",
    );
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      "resolved-output-non-canonical",
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "resolved-output-non-canonical forced output",
    );
    return Data.to(
      {
        Continue: [
          {
            source: {
              ForcedSource: {
                ...forcedSource,
                membership: forcedLeafMembershipV1(forcedSource.membership),
                input_index: inputIndex,
                output_index: outputIndex,
              },
            },
            source_kind: BigInt(finding.coordinate.sourceKind),
            input_index: BigInt(finding.coordinate.inputIndex),
          },
        ],
      } as never,
      ResolvedOutputStep01RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const transaction = await buildLinearFaultContinue({
    lucid,
    signer,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[0].spendingScript,
    stepRole: "resolved-output-non-canonical step-01 forced",
    nextAddress: contracts.steps[1].spendingScriptAddress,
    nextDatum,
    redeemer,
  });
  if (outputIndex === undefined)
    throw new Error("resolved-output-non-canonical: forced layout unresolved");
  return {
    transaction,
    nextThreadOutRef: `${transaction.txHash}#${outputIndex.toString()}`,
  };
};
