import { decodeMidgardFieldPreimage } from "@al-ft/midgard-core";
import {
  type FieldOpening,
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
  faultProofFieldOpening,
  planFaultProofFieldOpening,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening-v1.js";
import {
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family-v1.js";
import { submitLinearFaultContinue } from "../linear-fault-submit-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary-v1.js";
import type { MintDeclaredAssetLimitContracts } from "./contracts-v1.js";
import type { MintDeclaredAssetLimitEvidence } from "./family-v1.js";
import {
  MintDeclaredAssetLimitStep02DatumSchema,
  MintDeclaredAssetLimitStep02RedeemerSchema,
  MintDeclaredAssetLimitStep03DatumSchema,
} from "./schemas-v1.js";
import {
  encodeMintDeclaredGrammarCheckpoint,
  hashMintDeclaredGrammarCheckpoint,
  hashMintDeclaredWalkCheckpoint,
  type MintDeclaredAssetLimitStagedPlan,
} from "./staged-plan-v1.js";

export type MintDeclaredAssetLimitStep02Action =
  | { readonly kind: "direct" }
  | { readonly kind: "grammar_start" }
  | { readonly kind: "grammar_resume"; readonly nextOrdinal: number }
  | { readonly kind: "grammar_finish" };

export const submitMintDeclaredAssetLimitStep02 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  nativeTxCompactCbor,
  staged,
  action,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MintDeclaredAssetLimitContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: MintDeclaredAssetLimitEvidence;
  readonly nativeTxCompactCbor: string;
  readonly staged: MintDeclaredAssetLimitStagedPlan;
  readonly action: MintDeclaredAssetLimitStep02Action;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 1;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "mint-declared-asset-limit",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<Record<string, unknown>>({
    threadUtxo,
    signer,
    schema: MintDeclaredAssetLimitStep02DatumSchema as never,
    family: "mint-declared-asset-limit",
    stepIndex,
  });
  const selected =
    "Bound" in state
      ? (state.Bound as { bound: Record<string, unknown> }).bound
      : "Grammar" in state
        ? ((state.Grammar as Record<string, unknown>).bound as Record<
            string,
            unknown
          >)
        : undefined;
  if (
    selected === undefined ||
    selected.policy_index !== BigInt(evidence.policyIndex)
  )
    throw new Error("mintDeclaredAssetLimit: step-02 datum coordinate changed");
  const items = decodeMidgardFieldPreimage(
    Buffer.from(evidence.fieldPreimageHex, "hex"),
  );
  const planned = planFaultProofFieldOpening({
    fieldIndex: 5,
    anchorTxId: evidence.subject.transaction_id,
    nativeTxCompactCbor,
    itemCbors: items,
    owner: signer.paymentKeyHash,
    publish: true,
    label: "mintDeclaredAssetLimit field 5",
  });
  if (action.kind === "direct" && planned.plan.tier === "Certified")
    throw new Error("mintDeclaredAssetLimit: certified field requires grammar");
  const carriageUtxos = await resolveFaultProofFieldCarriagePublications({
    lucid,
    publisherAddress: signer.address,
    planned,
  });
  if (carriageUtxos === undefined)
    throw new Error("mintDeclaredAssetLimit: field carriage disappeared");
  const certificateUtxo =
    planned.plan.tier === "Certified"
      ? await resolveFaultProofFieldPreimageCertificate({
          lucid,
          network: lucid.config().network!,
          planned,
          certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
        })
      : undefined;
  if (planned.plan.tier === "Certified" && certificateUtxo === undefined)
    throw new Error("mintDeclaredAssetLimit: field certificate disappeared");
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[1].spendingScriptHash,
    family: "mint-declared-asset-limit",
    stepIndex,
  });
  const opening: FieldOpening = faultProofFieldOpening({
    planned,
    referenceInputs: [
      ...carriageUtxos,
      stepReference,
      ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
    ],
    certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
    label: "mintDeclaredAssetLimit field 5",
  });
  const grammarCheckpoint = (() => {
    if (action.kind === "grammar_start") return staged.grammar[0];
    if (action.kind === "grammar_resume")
      return staged.grammar[action.nextOrdinal];
    return undefined;
  })();
  if (
    (action.kind === "grammar_start" || action.kind === "grammar_resume") &&
    grammarCheckpoint === undefined
  )
    throw new Error("mintDeclaredAssetLimit: grammar ordinal is outside plan");
  const advancesToFold =
    action.kind === "direct" || action.kind === "grammar_finish";
  const nextData = advancesToFold
    ? {
        subject: evidence.subject,
        policy_index: BigInt(evidence.policyIndex),
        target_policy_id: evidence.targetPolicyId,
        target_declared_count: BigInt(evidence.targetDeclaredCount),
        checkpoint_hash: hashMintDeclaredWalkCheckpoint(staged.initialWalk),
        accumulated_count: 0n,
        previous_policy: "",
        outcome: 0n,
      }
    : {
        Grammar: {
          bound: {
            subject: evidence.subject,
            policy_index: BigInt(evidence.policyIndex),
          },
          checkpoint_hash: hashMintDeclaredGrammarCheckpoint(
            grammarCheckpoint!,
          ),
        },
      };
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextData } as never,
    (advancesToFold
      ? MintDeclaredAssetLimitStep03DatumSchema
      : MintDeclaredAssetLimitStep02DatumSchema) as never,
  );
  const nextStep = advancesToFold ? contracts.steps[2] : contracts.steps[1];
  const outputMatches = computationThreadOutputPredicate({
    address: nextStep.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "mintDeclaredAssetLimit step-02");
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      "mintDeclaredAssetLimit",
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "mintDeclaredAssetLimit step-02 output",
    );
    const common = {
      input_index: inputIndex,
      output_index: outputIndex,
      opening,
    };
    const familyAction =
      action.kind === "direct"
        ? { AuthenticateDirect: common }
        : action.kind === "grammar_start"
          ? { StartGrammar: { ...common, item_budget: 24n } }
          : action.kind === "grammar_resume"
            ? {
                ResumeGrammar: {
                  ...common,
                  checkpoint_bytes: encodeMintDeclaredGrammarCheckpoint(
                    staged.grammar[action.nextOrdinal - 1]!,
                  ).toString("hex"),
                  item_budget: 24n,
                },
              }
            : {
                FinishGrammar: {
                  ...common,
                  checkpoint_bytes: encodeMintDeclaredGrammarCheckpoint(
                    staged.grammar.at(-1)!,
                  ).toString("hex"),
                },
              };
    return Data.to(
      { Continue: [familyAction] } as never,
      MintDeclaredAssetLimitStep02RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference,
    stepScript: contracts.steps[1].spendingScript,
    stepRole: `mintDeclaredAssetLimit step-02 ${action.kind}`,
    nextAddress: nextStep.spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos,
    extraReferenceInputs:
      certificateUtxo === undefined ? [] : [certificateUtxo],
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error("mintDeclaredAssetLimit: step-02 layout unresolved");
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};
