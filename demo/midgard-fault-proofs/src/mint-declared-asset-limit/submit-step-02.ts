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
} from "../field-opening.js";
import {
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { submitLinearFaultContinue } from "../linear-fault-submit.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { MintDeclaredAssetLimitContracts } from "./contracts.js";
import {
  initialMintDeclaredFoldCursor,
  type MintDeclaredAssetLimitEvidence,
  mintDeclaredFoldStateData,
} from "./family.js";
import {
  MintDeclaredAssetLimitStep02DatumSchema,
  MintDeclaredAssetLimitStep02RedeemerSchema,
  MintDeclaredAssetLimitStep03DatumSchema,
} from "./schemas.js";
import {
  encodeMintDeclaredGrammarCheckpoint,
  hashMintDeclaredGrammarCheckpoint,
  hashMintDeclaredWalkCheckpoint,
  type MintDeclaredAssetLimitStagedPlan,
} from "./staged-plan.js";

export type PlannedMintDeclaredFieldOpening = ReturnType<
  typeof planFaultProofFieldOpening
>;

export type MintDeclaredAssetLimitStep02Action =
  | { readonly kind: "direct" }
  | { readonly kind: "grammar_start" }
  | { readonly kind: "grammar_resume"; readonly nextOrdinal: number }
  | { readonly kind: "grammar_finish" };

/** The redeemer-level shape of one step-02 action, checkpoint bytes included. */
export type MintDeclaredAssetLimitStep02WireAction =
  | { readonly kind: "direct" }
  | { readonly kind: "grammar_start"; readonly itemBudget: bigint }
  | {
      readonly kind: "grammar_resume";
      readonly checkpointBytesHex: string;
      readonly itemBudget: bigint;
    }
  | { readonly kind: "grammar_finish"; readonly checkpointBytesHex: string };

export type MintDeclaredAssetLimitStep02Successor =
  | Readonly<{ kind: "grammar"; checkpointHash: string }>
  | Readonly<{ kind: "fold"; checkpointHash: string }>;

/** Resolves the published carriage (and certificate) for a planned opening. */
export const resolveMintDeclaredFieldCarriage = async ({
  lucid,
  contracts,
  signer,
  planned,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MintDeclaredAssetLimitContracts;
  readonly signer: ResolvedProverSigner;
  readonly planned: PlannedMintDeclaredFieldOpening;
}): Promise<{
  readonly carriageUtxos: readonly UTxO[];
  readonly certificateUtxo: UTxO | undefined;
}> => {
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
  return { carriageUtxos, certificateUtxo };
};

/** The planned field-5 opening of the evidence transaction. */
export const planMintDeclaredFieldOpening = ({
  evidence,
  nativeTxCompactCbor,
  signer,
  label,
}: {
  readonly evidence: MintDeclaredAssetLimitEvidence;
  readonly nativeTxCompactCbor: string;
  readonly signer: ResolvedProverSigner;
  readonly label: string;
}): PlannedMintDeclaredFieldOpening =>
  planFaultProofFieldOpening({
    fieldIndex: 5,
    anchorTxId: evidence.subject.transaction_id,
    nativeTxCompactCbor,
    itemCbors: decodeMidgardFieldPreimage(
      Buffer.from(evidence.fieldPreimageHex, "hex"),
    ),
    owner: signer.paymentKeyHash,
    publish: true,
    label,
  });

/**
 * Builds one step-02 transaction from explicit wire inputs: the opening and
 * the carriage it references, the action with its checkpoint bytes, the
 * successor state and the successor script. The plan-driven builder below
 * derives all of them; this form lets a lifecycle present a substituted
 * carriage, transaction, checkpoint, state or successor on chain.
 */
export const submitMintDeclaredAssetLimitStep02Raw = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  opening,
  carriageUtxos,
  certificateUtxo,
  action,
  next,
  nextStepIndex = next.kind === "fold" ? 2 : 1,
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
  readonly opening: FieldOpening;
  readonly carriageUtxos: readonly UTxO[];
  readonly certificateUtxo: UTxO | undefined;
  readonly action: MintDeclaredAssetLimitStep02WireAction;
  readonly next: MintDeclaredAssetLimitStep02Successor;
  readonly nextStepIndex?: 0 | 1 | 2 | 3;
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
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[1].spendingScriptHash,
    family: "mint-declared-asset-limit",
    stepIndex,
  });
  const nextData =
    next.kind === "fold"
      ? mintDeclaredFoldStateData({
          subject: evidence.subject,
          target: {
            policyIndex: evidence.policyIndex,
            targetPolicyId: evidence.targetPolicyId,
            targetDeclaredCount: evidence.targetDeclaredCount,
          },
          cursor: initialMintDeclaredFoldCursor(),
          checkpointHash: next.checkpointHash,
        })
      : {
          Grammar: {
            bound: {
              subject: evidence.subject,
              policy_index: BigInt(evidence.policyIndex),
            },
            checkpoint_hash: next.checkpointHash,
          },
        };
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextData } as never,
    (next.kind === "fold"
      ? MintDeclaredAssetLimitStep03DatumSchema
      : MintDeclaredAssetLimitStep02DatumSchema) as never,
  );
  const nextStep = contracts.steps[nextStepIndex];
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
          ? { StartGrammar: { ...common, item_budget: action.itemBudget } }
          : action.kind === "grammar_resume"
            ? {
                ResumeGrammar: {
                  ...common,
                  checkpoint_bytes: action.checkpointBytesHex,
                  item_budget: action.itemBudget,
                },
              }
            : {
                FinishGrammar: {
                  ...common,
                  checkpoint_bytes: action.checkpointBytesHex,
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

/** The wire action and successor the staged plan prescribes for `action`. */
export const mintDeclaredStep02WirePlan = ({
  staged,
  action,
}: {
  readonly staged: MintDeclaredAssetLimitStagedPlan;
  readonly action: MintDeclaredAssetLimitStep02Action;
}): {
  readonly action: MintDeclaredAssetLimitStep02WireAction;
  readonly next: MintDeclaredAssetLimitStep02Successor;
} => {
  const itemBudget = 24n;
  const fold = {
    kind: "fold",
    checkpointHash: hashMintDeclaredWalkCheckpoint(staged.initialWalk),
  } as const;
  if (action.kind === "direct")
    return { action: { kind: "direct" }, next: fold };
  if (action.kind === "grammar_finish")
    return {
      action: {
        kind: "grammar_finish",
        checkpointBytesHex: encodeMintDeclaredGrammarCheckpoint(
          staged.grammar.at(-1)!,
        ).toString("hex"),
      },
      next: fold,
    };
  const checkpoint =
    action.kind === "grammar_start"
      ? staged.grammar[0]
      : staged.grammar[action.nextOrdinal];
  if (checkpoint === undefined)
    throw new Error("mintDeclaredAssetLimit: grammar ordinal is outside plan");
  const next = {
    kind: "grammar",
    checkpointHash: hashMintDeclaredGrammarCheckpoint(checkpoint),
  } as const;
  if (action.kind === "grammar_start")
    return { action: { kind: "grammar_start", itemBudget }, next };
  return {
    action: {
      kind: "grammar_resume",
      checkpointBytesHex: encodeMintDeclaredGrammarCheckpoint(
        staged.grammar[action.nextOrdinal - 1]!,
      ).toString("hex"),
      itemBudget,
    },
    next,
  };
};

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
  const planned = planMintDeclaredFieldOpening({
    evidence,
    nativeTxCompactCbor,
    signer,
    label: "mintDeclaredAssetLimit field 5",
  });
  if (action.kind === "direct" && planned.plan.tier === "Certified")
    throw new Error("mintDeclaredAssetLimit: certified field requires grammar");
  const carriage = await resolveMintDeclaredFieldCarriage({
    lucid,
    contracts,
    signer,
    planned,
  });
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[1].spendingScriptHash,
    family: "mint-declared-asset-limit",
    stepIndex: 1,
  });
  const opening: FieldOpening = faultProofFieldOpening({
    planned,
    referenceInputs: [
      ...carriage.carriageUtxos,
      stepReference,
      ...(carriage.certificateUtxo === undefined
        ? []
        : [carriage.certificateUtxo]),
    ],
    certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
    label: "mintDeclaredAssetLimit field 5",
  });
  return await submitMintDeclaredAssetLimitStep02Raw({
    lucid,
    contracts,
    categoryId,
    signer,
    threadOutRef,
    evidence,
    opening,
    ...carriage,
    ...mintDeclaredStep02WirePlan({ staged, action }),
    referenceScriptUtxo,
    preSubmitBoundary,
    awaitConfirmation,
  });
};
