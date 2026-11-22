import { decodeMidgardFieldPreimage } from "@al-ft/midgard-core";
import {
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk/core";
import { type FieldOpening } from "@al-ft/midgard-sdk/fraud-proof";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type RedeemerContext,
  type UTxO,
} from "@lucid-evolution/lucid";

import { resolveFaultProofFieldPreimageCertificate } from "../field-opening.certify-fault-proof-field-carriage.js";
import { planFaultProofFieldOpening } from "../field-opening.plan-fault-proof-field-opening.js";
import {
  faultProofFieldOpening,
  resolveFaultProofFieldCarriagePublications,
} from "../field-opening.publish-fault-proof-field-carriage.js";
import {
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { buildLinearFaultContinue } from "../linear-fault-submit.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { ProofMaterialsAccess } from "../workflow/proof-materials-view.js";
import type { MintDeclaredAssetLimitContracts } from "./contracts.js";
import type { MintDeclaredAssetLimitEvidence } from "./family.advance-mint-declared-fold.js";
import { initialMintDeclaredFoldCursor } from "./family.begin-mint-declared-policy.js";
import { mintDeclaredFoldStateData } from "./family.mint-declared-fold-state-data.js";
import {
  MintDeclaredAssetLimitStep02DatumSchema,
  MintDeclaredAssetLimitStep02RedeemerSchema,
  MintDeclaredAssetLimitStep03DatumSchema,
  MintDeclaredAssetLimitStep04DatumSchema,
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
  | { readonly kind: "grammar_finish" }
  /**
   * Closes a rejection naming a policy field 5 lacks: a negative coordinate
   * at once, and one past the field's last item from the directly opened
   * field or, `fromGrammar`, from the certified walk.
   */
  | { readonly kind: "absent"; readonly fromGrammar: boolean };

/** The redeemer-level shape of one step-02 action, checkpoint bytes included. */
export type MintDeclaredAssetLimitStep02WireAction =
  | { readonly kind: "direct" }
  | { readonly kind: "grammar_start"; readonly itemBudget: bigint }
  | {
      readonly kind: "grammar_resume";
      readonly checkpointBytesHex: string;
      readonly itemBudget: bigint;
    }
  | { readonly kind: "grammar_finish"; readonly checkpointBytesHex: string }
  | { readonly kind: "absent"; readonly checkpointBytesHex: string };

export type MintDeclaredAssetLimitStep02Successor =
  | Readonly<{ kind: "grammar"; checkpointHash: string }>
  | Readonly<{ kind: "fold"; checkpointHash: string }>
  | Readonly<{ kind: "absent" }>;

/** Resolves the published carriage (and certificate) for a planned opening. */
export const resolveMintDeclaredFieldCarriage = async ({
  lucid,
  contracts,
  signer,
  planned,
  materials,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MintDeclaredAssetLimitContracts;
  readonly signer: ResolvedProverSigner;
  readonly planned: PlannedMintDeclaredFieldOpening;
  /** The workflow's view of its proof-materials address, on a workflow. */
  readonly materials?: ProofMaterialsAccess;
}): Promise<{
  readonly carriageUtxos: readonly UTxO[];
  readonly certificateUtxo: UTxO | undefined;
}> => {
  const carriageUtxos = await resolveFaultProofFieldCarriagePublications({
    materials,
    lucid,
    publisherAddress: signer.proofMaterialsAddress,
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
    anchorSourceKind: evidence.subject.source_kind === 1n ? 1n : 0n,
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
export const buildMintDeclaredAssetLimitStep02Raw = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  openingFor,
  carriageUtxos,
  certificateUtxo,
  action,
  next,
  nextStepIndex = next.kind === "fold" ? 2 : next.kind === "absent" ? 3 : 1,
  referenceScriptUtxo,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MintDeclaredAssetLimitContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: MintDeclaredAssetLimitEvidence;
  /** The opening, resolved against the builder's final reference inputs. */
  readonly openingFor: (ctx: RedeemerContext) => FieldOpening;
  readonly carriageUtxos: readonly UTxO[];
  readonly certificateUtxo: UTxO | undefined;
  readonly action: MintDeclaredAssetLimitStep02WireAction;
  readonly next: MintDeclaredAssetLimitStep02Successor;
  readonly nextStepIndex?: 0 | 1 | 2 | 3;
  readonly referenceScriptUtxo: UTxO;
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
  if (selected === undefined || selected.policy_index !== evidence.policyIndex)
    throw new Error("mintDeclaredAssetLimit: step-02 datum coordinate changed");
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[1].spendingScriptHash,
    family: "mint-declared-asset-limit",
    stepIndex,
  });
  const nextData =
    next.kind === "absent"
      ? {
          subject: evidence.subject,
          policy_index: evidence.policyIndex,
          crossing: false,
        }
      : next.kind === "fold"
        ? mintDeclaredFoldStateData({
            subject: evidence.subject,
            target: {
              policyIndex: Number(evidence.policyIndex),
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
                policy_index: evidence.policyIndex,
              },
              checkpoint_hash: next.checkpointHash,
            },
          };
  const nextDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: nextData } as never,
    (next.kind === "absent"
      ? MintDeclaredAssetLimitStep04DatumSchema
      : next.kind === "fold"
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
      opening: openingFor(ctx),
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
            : action.kind === "grammar_finish"
              ? {
                  FinishGrammar: {
                    ...common,
                    checkpoint_bytes: action.checkpointBytesHex,
                  },
                }
              : {
                  AuthenticateAbsent: {
                    ...common,
                    checkpoint_bytes: action.checkpointBytesHex,
                  },
                };
    return Data.to(
      { Continue: [familyAction] } as never,
      MintDeclaredAssetLimitStep02RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const transaction = await buildLinearFaultContinue({
    lucid,
    signer,
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
  });
  if (outputIndex === undefined)
    throw new Error("mintDeclaredAssetLimit: step-02 layout unresolved");
  return {
    transaction,
    nextThreadOutRef: `${transaction.txHash}#${outputIndex.toString()}`,
  };
};

/** The wire action and successor the staged plan prescribes for `action`. */
export const mintDeclaredStep02WirePlan = ({
  staged,
  action,
}: {
  readonly staged: Pick<
    MintDeclaredAssetLimitStagedPlan,
    "grammar" | "initialWalk"
  >;
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
  if (action.kind === "absent")
    return {
      action: {
        kind: "absent",
        checkpointBytesHex: action.fromGrammar
          ? encodeMintDeclaredGrammarCheckpoint(
              staged.grammar.at(-1)!,
            ).toString("hex")
          : "",
      },
      next: { kind: "absent" },
    };
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

export const buildMintDeclaredAssetLimitStep02 = async ({
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
  materials,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MintDeclaredAssetLimitContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: MintDeclaredAssetLimitEvidence;
  readonly nativeTxCompactCbor: string;
  /** The staged plan; a grammar plan suffices for an absent policy. */
  readonly staged: Pick<
    MintDeclaredAssetLimitStagedPlan,
    "grammar" | "initialWalk"
  >;
  readonly action: MintDeclaredAssetLimitStep02Action;
  readonly referenceScriptUtxo: UTxO;
  /** The workflow's view of its proof-materials address, on a workflow. */
  readonly materials?: ProofMaterialsAccess;
}) => {
  const planned = planMintDeclaredFieldOpening({
    evidence,
    nativeTxCompactCbor,
    signer,
    label: "mintDeclaredAssetLimit field 5",
  });
  // A negative coordinate closes absent without reading the field's count,
  // so it needs no grammar whatever the field's tier.
  if (
    (action.kind === "direct" ||
      (action.kind === "absent" &&
        !action.fromGrammar &&
        evidence.policyIndex >= 0n)) &&
    planned.plan.tier === "Certified"
  )
    throw new Error("mintDeclaredAssetLimit: certified field requires grammar");
  // An absent policy certifies the grammar and then closes; a present one
  // never closes absent.
  if (
    evidence.absent
      ? action.kind === "direct" || action.kind === "grammar_finish"
      : action.kind === "absent"
  )
    throw new Error(
      "mintDeclaredAssetLimit: step-02 action does not fit the evidence",
    );
  const carriage = await resolveMintDeclaredFieldCarriage({
    materials,
    lucid,
    contracts,
    signer,
    planned,
  });
  return await buildMintDeclaredAssetLimitStep02Raw({
    lucid,
    contracts,
    categoryId,
    signer,
    threadOutRef,
    evidence,
    openingFor: (ctx) =>
      faultProofFieldOpening({
        planned,
        referenceInputs: ctx.referenceInputs,
        certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
        label: "mintDeclaredAssetLimit field 5",
      }),
    ...carriage,
    ...mintDeclaredStep02WirePlan({ staged, action }),
    referenceScriptUtxo,
  });
};
