/** A direct walk family declared once: its cursor spec, custody, artifact
 * and replay. The kit derives the rest of its family definition: the step
 * datum schemas and contract names, the transaction port, a field-carriage
 * requirement per opened field, and the step-01 proof chunk. */
import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type {
  CompleteCanonicalReplay,
  CompleteCanonicalReplayContext,
} from "../workflow/complete-replay.js";
import { parseComputationThreadStage } from "../workflow/computation-thread-steps.js";
import { cursorFamilyActionInput } from "../workflow/cursor-family-runtime.js";
import type { CursorFamilySpec } from "../workflow/cursor-family-state.js";
import {
  CHUNKED_VERIFY_WITNESS_ROLES,
  cursorStepContractNames,
  defineFamily,
  type FamilyAssemblyContext,
  type FamilyCategory,
  type FamilyStepTuple,
  type LinearFamilyPrerequisiteInput,
  type LinearFamilyStepDatumSchema,
  type ManifestBoundFamilyWorkflow,
} from "../workflow/family-definition.js";
import type { FieldCarriageRequirement } from "../workflow/field-carriage-prerequisite.js";
import type { FraudProofL1Source } from "../workflow/l1-source.js";
import {
  directPurposeSourceInclusion,
  planDirectPurposeArtifact,
} from "./artifact.js";
import type {
  DirectPurposeArtifactFamily,
  DirectPurposeProof,
  DirectPurposeReplay,
} from "./artifact-family.js";
import {
  DIRECT_PURPOSE_FIELDS,
  type DirectPurposeFieldPlan,
} from "./custody.js";
import type {
  DirectPurposeCustodyFamily,
  DirectPurposeFamily,
} from "./family.js";
import {
  createDirectWalkTransactionPort,
  directWalkContracts,
  directWalkFieldPlan,
  type DirectWalkReferenceScripts,
  directWalkRoleAt,
} from "./transaction-port.js";

/** A walk's proof: its planned custody and, when it opens committed fields,
 * the plan of each. */
export type DirectWalkProof = DirectPurposeProof &
  Readonly<{ plan?: (field: number) => DirectPurposeFieldPlan }>;

/** What a walk's terminal verdict is built from. */
export type DirectWalkTerminalInput<Proof extends DirectWalkProof> = Readonly<{
  admitted: Pick<DirectPurposeReplay, "material">;
  proof: Proof;
}>;

export type DirectWalkFamily<
  Category extends FamilyCategory,
  StepCount extends number,
  Prepared extends DirectPurposeReplay,
  Proof extends DirectWalkProof,
> = Readonly<{
  /** The chain topology; names the category and sizes the step tuples. */
  spec: CursorFamilySpec<Category> & Readonly<{ stepCount: StepCount }>;
  artifact: DirectPurposeArtifactFamily<Prepared, Proof>;
  replay: CompleteCanonicalReplay;
  /** The fields the custody can open, each carried ahead of an action whose
   * role opens it; every direct purpose field when absent. */
  fields?: readonly number[];
  /** False when step 01 publishes no proof as chunks: a family whose sources
   * are all forced (`executionSourceScriptDecoding`) has no inclusion proof. */
  proofChunk?: false;
}> &
  (
    | Readonly<{
        /** The custody and the verdict its terminal is judged by. */
        family: DirectPurposeFamily<unknown>;
        terminal?: undefined;
      }>
    | Readonly<{
        family: DirectPurposeCustodyFamily;
        /** The terminal's family, its verdict built from the admitted
         * material and the proof. */
        terminal: (
          input: DirectWalkTerminalInput<Proof>,
        ) => DirectPurposeFamily<unknown>;
      }>
  );

type WitnessRole = (typeof CHUNKED_VERIFY_WITNESS_ROLES)[number];

export type DirectWalkWorkflow<
  Category extends FamilyCategory,
  StepCount extends number,
> = ManifestBoundFamilyWorkflow<Category, true, StepCount>;

export type DirectWalkWorkflowConfig<
  Category extends FamilyCategory,
  StepCount extends number,
> = Readonly<{
  replayContext?: CompleteCanonicalReplayContext;
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: DirectWalkReferenceScripts<Category, StepCount>;
  l1Source: FraudProofL1Source;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

const S = FraudProofComputationThreadStepDatum;

/** The walk with its family definition. */
export const defineDirectWalkFamily = <
  Category extends FamilyCategory,
  StepCount extends number,
  Prepared extends DirectPurposeReplay,
  Proof extends DirectWalkProof,
>(
  walk: DirectWalkFamily<Category, StepCount, Prepared, Proof>,
) => {
  const { spec, artifact } = walk;
  type Context = FamilyAssemblyContext<Category, WitnessRole, true, StepCount>;
  const contracts = (context: Context) =>
    directWalkContracts(walk, context.binding, context.certificate);
  const planned = (context: Context, input: LinearFamilyPrerequisiteInput) =>
    planDirectPurposeArtifact(artifact, {
      artifact: input.artifact,
      contracts: contracts(context),
      prover: context.signer.paymentKeyHash,
    });
  /** The custody role an action spends; `null` for init, removal, step-01
   * and the terminal, which open no committed field. */
  const custodyRole = (input: LinearFamilyPrerequisiteInput) => {
    const { stage } = cursorFamilyActionInput({
      category: spec.category,
      action: input.action,
    });
    const step = parseComputationThreadStage(stage);
    if (step === undefined) return null;
    const role = directWalkRoleAt(walk, step);
    return role === "step01" || role === "terminal" ? null : role;
  };
  /** The carriage of `field` ahead of any action whose role opens it; the
   * entry names its field. */
  const fieldCarriage = (field: number) => ({
    field,
    requirementForAction: async (
      context: Context,
      input: LinearFamilyPrerequisiteInput,
    ) => {
      const role = custodyRole(input);
      if (role === null) return null;
      const plan = await planned(context, input);
      if (!plan.fieldsOf(role).includes(field)) return null;
      const { material } = plan.admitted;
      return {
        planned: directWalkFieldPlan(spec.category, plan.proof)(field)
          .openingPlan,
        compactCbor: material.bound.compact_cbor,
        witnessSetCompactCbor:
          material.material.proofSource.witnessSetCompactCbor.toString("hex"),
        certificate: {
          policyId: context.certificate.policyId,
          mintingScript: context.certificate.mintingScript,
          referenceScriptUtxo: context.references.fieldPreimageCertificateMint,
        },
      } satisfies FieldCarriageRequirement;
    },
  });
  const definition = defineFamily({
    category: spec.category,
    stepDatumSchemas: Array.from(
      { length: spec.stepCount },
      () => S,
    ) as unknown as FamilyStepTuple<
      Category,
      StepCount,
      LinearFamilyStepDatumSchema
    >,
    witnessRoles: CHUNKED_VERIFY_WITNESS_ROLES,
    fieldPreimageCertificate: true,
    replayer: () => walk.replay,
    adapter: {
      kind: "cursor",
      spec,
      stepContractNames: cursorStepContractNames<Category, StepCount>(spec),
      transactionPort: (context) =>
        createDirectWalkTransactionPort(walk, {
          ...context,
          contracts: contracts(context),
        }),
    },
    fieldCarriage: (walk.fields ?? DIRECT_PURPOSE_FIELDS).map(fieldCarriage),
    ...(walk.proofChunk === false
      ? {}
      : {
          proofChunk: async (
            context: Context,
            input: LinearFamilyPrerequisiteInput,
          ) => {
            if (input.action.input.stage !== "step_01") return null;
            const { admitted } = await planned(context, input);
            return admitted.coordinate.sourceKind === 0
              ? (await directPurposeSourceInclusion(artifact, admitted))
                  .txMembershipProofCbor
              : null;
          },
        }),
  });
  return Object.freeze({ ...walk, definition });
};
