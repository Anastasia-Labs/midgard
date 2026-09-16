import { WorkflowActionChangedError } from "../workflow/action-changed.js";
import {
  encodeWorkflowArtifact,
  requireWorkflowArtifactMatches,
} from "../workflow/artifact-codec.js";
import {
  createCursorFamilyWorkflowAdapter,
  CURSOR_FAMILY_TRANSACTION_PORT,
  type CursorFamilyTransactionPort,
} from "../workflow/cursor-family-adapter.js";
import {
  captureCursorRemoval,
  cursorFamilyActionInput,
  cursorStringField,
} from "../workflow/cursor-family-runtime.js";
import {
  createAuthenticatedFieldCarriagePrerequisitePort,
  withFieldCarriagePrerequisite,
} from "../workflow/field-carriage-prerequisite.js";
import {
  journalJsonDigest,
  type JournalJsonObject,
} from "../workflow/journal.js";
import {
  captureLocallyEvaluatedTransaction,
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "../workflow/transaction-boundary.js";
import type { ManifestBoundFieldPreimageLengthWorkflow } from "./authenticated-workflow.js";
import {
  planFieldPreimageLengthCarriage,
  resolveFieldPreimageLengthCarriage,
} from "./carriage.js";
import {
  createConcreteFieldPreimageLengthLucidBuilders,
  createFieldPreimageLengthLucidSubmission,
} from "./config.js";
import {
  type AuthenticatedFieldPreimageLengthEvidence,
  fieldPreimageLengthEvidenceFromCanonicalBlock,
} from "./evidence.js";
import type { FieldPreimageLengthAction } from "./workflow.js";
import { FIELD_PREIMAGE_LENGTH_CURSOR_SPEC } from "./workflow-spec.js";

type BoundWorkflow = Pick<
  ManifestBoundFieldPreimageLengthWorkflow,
  | "config"
  | "binding"
  | "l1"
  | "decisionDigest"
  | "stateQueueMutationLeaseCoordinator"
>;
const CATEGORY = "fieldPreimageLengthMismatch";

const proofMaterial = (
  evidence: AuthenticatedFieldPreimageLengthEvidence,
): AuthenticatedFieldPreimageLengthEvidence => ({
  prepared: evidence.prepared,
  fieldMaterial: evidence.fieldMaterial,
  stageEvidence: evidence.stageEvidence,
});

/** Existing submitters capture one evaluated body; shared recovery owns every durable attempt. */
export const createFieldPreimageLengthRecoveryAdapter = (
  workflow: BoundWorkflow,
) => {
  const { config, binding, l1, stateQueueMutationLeaseCoordinator } = workflow;
  let current:
    | { digest: string; evidence: AuthenticatedFieldPreimageLengthEvidence }
    | undefined;
  const admit = (
    evidence: AuthenticatedFieldPreimageLengthEvidence,
    artifact?: JournalJsonObject,
  ) => {
    if (evidence.prepared.headerHash !== binding.definition.headerHash)
      throw new Error(`${CATEGORY} recovery evidence changed target`);
    const fresh = proofMaterial(evidence);
    const value = artifact ?? encodeWorkflowArtifact(fresh);
    if (artifact !== undefined) requireWorkflowArtifactMatches(artifact, fresh);
    current = { digest: journalJsonDigest(value), evidence: fresh };
    return value;
  };
  const requireMaterial = (artifact: JournalJsonObject) => {
    if (current === undefined || current.digest !== journalJsonDigest(artifact))
      throw new Error(
        `${CATEGORY} proof material was not freshly authenticated`,
      );
    return current.evidence;
  };
  const transactions: CursorFamilyTransactionPort<typeof CATEGORY> = {
    portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
    category: CATEGORY,
    prepare: async ({ evidence }) => {
      current = undefined;
      return admit(
        await fieldPreimageLengthEvidenceFromCanonicalBlock(evidence),
      );
    },
    validatePreparedArtifact: async ({ evidence, artifact }) => {
      current = undefined;
      admit(
        await fieldPreimageLengthEvidenceFromCanonicalBlock(evidence),
        artifact,
      );
    },
    prepareRaw: async (routed) => {
      current = undefined;
      if (routed.kind !== "field_preimage_length_mismatch")
        throw new Error(`${CATEGORY} raw evidence changed family`);
      return admit(routed.evidence);
    },
    validatePreparedRawArtifact: async ({ routed, artifact }) => {
      current = undefined;
      if (routed.kind !== "field_preimage_length_mismatch")
        throw new Error(`${CATEGORY} raw evidence changed family`);
      admit(routed.evidence, artifact);
    },
    capture: async ({ action, artifact }) => {
      const input = cursorFamilyActionInput({ category: CATEGORY, action });
      if (input.stage === "remove")
        return await captureCursorRemoval({
          category: CATEGORY,
          lucid: config.lucid,
          blueprint: binding.blueprint,
          deploymentInfo: binding.deploymentInfo,
          network: binding.network,
          signer: config.signer,
          headerHash: binding.definition.headerHash,
          input,
          stateQueueMutationLeaseCoordinator,
          fraudProverRewardLovelace: BigInt(
            binding.releaseEconomics.policy.fraudProverRewardLovelace,
          ),
        });
      const evidence = requireMaterial(artifact);
      const selected: Exclude<
        FieldPreimageLengthAction,
        "complete" | "remove"
      > =
        input.stage === "init"
          ? "init"
          : input.stage === "step_01"
            ? "dispatch"
            : input.stage === "step_02" || input.stage === "step_03"
              ? "authenticate"
              : input.stage === "step_04"
                ? "finalize"
                : (() => {
                    throw new Error(
                      `${CATEGORY} cursor action is outside its topology`,
                    );
                  })();
      if (
        (input.stage === "step_02" &&
          evidence.prepared.direction !== "wrongfulAcceptance") ||
        (input.stage === "step_03" &&
          evidence.prepared.direction !== "wrongfulRejection")
      )
        throw new Error(
          `${CATEGORY} authenticated thread changed proof direction`,
        );
      const needsCarriage =
        selected === "authenticate" ||
        (selected === "dispatch" &&
          evidence.prepared.direction === "wrongfulAcceptance");
      const transaction = await captureLocallyEvaluatedTransaction(
        async (boundary) => {
          const builders = createConcreteFieldPreimageLengthLucidBuilders({
            resolveStage: async () => {
              const observed = (
                await l1.observe({ headerHash: binding.definition.headerHash })
              ).stage;
              if (
                observed.kind === "removed" ||
                observed.kind === "proof_token" ||
                (selected === "init"
                  ? observed.kind !== "not_started"
                  : observed.kind !== "step" ||
                    observed.threadOutRef !==
                      cursorStringField(input, "threadOutRef")) ||
                observed.stateQueueBlockOutRef !==
                  cursorStringField(input, "stateQueueBlockOutRef")
              ) {
                throw new WorkflowActionChangedError(
                  `${CATEGORY} authenticated action changed before capture`,
                );
              }
              const carriage = needsCarriage
                ? await resolveFieldPreimageLengthCarriage({
                    workflow,
                    evidence,
                    allowPublication: false,
                  })
                : undefined;
              return {
                fraudulentBlockOutRef: observed.stateQueueBlockOutRef,
                ...(observed.kind === "step"
                  ? {
                      threadOutRef: observed.threadOutRef,
                      stateQueueBlockOutRef: observed.stateQueueBlockOutRef,
                    }
                  : {}),
                ...evidence.stageEvidence,
                ...(carriage === undefined
                  ? {}
                  : evidence.prepared.direction === "wrongfulAcceptance"
                    ? {
                        acceptedClaimResolver: carriage.claimResolver,
                        acceptedCarriageReferenceInputs:
                          carriage.carriageReferences,
                      }
                    : {
                        forcedClaimResolver: carriage.claimResolver,
                        forcedCarriageReferenceInputs:
                          carriage.carriageReferences,
                      }),
              };
            },
            remove: async () => {
              throw new Error(
                `${CATEGORY} removal must use its exact cursor capture`,
              );
            },
            boundary: () => boundary,
          });
          await createFieldPreimageLengthLucidSubmission({
            config,
            builders,
          }).submit(selected, evidence.prepared);
        },
      );
      if (
        selected !== "init" &&
        !workflowTransactionInputOutRefs(transaction.signed).includes(
          cursorStringField(input, "threadOutRef"),
        )
      )
        throw new Error(
          `${CATEGORY} captured body changed its authenticated thread input`,
        );
      if (
        selected === "init" &&
        !workflowTransactionReferenceInputOutRefs(transaction.signed).includes(
          cursorStringField(input, "stateQueueBlockOutRef"),
        )
      )
        throw new Error(
          `${CATEGORY} init changed its authenticated header reference`,
        );
      return { transaction };
    },
  };
  const base = createCursorFamilyWorkflowAdapter({
    spec: FIELD_PREIMAGE_LENGTH_CURSOR_SPEC,
    l1,
    transactions,
    stateQueueMutationLeaseCoordinator,
  });
  const adapter = withFieldCarriagePrerequisite({
    category: CATEGORY,
    base,
    prerequisite: createAuthenticatedFieldCarriagePrerequisitePort({
      category: CATEGORY,
      lucid: config.lucid,
      network: binding.network,
      signer: config.signer,
      publications: l1.publications,
      transactionConfirmed: async ({ headerHash, txHash }) =>
        await l1.transactionConfirmed({ headerHash, txHash }),
      requirementForAction: ({ action, artifact }) => {
        if (
          action.input.stage !== "step_01" &&
          action.input.stage !== "step_02" &&
          action.input.stage !== "step_03"
        )
          return null;
        const evidence = requireMaterial(artifact);
        if (
          action.input.stage !== "step_02" &&
          action.input.stage !== "step_03" &&
          !(
            action.input.stage === "step_01" &&
            evidence.prepared.direction === "wrongfulAcceptance"
          )
        )
          return null;
        const planned = planFieldPreimageLengthCarriage({ workflow, evidence });
        const certificate = config.contracts.fieldPreimageCertificate;
        return {
          planned,
          compactCbor: evidence.fieldMaterial.nativeTxCompactCbor,
          witnessSetCompactCbor: evidence.fieldMaterial.witnessSetCompactCbor,
          certificate: {
            policyId: certificate.policyId,
            mintingScript: certificate.mintingScript,
            referenceScriptUtxo:
              config.referenceScripts.fieldPreimageCertificateMint,
          },
        };
      },
    }),
  });
  return { adapter, transactions };
};
