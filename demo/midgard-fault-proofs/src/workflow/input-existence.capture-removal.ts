import {
  type FaultProofFieldOpeningPlan,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.js";
import type {
  InputExistenceCategory,
  InputExistenceKind,
} from "../input-existence/kind.js";
import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import {
  type LinearFamilyAssemblyContext,
  type LinearFamilyReferenceScripts,
  MEMBERSHIP_WITNESS_ROLES,
} from "./family-definition.js";
import { type FraudProofWorkflowAction } from "./orchestrator.js";
import type { OwnOutputJournal } from "./own-outputs.js";
import { resolveDirectFirstProofChunks } from "./proof-chunk-prerequisite.js";
import {
  captureLocallyEvaluatedTransaction,
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary.js";

export const WITNESS_ROLES = MEMBERSHIP_WITNESS_ROLES;

export type InputExistenceWorkflowReferenceScripts<
  Category extends InputExistenceCategory,
> = LinearFamilyReferenceScripts<
  Category,
  (typeof WITNESS_ROLES)[number],
  true
>;

export type BoundConfig<Category extends InputExistenceCategory> =
  LinearFamilyAssemblyContext<Category, (typeof WITNESS_ROLES)[number], true>;

const record = (
  value: unknown,
  label: string,
): Readonly<Record<string, unknown>> => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype
  ) {
    throw new Error(`${label} must be a plain object`);
  }
  return value as Readonly<Record<string, unknown>>;
};

export const resolveChunks = async ({
  action,
  config,
  proofCbor,
}: {
  readonly action: FraudProofWorkflowAction;
  readonly config: BoundConfig<InputExistenceCategory>;
  readonly proofCbor: string;
}) => {
  const chunks = await resolveDirectFirstProofChunks({
    action,
    lucid: config.lucid,
    address: config.signer.address,
    proofCbor,
  });
  // Absence is the expected state for the direct-first attempt. Once a
  // publication route is journal-authorized, the outer prerequisite refuses
  // to expose the base step until the exact raw-L1-confirmed chunks exist.
  return chunks;
};

/** A family's reading of its workflow actions, and its block removal. */
export const inputExistenceRemoval = <Category extends InputExistenceCategory>({
  category,
  name,
}: InputExistenceKind<Category>) => {
  const actionInput = (
    action: FraudProofWorkflowAction,
  ): Readonly<Record<string, unknown>> => {
    const input = record(action.input, `${name} workflow action`);
    if (
      input.schemaVersion !== "midgard-production-linear-family-action-v1" ||
      input.category !== category ||
      typeof input.stage !== "string"
    ) {
      throw new Error(`${name} workflow action changed identity`);
    }
    return input;
  };

  const stringField = (
    input: Readonly<Record<string, unknown>>,
    field: string,
  ): string => {
    const value = input[field];
    if (typeof value !== "string") {
      throw new Error(`${name} workflow action omitted ${field}`);
    }
    return value;
  };

  const resolveField = async (
    config: BoundConfig<Category>,
    planned: FaultProofFieldOpeningPlan,
    ownOutputs: OwnOutputJournal,
  ) => {
    const publications = await resolveFaultProofFieldCarriagePublications({
      ownOutputs,
      lucid: config.lucid,
      publisherAddress: config.signer.address,
      planned,
    });
    if (publications === undefined) {
      throw new Error(`${name} field publications disappeared`);
    }
    const certificate = await resolveFaultProofFieldPreimageCertificate({
      ownOutputs,
      lucid: config.lucid,
      network: config.binding.network,
      planned,
      certificatePolicyId: config.certificate.policyId,
    });
    if (planned.plan.tier === "Certified" && certificate === undefined) {
      throw new Error(`${name} field certificate disappeared`);
    }
    return Object.freeze({ publications, certificate });
  };

  const captureRemoval = async (
    config: BoundConfig<Category>,
    input: Readonly<Record<string, unknown>>,
  ) => {
    let mutationLease: StateQueueMutationLease | undefined;
    const retainingCoordinator: StateQueueMutationLeaseCoordinator = {
      acquire: async () => {
        const acquired =
          await config.stateQueueMutationLeaseCoordinator.acquire();
        mutationLease = acquired;
        return acquired;
      },
    };
    const nextRemovalOutRef = stringField(input, "nextRemovalOutRef");
    const fraudProofOutRef = stringField(input, "fraudProofOutRef");
    const transaction = await captureLocallyEvaluatedTransaction(
      async (boundary) => {
        await submitRemoveFraudulentBlock({
          fraudProofOutRef,
          lucid: config.lucid,
          blueprint: config.binding.blueprint,
          deploymentInfo: config.binding.deploymentInfo,
          network: config.binding.network,
          signer: config.signer,
          fraudCategory: category,
          fraudulentHeaderHash: config.binding.definition.headerHash,
          requireReferenceScripts: true,
          stateQueueMutationLeaseCoordinator: retainingCoordinator,
          fraudProverRewardLovelace: BigInt(
            config.binding.releaseEconomics.policy.fraudProverRewardLovelace,
          ),
          preSubmitBoundary: async (built) => {
            if (
              !workflowTransactionInputOutRefs(built.signed).includes(
                nextRemovalOutRef,
              ) ||
              !workflowTransactionReferenceInputOutRefs(built.signed).includes(
                fraudProofOutRef,
              )
            ) {
              throw new Error(`${name} removal changed authenticated inputs`);
            }
            await boundary(built);
          },
        });
      },
    );
    return Object.freeze({
      transaction,
      ...(mutationLease === undefined ? {} : { mutationLease }),
    });
  };
  return { actionInput, stringField, resolveField, captureRemoval };
};
