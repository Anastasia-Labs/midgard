import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import type { LucidEvolution, Network } from "@lucid-evolution/lucid";

import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FraudProofWorkflowDeploymentBindingV1 } from "./deployment-manifest-binding-v1.js";
import type { FraudProofWorkflowActionV1 } from "./orchestrator-v1.js";
import { PRODUCTION_CURSOR_FAMILY_ACTION_V1 } from "./production-cursor-family-state-v1.js";
import {
  captureLocallyEvaluatedTransactionV1,
  workflowTransactionInputOutRefsV1,
  workflowTransactionReferenceInputOutRefsV1,
} from "./transaction-boundary-v1.js";

export type ProductionCursorFamilyActionInputV1 = Readonly<
  Record<string, unknown>
> &
  Readonly<{ stage: string }>;

export const productionCursorFamilyActionInputV1 = <
  Category extends FraudProofCatalogueCategoryName,
>({
  category,
  action,
}: {
  readonly category: Category;
  readonly action: FraudProofWorkflowActionV1;
}): ProductionCursorFamilyActionInputV1 => {
  const input = action.input;
  if (
    input.schemaVersion !== PRODUCTION_CURSOR_FAMILY_ACTION_V1 ||
    input.category !== category ||
    typeof input.stage !== "string"
  ) {
    throw new Error(`${category} cursor action changed identity`);
  }
  return input as ProductionCursorFamilyActionInputV1;
};

export const productionCursorStringFieldV1 = (
  input: Readonly<Record<string, unknown>>,
  field: string,
): string => {
  const value = input[field];
  if (typeof value !== "string") {
    throw new Error(`production cursor action omitted ${field}`);
  }
  return value;
};

export const captureProductionCursorRemovalV1 = async <
  Category extends FraudProofCatalogueCategoryName,
>({
  category,
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  headerHash,
  input,
  stateQueueMutationLeaseCoordinator,
  fraudProverRewardLovelace,
}: {
  readonly category: Category;
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly headerHash: string;
  readonly input: ProductionCursorFamilyActionInputV1;
  readonly stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  readonly fraudProverRewardLovelace: bigint;
}): Promise<
  Readonly<{
    transaction: Awaited<
      ReturnType<typeof captureLocallyEvaluatedTransactionV1>
    >;
    mutationLease?: StateQueueMutationLease;
  }>
> => {
  let mutationLease: StateQueueMutationLease | undefined;
  const retainingCoordinator: StateQueueMutationLeaseCoordinator = {
    acquire: async () => {
      const acquired = await stateQueueMutationLeaseCoordinator.acquire();
      mutationLease = acquired;
      return acquired;
    },
  };
  const nextRemovalOutRef = productionCursorStringFieldV1(
    input,
    "nextRemovalOutRef",
  );
  const fraudProofOutRef = productionCursorStringFieldV1(
    input,
    "fraudProofOutRef",
  );
  const transaction = await captureLocallyEvaluatedTransactionV1(
    async (boundary) => {
      await submitRemoveFraudulentBlock({
        lucid,
        blueprint,
        deploymentInfo,
        network,
        signer,
        fraudCategory: category,
        fraudulentHeaderHash: headerHash,
        requireReferenceScripts: true,
        stateQueueMutationLeaseCoordinator: retainingCoordinator,
        fraudProverRewardLovelace,
        preSubmitBoundary: async (built) => {
          if (
            !workflowTransactionInputOutRefsV1(built.signed).includes(
              nextRemovalOutRef,
            ) ||
            !workflowTransactionReferenceInputOutRefsV1(built.signed).includes(
              fraudProofOutRef,
            )
          ) {
            throw new Error(
              `${category} removal changed its authenticated inputs`,
            );
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

export type CursorFamilyBoundBaseV1<
  Category extends FraudProofCatalogueCategoryName,
> = Readonly<{
  binding: FraudProofWorkflowDeploymentBindingV1<Category>;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;
