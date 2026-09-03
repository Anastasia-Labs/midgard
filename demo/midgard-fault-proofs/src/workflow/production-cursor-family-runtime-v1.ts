import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import type { LucidEvolution, Network } from "@lucid-evolution/lucid";

import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { FraudProofWorkflowDeploymentBinding } from "./deployment-manifest-binding-v1.js";
import type { FraudProofWorkflowAction } from "./orchestrator-v1.js";
import { CURSOR_FAMILY_ACTION } from "./production-cursor-family-state-v1.js";
import {
  captureLocallyEvaluatedTransaction,
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary-v1.js";

export type CursorFamilyActionInput = Readonly<Record<string, unknown>> &
  Readonly<{ stage: string }>;

export const cursorFamilyActionInput = <
  Category extends FraudProofCatalogueCategoryName,
>({
  category,
  action,
}: {
  readonly category: Category;
  readonly action: FraudProofWorkflowAction;
}): CursorFamilyActionInput => {
  const input = action.input;
  if (
    input.schemaVersion !== CURSOR_FAMILY_ACTION ||
    input.category !== category ||
    typeof input.stage !== "string"
  ) {
    throw new Error(`${category} cursor action changed identity`);
  }
  return input as CursorFamilyActionInput;
};

export const cursorStringField = (
  input: Readonly<Record<string, unknown>>,
  field: string,
): string => {
  const value = input[field];
  if (typeof value !== "string") {
    throw new Error(`production cursor action omitted ${field}`);
  }
  return value;
};

export const captureCursorRemoval = async <
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
  readonly input: CursorFamilyActionInput;
  readonly stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  readonly fraudProverRewardLovelace: bigint;
}): Promise<
  Readonly<{
    transaction: Awaited<ReturnType<typeof captureLocallyEvaluatedTransaction>>;
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
  const nextRemovalOutRef = cursorStringField(input, "nextRemovalOutRef");
  const fraudProofOutRef = cursorStringField(input, "fraudProofOutRef");
  const transaction = await captureLocallyEvaluatedTransaction(
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
            !workflowTransactionInputOutRefs(built.signed).includes(
              nextRemovalOutRef,
            ) ||
            !workflowTransactionReferenceInputOutRefs(built.signed).includes(
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

export type CursorFamilyBoundBase<
  Category extends FraudProofCatalogueCategoryName,
> = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<Category>;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;
