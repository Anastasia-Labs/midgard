import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import {
  type NativeInclusionTwoStepCategory,
  record,
} from "./native-inclusion-two-step.parse-artifact.js";
import { type BoundConfig } from "./native-inclusion-two-step.prepare-native-inclusion-two-step-artifact.js";
import { type FraudProofWorkflowAction } from "./orchestrator.js";
import {
  captureLocallyEvaluatedTransaction,
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary.js";

export const actionInput = ({
  category,
  action,
}: {
  readonly category: NativeInclusionTwoStepCategory;
  readonly action: FraudProofWorkflowAction;
}): Readonly<Record<string, unknown>> => {
  const input = record(action.input, `${category} workflow action`);
  if (
    input.schemaVersion !== "midgard-production-linear-family-action-v1" ||
    input.category !== category ||
    typeof input.stage !== "string"
  ) {
    throw new Error(`${category} workflow action changed identity`);
  }
  return input;
};

export const stringField = (
  input: Readonly<Record<string, unknown>>,
  field: string,
): string => {
  const value = input[field];
  if (typeof value !== "string")
    throw new Error(`workflow action omitted ${field}`);
  return value;
};

export const captureRemoval = async <
  Category extends NativeInclusionTwoStepCategory,
>({
  config,
  input,
}: {
  readonly config: BoundConfig<Category>;
  readonly input: Readonly<Record<string, unknown>>;
}) => {
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
        lucid: config.lucid,
        blueprint: config.blueprint,
        deploymentInfo: config.deploymentInfo,
        network: config.network,
        signer: config.signer,
        fraudCategory: config.category,
        fraudulentHeaderHash: config.headerHash,
        requireReferenceScripts: true,
        stateQueueMutationLeaseCoordinator: retainingCoordinator,
        fraudProverRewardLovelace: config.fraudProverRewardLovelace,
        preSubmitBoundary: async (built) => {
          if (
            !workflowTransactionInputOutRefs(built.signed).includes(
              nextRemovalOutRef,
            )
          ) {
            throw new Error(
              `${config.category} removal changed its authenticated queue input`,
            );
          }
          if (
            !workflowTransactionReferenceInputOutRefs(built.signed).includes(
              fraudProofOutRef,
            )
          ) {
            throw new Error(
              `${config.category} removal did not reference the retained proof token`,
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
