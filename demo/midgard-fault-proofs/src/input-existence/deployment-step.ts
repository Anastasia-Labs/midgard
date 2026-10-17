/** Resolve one family's deployment, then use the common authenticated physical
 * route. Every stage consumes the same independently replayed preparation. */
import {
  resolveNonExistentInputDeploymentContracts,
  resolveNoReferenceInputDeploymentContracts,
} from "../runtime.js";
import type { InputExistenceContracts } from "./prepare-step-carriage.js";
import {
  admitInputExistenceArtifact,
  inputExistenceArtifact,
} from "./artifact.js";
import { submitInputExistenceStep } from "./submit.js";

export type InputExistenceDeploymentStepArguments = Omit<
  Parameters<typeof submitInputExistenceStep>[0],
  "contracts" | "categoryId" | "stepIndex"
> &
  Readonly<{ deploymentInfo: unknown }>;

export const createInputExistenceDeploymentStep =
  (field: 0 | 1, stepIndex: 0 | 1 | 2 | 3) =>
  async ({
    deploymentInfo,
    ...args
  }: InputExistenceDeploymentStepArguments) => {
    const prepared = admitInputExistenceArtifact(
      inputExistenceArtifact(args.prepared),
      args.signer.paymentKeyHash,
    );
    if (prepared.field !== field)
      throw new Error(
        "input-existence stage received another family's preparation",
      );
    const parameters = {
      blueprint: args.blueprint,
      deploymentInfo,
      network: args.network,
    };
    let categoryId: string;
    let contracts: InputExistenceContracts;
    if (field === 0) {
      const resolved =
        await resolveNonExistentInputDeploymentContracts(parameters);
      categoryId = resolved.nonExistentInputCategory.categoryId;
      contracts = {
        hubOraclePolicyId: resolved.hubOraclePolicyId,
        steps: resolved.contracts.nonExistentInput.steps,
        computationThread: resolved.contracts.computationThread,
        fraudProof: resolved.contracts.fraudProof,
      };
    } else {
      const resolved =
        await resolveNoReferenceInputDeploymentContracts(parameters);
      categoryId = resolved.noReferenceInputCategory.categoryId;
      contracts = {
        hubOraclePolicyId: resolved.hubOraclePolicyId,
        steps: resolved.contracts.noReferenceInput.steps,
        computationThread: resolved.contracts.computationThread,
        fraudProof: resolved.contracts.fraudProof,
      };
    }
    return submitInputExistenceStep({
      ...args,
      prepared,
      contracts,
      categoryId,
      stepIndex,
    });
  };
