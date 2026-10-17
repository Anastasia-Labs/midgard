import { type DeploymentRunState } from "./run-state.deployment-run-identity.js";
import {
  loadDeploymentRunState,
  withDeploymentRunStateLock,
  writeDeploymentRunStateAtomic,
} from "./run-state.parse-deployment-run-state.js";

export const mutateDeploymentRunState = async (
  path: string,
  createInitial: () => DeploymentRunState,
  mutate: (
    state: DeploymentRunState,
  ) => DeploymentRunState | Promise<DeploymentRunState>,
): Promise<DeploymentRunState> =>
  withDeploymentRunStateLock(path, async () => {
    const current = (await loadDeploymentRunState(path)) ?? createInitial();
    const next = await mutate(current);
    await writeDeploymentRunStateAtomic(path, next);
    return next;
  });
