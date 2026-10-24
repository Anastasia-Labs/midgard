import { isAbsolute, join } from "node:path";

import { verifyFinalizedDeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";

import type { StackRunExpectation } from "./e2e-finalize-summary.stack-run.js";

export type StackRunInputs = {
  readonly expectation: StackRunExpectation;
  /** The node endpoint the stack configuration binds. */
  readonly endpoint: string;
  /** The environment the stack gives its host node commands. */
  readonly nodeEnvironment: Readonly<Record<string, string>>;
};

/**
 * Loads a stack configuration the way `e2e-stack --check` does, which reads
 * and runs nothing that changes state. The configuration refuses anything but
 * Preprod with local Kupmios and no provider failover.
 */
export async function loadStackRunInputs(
  configPath: string,
): Promise<StackRunInputs> {
  // A package script runs from the package directory, so a relative path would not be the caller's.
  if (!isAbsolute(configPath))
    throw new Error("--stack-config must be an absolute path");
  const [
    { loadStackConfig },
    { StackProcesses },
    { readJsonIfPresent },
    deployment,
    runtime,
    { journeySteps },
  ] = await Promise.all([
    import("../full-stack/config.js"),
    import("../full-stack/process.js"),
    import("../full-stack/journal.js"),
    import("../full-stack/deployment.js"),
    import("../full-stack/runtime.js"),
    import("../full-stack/journey.js"),
  ]);
  const { config, env, intentDigest } = await loadStackConfig(configPath);
  const processes = new StackProcesses(config, env);
  const manifestPath = deployment.stackPaths(processes).manifest;
  const manifest = await readJsonIfPresent(manifestPath);
  if (manifest === undefined)
    throw new Error(`No finalized deployment manifest at ${manifestPath}`);
  const { manifestId } = verifyFinalizedDeploymentManifest(manifest);
  // The controller's own step list, so the expected journal cannot drift from it.
  const stepIds = [
    ...deployment.deploymentSteps(processes),
    ...runtime.runtimeSteps(processes),
    ...journeySteps(processes),
  ].map((step) => step.id);
  await deployment.restoreDeploymentEnvironment(processes);
  runtime.restoreRuntimeEnvironment(processes);
  return {
    expectation: {
      runDirectory: config.runDirectory,
      intentDigest,
      manifestId: manifestId as string,
      stepIds,
      cycles: config.journey.cycles,
      depositLovelace: config.journey.depositLovelace,
      transferLovelace: config.journey.transferLovelace,
    },
    endpoint: config.endpoint,
    // As StackProcesses.node: the stack's own database on its host port.
    nodeEnvironment: {
      ...processes.env,
      POSTGRES_HOST: "127.0.0.1",
      POSTGRES_PORT: env.MIDGARD_POSTGRES_HOST_PORT!,
      MIDGARD_RUN_STATE_PATH: join(
        config.runDirectory,
        "deployment-run-state.json",
      ),
    },
  };
}
