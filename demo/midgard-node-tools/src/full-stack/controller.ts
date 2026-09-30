import { mkdir } from "node:fs/promises";
import { isAbsolute, join } from "node:path";

import { loadStackConfig } from "./config.js";
import {
  assertHoldsControllerLock,
  CONTROLLER_LOCK_ENV,
  runUnderControllerLock,
} from "./controller-lock.js";
import { readJsonIfPresent, writeDurableJson } from "./journal.js";
import { StackProcesses } from "./process.js";
import { runStackWorkflow } from "./workflow.js";

/** The `e2e-stack` command: sets up or resumes the Preprod stack under one controller lock. */
export async function runStackController(options: {
  config: string;
  setupOnly?: boolean;
  check?: boolean;
}) {
  // A package script runs from the package directory, so a relative path would not be the caller's.
  if (!isAbsolute(options.config))
    throw new Error("--config must be an absolute path");
  const { config, env, intentDigest } = await loadStackConfig(options.config);
  if (options.check) {
    console.log(
      JSON.stringify({
        network: "Preprod",
        nodeRoot: config.nodeRoot,
        runDirectory: config.runDirectory,
        cycles: config.journey.cycles,
        setupOnly: options.setupOnly === true,
      }),
    );
    return;
  }
  process.umask(0o077);
  const lock = join(config.nodeRoot, "logs/full-stack-controller.lock");
  await mkdir(join(config.nodeRoot, "logs"), { recursive: true, mode: 0o700 });
  if (process.env[CONTROLLER_LOCK_ENV] !== lock)
    return runUnderControllerLock(lock, process.argv.slice(1), process.env);
  await assertHoldsControllerLock(lock);
  await mkdir(config.runDirectory, { recursive: true, mode: 0o700 });
  const bindingPath = join(
    config.nodeRoot,
    "deploymentInfo/full-stack-intent.json",
  );
  const binding = { runDirectory: config.runDirectory, intentDigest };
  const saved = await readJsonIfPresent(bindingPath);
  if (saved !== undefined && JSON.stringify(saved) !== JSON.stringify(binding))
    throw new Error(
      "This node directory already belongs to another stack configuration; preserve its deployment and records",
    );
  const processes = new StackProcesses(config, env);
  const { prepareStackPrerequisites } = await import("./prerequisites.js");
  console.log("prerequisites: running");
  const prerequisites = await prepareStackPrerequisites(processes);
  await writeDurableJson(
    join(config.runDirectory, "prerequisites.json"),
    prerequisites,
  );
  console.log("prerequisites: confirmed");
  const { deploymentSteps } = await import("./deployment.js");
  const { runtimeSteps } = await import("./runtime.js");
  const { journeySteps } = await import("./journey.js");
  const journal = await runStackWorkflow(
    {
      directory: config.runDirectory,
      intentDigest,
      // Only a controller whose journal was accepted claims the node directory.
      onJournalAccepted: () => writeDurableJson(bindingPath, binding),
      onProgress: (id, status) => console.log(`${id}: ${status}`),
    },
    [
      ...deploymentSteps(processes),
      ...runtimeSteps(processes),
      ...(options.setupOnly ? [] : journeySteps(processes)),
    ],
  );
  const summary = {
    schemaVersion: "midgard-full-stack-summary-v1",
    runId: journal.runId,
    result: options.setupOnly ? "setup-complete" : "wallet-journey-complete",
    network: "Preprod",
    confirmedSteps: Object.entries(journal.steps)
      .filter(([, record]) => record.status === "complete")
      .map(([id]) => id),
    journal: join(config.runDirectory, "stack-journal.json"),
    services: "docker-compose",
    cycles: options.setupOnly ? 0 : config.journey.cycles,
  };
  await writeDurableJson(
    join(
      config.runDirectory,
      options.setupOnly ? "setup-summary.json" : "journey-summary.json",
    ),
    summary,
  );
  console.log(JSON.stringify(summary, null, 2));
}
