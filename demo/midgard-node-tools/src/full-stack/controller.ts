import { mkdir, readFile } from "node:fs/promises";
import { isAbsolute, join } from "node:path";

import { isAttachedStackConfig } from "./attached-config.js";
import { loadStackConfig } from "./config.js";
import {
  assertHoldsControllerLock,
  CONTROLLER_LOCK_ENV,
  runUnderControllerLock,
} from "./controller-lock.js";
import { readJsonIfPresent, writeDurableJson } from "./journal.js";
import { stackNodeTls, trustNodeTls } from "./node-tls.js";
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
  // A second instance attaches to a running stack as another operator's node.
  const attached = isAttachedStackConfig(
    JSON.parse(await readFile(options.config, "utf8")),
  )
    ? await (
        await import("./attached-config.js")
      ).loadAttachedStackConfig(options.config)
    : undefined;
  const { config, env, intentDigest } =
    attached ?? (await loadStackConfig(options.config));
  // The one public input checked over the network, by --check and a full run alike.
  if (config.public !== undefined)
    await (
      await import("./public-inputs.js")
    ).assertRetainedDaHostResolves(config.public.retainedDaHost);
  if (options.check) {
    console.log(
      JSON.stringify({
        network: "Preprod",
        profile: env.MIDGARD_DEPLOYMENT_PROFILE,
        nodeRoot: config.nodeRoot,
        runDirectory: config.runDirectory,
        ...(attached === undefined
          ? {
              cycles: config.journey.cycles,
              setupOnly: options.setupOnly === true,
            }
          : {
              attachedTo: config.attachedTo!.runDirectory,
              admittedProducer: attached.admittedProducer,
            }),
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
  // The controller's own readiness and ledger reads reach a public node over HTTPS.
  const nodeTls = stackNodeTls(config, env);
  if (nodeTls !== undefined) trustNodeTls(nodeTls.caFile, nodeTls.servername);
  const { prepareStackPrerequisites, prepareAttachedPrerequisites } =
    await import("./prerequisites.js");
  console.log("prerequisites: running");
  const prerequisites = await (
    attached === undefined
      ? prepareStackPrerequisites
      : prepareAttachedPrerequisites
  )(processes);
  await writeDurableJson(
    join(config.runDirectory, "prerequisites.json"),
    prerequisites,
  );
  console.log("prerequisites: confirmed");
  const setupOnly = options.setupOnly === true || attached !== undefined;
  const steps =
    attached === undefined
      ? [
          ...(await import("./deployment.js")).deploymentSteps(processes),
          ...(await import("./runtime.js")).runtimeSteps(processes),
          (await import("./deployment.js")).operatorStep(processes),
          ...(setupOnly
            ? []
            : (await import("./journey.js")).journeySteps(processes)),
        ]
      : (await import("./attached.js")).attachedSteps(processes);
  const journal = await runStackWorkflow(
    {
      directory: config.runDirectory,
      intentDigest,
      // Only a controller whose journal was accepted claims the node directory.
      onJournalAccepted: () => writeDurableJson(bindingPath, binding),
      onProgress: (id, status) => console.log(`${id}: ${status}`),
    },
    steps,
  );
  const summary = {
    schemaVersion: "midgard-full-stack-summary-v1",
    runId: journal.runId,
    result: setupOnly ? "setup-complete" : "wallet-journey-complete",
    network: "Preprod",
    confirmedSteps: Object.entries(journal.steps)
      .filter(([, record]) => record.status === "complete")
      .map(([id]) => id),
    journal: join(config.runDirectory, "stack-journal.json"),
    services: "docker-compose",
    cycles: setupOnly ? 0 : config.journey.cycles,
  };
  await writeDurableJson(
    join(
      config.runDirectory,
      setupOnly ? "setup-summary.json" : "journey-summary.json",
    ),
    summary,
  );
  console.log(JSON.stringify(summary, null, 2));
}
