#!/usr/bin/env node
import { spawn } from "node:child_process";
import { mkdir } from "node:fs/promises";
import { join, resolve } from "node:path";

import { Command } from "commander";

import { loadStackConfig } from "./full-stack/config.js";
import { readJsonIfPresent, writeDurableJson } from "./full-stack/journal.js";
import { StackProcesses } from "./full-stack/process.js";
import { runStackWorkflow } from "./full-stack/workflow.js";

async function main(options: {
  config: string;
  setupOnly?: boolean;
  check?: boolean;
}) {
  const { config, env, intentDigest } = await loadStackConfig(
    resolve(options.config),
  );
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
  await mkdir(config.runDirectory, { recursive: true, mode: 0o700 });
  const lock = join(config.nodeRoot, "logs/full-stack-controller.lock");
  await mkdir(join(config.nodeRoot, "logs"), { recursive: true, mode: 0o700 });
  if (process.env.MIDGARD_STACK_CONTROLLER_LOCK !== lock) {
    const exitCode = await new Promise<number>((resolve, reject) => {
      const child = spawn(
        "flock",
        [
          "--nonblock",
          "--no-fork",
          lock,
          process.execPath,
          ...process.argv.slice(1),
        ],
        {
          stdio: "inherit",
          env: { ...process.env, MIDGARD_STACK_CONTROLLER_LOCK: lock },
        },
      );
      child.once("error", reject);
      child.once("exit", (code, signal) =>
        signal
          ? reject(new Error(`Stack controller stopped: ${signal}`))
          : resolve(code ?? 1),
      );
    });
    if (exitCode !== 0)
      throw new Error(
        `Stack controller exited ${exitCode}; another controller may hold ${lock}`,
      );
    return;
  }
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
  const { prepareStackPrerequisites } = await import(
    "./full-stack/prerequisites.js"
  );
  console.log("prerequisites: running");
  const prerequisites = await prepareStackPrerequisites(processes);
  await writeDurableJson(
    join(config.runDirectory, "prerequisites.json"),
    prerequisites,
  );
  console.log("prerequisites: confirmed");
  await writeDurableJson(bindingPath, binding);
  const { deploymentSteps } = await import("./full-stack/deployment.js");
  const { runtimeSteps } = await import("./full-stack/runtime.js");
  const { journeySteps } = await import("./full-stack/journey.js");
  const journal = await runStackWorkflow(
    {
      directory: config.runDirectory,
      intentDigest,
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

new Command()
  .name("midgard-e2e-stack")
  .description(
    "Set up or resume the persistent local-provider Preprod stack and verify wallet journeys",
  )
  .requiredOption(
    "--config <file>",
    "Absolute or relative stack configuration JSON",
  )
  .option(
    "--setup-only",
    "Finish after setup; Compose continues supervising services",
  )
  .option(
    "--check",
    "Validate configuration without starting services or submitting transactions",
  )
  .action(async (options) => {
    try {
      const [major, minor] = process.versions.node.split(".").map(Number);
      if (major! < 22 || (major === 22 && minor! < 16))
        throw new Error("Node 22.16 or newer is required");
      await main(options);
    } catch (error) {
      console.error(
        error instanceof Error
          ? error.message
          : "Stack command failed; inspect saved artifacts",
      );
      process.exitCode = 1;
    }
  })
  .parseAsync()
  .catch(() => {
    process.exitCode = 1;
  });
