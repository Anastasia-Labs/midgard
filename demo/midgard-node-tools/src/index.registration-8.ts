import "./index.registration-7.js";

import {
  collectStringOption,
  failCli,
  parseE2EEnvInheritanceOption,
  parsePositiveIntegerOption,
  parseStringListOption,
  writeJson,
} from "midgard-node/commands/cli-runtime";
import { parseEnvOverrides } from "midgard-node/e2e/env";

import { runJournalKillRecoveryAcceptance } from "./commands/e2e-journal-kill-recovery-acceptance.js";
import * as E2EProcessCleanupCommand from "./commands/e2e-process-cleanup.js";
import * as E2EServiceCommand from "./commands/e2e-service.js";
import { program } from "./index.registration.js";

program
  .command("e2e-clean-owned-process-group")
  .description(
    "Fail-closed cleanup of a detached e2e process group after validating its durable /proc ownership record",
  )
  .requiredOption("--record <path>", "Owned process-group record path")
  .requiredOption(
    "--run-token-env <name>",
    "Environment variable containing the private run token",
  )
  .action(
    async (options: {
      readonly record: string;
      readonly runTokenEnv: string;
    }) => {
      try {
        const result =
          await E2EProcessCleanupCommand.cleanupOwnedProcessGroupFromEnv({
            recordPath: options.record,
            runTokenEnv: options.runTokenEnv,
          });
        writeJson(result);
        if (!result.success) process.exitCode = 1;
      } catch (error) {
        failCli("e2e-clean-owned-process-group", error);
      }
    },
  );

program
  .command("e2e-journal-kill-recovery-acceptance")
  .description(
    "Run the operator-enabled Phase 4 journal-before-submit SIGKILL recovery acceptance (two nodes, default commit path) against a matched local-devnet snapshot",
  )
  .action(async () => {
    try {
      console.log(
        JSON.stringify(await runJournalKillRecoveryAcceptance(), null, 2),
      );
    } catch (error) {
      failCli("e2e-journal-kill-recovery-acceptance", error);
    }
  });

program
  .command("e2e-start-service")
  .description(
    "Start a long-running e2e service, write a PID file, and wait for readiness",
  )
  .requiredOption("--service <name>", "Service label")
  .requiredOption("--cwd <path>", "Working directory")
  .requiredOption("--raw-log <path>", "Raw log path")
  .requiredOption("--pid-file <path>", "PID file path")
  .requiredOption("--ready-url <url>", "Readiness endpoint URL")
  .option("--health-url <url>", "Optional health endpoint URL")
  .option("--ready-timeout-ms <ms>", "Readiness timeout", "120000")
  .option("--poll-interval-ms <ms>", "Readiness polling interval", "5000")
  .option(
    "--env-file <path>",
    "Dotenv-compatible env file to apply before explicit --env overrides; repeatable",
    collectStringOption,
    [],
  )
  .option(
    "--env <KEY=VALUE>",
    "Explicit environment override; repeatable and applied after --env-file",
    collectStringOption,
    [],
  )
  .option(
    "--env-inheritance <mode>",
    "Environment inheritance mode: process or none",
    "process",
  )
  .argument("<command>", "Command to execute")
  .argument("[args...]", "Command arguments")
  .action(async (command, args, opts) => {
    try {
      const summary = await E2EServiceCommand.startManagedService({
        service: opts.service,
        command,
        args,
        cwd: opts.cwd,
        envFiles: parseStringListOption(opts.envFile, "--env-file"),
        env: parseEnvOverrides(parseStringListOption(opts.env, "--env")),
        envInheritance: parseE2EEnvInheritanceOption(opts.envInheritance),
        rawLogPath: opts.rawLog,
        pidFilePath: opts.pidFile,
        readyUrl: opts.readyUrl,
        ...(typeof opts.healthUrl === "string"
          ? { healthUrl: opts.healthUrl }
          : {}),
        readyTimeoutMs: parsePositiveIntegerOption(
          opts.readyTimeoutMs,
          "--ready-timeout-ms",
        ),
        pollIntervalMs: parsePositiveIntegerOption(
          opts.pollIntervalMs,
          "--poll-interval-ms",
        ),
      });
      writeJson(summary);
    } catch (error) {
      failCli("e2e-start-service", error);
    }
  });

program
  .command("e2e-stack")
  .description(
    "Set up or resume the persistent local-provider Preprod stack and verify wallet journeys",
  )
  .requiredOption(
    "--config <file>",
    "Absolute path of the stack configuration JSON",
  )
  .option(
    "--setup-only",
    "Finish after setup; Compose continues supervising services",
  )
  .option(
    "--check",
    "Run the offline configuration checks only: no build, service start or transaction",
  )
  .action(
    async (options: {
      config: string;
      setupOnly?: boolean;
      check?: boolean;
    }) => {
      try {
        const [major, minor] = process.versions.node.split(".").map(Number);
        if (major! < 22 || (major === 22 && minor! < 16))
          throw new Error("Node 22.16 or newer is required");
        const { runStackController } = await import(
          "./full-stack/controller.js"
        );
        await runStackController(options);
      } catch (error) {
        console.error(
          error instanceof Error
            ? error.message
            : "Stack command failed; inspect saved artifacts",
        );
        process.exitCode = 1;
      }
    },
  );

program.parse(process.argv);
