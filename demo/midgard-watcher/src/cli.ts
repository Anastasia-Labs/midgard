#!/usr/bin/env node

import {
  runWatcherCommand,
  WATCHER_COMMAND_FAILURE_EXIT_CODE,
  WATCHER_PACKAGE_NAME,
  type WatcherCommand,
} from "./runtime/scaffold.js";

const USAGE = `${WATCHER_PACKAGE_NAME}

Usage:
  midgard-watcher authority --config /absolute/path/authority.json
  midgard-watcher start --config /absolute/path/watcher-process.json
  midgard-watcher replay --config /absolute/path/watcher-process.json
`;

type ParsedArguments =
  | Readonly<{ kind: "command"; command: WatcherCommand; configPath: string }>
  | Readonly<{ kind: "help" }>
  | Readonly<{ kind: "invalid"; reason: string }>;

export const parseWatcherArguments = (
  arguments_: readonly string[],
): ParsedArguments => {
  if (
    arguments_.length === 1 &&
    ["--help", "-h", "help"].includes(arguments_[0]!)
  ) {
    return { kind: "help" };
  }
  const [command, flag, configPath] = arguments_;
  if (
    arguments_.length === 3 &&
    ["authority", "start", "replay"].includes(command ?? "") &&
    flag === "--config" &&
    typeof configPath === "string" &&
    configPath.length > 0
  ) {
    return {
      kind: "command",
      command: command as WatcherCommand,
      configPath,
    };
  }
  return {
    kind: "invalid",
    reason: "expected an explicit command and --config path",
  };
};

export const main = async (arguments_: readonly string[]): Promise<number> => {
  const parsed = parseWatcherArguments(arguments_);
  if (parsed.kind === "help") {
    process.stdout.write(USAGE);
    return 0;
  }
  if (parsed.kind === "invalid") {
    process.stderr.write(
      `${JSON.stringify({
        packageName: WATCHER_PACKAGE_NAME,
        state: "invalid_arguments",
        productionReady: false,
        reason: parsed.reason,
      })}\n`,
    );
    return 64;
  }
  try {
    return await runWatcherCommand(parsed.command, parsed.configPath, {
      writeOutput: (text) => process.stdout.write(text),
      writeError: (text) => process.stderr.write(text),
    });
  } catch (error) {
    process.stderr.write(
      `${JSON.stringify({
        packageName: WATCHER_PACKAGE_NAME,
        command: parsed.command,
        state: "failed_closed",
        productionReady: false,
        error:
          error instanceof Error ? error.message : "unknown production failure",
      })}\n`,
    );
    return WATCHER_COMMAND_FAILURE_EXIT_CODE;
  }
};

if (import.meta.url === `file://${process.argv[1]}`) {
  process.exitCode = await main(process.argv.slice(2));
}

export { WATCHER_COMMAND_FAILURE_EXIT_CODE };
