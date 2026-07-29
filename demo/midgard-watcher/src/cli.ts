#!/usr/bin/env node

import {
  runWatcherCommand,
  WATCHER_NOT_READY_EXIT_CODE,
  WATCHER_PACKAGE_NAME,
  type WatcherCommand,
} from "./scaffold.js";

const USAGE = `${WATCHER_PACKAGE_NAME}

Usage:
  midgard-watcher start
  midgard-watcher replay

Both commands fail closed while only the W00 foundation is implemented.
`;

type ParsedArguments =
  | Readonly<{ kind: "command"; command: WatcherCommand }>
  | Readonly<{ kind: "help" }>
  | Readonly<{ kind: "invalid"; argument: string }>;

export const parseWatcherArguments = (
  arguments_: readonly string[],
): ParsedArguments => {
  const [argument = "start"] = arguments_;

  if (argument === "--help" || argument === "-h" || argument === "help") {
    return { kind: "help" };
  }
  if (argument === "start" || argument === "replay") {
    return { kind: "command", command: argument };
  }
  return { kind: "invalid", argument };
};

export const main = (arguments_: readonly string[]): number => {
  const parsed = parseWatcherArguments(arguments_);

  if (parsed.kind === "help") {
    process.stdout.write(USAGE);
    return 0;
  }
  if (parsed.kind === "invalid") {
    process.stderr.write(
      `${JSON.stringify({
        packageName: WATCHER_PACKAGE_NAME,
        state: "invalid_command",
        productionReady: false,
        argument: parsed.argument,
      })}\n`,
    );
    return 64;
  }
  return runWatcherCommand(parsed.command, {
    writeError: (text) => process.stderr.write(text),
  });
};

if (import.meta.url === `file://${process.argv[1]}`) {
  process.exitCode = main(process.argv.slice(2));
}

export { WATCHER_NOT_READY_EXIT_CODE };
