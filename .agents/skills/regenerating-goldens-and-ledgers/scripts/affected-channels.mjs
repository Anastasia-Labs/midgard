#!/usr/bin/env node
// Map changed files to the generated-artifact channels they make stale, and
// print the check and sync commands for each. The channel table lives next to
// this file in channels.json; `--verify-table` proves the table still matches
// the tree (every generator, ledger and generated file is claimed, every
// declared path, script, command and CI step exists).
//
// Exit codes:
//   0  looked; the report lists the affected channels, or says none are
//   1  looked and found problems (an unclaimed generator or generated file,
//      or a table that no longer matches the tree)
//   2  could not look (bad arguments, unreadable table, git diff or
//      git ls-files failed, unknown base ref)

import "./affected-channels.channel-triggers.mjs";
import "node:child_process";
import "node:fs";
import "node:path";
import "node:url";
import "./affected-channels.parse-arguments.mjs";
import "./affected-channels.channel-triggers.mjs";
import "./affected-channels.verify-command.mjs";
import "./affected-channels.verify-table.mjs";
import "./affected-channels.main.mjs";

import { resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { main } from "./affected-channels.main.mjs";

if (
  process.argv[1] &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url)
) {
  process.exitCode = main(process.argv.slice(2));
}
export {
  changedFiles,
  channelTriggers,
  looksLikeGenerator,
  unclaimed,
} from "./affected-channels.channel-triggers.mjs";
export { main } from "./affected-channels.main.mjs";
export {
  createTree,
  globToRegExp,
  ledgerModules,
  loadTable,
  matchesGlob,
  parseArguments,
  producerClosure,
  resolveAikenModule,
} from "./affected-channels.parse-arguments.mjs";
export {
  affectedChannels,
  verifyCommand,
  workflowStep,
} from "./affected-channels.verify-command.mjs";
export { verifyTable } from "./affected-channels.verify-table.mjs";
