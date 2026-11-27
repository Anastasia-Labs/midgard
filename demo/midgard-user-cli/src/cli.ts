#!/usr/bin/env node
import { defaultUserCliDeps } from "./deps.js";
import { runUserCli } from "./run.js";

process.exitCode = await runUserCli(
  process.argv.slice(2),
  defaultUserCliDeps(),
);
