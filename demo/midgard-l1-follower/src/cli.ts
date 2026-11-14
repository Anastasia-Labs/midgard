#!/usr/bin/env node

import { runFollowerCli } from "./cli/run.js";

process.exitCode = await runFollowerCli(process.argv.slice(2), process.env, {
  stdout: (text) => process.stdout.write(text),
  stderr: (text) => process.stderr.write(text),
});
