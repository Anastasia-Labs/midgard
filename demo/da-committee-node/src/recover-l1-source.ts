#!/usr/bin/env node
import { runL1RecoveryCommand } from "./l1/recovery-command.js";

try {
  process.exitCode = await runL1RecoveryCommand(
    process.argv.slice(2),
    (value) => {
      process.stdout.write(
        `${typeof value === "string" ? value : JSON.stringify(value)}\n`,
      );
    },
  );
} catch {
  process.stderr.write(
    "Recovery could not complete its configured evidence and exclusive store operation; inspect configuration and stop/join the held daemon. Run inspect to determine durable status; no reset or effect retry was performed.\n",
  );
  process.exitCode = 78;
}
