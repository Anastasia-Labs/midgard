import { isAbsolute, resolve } from "node:path";

import type { Command } from "commander";

import {
  type AcceptancePayoutReadLimits,
  sampleAcceptancePayouts,
} from "./acceptance-payout-sample.js";
import { startRunUser } from "./fresh-controller.js";
import { makeLayout } from "./layout.js";

const flags = {
  timeoutMs: "--timeout-ms",
  maxTransactionBytes: "--max-transaction-bytes",
  maxLineageTransactions: "--max-lineage-transactions",
  maxSettlementRows: "--max-settlement-rows",
  maxKupoResponseBytes: "--max-kupo-response-bytes",
  maxUtxoResponseBytes: "--max-utxo-response-bytes",
  maxReferenceInputs: "--max-reference-inputs",
  blockScanLimit: "--block-scan-limit",
  maxDrillEvidenceBytes: "--max-drill-evidence-bytes",
} as const;

/** Explicit bounds come from the activated source envelope; no implicit production defaults. */
export const registerAcceptancePayouts = (program: Command) => {
  const command = program
    .command("acceptance-payouts")
    .description(
      "Read and verify four exact canonical payouts at one fresh native boundary",
    )
    .requiredOption("--run-dir <path>", "Existing absolute run directory");
  for (const flag of Object.values(flags))
    command.requiredOption(`${flag} <integer>`, "Explicit measured read bound");
  command.action(async (options: Record<string, string>) => {
    if (!isAbsolute(options.runDir!))
      throw new Error("--run-dir must be absolute");
    const limits = Object.fromEntries(
      Object.entries(flags).map(([name, flag]) => {
        const raw = options[name];
        const value = Number(raw);
        if (
          typeof raw !== "string" ||
          !/^[1-9]\d*$/u.test(raw) ||
          !Number.isSafeInteger(value) ||
          value > 2_147_483_647
        )
          throw new Error(
            `${flag} must be a positive whole integer within Node's timer range`,
          );
        return [name, value];
      }),
    ) as AcceptancePayoutReadLimits;
    const layout = makeLayout(resolve(options.runDir!));
    startRunUser(layout, "journey");
    startRunUser(layout, "drill");
    const abort = new AbortController();
    const cancel = () => abort.abort(new Error("payout read cancelled"));
    const signals = ["SIGTERM", "SIGINT", "SIGHUP"] as const;
    for (const signal of signals) process.on(signal, cancel);
    try {
      console.log(
        JSON.stringify(
          await sampleAcceptancePayouts(layout, limits, abort.signal),
        ),
      );
    } finally {
      for (const signal of signals) process.off(signal, cancel);
    }
  });
};
