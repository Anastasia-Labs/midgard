import type { Command } from "commander";

import { runFinalAcceptance } from "./acceptance.js";
import type { DeployContext } from "./deploy.js";
import type { FundingRecord } from "./funding.js";
import { Journal } from "./journal.js";
import type { HubOracleOneShot } from "./node-env.js";
import { provisionReserveFloat } from "./reserve-float-chain.js";

/** The caller acquires both current-code guarded run locks before wiring. */
export const registerFinalAcceptance = (
  program: Command,
  wire: (runDir: string) => {
    context: DeployContext;
    oneShot: HubOracleOneShot;
  },
) =>
  program
    .command("acceptance")
    .description(
      "Run the fresh full journey and twelve finite drills; exact canonical payout verification remains required",
    )
    .requiredOption("--run-dir <path>", "Absolute run directory")
    .requiredOption(
      "--deadline <seconds>",
      "Finite injection/journey deadline; cancellation joins owned restoration",
    )
    .action(async (options: { runDir: string; deadline: string }) => {
      const deadlineMs = Number(options.deadline) * 1000;
      if (
        !/^[1-9]\d*$/u.test(options.deadline) ||
        !Number.isSafeInteger(deadlineMs) ||
        deadlineMs > 2_147_483_647
      )
        throw new Error(
          "--deadline must be a positive whole number within Node's timer range",
        );
      const { context, oneShot } = wire(options.runDir);
      const journal = new Journal(context.layout.journal);
      const funding = journal.get<FundingRecord>("funding");
      if (funding === undefined)
        throw new Error("the run has no funding record");
      const abort = new AbortController();
      const cancel = () => abort.abort();
      const signals = ["SIGTERM", "SIGINT", "SIGHUP"] as const;
      for (const signal of signals) process.on(signal, cancel);
      try {
        await provisionReserveFloat(
          context.layout,
          context.run,
          journal,
          undefined,
          abort.signal,
        );
        const receipt = await runFinalAcceptance(
          context,
          oneShot,
          funding.assets,
          { deadlineMs, signal: abort.signal },
        );
        console.log(
          JSON.stringify({ ...receipt, exactPayoutVerificationRequired: true }),
        );
      } finally {
        for (const signal of signals) process.off(signal, cancel);
      }
    });
