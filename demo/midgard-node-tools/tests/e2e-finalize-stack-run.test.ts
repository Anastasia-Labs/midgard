import { appendFile, rm } from "node:fs/promises";
import { join } from "node:path";

import { describe, expect, it } from "vitest";

import { readStackRun } from "../src/commands/e2e-finalize-summary.js";
import type { DbEvidence } from "../src/e2e/summary.js";
import {
  HEADER_HASH,
  honestRun,
  payoutTransaction,
  STEP_IDS,
} from "./e2e-finalize-stack-run.fixture.js";

const gate = (db: readonly DbEvidence[], label: string) => {
  const found = db.find((entry) => entry.label === label);
  if (found === undefined) throw new Error(`no ${label} gate`);
  return found;
};

/** Runs the reader and returns the one gate expected to refuse, with its reason. */
async function refusal(
  run: Awaited<ReturnType<typeof honestRun>>,
  label: string,
) {
  const { db } = await readStackRun(run.expectation);
  const refused = gate(db, label);
  expect(refused.status).toBe("failed");
  for (const other of db)
    if (other.label !== label && label !== "stack_transfer_finality")
      expect(other.status, other.label).toBe("satisfied");
  return label === "stack_run_identity"
    ? refused.details.reason
    : refused.details["cycle-0"];
}

describe("e2e-finalize-summary stack-run reader", () => {
  it("verifies an honest wallet-journey run from its records", async () => {
    const run = await honestRun();
    const { journal, db } = await readStackRun(run.expectation);
    expect(journal?.runId).toBe("stack-run-honest");
    expect(db.map(({ label, status }) => [label, status])).toEqual([
      ["stack_run_identity", "satisfied"],
      ["stack_deposit_credit", "satisfied"],
      ["stack_transfer_finality", "satisfied"],
      ["stack_withdrawal_payout", "satisfied"],
    ]);
    expect(gate(db, "stack_withdrawal_payout").details).toEqual({
      "cycle-0": "verified",
    });
  });

  describe("run identity (journal.ts, workflow.ts, controller.ts)", () => {
    it("refuses a missing journal and verifies no cycle", async () => {
      const run = await honestRun();
      await rm(join(run.expectation.runDirectory, "stack-journal.json"));
      const { journal, db } = await readStackRun(run.expectation);
      expect(journal).toBeUndefined();
      expect(gate(db, "stack_run_identity").details.reason).toBe(
        "missing stack-journal.json",
      );
      for (const label of [
        "stack_deposit_credit",
        "stack_transfer_finality",
        "stack_withdrawal_payout",
      ])
        expect(gate(db, label)).toMatchObject({
          status: "failed",
          details: { "cycle-0": "the stack journal did not verify" },
        });
    });
    it("refuses a journal whose identity digest belongs to another configuration", async () => {
      const run = await honestRun();
      await run.edit("stack-journal.json", (journal) => {
        journal.intentDigest = "f".repeat(64);
      });
      const { journal, db } = await readStackRun(run.expectation);
      expect(journal).toBeUndefined();
      expect(gate(db, "stack_run_identity").details.reason).toBe(
        "Saved stack identity differs from this configuration; preserve the run directory",
      );
    });
    it("refuses a journal that lacks a step, adds one, or left one running", async () => {
      const lacks = await honestRun();
      await lacks.edit("stack-journal.json", (journal) => {
        delete journal.steps.services;
      });
      expect(await refusal(lacks, "stack_run_identity")).toBe(
        "journal lacks step services",
      );
      const extra = await honestRun();
      await extra.edit("stack-journal.json", (journal) => {
        journal.steps["cycle-1-deposit"] = {
          status: "complete",
          attempts: 1,
          data: null,
        };
      });
      expect(await refusal(extra, "stack_run_identity")).toBe(
        "journal records steps this configuration does not run: cycle-1-deposit",
      );
      const running = await honestRun();
      await running.edit("stack-journal.json", (journal) => {
        journal.steps.services.status = "running";
      });
      expect(await refusal(running, "stack_run_identity")).toBe(
        "services has not been confirmed",
      );
    });
    it("refuses a journey summary not written from this journal", async () => {
      const cycles = await honestRun();
      await cycles.edit("journey-summary.json", (summary) => {
        summary.cycles = 2;
      });
      expect(await refusal(cycles, "stack_run_identity")).toBe(
        "journey-summary.json cycles differs from the journal",
      );
      const steps = await honestRun();
      await steps.edit("journey-summary.json", (summary) => {
        summary.confirmedSteps = STEP_IDS.slice(1);
      });
      expect(await refusal(steps, "stack_run_identity")).toBe(
        "journey-summary.json confirmedSteps differs from the journal",
      );
    });
  });

  describe("deposit (journey.ts deposit step)", () => {
    it("refuses a credit other than the configured deposit", async () => {
      const run = await honestRun();
      await run.edit("cycle-0-deposit-balance.json", (balance) => {
        balance.after = String(BigInt(balance.after) - 1n);
      });
      expect(await refusal(run, "stack_deposit_credit")).toBe(
        "Deposit did not credit the exact L2 balance",
      );
    });
    it("refuses a journal deposit event that is not the receipt's", async () => {
      const run = await honestRun();
      await run.editStep("cycle-0-deposit", (data) => {
        data.eventId = "0d".repeat(32);
      });
      expect(await refusal(run, "stack_deposit_credit")).toBe(
        "journal deposit event differs from its receipt",
      );
    });
    it("refuses a deposit whose settlement was not observed complete", async () => {
      const run = await honestRun();
      await run.editStep("cycle-0-deposit", (data) => {
        data.observed.jobs = [{ phase: "complete" }, { phase: "complete" }];
      });
      expect(await refusal(run, "stack_deposit_credit")).toBe(
        "deposit settlement was not observed complete",
      );
    });
  });

  describe("transfer (journey.ts transfer step, public-da.ts)", () => {
    it("refuses a saved transfer whose fee was changed", async () => {
      const run = await honestRun();
      await run.edit("cycle-0-transfer.json", (transfer) => {
        transfer.fee = String(BigInt(transfer.fee) + 1n);
      });
      expect(await refusal(run, "stack_transfer_finality")).toBe(
        "Saved transfer differs from its exact transaction identity or fee",
      );
    });
    it("refuses a transfer that did not reach confirmed-ledger finality", async () => {
      const run = await honestRun();
      await run.editStep("cycle-0-transfer", (data) => {
        data.merged.confirmedLedgerFinalized = false;
      });
      const { db } = await readStackRun(run.expectation);
      expect(gate(db, "stack_transfer_finality").details["cycle-0"]).toBe(
        "Transfer did not reach confirmed-ledger finality",
      );
      // The withdrawal spends the transfer's output, so it cannot verify either.
      expect(gate(db, "stack_withdrawal_payout").details["cycle-0"]).toBe(
        "the transfer it withdraws did not verify",
      );
    });
    it("refuses public DA bytes changed after they were saved", async () => {
      const run = await honestRun();
      await appendFile(
        join(run.expectation.runDirectory, `da-${HEADER_HASH}.cbor`),
        "x",
      );
      expect(await refusal(run, "stack_transfer_finality")).toBe(
        "Saved public DA bytes changed",
      );
    });
    it("refuses a recipient balance other than the exact transfer", async () => {
      const run = await honestRun();
      await run.edit("cycle-0-transfer-balance.json", (balance) => {
        balance.recipientBalance = String(
          BigInt(balance.recipientBalance) + 1n,
        );
      });
      expect(await refusal(run, "stack_transfer_finality")).toBe(
        "Recipient L2 balance does not match the transfer",
      );
    });
  });

  describe("withdrawal and automatic payout (journey.ts, payout.ts, payout-body.ts)", () => {
    it("refuses a run whose payout record is missing", async () => {
      const run = await honestRun();
      await rm(join(run.expectation.runDirectory, "cycle-0-withdrawal.json"));
      expect(await refusal(run, "stack_withdrawal_payout")).toBe(
        "missing cycle-0-withdrawal.json",
      );
    });
    it("refuses a payout Cardano has not included", async () => {
      const run = await honestRun();
      await run.editStep("cycle-0-withdrawal", (data) => {
        data.observation.status = "pending";
      });
      expect(await refusal(run, "stack_withdrawal_payout")).toBe(
        "payout was not observed included on Cardano",
      );
    });
    it("refuses a journal payout of another value", async () => {
      const run = await honestRun();
      await run.editStep("cycle-0-withdrawal", (data) => {
        data.assets = { lovelace: "9999999" };
      });
      expect(await refusal(run, "stack_withdrawal_payout")).toBe(
        "payout destination or value differs from the withdrawal",
      );
    });
    it("refuses a payout transaction that pays another value", async () => {
      const run = await honestRun();
      const other = payoutTransaction([
        { address: run.recipientAddress, lovelace: 9_999_999n },
      ]);
      await run.editStep("cycle-0-withdrawal", (data) => {
        data.txHash = other.txHash;
        data.observation.transactionHash = other.txHash;
        data.observation.signedTransactionCborHex = other.cbor;
      });
      expect(await refusal(run, "stack_withdrawal_payout")).toBe(
        "Payout must contain exactly one output with the exact destination and value",
      );
    });
    it("refuses payout bytes that are not the journal's transaction", async () => {
      const run = await honestRun();
      await run.editStep("cycle-0-withdrawal", (data) => {
        data.txHash = "0f".repeat(32);
        data.observation.transactionHash = "0f".repeat(32);
      });
      expect(await refusal(run, "stack_withdrawal_payout")).toBe(
        "Workflow recovery signed bytes differ from their durable transaction identity",
      );
    });
    it("refuses a payout output index that is not the exact output", async () => {
      const run = await honestRun();
      await run.editStep("cycle-0-withdrawal", (data) => {
        data.outputIndex = 1;
      });
      expect(await refusal(run, "stack_withdrawal_payout")).toBe(
        "payout output index differs from the exact payout output",
      );
    });
    it("refuses a withdrawal of an output the transfer did not create", async () => {
      const run = await honestRun();
      await run.edit("cycle-0-withdrawal-intent.json", (intent) => {
        intent.l2OutRef = `${"45".repeat(32)}#0`;
      });
      expect(await refusal(run, "stack_withdrawal_payout")).toBe(
        "Cannot identify the exact transferred output for withdrawal",
      );
    });
    it("refuses an L2 debit other than the withdrawn value", async () => {
      const run = await honestRun();
      await run.edit("cycle-0-withdrawal-balance.json", (balance) => {
        balance.after = "1";
      });
      expect(await refusal(run, "stack_withdrawal_payout")).toBe(
        "Withdrawal did not debit the exact recipient L2 balance",
      );
    });
  });

  type Tamper = (run: Awaited<ReturnType<typeof honestRun>>) => Promise<void>;
  // Each record the stack saved must agree with the others exactly.
  const inconsistent: readonly [string, string, Tamper][] = [
    [
      "stack_deposit_credit",
      "Deposit receipt lacks its exact event identity",
      (run) =>
        run.edit("cycle-0-deposit.json", (receipt) => {
          receipt.metadata.depositEventId = "DE";
        }),
    ],
    [
      "stack_deposit_credit",
      "deposit balance does not start from its intent",
      (run) =>
        run.edit("cycle-0-deposit-intent.json", (intent) => {
          intent.balance = "1";
        }),
    ],
    [
      "stack_deposit_credit",
      "deposit credit differs from the configured deposit",
      (run) =>
        run.edit("cycle-0-deposit-balance.json", (balance) => {
          balance.credited = "1";
        }),
    ],
    [
      "stack_transfer_finality",
      "Saved transfer lacks its exact signed transaction",
      (run) =>
        run.edit("cycle-0-transfer.json", (transfer) => {
          transfer.signedTxCbor = "zz";
        }),
    ],
    [
      "stack_transfer_finality",
      "journal transfer status belongs to another transaction",
      (run) =>
        run.editStep("cycle-0-transfer", (data) => {
          data.merged.txId = "00".repeat(32);
        }),
    ],
    [
      "stack_transfer_finality",
      "Transfer was not committed in a block",
      (run) =>
        run.editStep("cycle-0-transfer", (data) => {
          data.merged.status = "pending";
        }),
    ],
    [
      "stack_transfer_finality",
      "Invalid committed header hash",
      (run) =>
        run.editStep("cycle-0-transfer", (data) => {
          data.da.headerHash = "cd";
        }),
    ],
    [
      "stack_transfer_finality",
      "transfer balance record differs from its transfer",
      (run) =>
        run.edit("cycle-0-transfer-balance.json", (balance) => {
          balance.received = "1";
        }),
    ],
    [
      "stack_withdrawal_payout",
      "withdrawal intent lacks its L1 destination",
      (run) =>
        run.edit("cycle-0-withdrawal-intent.json", (intent) => {
          delete intent.address;
        }),
    ],
    [
      "stack_withdrawal_payout",
      "Recipient did not receive the exact transfer",
      (run) =>
        run.edit("cycle-0-withdrawal-intent.json", (intent) => {
          intent.assets = { lovelace: "9999999" };
        }),
    ],
    [
      "stack_withdrawal_payout",
      "Noncanonical event ID",
      (run) =>
        run.edit("cycle-0-withdrawal.json", (receipt) => {
          receipt.withdrawalEventId = "EE";
        }),
    ],
    [
      "stack_withdrawal_payout",
      "journal payout belongs to another withdrawal",
      (run) =>
        run.editStep("cycle-0-withdrawal", (data) => {
          data.eventId = "0e".repeat(32);
        }),
    ],
    [
      "stack_withdrawal_payout",
      "withdrawal balance record differs from its intent",
      (run) =>
        run.edit("cycle-0-withdrawal-balance.json", (balance) => {
          balance.debited = "1";
        }),
    ],
  ];
  it.each(inconsistent)(
    "refuses inconsistent records at %s: %s",
    async (label, reason, tamper) => {
      const run = await honestRun();
      await tamper(run);
      const { db } = await readStackRun(run.expectation);
      expect(gate(db, label).details["cycle-0"]).toBe(reason);
    },
  );
});
