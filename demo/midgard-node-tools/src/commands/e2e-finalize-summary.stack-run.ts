import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";
import { join } from "node:path";
import { isDeepStrictEqual } from "node:util";

import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
} from "@al-ft/midgard-core/codec";

import type { DbEvidence } from "../e2e/summary.js";
import {
  parseJournal,
  readJsonIfPresent,
  type StackJournal,
} from "../full-stack/journal.js";
import {
  assertDepositCredit,
  assertTransferDeltas,
  assertWithdrawalDebit,
} from "../full-stack/journey-balances.js";
import {
  equalAssets,
  payoutConclusion,
  payoutOutRef,
  type SettlementObservation,
} from "../full-stack/payout-body.js";

/** What one stack configuration must have left in its run directory. */
export type StackRunExpectation = {
  readonly runDirectory: string;
  readonly intentDigest: string;
  /** The finalized deployment manifest's identity. */
  readonly manifestId: string;
  /** Every step id the controller runs for this configuration. */
  readonly stepIds: readonly string[];
  readonly cycles: number;
  readonly depositLovelace: string;
  readonly transferLovelace: string;
};

/**
 * The stack database's L1 facts for one withdrawal event (stack-database.ts
 * `collectStackDatabase`): its settlement job and attempts and the payout
 * token's outputs; undefined when the database holds none.
 */
export type WithdrawalOnL1 = (
  eventId: string,
) => Promise<SettlementObservation | undefined>;

export type StackRunRecords = {
  /** The journal, when it exists and belongs to this configuration. */
  readonly journal: StackJournal | undefined;
  readonly db: readonly DbEvidence[];
};

type Fields = Record<string, unknown>;

const SOURCE = "e2e-stack";

const isFields = (value: unknown): value is Fields =>
  typeof value === "object" && value !== null && !Array.isArray(value);

async function record(directory: string, name: string): Promise<Fields> {
  const value = await readJsonIfPresent(join(directory, name));
  if (value === undefined) throw new Error(`missing ${name}`);
  if (!isFields(value)) throw new Error(`malformed ${name}`);
  return value;
}

/** Balances, fees and amounts are written as non-negative decimal strings. */
function lovelace(value: unknown, field: string): bigint {
  if (typeof value !== "string" || !/^(0|[1-9][0-9]*)$/.test(value))
    throw new Error(`${field} is not an exact lovelace amount`);
  return BigInt(value);
}

function assets(value: unknown, field: string): Record<string, string> {
  if (!isFields(value)) throw new Error(`${field} is not an asset map`);
  for (const [unit, amount] of Object.entries(value))
    lovelace(amount, `${field}.${unit}`);
  return value as Record<string, string>;
}

function completeData(journal: StackJournal, id: string): Fields {
  const step = journal.steps[id];
  if (step?.status !== "complete") throw new Error(`${id} is not complete`);
  if (!isFields(step.data)) throw new Error(`${id} has no confirmed data`);
  return step.data;
}

/** journal.ts: the journal exists and belongs to this configuration. */
async function acceptedJournal(expectation: StackRunExpectation) {
  const saved = await readJsonIfPresent(
    join(expectation.runDirectory, "stack-journal.json"),
  );
  if (saved === undefined) throw new Error("missing stack-journal.json");
  return parseJournal(saved, expectation.intentDigest);
}

/** workflow.ts and controller.ts: every step this configuration runs is confirmed, and the summary was written from that journal. */
async function checkRunIdentity(
  expectation: StackRunExpectation,
  journal: StackJournal,
) {
  const directory = expectation.runDirectory;
  const recorded = Object.keys(journal.steps);
  const missing = expectation.stepIds.filter((id) => !(id in journal.steps));
  if (missing.length > 0)
    throw new Error(`journal lacks step ${missing.join(",")}`);
  const expected = new Set(expectation.stepIds);
  const extra = recorded.filter((id) => !expected.has(id));
  if (extra.length > 0)
    throw new Error(
      `journal records steps this configuration does not run: ${extra.join(",")}`,
    );
  const running = recorded.filter(
    (id) => journal.steps[id]!.status !== "complete",
  );
  if (running.length > 0)
    throw new Error(`${running.join(",")} has not been confirmed`);
  const summary = await record(directory, "journey-summary.json");
  const expectedSummary = {
    schemaVersion: "midgard-full-stack-summary-v1",
    runId: journal.runId,
    result: "wallet-journey-complete",
    network: "Preprod",
    confirmedSteps: recorded,
    journal: join(directory, "stack-journal.json"),
    services: "docker-compose",
    cycles: expectation.cycles,
  };
  for (const [field, value] of Object.entries(expectedSummary))
    if (!isDeepStrictEqual(summary[field], value))
      throw new Error(`journey-summary.json ${field} differs from the journal`);
}

/**
 * The cycle's settlement step (journey.ts), after its payout: the deposit's
 * settlement job and the transfer's merge, each observed complete.
 */
function settlementData(journal: StackJournal, prefix: string) {
  const data = completeData(journal, `${prefix}-settlement`);
  return {
    deposit: isFields(data.deposit) ? data.deposit : {},
    transfer: isFields(data.transfer) ? data.transfer : {},
  };
}

/** journey.ts deposit and settlement steps: exact event identity, exact L2 credit, one complete settlement job. */
async function checkDeposit(
  expectation: StackRunExpectation,
  journal: StackJournal,
  prefix: string,
) {
  const directory = expectation.runDirectory;
  const receipt = await record(directory, `${prefix}-deposit.json`);
  const eventId = receipt.eventId;
  if (typeof eventId !== "string" || !/^[0-9a-f]+$/.test(eventId))
    throw new Error("Deposit receipt lacks its exact event identity");
  const data = completeData(journal, `${prefix}-deposit`);
  const settled = settlementData(journal, prefix).deposit;
  if (data.eventId !== eventId || settled.eventId !== eventId)
    throw new Error("journal deposit event differs from its receipt");
  const observed = settled.observed as SettlementObservation | undefined;
  if (observed?.jobs?.length !== 1 || observed.jobs[0]!.phase !== "complete")
    throw new Error("deposit settlement was not observed complete");
  const intent = await record(directory, `${prefix}-deposit-intent.json`);
  const balance = await record(directory, `${prefix}-deposit-balance.json`);
  if (balance.before !== intent.balance)
    throw new Error("deposit balance does not start from its intent");
  if (balance.credited !== expectation.depositLovelace)
    throw new Error("deposit credit differs from the configured deposit");
  assertDepositCredit(
    lovelace(balance.before, "deposit balance before"),
    lovelace(balance.after, "deposit balance after"),
    BigInt(expectation.depositLovelace),
  );
}

/** journey.ts transfer and settlement steps and public-da.ts: exact signed transfer, its commit, then confirmed-ledger finality in the same block, unchanged public DA bytes, exact deltas. */
async function checkTransfer(
  expectation: StackRunExpectation,
  journal: StackJournal,
  prefix: string,
): Promise<string> {
  const directory = expectation.runDirectory;
  const intent = await record(directory, `${prefix}-transfer.json`);
  const txId = intent.txId;
  const signed = intent.signedTxCbor;
  if (
    typeof txId !== "string" ||
    typeof signed !== "string" ||
    !/^(?:[0-9a-f]{2})+$/.test(signed)
  )
    throw new Error("Saved transfer lacks its exact signed transaction");
  const decoded = decodeMidgardNativeTxFullFromCanonicalCbor(
    Buffer.from(signed, "hex"),
  );
  if (
    computeMidgardNativeTxId(decoded).toString("hex") !== txId ||
    decoded.body.fee !== lovelace(intent.fee, "transfer fee")
  )
    throw new Error(
      "Saved transfer differs from its exact transaction identity or fee",
    );
  const data = completeData(journal, `${prefix}-transfer`);
  const committed = isFields(data.committed) ? data.committed : {};
  const settled = settlementData(journal, prefix).transfer;
  const merged = isFields(settled.merged) ? settled.merged : {};
  if (committed.txId !== txId || merged.txId !== txId)
    throw new Error("journal transfer status belongs to another transaction");
  if (
    committed.status !== "committed" ||
    typeof committed.headerHash !== "string" ||
    committed.headerHash === "" ||
    merged.status !== "committed" ||
    merged.headerHash !== committed.headerHash
  )
    throw new Error("Transfer was not committed in a block");
  if (merged.confirmedLedgerFinalized !== true)
    throw new Error("Transfer did not reach confirmed-ledger finality");
  const da = isFields(data.da) ? data.da : {};
  const headerHash = da.headerHash;
  if (typeof headerHash !== "string" || !/^[0-9a-f]{56}$/.test(headerHash))
    throw new Error("Invalid committed header hash");
  const saved = await record(directory, `da-${headerHash}.json`);
  const digest = createHash("sha256")
    .update(await readFile(join(directory, `da-${headerHash}.cbor`)))
    .digest("hex");
  if (
    saved.sha256 !== digest ||
    saved.headerHash !== headerHash ||
    saved.deploymentId !== expectation.manifestId ||
    !isDeepStrictEqual(saved, da)
  )
    throw new Error("Saved public DA bytes changed");
  const balance = await record(directory, `${prefix}-transfer-balance.json`);
  if (
    balance.fee !== intent.fee ||
    balance.received !== expectation.transferLovelace
  )
    throw new Error("transfer balance record differs from its transfer");
  assertTransferDeltas({
    amount: BigInt(expectation.transferLovelace),
    fee: lovelace(intent.fee, "transfer fee"),
    senderBefore: lovelace(intent.senderBalanceBefore, "sender before"),
    senderAfter: lovelace(balance.senderBalance, "sender after"),
    recipientBefore: lovelace(
      intent.recipientBalanceBefore,
      "recipient before",
    ),
    recipientAfter: lovelace(balance.recipientBalance, "recipient after"),
    received: [lovelace(balance.received, "received")],
  });
  return txId;
}

/**
 * journey.ts withdrawal step, payout.ts and payout-body.ts: the transferred
 * output, the journal's payout as the transaction that concluded it on L1
 * (whichever operator sent it), its exact payout output, exact L2 debit.
 */
async function checkWithdrawal(
  expectation: StackRunExpectation,
  journal: StackJournal,
  prefix: string,
  transferTxId: string,
  withdrawalOnL1: WithdrawalOnL1,
) {
  const directory = expectation.runDirectory;
  const intent = await record(directory, `${prefix}-withdrawal-intent.json`);
  if (
    typeof intent.l2OutRef !== "string" ||
    !intent.l2OutRef.startsWith(`${transferTxId}#`) ||
    !/^(0|[1-9][0-9]*)$/.test(intent.l2OutRef.slice(transferTxId.length + 1))
  )
    throw new Error(
      "Cannot identify the exact transferred output for withdrawal",
    );
  const address = intent.address;
  if (typeof address !== "string" || address === "")
    throw new Error("withdrawal intent lacks its L1 destination");
  const withdrawn = assets(intent.assets, "withdrawal assets");
  if (withdrawn.lovelace !== expectation.transferLovelace)
    throw new Error("Recipient did not receive the exact transfer");
  const receipt = await record(directory, `${prefix}-withdrawal.json`);
  const eventId = receipt.eventId;
  if (typeof eventId !== "string" || !/^[0-9a-f]+$/.test(eventId))
    throw new Error("Noncanonical event ID");
  const payout = completeData(journal, `${prefix}-withdrawal`);
  if (payout.eventId !== eventId)
    throw new Error("journal payout belongs to another withdrawal");
  if (
    payout.address !== address ||
    !equalAssets(assets(payout.assets, "payout assets"), withdrawn)
  )
    throw new Error("payout destination or value differs from the withdrawal");
  const conclusion = payoutConclusion(
    (await withdrawalOnL1(eventId)) ?? {
      jobs: null,
      attempts: null,
    },
  );
  if (conclusion === undefined)
    throw new Error("payout is not concluded on L1");
  if (payout.txHash !== conclusion.txHash)
    throw new Error("L1 payout differs from the journal's payout");
  const { outputIndex } = payoutOutRef(conclusion, address, withdrawn);
  if (payout.outputIndex !== outputIndex)
    throw new Error("payout output index differs from the exact payout output");
  const balance = await record(directory, `${prefix}-withdrawal-balance.json`);
  if (
    balance.before !== intent.recipientBalanceBefore ||
    balance.debited !== withdrawn.lovelace
  )
    throw new Error("withdrawal balance record differs from its intent");
  assertWithdrawalDebit(
    lovelace(balance.before, "withdrawal balance before"),
    lovelace(balance.after, "withdrawal balance after"),
    lovelace(balance.debited, "withdrawal debit"),
  );
}

const message = (error: unknown) =>
  error instanceof Error ? error.message : String(error);

/** One gate per journey kind, with one detail per cycle. */
function cycleGate(
  label: string,
  outcomes: readonly (string | undefined)[],
): DbEvidence {
  const details = Object.fromEntries(
    outcomes.map((failure, cycle) => [`cycle-${cycle}`, failure ?? "verified"]),
  );
  return {
    label,
    status: outcomes.every((failure) => failure === undefined)
      ? "satisfied"
      : "failed",
    source: SOURCE,
    details,
  };
}

/**
 * Re-derives the wallet journey's functional evidence from the stack's own
 * run directory: the journal, the journey summary and each cycle's receipts.
 * Each check is the one the stack made before confirming the step, applied to
 * what it saved, so a record changed afterwards or a step never confirmed
 * fails here with the stack's own message. A payout is judged from the L1
 * facts `withdrawalOnL1` reads, as payout.ts judged it.
 */
export async function readStackRun(
  expectation: StackRunExpectation,
  withdrawalOnL1: WithdrawalOnL1,
): Promise<StackRunRecords> {
  let journal: StackJournal | undefined;
  let identityFailure: string | undefined;
  try {
    journal = await acceptedJournal(expectation);
    await checkRunIdentity(expectation, journal);
  } catch (error) {
    identityFailure = message(error);
  }
  const deposits: (string | undefined)[] = [];
  const transfers: (string | undefined)[] = [];
  const withdrawals: (string | undefined)[] = [];
  for (let cycle = 0; cycle < expectation.cycles; cycle++) {
    if (journal === undefined) {
      const unverified = "the stack journal did not verify";
      deposits.push(unverified);
      transfers.push(unverified);
      withdrawals.push(unverified);
      continue;
    }
    const prefix = `cycle-${cycle}`;
    deposits.push(
      await checkDeposit(expectation, journal, prefix).then(
        () => undefined,
        message,
      ),
    );
    let transferTxId: string | undefined;
    transfers.push(
      await checkTransfer(expectation, journal, prefix).then((txId) => {
        transferTxId = txId;
        return undefined;
      }, message),
    );
    withdrawals.push(
      transferTxId === undefined
        ? "the transfer it withdraws did not verify"
        : await checkWithdrawal(
            expectation,
            journal,
            prefix,
            transferTxId,
            withdrawalOnL1,
          ).then(() => undefined, message),
    );
  }
  return {
    journal,
    db: [
      {
        label: "stack_run_identity",
        status: identityFailure === undefined ? "satisfied" : "failed",
        source: SOURCE,
        details: {
          runDirectory: expectation.runDirectory,
          runId: journal?.runId ?? "",
          manifestId: expectation.manifestId,
          cycles: String(expectation.cycles),
          ...(identityFailure === undefined ? {} : { reason: identityFailure }),
        },
      },
      cycleGate("stack_deposit_credit", deposits),
      cycleGate("stack_transfer_finality", transfers),
      cycleGate("stack_withdrawal_payout", withdrawals),
    ],
  };
}
