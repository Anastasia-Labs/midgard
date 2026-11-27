import { describe, expect, it } from "vitest";

import {
  awaitStackLedgerOwnerFunded,
  type StackLedgerOwnerDeposit,
  stackLedgerOwnerDepositStatus,
} from "./stack-ledger-owner.js";

const POLL_MS = 30_000;
const ABSORB_WITHIN_MS = 600_000;
const DEPOSIT_VALIDITY_MS = 300_000;

const txHash = (n: number) => n.toString(16).padStart(64, "0");

/**
 * An attached stack on a clock that moves only while the drill pauses. A
 * deposit lands `landsAfterMs` after it is submitted, unless `dropped`, and
 * expires unlanded past its validity; the stack's node absorbs a landed
 * deposit `absorbsAfterMs` after the deposit's inclusion time. `pending` is
 * a deposit an earlier run recorded, which landed when `pendingLanded`.
 */
const stack = (input: {
  funded?: boolean;
  pending?: StackLedgerOwnerDeposit;
  pendingLanded?: boolean;
  landsAfterMs?: number;
  dropped?: boolean;
  absorbsAfterMs?: number;
}) => {
  let clock = 1_000_000;
  let pending = input.pending;
  const landsAt = new Map<string, number>();
  if (input.pending !== undefined && input.pendingLanded === true)
    landsAt.set(input.pending.txHash, 0);
  const deposits: StackLedgerOwnerDeposit[] = [];
  const archived: StackLedgerOwnerDeposit[] = [];
  const stages: string[] = [];
  const landed = (deposit: StackLedgerOwnerDeposit) =>
    clock >= (landsAt.get(deposit.txHash) ?? Infinity);
  /** Our node's ledger and its attested tail hold the owner's output alike. */
  const holdsOutput = async () => {
    if (input.funded === true) return true;
    const due = pending ?? deposits.at(-1);
    return (
      due !== undefined &&
      landed(due) &&
      input.absorbsAfterMs !== undefined &&
      clock >= due.inclusionTime + input.absorbsAfterMs
    );
  };
  return {
    deposits,
    archived,
    stages,
    pending: () => pending,
    now: () => clock,
    fund: () =>
      awaitStackLedgerOwnerFunded({
        ourLedgerHoldsOutput: holdsOutput,
        ownerHoldsOutput: holdsOutput,
        readPendingDeposit: async () => pending,
        depositStatus: async (deposit) =>
          landed(deposit)
            ? "landed"
            : clock > deposit.validTo
              ? "expired"
              : "pending",
        archivePendingDeposit: async () => {
          if (pending !== undefined) archived.push(pending);
          pending = undefined;
        },
        beforeDeposit: async () => {},
        buildDeposit: async () => {
          const deposit = {
            txHash: txHash(deposits.length + 1),
            inclusionTime: clock + 120_000,
            validTo: clock + DEPOSIT_VALIDITY_MS,
          };
          return {
            ...deposit,
            submit: async () => {
              deposits.push(deposit);
              pending = deposit;
              if (input.dropped !== true)
                landsAt.set(deposit.txHash, clock + (input.landsAfterMs ?? 0));
            },
          };
        },
        admitsDeposit: async () => true,
        clearPendingDeposit: async () => {
          pending = undefined;
        },
        now: () => clock,
        pause: async () => {
          clock += POLL_MS;
        },
        absorbWithinMs: ABSORB_WITHIN_MS,
        onStage: (name) => stages.push(name),
      }),
  };
};

/** A deposit an earlier run recorded, whose validity ended before this run. */
const stale: StackLedgerOwnerDeposit = {
  txHash: txHash(0xee),
  inclusionTime: 950_000,
  validTo: 990_000,
};

describe("funding the drill's ledger owner on an attached stack", () => {
  it("deposits nothing for an owner that already holds an output, and forgets an earlier deposit", async () => {
    const l2 = stack({ funded: true, pending: stale });
    await l2.fund();
    expect(l2.deposits).toEqual([]);
    expect(l2.pending()).toBeUndefined();
    expect(l2.now()).toBe(1_000_000);
  });

  it("deposits once, waits for it to land, and waits for the stack to absorb it", async () => {
    const l2 = stack({ landsAfterMs: 60_000, absorbsAfterMs: 240_000 });
    await l2.fund();
    expect(l2.deposits.map(({ inclusionTime }) => inclusionTime)).toEqual([
      1_120_000,
    ]);
    expect(l2.pending()).toBeUndefined();
    expect(l2.now()).toBeGreaterThanOrEqual(1_120_000 + 240_000);
    expect(l2.stages.at(-1)).toMatch(/due at 1120000/u);
  });

  it("awaits an earlier run's deposit that landed rather than depositing again", async () => {
    const landed = { ...stale, inclusionTime: 1_050_000 };
    const l2 = stack({
      pending: landed,
      pendingLanded: true,
      absorbsAfterMs: 90_000,
    });
    await l2.fund();
    expect(l2.deposits).toEqual([]);
    expect(l2.archived).toEqual([]);
    expect(l2.pending()).toBeUndefined();
    expect(l2.now()).toBeGreaterThanOrEqual(1_140_000);
  });

  it("archives an earlier run's deposit whose validity passed without it landing, and deposits again", async () => {
    const l2 = stack({ pending: stale, absorbsAfterMs: 90_000 });
    await l2.fund();
    expect(l2.archived).toEqual([stale]);
    expect(l2.deposits).toHaveLength(1);
    expect(l2.deposits[0]!.txHash).not.toBe(stale.txHash);
    expect(l2.pending()).toBeUndefined();
  });

  it("deposits again only once an earlier run's unlanded deposit can no longer land", async () => {
    const pending = { ...stale, validTo: 1_100_000 };
    const l2 = stack({ pending, absorbsAfterMs: 90_000 });
    await l2.fund();
    expect(l2.archived).toEqual([pending]);
    expect(l2.deposits).toHaveLength(1);
    expect(l2.deposits[0]!.inclusionTime).toBeGreaterThan(1_100_000 + 120_000);
  });

  it("fails when its own deposit's validity passes without it landing, keeping the record", async () => {
    const l2 = stack({ dropped: true });
    await expect(l2.fund()).rejects.toThrow(
      `The ledger owner's deposit ${txHash(1)} expired without landing`,
    );
    expect(l2.archived).toEqual([]);
    expect(l2.pending()).toEqual(l2.deposits[0]);
  });

  it("fails, naming the deposit's due time, when the stack never absorbs it", async () => {
    const l2 = stack({});
    await expect(l2.fund()).rejects.toThrow(
      /did not absorb the ledger owner's deposit, due at 1120000/u,
    );
    expect(l2.deposits).toHaveLength(1);
    expect(l2.pending()).toEqual(l2.deposits[0]);
    expect(l2.now()).toBeGreaterThan(1_120_000 + ABSORB_WITHIN_MS);
    expect(l2.now()).toBeLessThanOrEqual(
      1_120_000 + ABSORB_WITHIN_MS + POLL_MS,
    );
  });
});

describe("a recorded ledger-owner deposit on the stack's chain", () => {
  /** The stack's Kupo, indexed through `slot`, holding outputs of `included`. */
  const kupo =
    (slot: number, included: readonly string[]): typeof fetch =>
    async (url) => {
      const hash = /@([0-9a-f]{64})/u.exec(String(url))?.[1];
      return new Response(
        JSON.stringify(
          hash !== undefined && included.includes(hash)
            ? [{ transaction_id: hash, output_index: 0 }]
            : [],
        ),
        { headers: { "X-Most-Recent-Checkpoint": slot.toString() } },
      );
    };
  // One slot per second of ledger time.
  const status = (slot: number, included: readonly string[] = []) =>
    stackLedgerOwnerDepositStatus({
      kupoUrl: "http://127.0.0.1:1442",
      unixTimeToSlot: (ms) => Math.floor(ms / 1000),
      fetchImpl: kupo(slot, included),
    })(stale);

  it("has landed once the stack's Kupo indexed an output of it", async () => {
    await expect(status(1_200, [stale.txHash])).resolves.toBe("landed");
  });

  it("has expired once the stack's Kupo indexed its validity's end without it", async () => {
    await expect(status(990)).resolves.toBe("expired");
  });

  it("is pending while the stack's Kupo has not indexed its validity's end", async () => {
    await expect(status(989)).resolves.toBe("pending");
  });
});
