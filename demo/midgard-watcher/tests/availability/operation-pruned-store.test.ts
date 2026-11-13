import type { AvailabilityOperationIntent } from "@al-ft/midgard-core/availability-operation-journal";
import { CML } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it } from "vitest";

import {
  ACTOR,
  advance,
  closeJournals,
  HEADER,
  intake,
  MANIFEST,
  openJournal,
  pass,
  recorded,
  RECOVERY,
  spendOutput,
  stored,
  tipObservation,
} from "../support/availability-pruned-journal.js";
import {
  fixture,
  harness,
  K,
  pruneFixture,
  random,
  reasonOf,
} from "../support/l1-follower-raw-reads-fixture.js";

/**
 * Reconciling a signed availability intent against a follower store that has
 * pruned. The reads are the real follower raw reads over a pruned simulator
 * chain.
 */

/** A signed intent spending `spent`, valid from `validFrom`; it never lands. */
const intentSpending = (
  spent: string,
  validFrom: number,
): AvailabilityOperationIntent => {
  const [txHash, index] = spent.split("#") as [string, string];
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(txHash),
      BigInt(index),
    ),
  );
  const body = CML.TransactionBody.new(
    inputs,
    CML.TransactionOutputList.new(),
    200_000n,
  );
  body.set_validity_interval_start(BigInt(validFrom));
  body.set_ttl(BigInt(validFrom + 120));
  const signed = CML.Transaction.new(
    body,
    CML.TransactionWitnessSet.new(),
    true,
  );
  return {
    id: "open",
    deploymentIdentity: MANIFEST,
    actor: "actor",
    headerHash: "header",
    action: "open",
    signedCbor: signed.to_cbor_hex(),
    txHash: CML.hash_transaction(body).to_hex(),
    spentOutRefs: [spent],
    collateralOutRefs: [],
    expectedOutRefs: [],
    validUntilSlot: validFrom + 120,
    completesWorkflow: false,
  };
};

describe("availability operation reconcile on a pruned follower store", () => {
  it("reads a signed intent valid only above the pruned window as not landed", async () => {
    const fx = await fixture();
    const prunedThrough = await pruneFixture(fx);
    const found = await intake(fx).operation(
      tipObservation(fx),
      intentSpending(`${fx.hashes.b}#0`, prunedThrough + 1),
    );
    expect(found).toEqual({
      status: "unspent",
      currentSlot: Number(fx.tipPoint().slot),
    });
  });

  // The store may have pruned the transaction had it landed, but it keeps
  // every unspent row and records every spend of one: an input still
  // unspent at the point proves the transaction did not spend it there.
  it("reads an intent that could have landed inside the pruned window as not landed while its input is unspent", async () => {
    const fx = await fixture();
    const prunedThrough = await pruneFixture(fx);
    expect(reasonOf(await fx.reads().transactionInclusion(random(0x78)))).toBe(
      "beyond_retention",
    );
    const found = await intake(fx).operation(
      tipObservation(fx),
      intentSpending(`${fx.hashes.b}#0`, prunedThrough),
    );
    expect(found).toEqual({
      status: "unspent",
      currentSlot: Number(fx.tipPoint().slot),
    });
  });

  it("reads an intent that could have landed inside the pruned window, with no input left to prove otherwise, as beyond retention", async () => {
    const fx = await fixture();
    const prunedThrough = await pruneFixture(fx);
    const tip = fx.tipPoint();
    const found = await intake(fx).operation(
      tipObservation(fx),
      intentSpending(`${fx.hashes.a}#0`, prunedThrough),
    );
    expect(found).toEqual({
      status: "beyond_retention",
      currentSlot: Number(tip.slot),
      currentBlockNo: Number(tip.blockNo),
      reason: expect.stringMatching(/may have landed at or below slot/u),
    });
  });

  // A#0 was spent and its creating transaction pruned; the second input's
  // creating transaction never reached the store, as when a rollback drops
  // the parent intent whose output a child spends.
  it.each([
    [
      "spent and pruned",
      (fx: Awaited<ReturnType<typeof fixture>>) => `${fx.hashes.a}#0`,
    ],
    ["never stored", () => `${random(0x77)}#0`],
  ])(
    "reads an input whose row is %s as missing, not as a refusal",
    async (_, spent) => {
      const fx = await fixture();
      const prunedThrough = await pruneFixture(fx);
      const found = await intake(fx).operation(
        tipObservation(fx),
        intentSpending(spent(fx), prunedThrough + 1),
      );
      expect(found).toEqual({
        status: "inputs_missing",
        currentSlot: Number(fx.tipPoint().slot),
        missingOutRefs: [spent(fx)],
      });
    },
  );
});

/**
 * The availability journal's transactions on a follower that prunes them.
 * The store keeps K blocks, so a transaction whose outputs are all spent is
 * pruned long before the journal retires it past the recovery depth; the
 * journal, the reads and the reconciliation pass are the real ones.
 */
describe("availability journal transactions on a pruning follower", () => {
  afterEach(closeJournals);

  it("reads a confirmed transaction the follower would have pruned, and releases it once the journal retires it", async () => {
    const fx = await harness();
    const journal = openJournal();
    const { tx, intent } = recorded(fx, journal, "prepare");
    await fx.forward([tx]);
    await fx.forward([]);
    expect(await pass(fx, journal)).toMatchObject([
      { status: "confirmed", txHash: intent.txHash },
    ]);
    await spendOutput(fx, intent.txHash);
    await advance(fx, K + 2);
    // Still confirmed in the journal, which keeps it for the recovery depth.
    expect(await pass(fx, journal)).toMatchObject([
      { status: "confirmed", txHash: intent.txHash },
    ]);
    expect(journal.get(intent.id)?.state).toBe("confirmed");
    expect(await stored(fx, intent.txHash)).toBe("ok");

    await advance(fx, RECOVERY + 1);
    expect(await pass(fx, journal)).toMatchObject([
      { status: "confirmed", txHash: intent.txHash },
    ]);
    expect(journal.get(intent.id)).toBeNull();
    // The next pass releases it, and the follower prunes it.
    expect(await pass(fx, journal)).toEqual([]);
    await advance(fx, 1);
    expect(await stored(fx, intent.txHash)).toBe("beyond_retention");
  });

  it("holds an Open whose workflow is live past the recovery depth, and releases it with the workflow", async () => {
    const fx = await harness();
    const journal = openJournal();
    const { tx, intent } = recorded(fx, journal, "open");
    await fx.forward([tx]);
    await fx.forward([]);
    expect(await pass(fx, journal)).toMatchObject([
      { status: "confirmed", txHash: intent.txHash },
    ]);
    await spendOutput(fx, intent.txHash);
    await advance(fx, RECOVERY + 1);
    // A live challenge workflow keeps its Open in the journal at any depth.
    expect(await pass(fx, journal)).toMatchObject([
      { status: "confirmed", txHash: intent.txHash },
    ]);
    expect(journal.get(intent.id)?.state).toBe("confirmed");
    expect(await stored(fx, intent.txHash)).toBe("ok");

    // Someone else's terminal step ended the challenge, final past recovery.
    const lease = journal.acquire(ACTOR, "test", Date.now(), 60_000);
    journal.releaseWorkflow(
      lease,
      MANIFEST,
      HEADER,
      {
        openIntentId: intent.id,
        reason: "challenge-closed",
        txHash: random(0x6c),
        spendPoint: `1:${random(0x6d)}`,
        confirmationDepth: RECOVERY + 1,
        recoveryDepth: RECOVERY,
      },
      Date.now(),
    );
    journal.release(lease);
    await pass(fx, journal);
    expect(journal.get(intent.id)).toBeNull();
    expect(await pass(fx, journal)).toEqual([]);
    await advance(fx, 1);
    expect(await stored(fx, intent.txHash)).toBe("beyond_retention");
  });

  it("holds an earlier deployment's confirmed anchor until this deployment's pass retires it", async () => {
    const fx = await harness();
    const journal = openJournal();
    const earlier = "b2".repeat(32);
    const { tx, intent } = recorded(fx, journal, "prepare", {
      deploymentIdentity: earlier,
    });
    await fx.forward([tx]);
    await fx.forward([]);
    expect(await pass(fx, journal, earlier)).toMatchObject([
      { status: "confirmed", txHash: intent.txHash },
    ]);
    await spendOutput(fx, intent.txHash);
    await advance(fx, K + 2);
    // A redeploy reads the earlier anchor only to retire it.
    expect(await pass(fx, journal)).toEqual([]);
    expect(journal.get(intent.id)?.state).toBe("confirmed");
    expect(await stored(fx, intent.txHash)).toBe("ok");

    await advance(fx, RECOVERY + 1);
    expect(await pass(fx, journal)).toEqual([]);
    expect(journal.get(intent.id)).toBeNull();
    await pass(fx, journal);
    await advance(fx, 1);
    expect(await stored(fx, intent.txHash)).toBe("beyond_retention");
  });
});
