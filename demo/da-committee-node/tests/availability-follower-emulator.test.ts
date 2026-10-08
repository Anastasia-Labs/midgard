import { openAvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import * as SDK from "@al-ft/midgard-sdk";
import {
  getAddressDetails,
  paymentCredentialOf,
  type UTxO,
} from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it, vi } from "vitest";

import { availabilityResponderOperations } from "../src/availability/factory.js";
import {
  AvailabilityResponder,
  AvailabilityResponderAwaitingScanError,
} from "../src/availability/responder.js";
import { ownWallets } from "../src/l1/follower/committee-follower-config.js";
import {
  type EmulatorFollower,
  emulatorFollower,
  emulatorWallet,
  noChainIndexCommitteeConfig,
  transactionId,
} from "./helpers/emulator-follower.js";

/**
 * The availability responder's reconcile on the committee follower's facts,
 * from a config with no Kupo or Ogmios key: the transactions are the Lucid
 * emulator's signed bytes, landed in follower blocks, and every read
 * (transaction status, inputs, the verified rival spend, the boundary) is
 * the production read over the follower store.
 *
 * The committee's step is a Settle or Close spending only protocol UTxOs;
 * the wallet's coins stand in for them, as reconcile judges an intent by its
 * signed bytes and the spends of its inputs.
 */

const closers: (() => Promise<void> | void)[] = [];
afterEach(async () => {
  for (const close of closers.splice(0).reverse()) await close();
});

const outRef = (utxo: UTxO) => `${utxo.txHash}#${utxo.outputIndex.toString()}`;

const fixture = async (action: "settle" | "prepare" = "settle") => {
  const wallet = await emulatorWallet();
  const config = await noChainIndexCommitteeConfig(wallet.account.seedPhrase);
  // The production store options track the committee's own wallet.
  expect((await ownWallets(config)).map((a) => a.toString("hex"))).toEqual([
    getAddressDetails(wallet.address).address.hex,
  ]);
  const follower = await emulatorFollower(config);
  closers.push(follower.close);
  const actor = paymentCredentialOf(wallet.address).hash;
  const deploymentIdentity = String(config.contractDeploymentInfo.manifestId);
  const finality = config.finalityDepth;
  const split = await wallet.split(4);
  const normal = split.coins.slice(0, 3);
  const collateral = split.coins[3]!;
  const ours = SDK.inspectDaAvailabilitySignedIntent({
    deploymentIdentity,
    actor,
    headerHash: "bb".repeat(28),
    action,
    signedCbor: await wallet.spend({
      inputs: normal,
      lovelace: 10_000_000n,
      collateral,
    }),
  });
  const journal = openAvailabilityOperationJournal(
    config.availabilityJournalPath!,
  );
  closers.push(() => journal.close());
  const lease = journal.acquire(actor, "setup", Date.now(), 60_000);
  journal.persist(lease, ours, Date.now());
  journal.release(lease);
  // The rival: another transaction spending every normal input.
  const rivalCbor = await wallet.spend({
    inputs: normal,
    lovelace: 7_000_000n,
  });
  const rival = transactionId(rivalCbor);

  // The split lands on the follower's chain: its outputs are the wallet's.
  await follower.forward([{ cbor: split.cbor }]);

  const submit = vi.fn(async (_signedCbor: string) => ours.txHash);
  const operations = availabilityResponderOperations({
    lucid: follower.lucid,
    reads: follower.reads,
    assertSourceHealthy: async () => {},
    context: {
      deploymentIdentity,
      actor,
      journal,
      stateQueuePolicyId: config.stateQueuePolicyId,
      minimumConfirmationDepth: finality,
      transactionLimits: {
        maxTxSize: 16_384,
        maxTxExMem: 16_500_000n,
        maxTxExSteps: 10_000_000_000n,
        coinsPerUtxoByte: 4_310n,
        feeCeilings: { [action]: 2_000_000n },
      },
      submit,
    },
  });
  const discover = vi.fn(async () => []);
  const responder = new AvailabilityResponder({
    deploymentFingerprint: config.deploymentFingerprint,
    deploymentIdentity: config.hubOraclePolicyId,
    store: { getDaPayload: async () => undefined } as never,
    discover,
    reconcile: operations.reconcile,
    execute: vi.fn(async () => "pending" as const),
  });
  return {
    follower,
    finality,
    ours,
    rival: { txHash: rival, cbor: rivalCbor },
    normal,
    journal,
    discover,
    submit,
    responder,
    state: () => journal.get(ours.id)?.state,
  };
};

type Fixture = Awaited<ReturnType<typeof fixture>>;

/** Lands the rival at the first slot past the intent's validity. */
const landRival = (f: Fixture, valid = true) =>
  f.follower.forward(
    [{ cbor: f.rival.cbor, valid }],
    Math.max(f.follower.tip().slot + 1, f.ours.validUntilSlot),
  );

const expectNothingReleased = (f: Fixture) => {
  expect(f.discover).not.toHaveBeenCalled();
  expect(f.state()).toBe("pending");
  expect(f.journal.reservedOutRefs(f.ours.actor)).toEqual(
    expect.arrayContaining(f.normal.map(outRef)),
  );
};

const tip = (follower: EmulatorFollower) => follower.tip();

describe("the availability responder reconciles on the follower's facts, with no chain index configured", () => {
  it("confirms its own step once the follower holds it finality-deep, then discovers again", async () => {
    const f = await fixture();
    await f.follower.forward(
      [{ cbor: f.ours.signedCbor }],
      f.ours.validUntilSlot - 10,
    );
    await f.follower.empty(f.finality);
    await expect(f.responder.tick()).resolves.toStrictEqual({
      challenges: 0,
      status: "idle",
    });
    expect(f.state()).toBe("confirmed");
    expect(f.discover).toHaveBeenCalledTimes(1);
    expect(f.submit).not.toHaveBeenCalled();
  });

  it("records its own step as included, not confirmed, one block short of finality", async () => {
    const f = await fixture();
    await f.follower.forward(
      [{ cbor: f.ours.signedCbor }],
      f.ours.validUntilSlot - 10,
    );
    await f.follower.empty(f.finality - 1);
    // An included step blocks no new work; it stays reconciled each pass.
    await expect(f.responder.tick()).resolves.toStrictEqual({
      challenges: 0,
      status: "idle",
    });
    expect(f.state()).toBe("included");
    await f.follower.empty(1);
    await f.responder.tick();
    expect(f.state()).toBe("confirmed");
    expect(f.submit).not.toHaveBeenCalled();
  });

  it("returns its own step to the mempool when the follower rolls its block back", async () => {
    // A Prepare runs no script, so the stand-in's bytes pass the
    // rebroadcast's execution-reserve check.
    const f = await fixture("prepare");
    const before = tip(f.follower);
    await f.follower.forward(
      [{ cbor: f.ours.signedCbor }],
      f.ours.validUntilSlot - 20,
    );
    await f.follower.empty(f.finality - 1);
    await f.responder.tick();
    expect(f.state()).toBe("included");
    await f.follower.rollBackTo(before);
    await f.follower.empty(2);
    await expect(f.responder.tick()).resolves.toStrictEqual({
      challenges: 0,
      status: "pending",
    });
    expect(f.state()).toBe("pending");
    // The same signed bytes go to the node again: the inputs are live.
    expect(f.submit).toHaveBeenCalledWith(f.ours.signedCbor);
  });

  it("expires its step once a rival spend of its inputs is final, and discovers again", async () => {
    const f = await fixture();
    await landRival(f);
    await f.follower.empty(f.finality);
    await expect(f.responder.tick()).resolves.toStrictEqual({
      challenges: 0,
      status: "idle",
    });
    expect(f.state()).toBe("expired");
    expect(f.journal.get(f.ours.id)?.detail).toBe(
      "Expired with a normal input finally spent by another transaction",
    );
    expect(f.journal.reservedOutRefs(f.ours.actor)).toEqual([]);
    expect(f.discover).toHaveBeenCalledTimes(1);
  });

  it("keeps its step pending while the rival spend is one block short of finality", async () => {
    const f = await fixture();
    await landRival(f);
    await f.follower.empty(f.finality - 1);
    await expect(f.responder.tick()).resolves.toStrictEqual({
      challenges: 0,
      status: "pending",
    });
    expectNothingReleased(f);
  });

  it("keeps its step pending when the final rival spend is rolled back and lands again short of finality", async () => {
    const f = await fixture();
    const before = tip(f.follower);
    await landRival(f);
    await f.follower.empty(f.finality);
    await f.follower.rollBackTo(before);
    await landRival(f);
    await f.follower.empty(f.finality - 1);
    await expect(f.responder.tick()).resolves.toStrictEqual({
      challenges: 0,
      status: "pending",
    });
    expectNothingReleased(f);
  });

  it("does not take a phase-2-failed rival as a spend of its inputs", async () => {
    const f = await fixture();
    await landRival(f, false);
    await f.follower.empty(f.finality);
    await f.responder.tick();
    // The rival consumed nothing, so the step was never beaten: its inputs
    // are live past its validity, which expires it on that ground alone.
    expect(f.journal.get(f.ours.id)?.detail).toBe(
      "Expired with every normal input canonically unspent",
    );
    expect(f.submit).not.toHaveBeenCalled();
  });

  it("awaits the follower and releases nothing while the follower holds the committee", async () => {
    const f = await fixture();
    await landRival(f);
    await f.follower.empty(f.finality);
    f.follower.hold([
      { reason: "rollback_beyond_k", detail: "rolled back 2161 blocks" },
    ]);
    await expect(f.responder.tick()).resolves.toStrictEqual({
      challenges: 0,
      status: "awaiting_scan",
      detail: new AvailabilityResponderAwaitingScanError(
        "rollback_beyond_k: rolled back 2161 blocks",
      ).message,
    });
    expectNothingReleased(f);
  });
});
