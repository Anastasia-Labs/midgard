import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  type AvailabilityOperationIntent,
  type AvailabilityOperationJournal,
  openAvailabilityOperationJournal,
} from "@al-ft/midgard-core/availability-operation-journal";
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { encodeTxBody, type SimTx } from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it } from "vitest";

import { reconcileWatcherAvailabilityJournal } from "../../src/availability/journal-pins.js";
import { createWatcherAvailabilityObservation } from "../../src/availability/observation.js";
import { unsafeAdmitWatcherStateQueueObservationForReplayTest } from "../../src/indexers/authenticated-state-queue-observation.js";
import { RELEASE_FINALITY_DEPTH } from "../../src/indexers/authenticated-state-queue-observation.parse-persisted-header.js";
import { watcherAvailabilityTransactionPins } from "../../src/l1-follower/availability-pins.js";
import type { VerifiedWatcherDeploymentIdentity } from "../../src/runtime/deployment-identity.js";
import { watcherSha256CanonicalJson } from "../../src/storage/durable-store.js";
import {
  fixture,
  harness,
  K,
  pruneFixture,
  random,
  reasonOf,
  T,
  X,
} from "../support/l1-follower-raw-reads-fixture.js";

/**
 * Reconciling a signed availability intent against a follower store that has
 * pruned. The reads are the real follower raw reads over a pruned simulator
 * chain; only the deployment identity is a stand-in, since `operation` reads
 * nothing from it but the manifest id.
 */

const MANIFEST = "a1".repeat(32);

type Chain = Awaited<ReturnType<typeof harness>>;

/** The tip of the fixture chain, admitted as a finalized observation. */
const tipObservation = (fx: Chain) => {
  const tip = fx.tipPoint();
  const value = {
    schemaVersion: "midgard-watcher-production-state-queue-observation-v1",
    deploymentIdentityDigest: MANIFEST,
    protocolScriptAuthorityDigest: "45".repeat(32),
    stateQueuePolicyId: "46".repeat(28),
    hubOraclePolicyId: "47".repeat(28),
    nativePoint: {
      blockHash: tip.blockHash,
      parentBlockHash: null,
      slot: tip.slot,
      blockNo: tip.blockNo,
      chainPointId: "48".repeat(32),
      finalityDepth: RELEASE_FINALITY_DEPTH.toString(),
    },
    sourceId: "follower:test",
    previousObservationDigest: null,
    checkpoints: [],
    finalizedQueue: [{ headerHash: null, outRef: `${"41".repeat(32)}#0` }],
    finalizedHeaders: [],
    finalizedCorrectionLock: null,
    correctionLockWitnesses: [],
  };
  return unsafeAdmitWatcherStateQueueObservationForReplayTest({
    ...value,
    observationDigest: watcherSha256CanonicalJson(value),
  });
};

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

const intake = (fx: Chain) =>
  createWatcherAvailabilityObservation({
    identity: {
      manifestId: MANIFEST,
    } as unknown as VerifiedWatcherDeploymentIdentity,
    l1: { reads: fx.reads(), store: fx.store },
    confirmationDepth: 1,
    deployment: {} as SDK.DaAvailabilityDeployment,
  });

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

  it("still refuses an intent that could have landed inside the pruned window", async () => {
    const fx = await fixture();
    const prunedThrough = await pruneFixture(fx);
    await expect(
      intake(fx).operation(
        tipObservation(fx),
        intentSpending(`${fx.hashes.b}#0`, prunedThrough),
      ),
    ).rejects.toThrow(/beyond_retention/u);
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
  const RECOVERY = DEPLOYMENT_MANIFEST_L1_FINALITY.automaticRecoveryMaxDepth;
  const KEY = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x5a));
  const ACTOR = KEY.to_public().hash().to_hex();
  const HEADER = "4c".repeat(28);
  const directories: string[] = [];
  const journals: AvailabilityOperationJournal[] = [];
  afterEach(() => {
    for (const journal of journals.splice(0)) journal.close();
    for (const directory of directories.splice(0))
      rmSync(directory, { recursive: true, force: true });
  });

  const openJournal = (): AvailabilityOperationJournal => {
    const directory = mkdtempSync(join(tmpdir(), "availability-pins-"));
    directories.push(directory);
    const journal = openAvailabilityOperationJournal(
      join(directory, "journal.sqlite"),
    );
    journals.push(journal);
    return journal;
  };

  /** A transaction the actor signs and records, paying the tracked address. */
  const recorded = (
    fx: Chain,
    journal: AvailabilityOperationJournal,
    action: "prepare" | "open",
    deploymentIdentity = MANIFEST,
  ): Readonly<{ tx: SimTx; intent: AvailabilityOperationIntent }> => {
    const tx: SimTx = {
      inputs: [fx.chain.outsideInput()],
      collaterals: [fx.chain.outsideInput()],
      outputs: [{ address: T, lovelace: 2_000_000n }],
      invalidBefore: 0,
      invalidAfter: 1_000_000,
      nonce: fx.chain.nonce(),
    };
    const body = CML.TransactionBody.from_cbor_bytes(encodeTxBody(tx));
    const vkeys = CML.VkeywitnessList.new();
    vkeys.add(CML.make_vkey_witness(CML.hash_transaction(body), KEY));
    const witnesses = CML.TransactionWitnessSet.new();
    witnesses.set_vkeywitnesses(vkeys);
    const intent = SDK.inspectDaAvailabilitySignedIntent({
      deploymentIdentity,
      actor: ACTOR,
      headerHash: HEADER,
      action,
      signedCbor: CML.Transaction.new(
        body,
        witnesses,
        true,
        undefined,
      ).to_cbor_hex(),
    });
    const lease = journal.acquire(ACTOR, "test", Date.now(), 60_000);
    journal.persist(lease, intent, Date.now());
    journal.release(lease);
    return { tx, intent };
  };

  /** One reconciliation pass at the follower's tip. */
  const pass = (
    fx: Chain,
    journal: AvailabilityOperationJournal,
    deploymentIdentity = MANIFEST,
  ) =>
    reconcileWatcherAvailabilityJournal(
      {
        deploymentIdentity,
        actor: ACTOR,
        stateQueuePolicyId: "46".repeat(28),
        journal,
        minimumConfirmationDepth: 1,
        transactionLimits: {
          maxTxSize: 16_384,
          maxTxExMem: 0n,
          maxTxExSteps: 0n,
          coinsPerUtxoByte: 0n,
          feeCeilings: {},
        },
        assertActuationCurrent: () => undefined,
        observe: (intent) => intake(fx).operation(tipObservation(fx), intent),
        submit: () => Promise.reject(new Error("nothing is submitted")),
      },
      watcherAvailabilityTransactionPins(fx.store),
    );

  /** Spends the transaction's only output, so pruning may delete it. */
  const spendOutput = (fx: Chain, txHash: string) =>
    fx.forward([
      {
        inputs: [{ txHash: Buffer.from(txHash, "hex"), index: 0 }],
        outputs: [{ address: X, lovelace: 1_000_000n }],
        nonce: fx.chain.nonce(),
      },
    ]);

  const advance = async (fx: Chain, blocks: number): Promise<void> => {
    for (let i = 0; i < blocks; i += 1) await fx.forward([]);
    await fx.pruneAll();
  };

  /** The follower's answer for the transaction: `ok` while it is stored. */
  const stored = async (fx: Chain, txHash: string): Promise<string> =>
    reasonOf(await fx.reads().transactionInclusion(txHash));

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
    const { tx, intent } = recorded(fx, journal, "prepare", earlier);
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
