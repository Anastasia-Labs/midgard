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

import { reconcileWatcherAvailabilityJournal } from "../../src/availability/journal-pins.js";
import { createWatcherAvailabilityObservation } from "../../src/availability/observation.js";
import { unsafeAdmitWatcherStateQueueObservationForReplayTest } from "../../src/indexers/authenticated-state-queue-observation.js";
import { RELEASE_FINALITY_DEPTH } from "../../src/indexers/authenticated-state-queue-observation.parse-persisted-header.js";
import { watcherAvailabilityTransactionPins } from "../../src/l1-follower/availability-pins.js";
import type { VerifiedWatcherDeploymentIdentity } from "../../src/runtime/deployment-identity.js";
import { watcherSha256CanonicalJson } from "../../src/storage/durable-store.js";
import {
  type harness,
  okValue,
  reasonOf,
  T,
  X,
} from "./l1-follower-raw-reads-fixture.js";

/**
 * The availability journal reconciled against a follower store that prunes.
 * The reads, the journal and the reconciliation pass are the real ones; only
 * the deployment identity is a stand-in, since `operation` reads nothing
 * from it but the manifest id.
 */

export const MANIFEST = "a1".repeat(32);
export const RECOVERY =
  DEPLOYMENT_MANIFEST_L1_FINALITY.automaticRecoveryMaxDepth;
const KEY = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x5a));
export const ACTOR = KEY.to_public().hash().to_hex();
export const HEADER = "4c".repeat(28);

export type Chain = Awaited<ReturnType<typeof harness>>;
type Input = SimTx["inputs"][number];

/** The tip of the fixture chain, admitted as a finalized observation. */
export const tipObservation = (fx: Chain) => {
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

export const intake = (fx: Chain) =>
  createWatcherAvailabilityObservation({
    identity: {
      manifestId: MANIFEST,
    } as unknown as VerifiedWatcherDeploymentIdentity,
    l1: { reads: fx.reads(), store: fx.store },
    confirmationDepth: 1,
    deployment: {} as SDK.DaAvailabilityDeployment,
  });

/** Journals opened by {@link openJournal}, closed by {@link closeJournals}. */
const opened: { directory: string; journal: AvailabilityOperationJournal }[] =
  [];

export const openJournal = (): AvailabilityOperationJournal => {
  const directory = mkdtempSync(join(tmpdir(), "availability-pins-"));
  const journal = openAvailabilityOperationJournal(
    join(directory, "journal.sqlite"),
  );
  opened.push({ directory, journal });
  return journal;
};

export const closeJournals = (): void => {
  for (const { directory, journal } of opened.splice(0)) {
    journal.close();
    rmSync(directory, { recursive: true, force: true });
  }
};

/** A transaction the actor signs, paying the tracked address. */
export const signed = (
  fx: Chain,
  action: "prepare" | "open",
  options: Readonly<{
    deploymentIdentity?: string;
    inputs?: SimTx["inputs"];
    collaterals?: SimTx["inputs"];
    invalidAfter?: number;
  }> = {},
): Readonly<{ tx: SimTx; intent: AvailabilityOperationIntent }> => {
  const tx: SimTx = {
    inputs: options.inputs ?? [fx.chain.outsideInput()],
    collaterals: options.collaterals ?? [fx.chain.outsideInput()],
    outputs: [{ address: T, lovelace: 2_000_000n }],
    invalidBefore: 0,
    invalidAfter: options.invalidAfter ?? 1_000_000,
    nonce: fx.chain.nonce(),
  };
  const body = CML.TransactionBody.from_cbor_bytes(encodeTxBody(tx));
  const vkeys = CML.VkeywitnessList.new();
  vkeys.add(CML.make_vkey_witness(CML.hash_transaction(body), KEY));
  const witnesses = CML.TransactionWitnessSet.new();
  witnesses.set_vkeywitnesses(vkeys);
  const intent = SDK.inspectDaAvailabilitySignedIntent({
    deploymentIdentity: options.deploymentIdentity ?? MANIFEST,
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
  return { tx, intent };
};

/** Persists `intent` in the journal, as the SDK does before submitting it. */
export const persist = (
  journal: AvailabilityOperationJournal,
  intent: AvailabilityOperationIntent,
): void => {
  const lease = journal.acquire(ACTOR, "test", Date.now(), 60_000);
  journal.persist(lease, intent, Date.now());
  journal.release(lease);
};

/** A transaction the actor signs and records, paying the tracked address. */
export const recorded = (
  fx: Chain,
  journal: AvailabilityOperationJournal,
  action: "prepare" | "open",
  options: Parameters<typeof signed>[2] = {},
): Readonly<{ tx: SimTx; intent: AvailabilityOperationIntent }> => {
  const made = signed(fx, action, options);
  persist(journal, made.intent);
  return made;
};

/** One reconciliation pass at the follower's tip. */
export const pass = (
  fx: Chain,
  journal: AvailabilityOperationJournal,
  deploymentIdentity = MANIFEST,
  pin: (
    txHashes: readonly string[],
  ) => Promise<void> = watcherAvailabilityTransactionPins(fx.store),
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
    pin,
  );

/** A watcher from before the journal's pins: the follower holds nothing. */
export const unpinned = async (): Promise<void> => undefined;

/** `count` outputs at the tracked address, as the actor's wallet holds them. */
export const fund = async (fx: Chain, count: number): Promise<Input[]> => {
  const { hashes } = await fx.forward([
    {
      inputs: [fx.chain.outsideInput()],
      outputs: Array.from({ length: count }, () => ({
        address: T,
        lovelace: 3_000_000n,
      })),
      nonce: fx.chain.nonce(),
    },
  ]);
  return Array.from({ length: count }, (_, index) => ({
    txHash: Buffer.from(hashes[0]!, "hex"),
    index,
  }));
};

/** Someone else's transaction spending `input`. */
export const spendElsewhere = (fx: Chain, input: Input) =>
  fx.forward([
    {
      inputs: [input],
      outputs: [{ address: X, lovelace: 1_000_000n }],
      nonce: fx.chain.nonce(),
    },
  ]);

const label = (input: Input): string =>
  `${input.txHash.toString("hex")}#${input.index.toString()}`;

/** The store's answer for `input` at the tip: unspent, spent or pruned. */
export const row = async (fx: Chain, input: Input): Promise<string> => {
  const read = okValue(
    await fx.reads().utxosByOutRefAtPoint([label(input)], fx.tipPoint()),
  );
  return read.outputs.length > 0
    ? "unspent"
    : read.spends.length > 0
      ? "spent"
      : read.beyondRetention.length > 0
        ? "beyond_retention"
        : "unknown";
};

/** Spends the transaction's only output, so pruning may delete it. */
export const spendOutput = (fx: Chain, txHash: string) =>
  fx.forward([
    {
      inputs: [{ txHash: Buffer.from(txHash, "hex"), index: 0 }],
      outputs: [{ address: X, lovelace: 1_000_000n }],
      nonce: fx.chain.nonce(),
    },
  ]);

export const advance = async (fx: Chain, blocks: number): Promise<void> => {
  for (let i = 0; i < blocks; i += 1) await fx.forward([]);
  await fx.pruneAll();
};

/** The follower's answer for the transaction: `ok` while it is stored. */
export const stored = async (fx: Chain, txHash: string): Promise<string> =>
  reasonOf(await fx.reads().transactionInclusion(txHash));
