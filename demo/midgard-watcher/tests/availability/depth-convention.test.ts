import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  type AvailabilityOperationIntent,
  openAvailabilityOperationJournal,
} from "@al-ft/midgard-core/availability-operation-journal";
import type {
  FraudProofRawL1Point,
  FraudProofRawL1Transaction,
} from "@al-ft/midgard-fault-proofs";
import type { DaAvailabilityForeignSpendReaders } from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";
import { afterEach, expect, it, vi } from "vitest";

import type { WatcherAuthenticatedStateQueueObservation } from "../../src/indexers/authenticated-state-queue-observation.js";
import type { FollowerVerifiedSpend } from "../../src/l1-follower/raw-reads.types.js";
import {
  availabilityFollowerIo,
  availabilityFollowerL1,
} from "../support/availability-follower-l1.js";

const io = vi.hoisted(() => ({
  tip: 2260,
  included: true,
  raw: undefined as FraudProofRawL1Transaction | undefined,
  inclusion: undefined as FraudProofRawL1Point | undefined,
  spend: undefined as FollowerVerifiedSpend | undefined,
}));
const follower = availabilityFollowerIo();
vi.mock("../../src/indexers/authenticated-state-queue-observation.js", () => ({
  assertWatcherStateQueueObservation: () => {},
}));
vi.mock("@al-ft/midgard-sdk", async (original) => {
  const actual = await original<typeof import("@al-ft/midgard-sdk")>();
  return {
    ...actual,
    // Isolate workflow classification; retain the actual SDK canonical spend verifier and depth computation.
    resolveDaAvailabilityWorkflowRelease: async (
      readers: DaAvailabilityForeignSpendReaders,
      open: { intent: AvailabilityOperationIntent },
    ) => {
      const spend = await actual.resolveDaAvailabilityForeignSpend({
        ...readers,
        outRef: open.intent.expectedOutRefs[0],
      });
      if (!spend)
        throw new Error("Actual SDK did not authenticate fixture spend");
      return {
        reason: "challenge-closed",
        txHash: spend.spendingTxHash,
        spendPoint: spend.spendPoint,
        confirmationDepth: spend.confirmationDepth,
      };
    },
  };
});
import { createWatcherAvailabilityObservation } from "../../src/availability/observation.js";
import { releaseWatcherAvailabilityWorkflows } from "../../src/availability/runtime.release-watcher-availability-workflows.js";

const dirs: string[] = [];
afterEach(() =>
  dirs
    .splice(0)
    .forEach((dir) => rmSync(dir, { recursive: true, force: true })),
);
const openHash = "ab".repeat(32);
const txInputs = CML.TransactionInputList.new();
txInputs.add(
  CML.TransactionInput.new(CML.TransactionHash.from_hex(openHash), 0n),
);
const body = CML.TransactionBody.new(
  txInputs,
  CML.TransactionOutputList.new(),
  0n,
);
const witnesses = CML.TransactionWitnessSet.new();
const tx = CML.Transaction.new(body, witnesses, true);
const txHash = CML.hash_transaction(body).to_hex();
const signedCbor = tx.to_cbor_hex();
const baseIntent: AvailabilityOperationIntent = {
  id: "open",
  actor: "actor",
  deploymentIdentity: "deployment",
  headerHash: "header",
  action: "open",
  signedCbor,
  txHash: openHash,
  spentOutRefs: ["coin#0"],
  collateralOutRefs: [],
  expectedOutRefs: [`${openHash}#0`],
  validUntilSlot: 1,
  completesWorkflow: false,
};
const closeIntent: AvailabilityOperationIntent = {
  ...baseIntent,
  id: "close",
  action: "close",
  txHash,
  completesWorkflow: true,
  spentOutRefs: [`${openHash}#0`],
  expectedOutRefs: [`${txHash}#0`],
};
const setup = (after: number) => {
  io.tip = 100 + after;
  io.included = true;
  io.inclusion = {
    slot: "100",
    blockNo: "100",
    blockHash: "aa".repeat(32),
    pointId: "100:aa",
  };
  io.raw = {
    txHash,
    confirmationDepth: after + 1,
    bodyCbor: body.to_cbor_hex(),
    witnessSetCbor: witnesses.to_cbor_hex(),
    redeemersCbor: null,
    isValid: true,
    inclusionPoint: io.inclusion,
    resolvedInputs: [],
    resolvedReferenceInputs: [],
  };
  io.spend = {
    outRef: `${openHash}#0`,
    spendingTxHash: txHash,
    spendPoint: io.inclusion,
  };
  const observation: WatcherAuthenticatedStateQueueObservation = {
    schemaVersion: "midgard-watcher-production-state-queue-observation-v1",
    nativePoint: {
      slot: String(io.tip - 29),
      blockNo: String(io.tip - 29),
      blockHash: "cd".repeat(32),
      finalityDepth: "30",
      parentBlockHash: null,
      chainPointId: "point",
    },
    deploymentIdentityDigest: "deployment",
    protocolScriptAuthorityDigest: "scripts",
    stateQueuePolicyId: "statequeue",
    hubOraclePolicyId: "oracle",
    sourceId: "source",
    previousObservationDigest: null,
    checkpoints: [],
    finalizedQueue: [],
    finalizedHeaders: [],
    finalizedCorrectionLock: null,
    correctionLockWitnesses: [],
    observationDigest: "digest",
  };
  // Admission is mocked only at the raw-source/observation boundary. The real
  // watcher adapters, SQLite retirement journal and SDK spend verifier run.
  follower.inclusion.mockImplementation(async () =>
    io.included ? io.inclusion : null,
  );
  follower.transaction.mockImplementation(async () => {
    if (io.raw === undefined)
      throw new Error("raw transaction fixture missing");
    return { ...io.raw, confirmationDepth: io.tip - 100 + 1 };
  });
  follower.outrefs.mockImplementation(async () => {
    if (io.spend === undefined) throw new Error("spend fixture missing");
    return { outputs: [], spends: [io.spend] };
  });
  follower.predecessor.mockResolvedValue({
    slot: "99",
    blockHash: "bb".repeat(32),
  });
  const intake = createWatcherAvailabilityObservation({
    identity: { manifestId: "deployment", blueprintHash: "blueprint" },
    deployment: {},
    l1: availabilityFollowerL1(follower),
    confirmationDepth: 30,
  } as unknown as Parameters<typeof createWatcherAvailabilityObservation>[0]);
  const dir = mkdtempSync(join(tmpdir(), "availability-depth-convention-"));
  dirs.push(dir);
  const journal = openAvailabilityOperationJournal(join(dir, "journal.sqlite"));
  const lease = journal.acquire("actor", "owner", Date.now(), 60000);
  journal.persist(lease, baseIntent, Date.now());
  journal.transition(lease, "open", "confirmed", "100:aa", null, Date.now());
  return { observation, intake, journal, lease };
};
it.each([2160, 2161])(
  "own terminal retention uses after-inclusion distance %s",
  async (after) => {
    const { observation, intake, journal, lease } = setup(after);
    try {
      journal.persist(lease, closeIntent, Date.now());
      journal.transition(
        lease,
        "close",
        "confirmed",
        "100:aa",
        null,
        Date.now(),
      );
      const found = await intake.operation(observation, closeIntent);
      if (found.status !== "included")
        throw new Error("Expected exact signed-body inclusion");
      journal.retire(
        lease,
        "close",
        {
          confirmationDepth: found.confirmationDepth,
          currentSlot: found.currentSlot,
          currentBlockNo: Number(observation.nativePoint.blockNo),
          recoveryDepth: 2160,
        },
        Date.now(),
      );
      expect(journal.get("close") !== null).toBe(after === 2160);
    } finally {
      journal.release(lease);
      journal.close();
    }
  },
);
it.each([2160, 2161])(
  "foreign workflow release remains reversible at after-inclusion distance %s",
  async (after) => {
    const { observation, intake, journal, lease } = setup(after);
    journal.release(lease);
    try {
      await releaseWatcherAvailabilityWorkflows(
        journal,
        "actor",
        (open, header) => intake.workflowRelease(observation, open, header),
        () => {},
      );
      expect(journal.unsettledReleases("actor")).toHaveLength(
        after === 2160 ? 1 : 0,
      );
    } finally {
      journal.close();
    }
  },
);
it.each([2160, 2161])(
  "foreign expiry evidence reports after-inclusion distance %s",
  async (after) => {
    const { observation, intake, journal, lease } = setup(after);
    io.included = false;
    try {
      const found = await intake.operation(observation, closeIntent);
      if (found.status !== "inputs_missing")
        throw new Error("Expected foreign-spend expiry evidence");
      expect(found.foreignSpends?.[0]?.confirmationDepth).toBe(after);
    } finally {
      journal.release(lease);
      journal.close();
    }
  },
);
