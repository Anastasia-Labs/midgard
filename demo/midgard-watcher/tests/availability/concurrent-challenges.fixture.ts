import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  type AvailabilityOperationJournal,
  openAvailabilityOperationJournal,
} from "@al-ft/midgard-core/availability-operation-journal";
import type { LocalKupmiosFraudProofRawSource } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { type UTxO } from "@lucid-evolution/lucid";
import { afterEach, beforeEach, vi } from "vitest";

import {
  createWatcherAvailabilityRuntime,
  watcherAvailabilityTimeoutCollateralLovelace,
} from "../../src/availability/runtime.js";
import type { WatcherAuthenticatedStateQueueObservation } from "../../src/indexers/authenticated-state-queue-observation.js";
import type { VerifiedWatcherDeploymentIdentity } from "../../src/runtime/deployment-identity.js";
import type { WatcherProcessConfig } from "../../src/runtime/process-config.js";
import {
  DA_BOND_POOL_POLICY_ID,
  fixture,
  parametersFixture,
  utxo,
} from "../support/availability-challenge-fixture.js";

// The real runtime reconciliation and the real availability journal decide
// what runs; only chain I/O, signing and transaction bodies are stubbed. The
// stub runner exercises the journal exactly as the SDK runner does: it
// re-checks the workflow rule, builds, persists the intent and confirms it.
// A test that needs the SDK's own runner and reconciliation (with signed
// transaction bytes) routes `run` and `reconcile` to the real functions.
const io = vi.hoisted(() => ({
  utxos: [] as UTxO[],
  pool: vi.fn(),
  snapshot: vi.fn(),
  attestedCommitment: vi.fn(),
  run: vi.fn(),
  opens: [] as { validTo: bigint }[],
  timeouts: [] as { pool: UTxO }[],
  tipPool: undefined as UTxO | undefined,
  /**
   * `sdk` reads `tipPool` through the SDK's own `fetchDaBondPool` (its pool
   * authentication and datum decoder); `stub` hands it back as a Bonded pool.
   */
  tipPoolRead: "stub" as "stub" | "sdk",
  workflowRelease: vi.fn(),
  reconcile: vi.fn(),
  operation: vi.fn(),
  submitTx: vi.fn(),
  walletAddress: "availability",
  queuePolicy: "policy",
  limits: undefined as SDK.DaAvailabilityOperationLimits | undefined,
  timeoutTx: undefined as ((input: TimeoutInput) => unknown) | undefined,
}));
export type TimeoutInput = Parameters<
  typeof SDK.buildTimeoutDaAvailabilityChallengeTxProgram
>[2];
vi.mock("@al-ft/midgard-sdk", async (original) => {
  const { Effect } = await import("effect");
  const actual = await original<typeof import("@al-ft/midgard-sdk")>();
  return {
    ...actual,
    daAvailabilityOperationLimits: () => io.limits ?? {},
    reconcileDaAvailabilityOperations: io.reconcile,
    runDaAvailabilityOperation: io.run,
    buildDaAvailabilityFundingPreparationTx: async () => "prepare-tx",
    buildOpenDaAvailabilityChallengeTxProgram: (
      _lucid: unknown,
      _deployment: unknown,
      input: { validTo: bigint },
    ) => {
      io.opens.push(input);
      return Effect.succeed({ tx: "open-tx" });
    },
    buildCloseDaAvailabilityChallengeTxProgram: () =>
      Effect.succeed({ tx: "close-tx" }),
    buildTimeoutDaAvailabilityChallengeTxProgram: (
      _lucid: unknown,
      _deployment: unknown,
      input: TimeoutInput,
    ) => {
      io.timeouts.push(input);
      return Effect.succeed({
        tx: io.timeoutTx?.(input) ?? "timeout-tx",
        timeoutFeePartLovelace: 1n,
      });
    },
    fetchDaBondPool: async (
      _lucid: unknown,
      input: { policyId: string; address: string },
    ) => {
      if (input.policyId !== DA_BOND_POOL_POLICY_ID || input.address !== "pool")
        throw new Error("DA bond pool read at the wrong identity");
      if (io.tipPoolRead === "sdk")
        return actual.fetchDaBondPool(
          {
            utxosAtWithUnit: async () =>
              io.tipPool === undefined ? [] : [io.tipPool],
          } as unknown as Parameters<typeof actual.fetchDaBondPool>[0],
          input,
        );
      if (io.tipPool === undefined) throw new Error("no DA bond pool at tip");
      return { utxo: io.tipPool, datum: "Bonded" };
    },
    fetchSortedStateQueueUTxOsProgram: () => Effect.succeed([{ utxo: {} }]),
  };
});
vi.mock("@lucid-evolution/lucid", async (original) => ({
  ...(await original<typeof import("@lucid-evolution/lucid")>()),
  Lucid: async () => ({
    selectWallet: { fromSeed() {} },
    wallet: () => ({
      address: async () => io.walletAddress,
      getUtxos: async () => io.utxos,
    }),
    switchProvider: async () => {},
    config: () => ({
      protocolParameters: { coinsPerUtxoByte: 1n, collateralPercentage: 150 },
      provider: { submitTx: io.submitTx },
    }),
  }),
  calculateMinLovelaceFromUTxO: () => MIN_CHANGE,
  paymentCredentialOf: (address: string) => ({ hash: address }),
}));
vi.mock("@lucid-evolution/scalus-uplc", () => ({
  createScalusEvaluator: () => ({}),
}));
vi.mock("../../src/l1/native-reward-account.js", () => ({
  WatcherLocalKupmios: class {},
}));
vi.mock("../../src/runtime/process-config.js", () => ({
  loadWatcherSecretText: async () => "seed",
}));
vi.mock("../../src/indexers/authenticated-state-queue-observation.js", () => ({
  assertWatcherStateQueueObservation() {},
}));
vi.mock("../../src/storage/retained-da-runtime.js", () => ({
  createWatcherRetainedDaRuntime: async () => ({
    sources: [
      {
        sourceId: "public",
        fetchPayloadByHeaderHash: async () => ({ ok: false, attempts: [] }),
      },
    ],
    async close() {},
  }),
  bindWatcherL1AvailabilityPayloadSource: () => ({ close() {} }),
}));
vi.mock("../../src/availability/deployment.js", async () => {
  const support = await import("../support/availability-challenge-fixture.js");
  return {
    createWatcherAvailabilityDeployment: async () => ({
      contracts: {
        stateQueue: {
          policyId: io.queuePolicy,
          spendingScriptAddress: "queue",
        },
        daBondPool: {
          policyId: support.DA_BOND_POOL_POLICY_ID,
          spendingScriptAddress: "pool",
        },
      },
      parameters: support.parametersFixture(),
    }),
  };
});
vi.mock("../../src/availability/observation.js", () => ({
  createWatcherAvailabilityObservation: () => ({
    pool: io.pool,
    snapshot: io.snapshot,
    attestedCommitment: io.attestedCommitment,
    workflowRelease: io.workflowRelease,
    operation: io.operation,
    confirmationDepth: 1,
  }),
}));
vi.mock("../../src/availability/published-payload.js", () => ({
  createWatcherL1AvailabilityPayloadSource: () => ({}),
}));

export const MIN_CHANGE = 1_000_000n;
export const ACTOR = "availability";
export const DEPLOYMENT = "deployment";
export const PARAMETERS = parametersFixture();
export const TIMEOUT_COLLATERAL = watcherAvailabilityTimeoutCollateralLovelace({
  parameters: PARAMETERS,
  collateralPercentage: 150,
  minimumReturnLovelace: MIN_CHANGE,
});
export const OPENING =
  PARAMETERS.challenger_bond_lovelace +
  PARAMETERS.challenge_record_lovelace +
  PARAMETERS.max_open_fee_lovelace;
export const ADA = 1_000_000n;

let directory: string;
let journalPath: string;
beforeEach(() => {
  directory = mkdtempSync(join(tmpdir(), "watcher-concurrent-challenges-"));
  journalPath = join(directory, "journal.sqlite");
  io.pool.mockReset().mockResolvedValue(undefined);
  io.snapshot.mockReset();
  io.attestedCommitment.mockReset();
  io.utxos = [];
  io.opens = [];
  io.timeouts = [];
  io.tipPool = undefined;
  io.tipPoolRead = "stub";
  io.workflowRelease.mockReset().mockResolvedValue(undefined);
  io.run.mockReset().mockImplementation(stubRunner);
  io.reconcile.mockReset().mockResolvedValue([]);
  io.operation.mockReset();
  io.submitTx.mockReset().mockResolvedValue("submitted");
  io.walletAddress = "availability";
  io.queuePolicy = "policy";
  io.limits = undefined;
  io.timeoutTx = undefined;
});
afterEach(() => {
  rmSync(directory, { recursive: true, force: true });
});

const stubRunner = async (
  context: SDK.DaAvailabilityOperationContext,
  operation: Readonly<{
    headerHash: string;
    action: string;
    completesWorkflow?: boolean;
    build: () => Promise<unknown>;
  }>,
): Promise<SDK.DaAvailabilityOperationResult> => {
  const now = Date.now();
  const lease = context.journal.acquire(context.actor, "runner", now, 60_000);
  try {
    context.journal.assertWorkflow(
      lease,
      context.deploymentIdentity,
      operation.headerHash,
      operation.action,
      now,
    );
    await operation.build();
    const id = `${operation.action}-${operation.headerHash}`;
    context.journal.persist(
      lease,
      {
        id,
        deploymentIdentity: context.deploymentIdentity,
        actor: context.actor,
        headerHash: operation.headerHash,
        action: operation.action,
        signedCbor: id,
        txHash: id,
        spentOutRefs: [`${id}#0`],
        collateralOutRefs: [],
        expectedOutRefs: [],
        validUntilSlot: 1,
        completesWorkflow: operation.completesWorkflow ?? false,
      },
      now,
    );
    context.journal.transition(lease, id, "confirmed", "block", null, now);
    return { status: "submitted", txHash: id, expectedOutRefs: [] };
  } finally {
    context.journal.release(lease);
  }
};

/** Records `header`'s landed Open in the shared journal, as a restart finds it. */
export const withJournal = <T>(
  run: (journal: AvailabilityOperationJournal) => T,
) => {
  const journal = openAvailabilityOperationJournal(journalPath);
  try {
    return run(journal);
  } finally {
    journal.close();
  }
};
export const openLanded = (
  headerHash: string,
  deploymentIdentity = DEPLOYMENT,
) =>
  withJournal((journal) => {
    const lease = journal.acquire(ACTOR, "earlier", Date.now(), 60_000);
    const id = `open-${headerHash}`;
    journal.persist(
      lease,
      {
        id,
        deploymentIdentity,
        actor: ACTOR,
        headerHash,
        action: "open",
        signedCbor: id,
        txHash: id,
        spentOutRefs: [`${id}#0`],
        collateralOutRefs: [],
        expectedOutRefs: [],
        validUntilSlot: 1,
        completesWorkflow: false,
      },
      Date.now(),
    );
    journal.transition(lease, id, "confirmed", "block", null, Date.now());
    journal.release(lease);
  });
export const workflowLive = (headerHash: string): boolean =>
  withJournal((journal) => {
    const lease = journal.acquire(ACTOR, "probe", Date.now(), 60_000);
    try {
      journal.assertWorkflow(
        lease,
        DEPLOYMENT,
        headerHash,
        "prepare",
        Date.now(),
      );
      return false;
    } catch {
      return true;
    } finally {
      journal.release(lease);
    }
  });

/** Every live workflow row of the actor, as (deployment, header). */
export const workflowRows = () =>
  withJournal((journal) =>
    journal
      .workflows(ACTOR)
      .map(({ deploymentIdentity, headerHash }) => [
        deploymentIdentity,
        headerHash,
      ]),
  );

export const runtime = (
  manifestId = DEPLOYMENT,
  extra: Pick<
    Parameters<typeof createWatcherAvailabilityRuntime>[0],
    "onDaBondPool" | "onDaBondPoolReadFailure"
  > = {
    onDaBondPool: () => undefined,
    onDaBondPoolReadFailure: () => undefined,
  },
) =>
  createWatcherAvailabilityRuntime({
    ...extra,
    config: {
      watcherConfig: {
        l1: {
          source: {
            sourceMode: "local_node",
            queryServices: [
              { kind: "kupo", endpoint: "http://kupo" },
              { kind: "ogmios", endpoint: "http://ogmios" },
            ],
          },
        },
      },
      availability: {
        keySource: {},
        journalPath,
        minimumFundingLovelace: "0",
      },
    } as unknown as WatcherProcessConfig,
    identity: {
      network: "Custom",
      manifestId,
    } as VerifiedWatcherDeploymentIdentity,
    rawSource: {} as LocalKupmiosFraudProofRawSource,
    proverWalletAddress: "prover",
  });

export const observation = (
  snapshots: readonly SDK.DaAvailabilityChallengeSnapshot[],
): WatcherAuthenticatedStateQueueObservation => {
  io.snapshot.mockImplementation(async (_observation, headerHash: string) =>
    snapshots.find((snapshot) => snapshot.headerHash === headerHash),
  );
  return {
    observationDigest: "digest",
    deploymentIdentityDigest: DEPLOYMENT,
    nativePoint: {
      slot: "1",
      blockNo: "1",
      blockHash: "11".repeat(32),
      finalityDepth: "30",
    },
    finalizedHeaders: snapshots.map(({ headerHash }) => ({
      headerHash,
      daAvailability: { Attested: {} },
    })),
    finalizedCorrectionLock: { datum: "Idle" },
  } as unknown as WatcherAuthenticatedStateQueueObservation;
};

/** A withheld header whose Open deadline is still ahead. */
export const withheld = (headerByte: string) => {
  const header = fixture(headerByte, BigInt(Date.now()));
  io.attestedCommitment.mockImplementation(
    async (_observation, headerHash: string) =>
      headerHash === header.attested.headerHash ? header.commitment : undefined,
  );
  return header;
};
/** A challenge opened now, so its response window is still running. */
export const liveChallenge = (headerByte: string) => {
  const now = BigInt(Date.now());
  return fixture(headerByte, now, now);
};
/** A live challenge whose every tranche was answered: its next step is close. */
export const answered = (snapshot: SDK.DaAvailabilityChallengeSnapshot) => ({
  ...snapshot,
  terminalDatum: {
    ...snapshot.terminalDatum!,
    next_tranche_index: BigInt(
      snapshot.recordDatum!.commitment.tranche_descriptors.length,
    ),
    has_timed_out_tranche: false,
  },
});
/** An expired challenge at the queue head: its next step is Timeout. */
export const timedOut = (snapshot: SDK.DaAvailabilityChallengeSnapshot) => ({
  ...snapshot,
  pool: { ...utxo(8, 100_000n * ADA), address: "pool" },
  terminalDatum: {
    ...snapshot.terminalDatum!,
    next_tranche_index: BigInt(
      snapshot.recordDatum!.commitment.tranche_descriptors.length,
    ),
    has_timed_out_tranche: true,
  },
});
export const actions = () =>
  io.run.mock.calls.map(([, operation]) => [
    operation.headerHash,
    operation.action,
  ]);

export { io };
