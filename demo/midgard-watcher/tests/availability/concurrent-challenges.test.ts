import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  type AvailabilityOperationJournal,
  openAvailabilityOperationJournal,
} from "@al-ft/midgard-core/availability-operation-journal";
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import type { LocalKupmiosFraudProofRawSource } from "@al-ft/midgard-fault-proofs";
import type * as SDK from "@al-ft/midgard-sdk";
import {
  assertDaAvailabilityOpenWithinChallengeWindow,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
} from "@al-ft/midgard-sdk";
import { CML, type UTxO } from "@lucid-evolution/lucid";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import {
  createWatcherAvailabilityRuntime,
  watcherAvailabilityTimeoutCollateralLovelace,
} from "../../src/availability/runtime.js";
import type { WatcherAuthenticatedStateQueueObservation } from "../../src/indexers/authenticated-state-queue-observation.js";
import type { VerifiedWatcherDeploymentIdentity } from "../../src/runtime/deployment-identity.js";
import type { WatcherProcessConfig } from "../../src/runtime/process-config.js";
import {
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
  snapshot: vi.fn(),
  attestedCommitment: vi.fn(),
  run: vi.fn(),
  opens: [] as { validTo: bigint }[],
  timeouts: [] as { pool: UTxO }[],
  tipPool: undefined as UTxO | undefined,
  workflowRelease: vi.fn(),
  reconcile: vi.fn(),
  operation: vi.fn(),
  submitTx: vi.fn(),
  walletAddress: "availability",
  queuePolicy: "policy",
  limits: undefined as SDK.DaAvailabilityOperationLimits | undefined,
  timeoutTx: undefined as ((input: TimeoutInput) => unknown) | undefined,
}));
type TimeoutInput = Readonly<{
  pool: UTxO;
  record: UTxO;
  terminal: UTxO;
  queue: UTxO;
  collateralInputs: readonly UTxO[];
}>;
vi.mock("@al-ft/midgard-sdk", async (original) => {
  const { Effect } = await import("effect");
  return {
    ...(await original<typeof import("@al-ft/midgard-sdk")>()),
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
      if (input.policyId !== "pool-policy" || input.address !== "pool")
        throw new Error("DA bond pool read at the wrong identity");
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
          policyId: "pool-policy",
          spendingScriptAddress: "pool",
        },
      },
      parameters: support.parametersFixture(),
    }),
  };
});
vi.mock("../../src/availability/observation.js", () => ({
  createWatcherAvailabilityObservation: () => ({
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

const MIN_CHANGE = 1_000_000n;
const ACTOR = "availability";
const DEPLOYMENT = "deployment";
const PARAMETERS = parametersFixture();
const TIMEOUT_COLLATERAL = watcherAvailabilityTimeoutCollateralLovelace({
  parameters: PARAMETERS,
  collateralPercentage: 150,
  minimumReturnLovelace: MIN_CHANGE,
});
const OPENING =
  PARAMETERS.challenger_bond_lovelace +
  PARAMETERS.challenge_record_lovelace +
  PARAMETERS.max_open_fee_lovelace;
const ADA = 1_000_000n;

let directory: string;
let journalPath: string;
beforeEach(() => {
  directory = mkdtempSync(join(tmpdir(), "watcher-concurrent-challenges-"));
  journalPath = join(directory, "journal.sqlite");
  io.snapshot.mockReset();
  io.attestedCommitment.mockReset();
  io.utxos = [];
  io.opens = [];
  io.timeouts = [];
  io.tipPool = undefined;
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
const withJournal = <T>(run: (journal: AvailabilityOperationJournal) => T) => {
  const journal = openAvailabilityOperationJournal(journalPath);
  try {
    return run(journal);
  } finally {
    journal.close();
  }
};
const openLanded = (headerHash: string, deploymentIdentity = DEPLOYMENT) =>
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
const workflowLive = (headerHash: string): boolean =>
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
const workflowRows = () =>
  withJournal((journal) =>
    journal
      .workflows(ACTOR)
      .map(({ deploymentIdentity, headerHash }) => [
        deploymentIdentity,
        headerHash,
      ]),
  );

const runtime = (manifestId = DEPLOYMENT) =>
  createWatcherAvailabilityRuntime({
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

const observation = (
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
const withheld = (headerByte: string) => {
  const header = fixture(headerByte, BigInt(Date.now()));
  io.attestedCommitment.mockImplementation(
    async (_observation, headerHash: string) =>
      headerHash === header.attested.headerHash ? header.commitment : undefined,
  );
  return header;
};
/** A challenge opened now, so its response window is still running. */
const liveChallenge = (headerByte: string) => {
  const now = BigInt(Date.now());
  return fixture(headerByte, now, now);
};
/** A live challenge whose every tranche was answered: its next step is close. */
const answered = (snapshot: SDK.DaAvailabilityChallengeSnapshot) => ({
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
const timedOut = (snapshot: SDK.DaAvailabilityChallengeSnapshot) => ({
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
const actions = () =>
  io.run.mock.calls.map(([, operation]) => [
    operation.headerHash,
    operation.action,
  ]);

describe("concurrent availability challenges on one wallet (spec #685 E3, P9)", () => {
  it("prepares and Opens a second withheld header while the first header's challenge is live", async () => {
    const first = liveChallenge("44");
    const second = withheld("45");
    openLanded(first.challenged.headerHash);
    const collateral = utxo(0, TIMEOUT_COLLATERAL, "a1");
    io.utxos = [collateral, utxo(1, OPENING + 100n * ADA, "a2")];
    const watcher = await runtime();
    try {
      const state = observation([first.challenged, second.attested]);
      await watcher.reconcile(state, true);
      expect(watcher.status()).toMatchObject({ action: "prepare" });
      // The preparation produced the exact challenger coin.
      io.utxos = [
        collateral,
        utxo(0, OPENING, "a3"),
        utxo(1, 100n * ADA, "a3"),
      ];
      await watcher.reconcile(state, true);
      expect(watcher.status()).toMatchObject({ action: "open" });
      expect(watcher.status().openRefused).toBeUndefined();
    } finally {
      await watcher.close();
    }
    expect(actions()).toEqual([
      [second.attested.headerHash, "prepare"],
      [second.attested.headerHash, "open"],
    ]);
    // Both challenges now hold their own workflow in the one journal.
    expect(workflowLive(first.challenged.headerHash)).toBe(true);
    expect(workflowLive(second.attested.headerHash)).toBe(true);
  });

  it("refuses an unfundable Open with a typed reason and still closes the live challenge in the same reconciliation", async () => {
    const first = liveChallenge("44");
    const second = withheld("45");
    openLanded(first.challenged.headerHash);
    // Enough for a close's collateral, never for a Timeout's.
    io.utxos = [utxo(0, 5n * ADA, "b1"), utxo(1, 5n * ADA, "b2")];
    const watcher = await runtime();
    try {
      await watcher.reconcile(
        observation([answered(first.challenged), second.attested]),
        true,
      );
      expect(watcher.status()).toMatchObject({
        phase: "waiting",
        action: "close",
        openRefused: [
          {
            headerHash: second.attested.headerHash,
            reason: "insufficient-availability-capital",
            requiredLovelace: TIMEOUT_COLLATERAL.toString(),
            availableLovelace: (10n * ADA).toString(),
          },
        ],
      });
    } finally {
      await watcher.close();
    }
    expect(actions()).toEqual([[first.challenged.headerHash, "close"]]);
  });

  it("refuses an Open the wallet cannot bond and still times out the expired challenge in the same reconciliation", async () => {
    const first = fixture("44", BigInt(Date.now()));
    const second = withheld("45");
    openLanded(first.challenged.headerHash);
    io.utxos = [utxo(0, TIMEOUT_COLLATERAL, "c1"), utxo(1, 10n * ADA, "c2")];
    // Someone topped the shared pool up after the finalized snapshot.
    io.tipPool = { ...utxo(9, 100_005n * ADA, "c9"), address: "pool" };
    const watcher = await runtime();
    try {
      await watcher.reconcile(
        observation([timedOut(first.challenged), second.attested]),
        true,
      );
      expect(watcher.status()).toMatchObject({
        phase: "waiting",
        action: "timeout",
        openRefused: [
          {
            headerHash: second.attested.headerHash,
            reason: "insufficient-availability-capital",
            availableLovelace: (10n * ADA).toString(),
          },
        ],
      });
    } finally {
      await watcher.close();
    }
    expect(actions()).toEqual([[first.challenged.headerHash, "timeout"]]);
    // The Timeout spends the pool as it stands at the tip, not the finalized
    // snapshot's outref, which the TopUp already spent.
    expect(io.timeouts.map(({ pool }) => pool)).toEqual([io.tipPool]);
  });

  it("defers a Timeout whose pool is not found at the tip and still closes another live challenge in the same reconciliation (P13(2))", async () => {
    const expired = fixture("44", BigInt(Date.now()));
    const complete = liveChallenge("45");
    openLanded(expired.challenged.headerHash);
    openLanded(complete.challenged.headerHash);
    io.utxos = [utxo(0, TIMEOUT_COLLATERAL, "f1"), utxo(1, 10n * ADA, "f2")];
    // No authentic pool at the tip: the Timeout is not built this tick, and
    // never against the finalized snapshot's pool.
    io.tipPool = undefined;
    const watcher = await runtime();
    try {
      // Non-Open steps keep snapshot order, so the Timeout is tried first.
      await watcher.reconcile(
        observation([
          timedOut(expired.challenged),
          answered(complete.challenged),
        ]),
        true,
      );
      expect(watcher.status()).toMatchObject({
        phase: "waiting",
        action: "close",
        timeoutsDeferred: [
          {
            headerHash: expired.challenged.headerHash,
            reason: "tip-pool-unavailable",
            detail: "no DA bond pool at tip",
          },
        ],
      });
    } finally {
      await watcher.close();
    }
    expect(actions()).toEqual([[complete.challenged.headerHash, "close"]]);
    expect(io.timeouts).toEqual([]);
  });

  it("reports every step the journal refuses while another deployment's challenge is live, staying ready", async () => {
    const header = withheld("48");
    openLanded("49".repeat(28), "other-deployment");
    io.utxos = [
      utxo(0, TIMEOUT_COLLATERAL, "g1"),
      utxo(1, OPENING, "g2"),
      utxo(2, 100n * ADA, "g2"),
    ];
    const watcher = await runtime();
    try {
      await watcher.reconcile(observation([header.attested]), true);
      expect(watcher.status()).toMatchObject({
        phase: "ready",
        workflowRefused: [
          {
            headerHash: header.attested.headerHash,
            action: "open",
            detail: `Availability wallet capital belongs to an unresolved challenge workflow in another deployment (deployment other-deployment, header ${"49".repeat(28)})`,
          },
        ],
      });
      expect(watcher.status().workflowReleased).toBeUndefined();
    } finally {
      await watcher.close();
    }
    expect(actions()).toEqual([]);
    // The release check ran on the other deployment's Open and found no
    // terminal step, so the row stays.
    expect(io.workflowRelease).toHaveBeenCalledWith(
      expect.anything(),
      expect.objectContaining({
        state: "confirmed",
        intent: expect.objectContaining({ id: `open-${"49".repeat(28)}` }),
      }),
      "49".repeat(28),
    );
    expect(workflowRows()).toEqual([["other-deployment", "49".repeat(28)]]);
  });

  describe("releases a workflow someone else's terminal step ended (P20)", () => {
    const OTHER = "49".repeat(28);
    const CLOSE = {
      reason: "challenge-closed" as const,
      txHash: "ce".repeat(32),
      spendPoint: "10:ab",
      confirmationDepth: 30,
    };
    const walletForOpen = () => [
      utxo(0, TIMEOUT_COLLATERAL, "g1"),
      utxo(1, OPENING, "g2"),
      utxo(2, 100n * ADA, "g2"),
    ];

    it("releases the other deployment's row on a verified committee Close and admits this deployment's Open in the same reconciliation", async () => {
      const header = withheld("48");
      openLanded(OTHER, "other-deployment");
      io.utxos = walletForOpen();
      io.workflowRelease.mockImplementation(
        async (_observation, _open, headerHash: string) =>
          headerHash === OTHER ? CLOSE : undefined,
      );
      const watcher = await runtime();
      try {
        await watcher.reconcile(observation([header.attested]), true);
        expect(watcher.status()).toMatchObject({
          action: "open",
          workflowReleased: [
            {
              deployment: "other-deployment",
              headerHash: OTHER,
              reason: "challenge-closed",
              txHash: CLOSE.txHash,
              spendPoint: CLOSE.spendPoint,
            },
          ],
        });
        expect(watcher.status().workflowRefused).toBeUndefined();
      } finally {
        await watcher.close();
      }
      expect(actions()).toEqual([[header.attested.headerHash, "open"]]);
      // Only the released row went; this deployment's new Open holds its own.
      expect(workflowRows()).toEqual([
        [DEPLOYMENT, header.attested.headerHash],
      ]);
    });

    it("keeps the row and reports it when the release check fails, without aborting the reconciliation", async () => {
      const header = withheld("48");
      openLanded(OTHER, "other-deployment");
      io.utxos = walletForOpen();
      io.workflowRelease.mockRejectedValue(new Error("Ogmios unreachable"));
      const watcher = await runtime();
      try {
        await watcher.reconcile(observation([header.attested]), true);
        expect(watcher.status()).toMatchObject({
          phase: "ready",
          workflowReleaseDeferred: [
            {
              deployment: "other-deployment",
              headerHash: OTHER,
              detail: "Ogmios unreachable",
            },
          ],
          workflowRefused: [
            { headerHash: header.attested.headerHash, action: "open" },
          ],
        });
        expect(watcher.status().workflowReleased).toBeUndefined();
      } finally {
        await watcher.close();
      }
      expect(actions()).toEqual([]);
      expect(workflowRows()).toEqual([["other-deployment", OTHER]]);
    });

    it("keeps the row while this actor's own step for that header is unresolved", async () => {
      const header = withheld("48");
      openLanded(OTHER, "other-deployment");
      withJournal((journal) => {
        const lease = journal.acquire(ACTOR, "earlier", Date.now(), 60_000);
        const id = `close-${OTHER}`;
        journal.persist(
          lease,
          {
            id,
            deploymentIdentity: "other-deployment",
            actor: ACTOR,
            headerHash: OTHER,
            action: "close",
            signedCbor: id,
            txHash: id,
            spentOutRefs: [`${id}#0`],
            collateralOutRefs: [],
            expectedOutRefs: [],
            validUntilSlot: 1,
            completesWorkflow: true,
          },
          Date.now(),
        );
        journal.release(lease);
      });
      io.utxos = walletForOpen();
      io.workflowRelease.mockResolvedValue(CLOSE);
      const watcher = await runtime();
      try {
        await watcher.reconcile(observation([header.attested]), true);
        expect(watcher.status()).toMatchObject({
          workflowReleaseDeferred: [
            {
              deployment: "other-deployment",
              headerHash: OTHER,
              detail:
                "Availability workflow release requires no unresolved intent for the header",
            },
          ],
          workflowRefused: [
            { headerHash: header.attested.headerHash, action: "open" },
          ],
        });
      } finally {
        await watcher.close();
      }
      expect(actions()).toEqual([]);
      expect(workflowRows()).toEqual([["other-deployment", OTHER]]);
    });
  });

  it("caps an Open taken in the last minute of the window at the header's Open deadline", async () => {
    const window = BigInt(
      SELECTED_DEPLOYMENT_PROFILE.timing.da_challenge_window_ms,
    );
    // The deadline is 30 s away: inside the default 60 s validity horizon.
    const endTime = BigInt(Date.now()) - window + 30_000n;
    const header = fixture("47", endTime);
    io.attestedCommitment.mockImplementation(async () => header.commitment);
    io.utxos = [
      utxo(0, TIMEOUT_COLLATERAL, "e1"),
      utxo(1, OPENING, "e2"),
      utxo(2, 100n * ADA, "e2"),
    ];
    const watcher = await runtime();
    try {
      await watcher.reconcile(observation([header.attested]), true);
      expect(watcher.status()).toMatchObject({ action: "open" });
    } finally {
      await watcher.close();
    }
    const deadline = endTime + window;
    expect(io.opens.map(({ validTo }) => validTo)).toEqual([deadline]);
    // The SDK's window check admits the capped bound and refuses the uncapped
    // one the Open would otherwise carry.
    const accepts = (validTo: bigint) => () =>
      assertDaAvailabilityOpenWithinChallengeWindow({
        validTo,
        nodeEndTime: endTime,
        daChallengeWindowMs: window,
      });
    expect(accepts(deadline)).not.toThrow();
    expect(accepts(deadline + 30_000n)).toThrow("Challenge window closed");
  });
});

/**
 * A signed Timeout as the SDK runner persists it: the pool, record, terminal
 * and queue node as normal inputs, the wallet collateral, the target-node burn
 * and a redeemer, valid from `slot` to `slot + 1000`.
 */
const timeoutTransaction = (input: TimeoutInput, slot: number): string => {
  const outRef = (utxo: UTxO) =>
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(utxo.txHash),
      BigInt(utxo.outputIndex),
    );
  const inputs = CML.TransactionInputList.new();
  [input.pool, input.record, input.terminal, input.queue]
    .filter(
      (utxo, index, all) =>
        all.findIndex(
          (other) =>
            other.txHash === utxo.txHash &&
            other.outputIndex === utxo.outputIndex,
        ) === index,
    )
    .forEach((utxo) => inputs.add(outRef(utxo)));
  const body = CML.TransactionBody.new(
    inputs,
    CML.TransactionOutputList.new(),
    200_000n,
  );
  body.set_validity_interval_start(BigInt(slot));
  body.set_ttl(BigInt(slot + 1_000));
  const collateral = CML.TransactionInputList.new();
  input.collateralInputs.forEach((utxo) => collateral.add(outRef(utxo)));
  body.set_collateral_inputs(collateral);
  const mint = CML.Mint.new();
  mint.set(
    CML.ScriptHash.from_hex(io.queuePolicy),
    CML.AssetName.from_hex(
      STATE_QUEUE_NODE_ASSET_NAME_PREFIX + TIMED_OUT_HEADER,
    ),
    -1n,
  );
  body.set_mint(mint);
  const redeemers = CML.LegacyRedeemerList.new();
  redeemers.add(
    CML.LegacyRedeemer.new(
      CML.RedeemerTag.Spend,
      0n,
      CML.PlutusData.new_integer(CML.BigInteger.from_str("0")),
      CML.ExUnits.new(1_000_000n, 100_000_000n),
    ),
  );
  const witnesses = CML.TransactionWitnessSet.new();
  witnesses.set_redeemers(CML.Redeemers.new_arr_legacy_redeemer(redeemers));
  return CML.Transaction.new(body, witnesses, true, undefined).to_cbor_hex();
};
/** The builder the SDK runner signs with the wallet's key. */
const signable = (cbor: string, key: CML.PrivateKey) => ({
  toTransaction: () => CML.Transaction.from_cbor_hex(cbor),
  sign: {
    withWallet: () => ({
      complete: async () => {
        const unsigned = CML.Transaction.from_cbor_hex(cbor);
        const vkeys = CML.VkeywitnessList.new();
        vkeys.add(
          CML.make_vkey_witness(CML.hash_transaction(unsigned.body()), key),
        );
        const witnesses = unsigned.witness_set();
        witnesses.set_vkeywitnesses(vkeys);
        const signed = CML.Transaction.new(
          unsigned.body(),
          witnesses,
          true,
          undefined,
        ).to_cbor_hex();
        return { toCBOR: () => signed };
      },
    }),
  },
});
const txHashOf = (cbor: string) =>
  CML.hash_transaction(CML.Transaction.from_cbor_hex(cbor).body()).to_hex();
const inputsOf = (cbor: string) => {
  const inputs = CML.Transaction.from_cbor_hex(cbor).body().inputs();
  return Array.from({ length: inputs.len() }, (_, index) => {
    const input = inputs.get(index);
    return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
  });
};
const TIMED_OUT_HEADER = "44".repeat(28);

describe("a Timeout whose pool moved after it was built (P13(4)(b))", () => {
  it("expires the stranded Timeout through the partial-missing arm, then rebuilds it against the new tip pool and lands it", async () => {
    // The SDK's own runner and reconciliation, on real signed bytes.
    const actual =
      await vi.importActual<typeof import("@al-ft/midgard-sdk")>(
        "@al-ft/midgard-sdk",
      );
    io.run.mockImplementation(actual.runDaAvailabilityOperation);
    io.reconcile.mockImplementation(actual.reconcileDaAvailabilityOperations);
    const deployment = "d0".repeat(32);
    const key = CML.PrivateKey.generate_ed25519();
    // The mocked wallet's payment credential is its address.
    io.walletAddress = key.to_public().hash().to_hex();
    io.queuePolicy = "a8".repeat(28);
    io.limits = {
      maxTxSize: 16_384,
      maxTxExMem: 14_000_000n,
      maxTxExSteps: 10_000_000_000n,
      coinsPerUtxoByte: 4_310n,
      feeCeilings: { timeout: PARAMETERS.max_timeout_fee_lovelace },
      timeoutFeePartCeiling: PARAMETERS.da_slash_penalty_lovelace,
    };
    // The finalized point's slot, which reconciliation compares with each
    // intent's validity.
    let slot = 100;
    io.timeoutTx = (input) => signable(timeoutTransaction(input, slot), key);
    const broadcasts: string[] = [];
    io.submitTx.mockImplementation(async (cbor: string) => {
      broadcasts.push(cbor);
      return txHashOf(cbor);
    });
    // Canonical input state at the finalized point, as the source reports it.
    const spentElsewhere = new Set<string>();
    const landed = new Set<string>();
    io.operation.mockImplementation(
      async (
        _observation: unknown,
        intent: Readonly<{
          txHash: string;
          spentOutRefs: readonly string[];
        }>,
      ): Promise<SDK.DaAvailabilityOperationObservation> => {
        if (landed.has(intent.txHash))
          return {
            status: "included",
            txHash: intent.txHash,
            inclusionPoint: "block",
            confirmationDepth: 1,
          };
        const missingOutRefs = intent.spentOutRefs.filter((ref) =>
          spentElsewhere.has(ref),
        );
        return missingOutRefs.length === 0
          ? { status: "unspent", currentSlot: slot }
          : { status: "inputs_missing", currentSlot: slot, missingOutRefs };
      },
    );
    const expired = fixture("44", BigInt(Date.now()));
    expect(expired.challenged.headerHash).toBe(TIMED_OUT_HEADER);
    io.utxos = [utxo(0, TIMEOUT_COLLATERAL, "c1"), utxo(1, 10n * ADA, "c2")];
    const firstPool = { ...utxo(9, 100_005n * ADA, "c9"), address: "pool" };
    const toppedUp = { ...utxo(0, 101_005n * ADA, "ca"), address: "pool" };
    const poolRef = (pool: UTxO) =>
      `${pool.txHash}#${pool.outputIndex.toString()}`;
    io.tipPool = firstPool;
    const state = observation([timedOut(expired.challenged)]);
    const watcher = await runtime(deployment);
    try {
      // Tick 1 builds and broadcasts the Timeout against the tip pool.
      await watcher.reconcile(state, true);
      expect(watcher.status()).toMatchObject({ action: "timeout" });
      expect(broadcasts).toHaveLength(1);
      expect(inputsOf(broadcasts[0]!)).toContain(poolRef(firstPool));

      // A TopUp spends that pool before the Timeout lands: the Timeout's
      // only missing normal input is the pool, and its validity runs out.
      spentElsewhere.add(poolRef(firstPool));
      io.tipPool = toppedUp;
      slot = 1_100;

      // Tick 2 expires it and rebuilds against the topped-up pool.
      await watcher.reconcile(state, true);
      expect(watcher.status()).toMatchObject({
        phase: "waiting",
        action: "timeout",
      });
      expect(io.timeouts.map(({ pool }) => pool)).toEqual([
        firstPool,
        toppedUp,
      ]);
      expect(broadcasts).toHaveLength(2);
      expect(inputsOf(broadcasts[1]!)).toContain(poolRef(toppedUp));
      expect(inputsOf(broadcasts[1]!)).not.toContain(poolRef(firstPool));

      // Tick 3 (observing only) finds the rebuilt Timeout finalized.
      landed.add(txHashOf(broadcasts[1]!));
      await watcher.reconcile(state, false);
      expect(watcher.status()).toMatchObject({ phase: "ready" });
    } finally {
      await watcher.close();
    }
    expect(actions()).toEqual([
      [TIMED_OUT_HEADER, "timeout"],
      [TIMED_OUT_HEADER, "timeout"],
    ]);
    withJournal((journal) => {
      expect(journal.findTransaction(txHashOf(broadcasts[0]!))).toMatchObject({
        state: "expired",
        detail:
          "Expired with a normal input spent elsewhere and another still unspent",
      });
      expect(journal.findTransaction(txHashOf(broadcasts[1]!))).toMatchObject({
        state: "confirmed",
      });
      expect(journal.pending(deployment, io.walletAddress)).toEqual([]);
      expect(journal.reservedOutRefs(io.walletAddress)).toEqual([]);
    });
  });
});

describe("pool alerts never block the availability runtime (spec #685 E5)", () => {
  const backed =
    PARAMETERS.da_bond_pool_floor_lovelace + PARAMETERS.da_bond_lovelace;
  const withPool = (
    snapshot: SDK.DaAvailabilityChallengeSnapshot,
    lovelace: bigint,
    poolDatum: SDK.DaBondPoolDatum,
  ): SDK.DaAvailabilityChallengeSnapshot => ({
    ...snapshot,
    pool: { ...utxo(8, lovelace, "d9"), address: "pool" },
    poolDatum,
  });

  it.each([
    ["missing", (snapshot: SDK.DaAvailabilityChallengeSnapshot) => snapshot],
    [
      "withdrawing",
      (snapshot: SDK.DaAvailabilityChallengeSnapshot) =>
        withPool(snapshot, backed, { Withdrawing: { unlock_at: 1n } }),
    ],
    [
      "under_backed",
      (snapshot: SDK.DaAvailabilityChallengeSnapshot) =>
        withPool(snapshot, backed - 1n, "Bonded"),
    ],
  ] as const)(
    "reports a %s pool while staying ready and still Opening a withheld header",
    async (alert, poolOf) => {
      const header = withheld("46");
      io.utxos = [
        utxo(0, TIMEOUT_COLLATERAL, "d1"),
        utxo(1, OPENING, "d2"),
        utxo(2, 100n * ADA, "d2"),
      ];
      const watcher = await runtime();
      try {
        const state = observation([poolOf(header.attested)]);
        // Readiness is `phase !== "blocked"` (watcher-runtime status).
        await watcher.reconcile(state, false);
        expect(watcher.status()).toMatchObject({
          phase: "ready",
          poolAlert: alert,
        });
        await watcher.reconcile(state, true);
        expect(watcher.status()).toMatchObject({
          phase: "waiting",
          action: "open",
          poolAlert: alert,
        });
      } finally {
        await watcher.close();
      }
      expect(actions()).toEqual([[header.attested.headerHash, "open"]]);
    },
  );

  it("reports no alert for a Bonded pool backing a full DA bond", async () => {
    const header = withheld("46");
    const watcher = await runtime();
    try {
      await watcher.reconcile(
        observation([withPool(header.attested, backed, "Bonded")]),
        false,
      );
      expect(watcher.status().phase).toBe("ready");
      expect(watcher.status().poolAlert).toBeUndefined();
    } finally {
      await watcher.close();
    }
  });
});
