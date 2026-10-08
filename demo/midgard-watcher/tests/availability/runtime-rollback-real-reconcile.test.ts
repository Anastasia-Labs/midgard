import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { openAvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import {
  buildDaAvailabilityFundingPreparationTx,
  type DaAvailabilityOperationObservation,
  inspectDaAvailabilitySignedIntent,
} from "@al-ft/midgard-sdk";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { createWatcherAvailabilityRuntime } from "../../src/availability/runtime.js";
import type { WatcherAuthenticatedStateQueueObservation } from "../../src/indexers/authenticated-state-queue-observation.js";
import type { VerifiedWatcherDeploymentIdentity } from "../../src/runtime/deployment-identity.js";
import type { WatcherProcessConfig } from "../../src/runtime/process-config.js";

// runtime-rollback.test.ts replaces the SDK's reconciliation with a mock. Here
// the SDK's own reconciliation runs over a real journal holding a real signed
// intent, so the runtime's rollback recovery is the shipped one. Only the L1
// reads and the broadcast are injected.
const DEPLOYMENT = "ab".repeat(32);
const io = vi.hoisted(() => ({
  actor: "",
  operation: vi.fn(),
  submit: vi.fn(),
}));
vi.mock("@al-ft/midgard-sdk", async (original) => ({
  ...(await original<typeof import("@al-ft/midgard-sdk")>()),
  daAvailabilityOperationLimits: () => ({
    maxTxSize: 16384,
    maxTxExMem: 16500000n,
    maxTxExSteps: 10000000000n,
    coinsPerUtxoByte: 4310n,
    feeCeilings: { prepare: 1000000n },
  }),
}));
vi.mock("@lucid-evolution/lucid", async (original) => {
  const real = await original<typeof import("@lucid-evolution/lucid")>();
  return {
    ...real,
    // The runtime's wallet: it signs nothing here, it only names the actor.
    Lucid: async () => ({
      selectWallet: { fromSeed() {} },
      wallet: () => ({ address: async () => "availability" }),
      config: () => ({
        protocolParameters: {},
        provider: { submitTx: io.submit },
      }),
      // The parameter refresh re-reads the same provider: nothing changes.
      switchProvider: async () => {},
    }),
    paymentCredentialOf: (address: string) =>
      address === "availability"
        ? { hash: io.actor }
        : address === "prover"
          ? { hash: "00".repeat(28) }
          : real.paymentCredentialOf(address),
  };
});
vi.mock("@lucid-evolution/scalus-uplc", () => ({
  createScalusEvaluator: () => ({}),
}));
vi.mock("../../src/runtime/process-config.js", () => ({
  loadWatcherSecretText: async () => "seed",
}));
vi.mock("../../src/indexers/authenticated-state-queue-observation.js", () => ({
  assertWatcherStateQueueObservation() {},
}));
vi.mock("../../src/storage/retained-da-runtime.js", () => ({
  createWatcherRetainedDaRuntime: async () => ({
    sources: [],
    async close() {},
  }),
  bindWatcherL1AvailabilityPayloadSource: () => ({ close() {} }),
}));
vi.mock("../../src/availability/deployment.js", async () => {
  const support = await import("../support/availability-challenge-fixture.js");
  return {
    createWatcherAvailabilityDeployment: async () => ({
      contracts: {
        stateQueue: { policyId: "cc".repeat(28) },
        daBondPool: { policyId: support.DA_BOND_POOL_POLICY_ID },
      },
      parameters: support.parametersFixture(),
    }),
  };
});
vi.mock("../../src/availability/observation.js", () => ({
  createWatcherAvailabilityObservation: () => ({
    pool: async () => undefined,
    snapshot: vi.fn(),
    operation: (_observation: unknown, intent: { txHash: string }) =>
      io.operation(intent.txHash),
    confirmationDepth: 17,
  }),
}));
vi.mock("../../src/availability/published-payload.js", () => ({
  createWatcherL1AvailabilityPayloadSource: () => ({}),
}));

const observation = (slot: number) =>
  ({
    observationDigest: `${slot.toString()}:${"11".repeat(32)}`,
    deploymentIdentityDigest: DEPLOYMENT,
    nativePoint: {
      slot: String(slot),
      blockNo: String(slot),
      blockHash: "11".repeat(32),
      finalityDepth: "30",
    },
    finalizedHeaders: [],
    finalizedCorrectionLock: { datum: "Idle" },
  }) as unknown as WatcherAuthenticatedStateQueueObservation;

let dir: string;
beforeEach(() => {
  dir = mkdtempSync(join(tmpdir(), "watcher-availability-real-reconcile-"));
  io.operation.mockReset();
  io.submit.mockReset();
});
afterEach(() => {
  rmSync(dir, { recursive: true, force: true });
});

/** A confirmed preparation intent, signed by a real wallet, in a real journal. */
const confirmedIntent = async (journalPath: string) => {
  const lucidModule = await vi.importActual<
    typeof import("@lucid-evolution/lucid")
  >("@lucid-evolution/lucid");
  const account = lucidModule.generateEmulatorAccount({
    lovelace: 100_000_000n,
  });
  const emulator = new lucidModule.Emulator([account]);
  emulator.awaitBlock(5);
  const lucid = await lucidModule.Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(account.seedPhrase);
  io.actor = lucidModule.paymentCredentialOf(account.address).hash;
  const [fundingInput] = await lucid.wallet().getUtxos();
  const built = await buildDaAvailabilityFundingPreparationTx(lucid, {
    fundingInput: fundingInput!,
    outputLovelace: 50_000_000n,
    feeLovelace: 1_000_000n,
    validFrom: BigInt(emulator.now() - 60_000),
    validTo: BigInt(emulator.now() + 60_000),
  });
  const signed = await built.sign.withWallet().complete();
  const intent = inspectDaAvailabilitySignedIntent({
    deploymentIdentity: DEPLOYMENT,
    actor: io.actor,
    headerHash: "bb".repeat(28),
    action: "prepare",
    signedCbor: signed.toCBOR(),
  });
  const journal = openAvailabilityOperationJournal(journalPath);
  try {
    const lease = journal.acquire(io.actor, "setup", 0, 1_000);
    journal.persist(lease, intent, 1);
    journal.transition(lease, intent.id, "confirmed", "10:aa", null, 2);
    journal.release(lease);
  } finally {
    journal.close();
  }
  return intent;
};

describe("availability runtime recovery through the SDK's own reconciliation", () => {
  it("keeps a confirmed intent on inconclusive evidence, and rewinds and rebroadcasts its exact bytes once it left the chain", async () => {
    const journalPath = join(dir, "journal.sqlite");
    const intent = await confirmedIntent(journalPath);
    const runtime = await createWatcherAvailabilityRuntime({
      config: {
        watcherConfig: {
          l1: {
            requestTimeoutMs: 10_000,
            source: {
              sourceMode: "local_node",
              queryServices: [
                { kind: "kupo", endpoint: "http://kupo" },
                { kind: "ogmios", endpoint: "http://ogmios" },
              ],
            },
          },
        },
        availability: { keySource: {}, journalPath },
      } as unknown as WatcherProcessConfig,
      identity: {
        network: "Custom",
        manifestId: DEPLOYMENT,
      } as VerifiedWatcherDeploymentIdentity,
      l1: {
        reads: {},
        store: {},
        provider: {},
      } as unknown as Parameters<
        typeof createWatcherAvailabilityRuntime
      >[0]["l1"],
      confirmationDepth: 17,
      proverWalletAddress: "prover",
      onDaBondPool: () => undefined,
      onDaBondPoolReadFailure: () => undefined,
    });
    const state = () => {
      const journal = openAvailabilityOperationJournal(journalPath);
      try {
        return journal.get(intent.id)?.state;
      } finally {
        journal.close();
      }
    };
    try {
      // The source cannot tell: the confirmation stands and nothing is sent.
      io.operation.mockResolvedValue({
        status: "unknown",
        reason: "source lagging",
      } satisfies DaAvailabilityOperationObservation);
      await runtime.reconcile(observation(5), false);
      expect(state()).toBe("confirmed");
      expect(io.submit).not.toHaveBeenCalled();
      expect(runtime.status()).toMatchObject({
        phase: "waiting",
        txHash: intent.txHash,
      });
      // A rollback deeper than the confirmation depth: its input is back,
      // before its validity. The same signed bytes go out again.
      io.operation.mockResolvedValue({
        status: "unspent",
        currentSlot: 0,
      } satisfies DaAvailabilityOperationObservation);
      io.submit.mockResolvedValue(intent.txHash);
      await runtime.reconcile(observation(6), false);
      expect(state()).toBe("pending");
      expect(io.submit.mock.calls).toEqual([[intent.signedCbor]]);
      expect(runtime.status()).toMatchObject({
        phase: "waiting",
        txHash: intent.txHash,
      });
    } finally {
      await runtime.close();
    }
  });
});

vi.mock("../../src/storage/retained-da-runtime.read-scope.js", () => ({
  withWatcherRetainedDaReadScope: async (
    input: { scope: SDKScope },
    read: () => Promise<unknown>,
  ) => input.scope.read(read),
}));
type SDKScope = import("@al-ft/midgard-sdk").DaAvailabilityReadScope;
vi.mock("../../src/availability/runtime.read-attempt.js", async (original) => ({
  ...(await original<
    typeof import("../../src/availability/runtime.read-attempt.js")
  >()),
  watcherAvailabilityAuthenticatedOpenDeadline: () => Date.now() + 2_400_000,
}));
