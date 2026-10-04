import { createHash } from "node:crypto";
import { mkdtemp, rm } from "node:fs/promises";

import type {
  FraudProofRawL1Point,
  LocalKupmiosFraudProofRawSource,
} from "@al-ft/midgard-fault-proofs";
import { h32 } from "@al-ft/midgard-test-support/hex";
import { afterEach, describe, expect, it, vi } from "vitest";

import type { WatcherAvailabilityRuntime } from "../../src/availability/runtime.js";
import type { WatcherAuthenticatedStateQueueObservation } from "../../src/indexers/authenticated-state-queue-observation.js";
import { admitObservation } from "../../src/indexers/authenticated-state-queue-observation.parse-persisted-observation.js";
import type { WatcherVerificationDiagnostic } from "../../src/runtime/operations-observability.js";
import type { WatcherTrustedHeadClientRuntime } from "../../src/runtime/trusted-head-runtime.js";
import { createWatcherRuntime } from "../../src/runtime/watcher-runtime.js";
import {
  DEPLOYMENT,
  headerFixture,
  observation,
} from "../fault-proofs/fault-decision-bridge.observation.js";
import {
  chain,
  mergeTx,
  RELEASE_DEPTH,
} from "../indexers/state-queue-merged-headers.fixture.js";
import { writeWatcherRuntimeProcessConfig } from "../support/watcher-runtime-process-config.js";

type Readers = ReturnType<typeof chain>;

const boundary = vi.hoisted(() => ({
  trusted: undefined as WatcherTrustedHeadClientRuntime | undefined,
  identity: undefined as
    | Readonly<{ manifestId: string; blueprintHash: string }>
    | undefined,
  readers: undefined as Readers | undefined,
  observation: undefined as
    | WatcherAuthenticatedStateQueueObservation
    | undefined,
  availabilityInput: undefined as
    | Readonly<{
        mergedHeaders?: (
          observation: WatcherAuthenticatedStateQueueObservation,
        ) => Promise<ReadonlyMap<string, unknown>>;
      }>
    | undefined,
  assertMerged: undefined as
    | ((proof: ReadonlyMap<string, unknown>) => void)
    | undefined,
  records: [] as WatcherVerificationDiagnostic[],
  classifyHeader: vi.fn(async () => {
    throw new Error("classifier reached");
  }),
  readUnitHistory: vi.fn(async () => {
    throw new Error("predecessor reader reached");
  }),
  stop: vi.fn(async () => {
    throw new Error("test boundary after header classification");
  }),
}));

// The local Kupo/Ogmios edges answer from fixed spends. Everything between
// them and the bridge is production code: the observation source, its merge
// resolver, the runtime composition and the fault decision bridge.
vi.mock("@al-ft/midgard-fault-proofs", async (load) => ({
  ...(await load<typeof import("@al-ft/midgard-fault-proofs")>()),
  localKupmiosHttpOgmiosRawSourceDetails: (
    source: LocalKupmiosFraudProofRawSource,
  ) => ({
    sourceId: "watcher-runtime-merged-headers",
    deploymentIdentityDigest: boundary.identity!.manifestId,
    blueprintHash: boundary.identity!.blueprintHash,
    observationDepth: (source as { observationDepth?: string })
      .observationDepth,
    confirmationDepth: RELEASE_DEPTH,
    automaticRecoveryMaxDepth: 2160,
  }),
  readAdmittedLocalKupmiosBoundary: async () =>
    await boundary.readers!.readBoundary(),
  readAdmittedLocalKupmiosUtxosByOutRefAtPoint: async ({
    outRefs,
    point,
  }: {
    outRefs: readonly string[];
    point: FraudProofRawL1Point;
  }) => await boundary.readers!.readOutRefs(outRefs, point),
  readAdmittedLocalKupmiosRawTransaction: async ({
    txHash,
  }: {
    txHash: string;
  }) => await boundary.readers!.readTransaction(txHash),
  readAdmittedLocalKupmiosUnitHistoryAtPoint: boundary.readUnitHistory,
  authenticatedStateQueueObservationDigest: async () => "26".repeat(32),
  resolveProverSigner: () => ({ address: "prover" }),
}));
vi.mock("../../src/l1/local-kupmios-raw-source.js", () => ({
  createWatcherLocalKupmiosRawSource: ({
    observationDepth,
  }: {
    observationDepth?: string;
  }) =>
    Object.freeze({ observationDepth: observationDepth ?? "release_finality" }),
}));
vi.mock("../../src/runtime/state-queue-runtime.js", () => ({
  createWatcherStateQueueRuntime: async () =>
    Object.freeze({
      current: () => boundary.observation!,
      replayIntersection: null,
      catchupBoundary: { blockHash: h32(0x13), blockNo: "100", slot: "1000" },
    }),
}));
vi.mock("../../src/runtime/trusted-head-runtime.js", () => ({
  createWatcherTrustedHeadClientRuntime: async () => boundary.trusted!,
}));
vi.mock(
  "../../src/funding/workflow-funding-profile-overlay.js",
  async (load) => ({
    ...(await load<
      typeof import("../../src/funding/workflow-funding-profile-overlay.js")
    >()),
    loadWatcherWorkflowFundingProfileOverlay: async () => Object.freeze({}),
  }),
);
vi.mock("../../src/funding/prover-funding.js", async (load) => ({
  ...(await load<typeof import("../../src/funding/prover-funding.js")>()),
  createWatcherProtocolParameterRuntimeAuthority: async () => Object.freeze({}),
}));
vi.mock("../../src/funding/prover-funding-authority.js", async (load) => ({
  ...(await load<
    typeof import("../../src/funding/prover-funding-authority.js")
  >()),
  createWatcherProverFundingAuthorityFactory: () => ({
    releaseUnused: async () => undefined,
  }),
}));
vi.mock("../../src/fault-proofs/fault-proof-application.js", async (load) => {
  const actual =
    await load<
      typeof import("../../src/fault-proofs/fault-proof-application.js")
    >();
  return {
    ...actual,
    // The bridge's own module admission is the one seam opened here.
    assertWatcherFaultProofApplication: () => undefined,
    createWatcherFaultProofApplication: () => ({
      deploymentFingerprint: DEPLOYMENT,
      installedCategories: actual.WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
      assertStartupReady: async () => ({ ready: true }),
      retainedDaTransportStatus: () => ({ state: "idle", failure: null }),
      retainDecisionAuthorities: () => undefined,
      decisionUsesLocalEventHistory: () => false,
      classifyHeader: boundary.classifyHeader,
      close: async () => undefined,
    }),
  };
});
vi.mock("../../src/fault-proofs/fault-proof-supervisor.js", async (load) => ({
  ...(await load<
    typeof import("../../src/fault-proofs/fault-proof-supervisor.js")
  >()),
  createWatcherFaultProofSupervisor: () => ({
    status: () => ({ unfinishedObjectiveCount: 0 }),
    durableQueueStatus: () => ({ queuedJobCount: 0 }),
    requestProgress: async () => undefined,
    revokeAuthority: () => undefined,
    close: async () => undefined,
  }),
}));
vi.mock("../../src/fault-proofs/fault-proof-execution.js", () => ({
  createWatcherFaultProofExecution: () => Object.freeze({}),
}));
vi.mock("../../src/runtime/user-event-runtime.js", async (load) => ({
  ...(await load<typeof import("../../src/runtime/user-event-runtime.js")>()),
  createWatcherUserEventRuntime: async () => ({
    done: new Promise(() => undefined),
    advanceThrough: async () => undefined,
    close: async () => undefined,
  }),
}));
vi.mock("../../src/storage/retained-da-runtime.js", async (load) => ({
  ...(await load<typeof import("../../src/storage/retained-da-runtime.js")>()),
  bindWatcherRetainedDaOperations: () => ({ close: () => undefined }),
}));
vi.mock("../../src/runtime/operations-observability.js", async (load) => {
  const actual =
    await load<
      typeof import("../../src/runtime/operations-observability.js")
    >();
  return {
    ...actual,
    createWatcherOperationsObservability: (
      ...args: Parameters<typeof actual.createWatcherOperationsObservability>
    ) => {
      const operations = actual.createWatcherOperationsObservability(...args);
      return {
        ...operations,
        sink: {
          ...operations.sink,
          recordVerification: (value: WatcherVerificationDiagnostic) => {
            boundary.records.push(value);
            operations.sink.recordVerification(value);
          },
        },
      };
    },
  };
});
vi.mock("../../src/availability/runtime.js", async (load) => ({
  ...(await load<typeof import("../../src/availability/runtime.js")>()),
  createWatcherAvailabilityRuntime: async (
    input: NonNullable<typeof boundary.availabilityInput>,
  ): Promise<Partial<WatcherAvailabilityRuntime>> => {
    boundary.availabilityInput = input;
    return {
      reconcile: async (current) => {
        const proof = await input.mergedHeaders!(current);
        boundary.assertMerged?.(proof);
      },
      pendingAvailabilityHeaders: async () => new Set<string>(),
      invalidateForShutdown: () => undefined,
      close: async () => undefined,
    };
  },
}));
vi.mock("../../src/runtime/operations-http.js", () => ({
  startWatcherOperationsHttpServer: boundary.stop,
}));

const directories: string[] = [];
const key = Uint8Array.from({ length: 32 }, (_, index) => index + 1);

const start = async () => {
  const directory = await mkdtemp("/var/tmp/midgard-watcher-runtime-merged-");
  directories.push(directory);
  const config = await writeWatcherRuntimeProcessConfig(directory);
  const { loadWatcherVerifiedDeploymentAuthority } = await import(
    "../../src/runtime/deployment-authority.js"
  );
  boundary.identity = (
    await loadWatcherVerifiedDeploymentAuthority({
      path: config.deploymentAuthorityPath,
      ruleBundlePath: config.ruleBundlePath,
    })
  ).deploymentIdentity;
  let head: unknown = null;
  boundary.trusted = {
    rollbackAuthenticationKey: key,
    rollbackAuthenticationKeyId: createHash("sha256").update(key).digest("hex"),
    recordAuthenticationKeyId: h32(0x99),
    client: {
      readRecordAuthenticationKeyId: async () => h32(0x99),
      readCurrent: async () => head,
      compareAndSwap: async ({ expectedTrustedHead, nextTrustedHead }) => {
        if (JSON.stringify(expectedTrustedHead) !== JSON.stringify(head))
          return false;
        head = nextTrustedHead;
        return true;
      },
    },
  } as WatcherTrustedHeadClientRuntime;
  vi.stubEnv("MIDGARD_WATCHER_PROVER_KEY", `ed25519_sk${"q".repeat(48)}`);
  return createWatcherRuntime({ config });
};

/** One queued header over root `16..#0`, admitted as the source's own. */
const queued = () => {
  const current = admitObservation(observation([headerFixture("01")]));
  boundary.observation = current;
  return {
    current,
    root: current.finalizedQueue[0]!.outRef,
    header: current.finalizedHeaders[0]!,
  };
};

afterEach(async () => {
  vi.unstubAllEnvs();
  boundary.trusted = undefined;
  boundary.identity = undefined;
  boundary.readers = undefined;
  boundary.observation = undefined;
  boundary.availabilityInput = undefined;
  boundary.assertMerged = undefined;
  boundary.records.length = 0;
  boundary.classifyHeader.mockClear();
  boundary.readUnitHistory.mockClear();
  boundary.stop.mockClear();
  await Promise.all(
    directories
      .splice(0)
      .map((path) => rm(path, { recursive: true, force: true })),
  );
});

describe("production runtime over a merged state-queue header", () => {
  it("never classifies a header the source proves merged, and hands availability the same proof", async () => {
    const { current, root, header } = queued();
    const merge = mergeTx(root, header.queueOutRef, header.headerHash, 150);
    boundary.readers = chain([
      [root, merge],
      [header.queueOutRef, merge],
    ]);
    const assertMerged = vi.fn((released: ReadonlyMap<string, unknown>) => {
      expect([...released.keys()]).toEqual([header.headerHash]);
      expect(released.get(header.headerHash)).toMatchObject({
        mergeTransactionHash: merge.txHash,
      });
    });
    boundary.assertMerged = assertMerged;
    const stderr = vi
      .spyOn(process.stderr, "write")
      .mockImplementation(() => true);
    await expect(start()).rejects.toThrow(
      "test boundary after header classification",
    );
    // The production bridge warns once, as one JSON line on stderr.
    expect(
      stderr.mock.calls
        .map(([line]) => String(line))
        .filter((line) => line.includes("unverified_merged"))
        .map((line) => JSON.parse(line) as unknown),
    ).toEqual([
      {
        packageName: "midgard-watcher",
        level: "warn",
        event: "unverified_merged",
        headerHash: header.headerHash,
        transactionHash: merge.txHash,
        lockedCorrectionTarget: false,
      },
    ]);
    stderr.mockRestore();
    expect(boundary.readUnitHistory).not.toHaveBeenCalled();
    expect(boundary.classifyHeader).not.toHaveBeenCalled();
    expect(
      boundary.records.map(({ headerHash, outcome, mergeTransactionHash }) => ({
        headerHash,
        outcome,
        mergeTransactionHash,
      })),
    ).toEqual([
      {
        headerHash: header.headerHash,
        outcome: "unverified_merged",
        mergeTransactionHash: merge.txHash,
      },
    ]);
    expect(assertMerged).toHaveBeenCalledTimes(1);
    await expect(
      boundary.availabilityInput!.mergedHeaders!(current),
    ).rejects.toThrow("state-queue read scopes are closed");
  });

  it("proves consecutive merges that each spend the root and the head's node", async () => {
    const current = admitObservation(
      observation([headerFixture("01"), headerFixture("02")]),
    );
    boundary.observation = current;
    const root = current.finalizedQueue[0]!.outRef;
    const [first, second] = current.finalizedHeaders;
    const one = mergeTx(root, first!.queueOutRef, first!.headerHash, 150);
    const two = mergeTx(
      `${one.txHash}#0`,
      second!.queueOutRef,
      second!.headerHash,
      160,
    );
    boundary.readers = chain([
      [root, one],
      [first!.queueOutRef, one],
      [`${one.txHash}#0`, two],
      [second!.queueOutRef, two],
    ]);
    const stderr = vi
      .spyOn(process.stderr, "write")
      .mockImplementation(() => true);
    await expect(start()).rejects.toThrow(
      "test boundary after header classification",
    );
    stderr.mockRestore();
    expect(boundary.classifyHeader).not.toHaveBeenCalled();
    expect(
      boundary.records.map(({ headerHash, outcome, mergeTransactionHash }) => ({
        headerHash,
        outcome,
        mergeTransactionHash,
      })),
    ).toEqual([
      {
        headerHash: first!.headerHash,
        outcome: "unverified_merged",
        mergeTransactionHash: one.txHash,
      },
      {
        headerHash: second!.headerHash,
        outcome: "unverified_merged",
        mergeTransactionHash: two.txHash,
      },
    ]);
  });

  it("classifies an unmerged header and hands availability no proof", async () => {
    const { current, root } = queued();
    boundary.readers = chain([]);
    const assertMerged = vi.fn((proof: ReadonlyMap<string, unknown>) => {
      expect(proof.size).toBe(0);
    });
    boundary.assertMerged = assertMerged;
    // Classification starts by re-authenticating the predecessor header.
    await expect(start()).rejects.toThrow("predecessor reader reached");
    expect(boundary.stop).not.toHaveBeenCalled();
    expect(boundary.records.map(({ outcome }) => outcome)).toEqual(["failed"]);
    expect(boundary.readers.readOutRefs).toHaveBeenCalledWith(
      [root, `${"17".repeat(32)}#0`],
      expect.anything(),
    );
    expect(assertMerged).toHaveBeenCalledTimes(1);
    await expect(
      boundary.availabilityInput!.mergedHeaders!(current),
    ).rejects.toThrow("state-queue read scopes are closed");
  });
});
