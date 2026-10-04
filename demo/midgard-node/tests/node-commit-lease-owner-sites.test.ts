import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { Effect, Exit, Ref } from "effect";
import { describe, expect, it, vi } from "vitest";

import * as MpfEngineStateDB from "../src/database/mpfEngineState.js";
import { Columns } from "../src/database/pendingBlockFinalizations.js";
import { buildAndSubmitCommitmentBlockAction } from "../src/fibers/block-commitment.build-and-submit-commitment-block-action.js";
import { runSpeculativeCommitBuilderOnce } from "../src/fibers/speculative-commit-builder.run-speculative-commit-builder-once.js";
import {
  ContractDeploymentIdentity,
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../src/services/index.js";
import { runCommitBlockHeaderWorkerProgram } from "../src/workers/commit-block-header.run-commit-block-header-worker-program.js";
import type { WorkerInput } from "../src/workers/utils/commit-block-header.js";
import { provideDatabaseLayers } from "./utils.js";

// Startup retires a killed node's ledger MPF lease only under the node-process
// owner prefix, so both node commit sites must hand their worker an owner
// carrying it. A site that went back to the offline `commit:` owner would leave
// a killed node's lease to its TTL. Each site is driven up to the worker it
// launches, and the worker input it builds is captured there.

const { captured } = vi.hoisted(() => ({
  captured: {
    commitWorkerOwner: undefined as string | undefined,
    speculativeOwner: undefined as string | undefined,
    activeJournal: undefined as unknown,
    acceptedWorkerOwner: undefined as string | undefined,
  },
}));

vi.mock("../src/database/index.js", async (importOriginal) => {
  const actual =
    await importOriginal<typeof import("../src/database/index.js")>();
  const { Effect } = await import("effect");
  return {
    ...actual,
    MpfEngineStateDB: {
      ...actual.MpfEngineStateDB,
      tryWithLedgerStoreLease: (owner: string) => {
        captured.acceptedWorkerOwner = owner;
        return Effect.succeed({ _tag: "Busy" });
      },
    },
  };
});

vi.mock("worker_threads", async (importOriginal) => ({
  ...(await importOriginal<typeof import("worker_threads")>()),
  Worker: class {
    constructor(
      _entry: URL,
      options: {
        readonly workerData: { data: { ledgerStoreLeaseOwner: string } };
      },
    ) {
      captured.commitWorkerOwner =
        options.workerData.data.ledgerStoreLeaseOwner;
      throw new Error("stop at the commitment worker");
    }
  },
}));
vi.mock("../src/fibers/resolve-worker-entry.js", () => ({
  resolveWorkerEntry: () => new URL("file:///commit-block-header.js"),
}));
vi.mock("../src/fibers/native-mpf-worker-input.js", async () => {
  const { Effect } = await import("effect");
  return {
    nativeMpfWorkerInput: () =>
      Effect.succeed({
        port: undefined,
        durableRoot: "",
        ownerBinarySha256: "",
      }),
  };
});
vi.mock("../src/lucid-time.js", async (importOriginal) => ({
  ...(await importOriginal<typeof import("../src/lucid-time.js")>()),
  canonicalSlotConfigForLucid: () => ({
    zeroTime: 0,
    zeroSlot: 0,
    slotLength: 1_000,
  }),
}));
vi.mock(
  "../src/fibers/speculative-commit-builder.apply-speculative-submission-output.js",
  async (importOriginal) => {
    const { Effect } = await import("effect");
    return {
      ...(await importOriginal<
        typeof import("../src/fibers/speculative-commit-builder.apply-speculative-submission-output.js")
      >()),
      spawnSpeculativeSession: (
        _globals: unknown,
        _config: unknown,
        input: { readonly data: { readonly ledgerStoreLeaseOwner: string } },
      ) => {
        captured.speculativeOwner = input.data.ledgerStoreLeaseOwner;
        return Effect.fail(new Error("stop at the speculative worker"));
      },
    };
  },
);
vi.mock(
  "../src/fibers/speculative-commit-builder.spawn-speculative-session-with-worker.js",
  async (importOriginal) => {
    const { Effect } = await import("effect");
    return {
      ...(await importOriginal<
        typeof import("../src/fibers/speculative-commit-builder.spawn-speculative-session-with-worker.js")
      >()),
      invalidateSpeculativeCommitCandidate: () => Effect.void,
    };
  },
);
vi.mock(
  "../src/database/pendingBlockFinalizations.js",
  async (importOriginal) => {
    const { Effect, Option } = await import("effect");
    return {
      ...(await importOriginal<
        typeof import("../src/database/pendingBlockFinalizations.js")
      >()),
      retrieveActive: () =>
        Effect.succeed(Option.fromNullable(captured.activeJournal)),
    };
  },
);

const BASE_HEADER_HASH = "ab".repeat(28);
const config = {
  SPECULATIVE_COMMIT_BUILD: true,
  SPECULATIVE_REBUILD_MAX_ATTEMPTS: 1,
  USER_EVENT_BARRIER_MAX_STALENESS_MS: 60_000,
  MPF_NATIVE_OWNER_BINARY_SHA256: "",
  VALIDATION_LEDGER_DELTA_LOG_MAX: 1,
} as unknown as NodeConfig["Type"];

/** Runs `site` with fresh globals holding an open native owner, and every
 * other service it reads stubbed: nothing past the worker launch runs. */
const driveToWorker = async (
  site: Effect.Effect<unknown, unknown, never>,
  setUp: (globals: Globals) => Effect.Effect<void>,
) =>
  Effect.runPromiseExit(
    Effect.gen(function* () {
      const globals = yield* Globals;
      yield* Ref.set(globals.NATIVE_MPF_OWNER, {} as never);
      yield* setUp(globals);
      return yield* site;
    }).pipe(
      Effect.provideService(NodeConfig, config),
      Effect.provideService(Lucid, { api: {} } as unknown as Lucid),
      Effect.provideService(MidgardContracts, {} as MidgardContracts),
      Effect.provide(Globals.Default),
    ),
  );

const expectNodeProcessOwner = (owner: string | undefined) => {
  expect(owner).toBeDefined();
  expect(
    owner!.startsWith(MpfEngineStateDB.NODE_PROCESS_COMMIT_LEASE_OWNER_PREFIX),
  ).toBe(true);
};

const probeWorkerLeaseOwner = (owner: string) => {
  const input = { data: { ledgerStoreLeaseOwner: owner } } as WorkerInput;
  return provideDatabaseLayers(
    runCommitBlockHeaderWorkerProgram(input).pipe(
      Effect.provideService(NodeConfig, config),
      Effect.provideService(MidgardContracts, {} as MidgardContracts),
      Effect.provideService(
        ContractDeploymentIdentity,
        ContractDeploymentIdentity.make({
          kind: "derived",
          consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        }),
      ),
    ),
  );
};

const expectWorkerAcceptsOwner = async (owner: string) => {
  captured.acceptedWorkerOwner = undefined;
  const exit = await Effect.runPromiseExit(probeWorkerLeaseOwner(owner));
  expect(Exit.isFailure(exit)).toBe(true);
  // A busy lease stops this probe before ledger or submission work. Reaching
  // the lease acquisition proves the real worker accepted the parent's owner.
  expect(captured.acceptedWorkerOwner).toBe(owner);
};

describe("node commit sites take the ledger MPF lease as a node process", () => {
  it("block commitment hands its worker a node-commit owner", async () => {
    const exit = await driveToWorker(
      buildAndSubmitCommitmentBlockAction() as Effect.Effect<
        unknown,
        unknown,
        never
      >,
      (globals) =>
        Effect.all(
          [
            // A history owner that admits the producer at once.
            Ref.set(globals.EVENT_HISTORY_OWNER, {
              runProducer: (
                work: (
                  token: unknown,
                  assertCurrent: Effect.Effect<void>,
                  coverage: unknown,
                ) => Effect.Effect<unknown, unknown>,
              ) => work({}, Effect.void, {}),
            } as never),
            // A pending local finalization skips the L1 state-queue preflight.
            Ref.set(globals.LOCAL_FINALIZATION_PENDING, true),
          ],
          { discard: true },
        ),
    );
    expect(Exit.isFailure(exit)).toBe(true);
    expectNodeProcessOwner(captured.commitWorkerOwner);
    await expectWorkerAcceptsOwner(captured.commitWorkerOwner!);
  });

  it("the speculative builder hands its worker a node-commit owner", async () => {
    captured.activeJournal = {
      [Columns.HEADER_HASH]: Buffer.from(BASE_HEADER_HASH, "hex"),
      [Columns.SUBMITTED_TX_HASH]: Buffer.from("cd".repeat(32), "hex"),
      [Columns.BLOCK_END_TIME]: new Date(1_000),
      [Columns.EXPECTED_UTXOS_ROOT]: Buffer.alloc(32),
      mempoolTxIds: [],
      depositEventIds: [],
      forcedTransactionEventIds: [],
      withdrawalEventIds: [],
    };
    const nowMs = Date.now();
    const exit = await driveToWorker(
      runSpeculativeCommitBuilderOnce(BASE_HEADER_HASH) as Effect.Effect<
        unknown,
        unknown,
        never
      >,
      (globals) =>
        Effect.all(
          [
            Ref.set(globals.SPECULATIVE_COMMIT_STATE, {
              _tag: "Building",
              baseHeaderHash: BASE_HEADER_HASH,
              rebuildAttempts: 0,
              startedAtMs: nowMs,
            }),
            Ref.set(globals.USER_EVENT_BARRIER_WATERMARKS, {
              depositMs: nowMs,
              withdrawalMs: nowMs,
              txOrderMs: nowMs,
              refreshedAtMs: nowMs,
            }),
          ],
          { discard: true },
        ),
    );
    expect(Exit.isFailure(exit)).toBe(true);
    expectNodeProcessOwner(captured.speculativeOwner);
    await expectWorkerAcceptsOwner(captured.speculativeOwner!);
  });

  it("accepts offline commit owners and refuses audit or shared owners", async () => {
    await expectWorkerAcceptsOwner(
      "commit:12345678-1234-4123-8123-123456789abc",
    );
    for (const owner of [
      "node-audit:12345678-1234-4123-8123-123456789abc",
      "audit:12345678-1234-4123-8123-123456789abc",
      "node-commit:shared",
      "commit:shared",
      "node-commit:12345678-1234-1123-8123-123456789abc",
    ]) {
      captured.acceptedWorkerOwner = undefined;
      const exit = await Effect.runPromiseExit(probeWorkerLeaseOwner(owner));
      expect(Exit.isFailure(exit)).toBe(true);
      expect(captured.acceptedWorkerOwner).toBeUndefined();
    }
  });
});
