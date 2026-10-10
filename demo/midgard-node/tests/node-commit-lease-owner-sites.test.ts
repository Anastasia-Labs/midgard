import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { SqlClient } from "@effect/sql";
import { Effect, Exit, Ref } from "effect";
import { describe, expect, it, vi } from "vitest";

import * as MpfEngineStateDB from "../src/database/mpfEngineState.js";
import { buildAndSubmitCommitmentBlockAction } from "../src/fibers/block-commitment.build-and-submit-commitment-block-action.js";
import {
  ContractDeploymentIdentity,
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../src/services/index.js";
import { runCommitBlockHeaderWorkerProgram } from "../src/workers/commit-block-header.run-commit-block-header-worker-program.js";
import type { WorkerInput } from "../src/workers/utils/commit-block-header.js";
import { openFollowerWriteGate } from "./helpers/follower-write-gate.js";
import { withoutFollowerJournal } from "./helpers/intent-journal.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

// Startup retires a killed node's ledger MPF lease only under the node-process
// owner prefix, so the node commit site must hand its worker an owner carrying
// it. A site that went back to the offline `commit:` owner would leave a killed
// node's lease to its TTL. The site is driven up to the worker it launches, and
// the worker input it builds is captured there.

const { captured } = vi.hoisted(() => ({
  captured: {
    commitWorkerOwner: undefined as string | undefined,
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
  "../src/database/pendingBlockFinalizations.js",
  async (importOriginal) => {
    const { Effect, Option } = await import("effect");
    return {
      ...(await importOriginal<
        typeof import("../src/database/pendingBlockFinalizations.js")
      >()),
      retrieveActive: () => Effect.succeed(Option.none()),
    };
  },
);

const config = {
  MPF_NATIVE_OWNER_BINARY_SHA256: "",
  VALIDATION_LEDGER_DELTA_LOG_MAX: 1,
} as unknown as NodeConfig["Type"];

/** Runs `site` with fresh globals holding an open native owner, and every
 * other service it reads stubbed or on the node database: nothing past the
 * worker launch runs. */
const driveToWorker = async (
  site: Effect.Effect<unknown, unknown, never>,
  setUp: (
    globals: Globals,
  ) => Effect.Effect<void, unknown, SqlClient.SqlClient>,
) =>
  Effect.runPromiseExit(
    Effect.gen(function* () {
      const globals = yield* Globals;
      yield* Ref.set(globals.NATIVE_MPF_OWNER, {} as never);
      yield* setUp(globals);
      return yield* site;
    }).pipe(
      provideDatabaseLayers,
      Effect.provideService(NodeConfig, config),
      Effect.provideService(Lucid, { api: {} } as unknown as Lucid),
      Effect.provideService(MidgardContracts, {} as MidgardContracts),
      Effect.provide(Globals.Default),
      withoutFollowerJournal,
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
    // The owner check refuses before any L1 read, so the Lucid is unused.
    runCommitBlockHeaderWorkerProgram(input, undefined, () =>
      Effect.succeed({} as Lucid),
    ).pipe(
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
  const exit = await Effect.runPromiseExit(
    withoutFollowerJournal(probeWorkerLeaseOwner(owner)),
  );
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
        Effect.gen(function* () {
          // A driver that applied a view: the producer's permit is taken at once.
          yield* resetApplicationTables;
          const { epoch } = yield* openFollowerWriteGate;
          yield* Ref.update(globals.FOLLOWER_WRITE_GATE, (local) => ({
            ...local,
            epoch,
          }));
          // A pending local finalization skips the L1 state-queue preflight.
          yield* Ref.set(globals.LOCAL_FINALIZATION_PENDING, true);
        }),
    );
    expect(Exit.isFailure(exit)).toBe(true);
    expectNodeProcessOwner(captured.commitWorkerOwner);
    await expectWorkerAcceptsOwner(captured.commitWorkerOwner!);
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
      const exit = await Effect.runPromiseExit(
        withoutFollowerJournal(probeWorkerLeaseOwner(owner)),
      );
      expect(Exit.isFailure(exit)).toBe(true);
      expect(captured.acceptedWorkerOwner).toBeUndefined();
    }
  });
});
