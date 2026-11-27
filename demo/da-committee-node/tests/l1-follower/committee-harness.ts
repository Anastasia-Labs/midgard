import {
  applyChainSyncEvent,
  type FactStore,
  type FollowStatus,
  openPostgresFactStore,
  openSqliteFactStore,
  stepSettled,
} from "@al-ft/midgard-l1-follower";
import {
  SIM_ORIGIN,
  simStoreOptions,
} from "@al-ft/midgard-l1-follower/testing";
import { blake2b } from "@noble/hashes/blake2.js";
import { expect } from "vitest";

import {
  CommitteeService,
  type CommitteeServiceDeps,
} from "../../src/committee-service.js";
import type { CommitteeConfig } from "../../src/config.js";
import type { ReconcileAttestationArgs } from "../../src/coordinator/on-chain.js";
import { SubmitterReconciler } from "../../src/coordinator/submitter-reconciler.js";
import { committeeL1Source } from "../../src/l1/follower/l1-follower.js";
import type { SlotTime } from "../../src/l1/follower/obligations.js";
import { committeeProjection } from "../../src/l1/follower/projection.js";
import { validateDaCommittee } from "../../src/signer.js";
import type { CommitteeStore } from "../../src/store.js";
import type { PostgresCommitteeStore } from "../../src/store/postgres.js";
import { bytesToHex } from "../../src/utils/hex.js";
import { minimalConfig, tempDir } from "../helpers.js";
import { openTestCommitteeStore } from "../helpers/committee-store.js";
import { postgresTestDatabases } from "../helpers/postgres-database.js";
import { QueueChain } from "./queue-chain.js";
import { SIM_DEPTHS, SIM_QUEUE, SIM_SLOT_TIME } from "./queue-sim.js";

const K = SIM_DEPTHS.securityParameter;
const projection = committeeProjection(SIM_QUEUE);

/** Fact stores in both dialects, for `describe.each`. */
export const factStoreDialects = (databasePrefix: string) => {
  const databases = postgresTestDatabases(databasePrefix);
  const openSqlite = async (): Promise<FactStore> =>
    openSqliteFactStore({
      ...simStoreOptions([projection], K, "sqlite"),
      path: ":memory:",
    });
  const openPostgres = async (): Promise<FactStore> => {
    const database = await databases.create();
    return openPostgresFactStore({
      ...simStoreOptions([projection], K, "postgres"),
      connection: { connectionString: database.url },
    });
  };
  return {
    databases,
    dialects: [
      ["SQLite", openSqlite],
      ["Postgres", openPostgres],
    ] as const,
  };
};

export type CommitteeServiceOverrides = Partial<
  Omit<CommitteeServiceDeps, "config" | "store" | "l1">
>;

/**
 * A committee member on its L1 follower's facts over the simulator's state
 * queue: `apply` lands one chain event in the fact store, and `service`
 * builds a committee service on the same facts and committee store (more
 * than one may share them).
 */
export const committeeOnQueueChain = async (
  factStore: FactStore,
  {
    slotTime = SIM_SLOT_TIME,
    config: configOverrides = {},
  }: {
    readonly slotTime?: SlotTime;
    readonly config?: Partial<CommitteeConfig>;
  } = {},
) => {
  expect(await factStore.start()).toMatchObject({ kind: "ready" });
  expect(
    await factStore.initialize({
      point: SIM_ORIGIN.point,
      height: SIM_ORIGIN.height,
    }),
  ).toMatchObject({ kind: "initialized" });
  const queue = new QueueChain(slotTime);
  const apply = async (
    event: Parameters<typeof applyChainSyncEvent>[1],
  ): Promise<void> => {
    expect(stepSettled(await applyChainSyncEvent(factStore, event))).toBe(true);
  };
  const dir = await tempDir();
  const config: CommitteeConfig = {
    ...minimalConfig({
      manifestPath: `${dir}/manifest.json`,
      deploymentInfoPath: `${dir}/deployment.json`,
      signerSeed: `${"00".repeat(31)}47`,
      signerPublicKey: "00".repeat(32),
    }),
    finalityDepth: SIM_DEPTHS.confirmationDepth,
    automaticRecoveryMaxDepth: K,
    ...configOverrides,
  };
  const store: PostgresCommitteeStore = await openTestCommitteeStore();
  const l1 = () =>
    committeeL1Source({
      store: factStore,
      parameters: SIM_DEPTHS,
      status: () =>
        ({
          readiness: [],
          cursor: {
            slot: queue.chain.tip.point.slot,
            height: queue.chain.tip.height,
            generation: 0,
          },
        }) as unknown as FollowStatus,
      slotTime: async () => slotTime,
    });
  const service = async (
    overrides: CommitteeServiceOverrides = {},
  ): Promise<CommitteeService> => {
    const built = new CommitteeService({
      config,
      store,
      l1: l1(),
      payloadSource: {
        fetchPayloadCandidates: async () => ({
          ok: true,
          candidates: [],
          attempts: [],
        }),
      },
      writeEvent: () => undefined,
      ...overrides,
    });
    await built.initialize();
    return built;
  };
  return { queue, apply, config, store, service };
};

/** A coordinator that counts posts; `hold` keeps the next one in flight. */
export const countingCoordinator = () => {
  const posts: string[] = [];
  let held: (() => void) | undefined;
  let holdNext = false;
  return {
    posts,
    hold: () => {
      holdNext = true;
    },
    release: () => held?.(),
    reconcileAttestation: async (
      args: ReconcileAttestationArgs,
    ): Promise<"posted"> => {
      posts.push(args.context.validation.stateQueueOutRef);
      if (holdNext) {
        holdNext = false;
        await new Promise<void>((resolve) => {
          held = resolve;
        });
      }
      return "posted";
    },
  };
};

/** A real submitter reconciler over `coordinator`. */
export const reconcilerFor = (
  config: CommitteeConfig,
  store: CommitteeStore,
  coordinator: ReturnType<typeof countingCoordinator>,
) =>
  new SubmitterReconciler({
    deploymentFingerprint: config.deploymentFingerprint,
    committeeValidation: validateDaCommittee({
      daParams: {
        ...config.daParams,
        committeeSignersHash: bytesToHex(
          blake2b(Buffer.from(config.daParams.committeeHex, "hex"), {
            dkLen: 32,
          }),
        ),
      },
    }),
    availabilityCommitmentAuthority: {
      deploymentIdentity: config.hubOraclePolicyId,
      responseGeometry: config.availabilityChallenge.responseGeometry,
    },
    store,
    coordinator,
  });
