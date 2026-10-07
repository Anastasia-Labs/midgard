import { readFile } from "node:fs/promises";
import { join } from "node:path";

import {
  type ChainSyncEvent,
  type ChainSyncStream,
  IntersectNotFoundError,
} from "@al-ft/l1-node-transport";
import {
  type FactStore,
  openSqliteFactStore,
  type OriginConfig,
  type OutRef,
} from "@al-ft/midgard-l1-follower";
import {
  buildForkSteps,
  SIM_ORIGIN,
  simStoreOptions,
} from "@al-ft/midgard-l1-follower/testing";
import { describe, expect, it } from "vitest";

import type { LoadedCommitteeConfig } from "../../src/config.js";
import { loadCommitteeConfig } from "../../src/config.js";
import { committeeProjection } from "../../src/l1/follower/projection.js";
import {
  runShadowFollower,
  type ShadowFollowerStatus,
} from "../../src/l1/follower/shadow-follower.js";
import {
  SHADOW_STATUS_ENV,
  shadowFollowerPlan,
} from "../../src/l1/follower/shadow-follower-runtime.js";
import { tempDir } from "../helpers.js";
import {
  libp2pConfigEnv,
  libp2pManifest,
  writeConfigFiles,
} from "../helpers/l1-recovery-cli-config.js";
import {
  committeeForkCorpus,
  committeeSimProjection,
  SIM_K,
  SIM_QUEUE,
  zeroStats,
} from "./queue-sim.js";

const scenario = committeeForkCorpus()[0]!.scenario;
const { steps } = buildForkSteps(scenario, [
  committeeSimProjection(zeroStats()),
]);
const events: readonly ChainSyncEvent[] = steps.map((step) => step.event);
const lastTip = events[events.length - 1]!.tip;

const UNSPENT: OutRef = { txHash: Buffer.alloc(32, 0xee), index: 0 };

const simOrigin = (hubOracleOneShot: OutRef = UNSPENT): OriginConfig => ({
  origin: SIM_ORIGIN.point,
  hubOracleOneShot,
});

const openStore = (): FactStore =>
  openSqliteFactStore({
    ...simStoreOptions([committeeProjection(SIM_QUEUE)], SIM_K, "sqlite"),
    path: ":memory:",
  });

type Script = {
  /** Events served so far, across every stream the follower opened. */
  served: number;
  opens: number;
  /** `opened` rejects with this on the matching open (0-based). */
  intersectNotFound?: Readonly<{ open: number; resuming: boolean }>;
  /** `next` throws once, after this many events were served. */
  failAfter?: number;
};

/**
 * A chain-sync transport that serves the scenario's events in order, one
 * stream after another: a stream that fails or ends leaves the rest for the
 * next one, which resumes where the store's cursor is. Once the events run
 * out, `next` waits until the stream is closed.
 */
const scriptedTransport = (script: Script) => ({
  openChainSync: (): ChainSyncStream => {
    const open = script.opens;
    script.opens += 1;
    let closed = false;
    let release: (() => void) | undefined;
    const closing = new Promise<undefined>((resolve) => {
      release = () => resolve(undefined);
    });
    const opened =
      script.intersectNotFound?.open === open
        ? Promise.reject(
            new IntersectNotFoundError(
              lastTip,
              script.intersectNotFound.resuming,
            ),
          )
        : Promise.resolve();
    opened.catch(() => undefined);
    return {
      opened,
      next: async () => {
        if (closed) return undefined;
        if (script.failAfter === script.served) {
          script.failAfter = undefined;
          throw new Error("the transport sidecar went away");
        }
        if (script.served < events.length) {
          const event = events[script.served]!;
          script.served += 1;
          return event;
        }
        return closing;
      },
      ack: () => undefined,
      close: () => {
        closed = true;
        release?.();
        return Promise.resolve();
      },
    } as unknown as ChainSyncStream;
  },
});

const readStatus = async (
  path: string,
): Promise<ShadowFollowerStatus | undefined> => {
  try {
    return JSON.parse(await readFile(path, "utf8")) as ShadowFollowerStatus;
  } catch {
    return undefined;
  }
};

/**
 * Runs the follower until `until` holds of its status file (or it returns
 * by itself), then stops it and returns the final status and its log.
 */
const follow = async (
  options: Readonly<{
    store: FactStore;
    script: Script;
    origin?: OriginConfig;
    until: (status: ShadowFollowerStatus) => boolean;
  }>,
): Promise<{ status: ShadowFollowerStatus; log: string[] }> => {
  const statusPath = join(await tempDir(), "shadow-status.json");
  const abort = new AbortController();
  const log: string[] = [];
  let returned = false;
  const running = runShadowFollower({
    store: options.store,
    transport: scriptedTransport(options.script),
    origin: options.origin ?? simOrigin(),
    statusPath,
    signal: abort.signal,
    backoffMs: { initial: 1, max: 5 },
    now: () => new Date(0),
    log: (line) => log.push(line),
  }).finally(() => {
    returned = true;
  });
  const deadline = Date.now() + 4_000;
  for (;;) {
    const status = await readStatus(statusPath);
    if (returned || (status !== undefined && options.until(status))) break;
    if (Date.now() > deadline)
      throw new Error(
        `timed out; status ${JSON.stringify(status)} log ${JSON.stringify(log.slice(0, 10))} opens ${options.script.opens} served ${options.script.served}`,
      );
    await new Promise((resolve) => setTimeout(resolve, 5));
  }
  abort.abort();
  await running;
  const status = await readStatus(statusPath);
  if (status === undefined) throw new Error("no status file");
  return { status, log };
};

const caughtUp = (status: ShadowFollowerStatus): boolean =>
  status.events === events.length;

/** An outref the scenario's final chain spends, and the slot of the spend. */
const spentQueueOutRef = async (
  store: FactStore,
): Promise<Readonly<{ outRef: OutRef; slot: number }>> => {
  for (const event of events) {
    if (event.kind !== "roll_forward" || event.point.kind !== "point") continue;
    const read = await store.liveUtxos(
      { by: "address", address: SIM_QUEUE.stateQueueAddress },
      {
        slot: Number(event.point.slot),
        hash: Buffer.from(event.point.hash, "hex"),
      },
    );
    if (read.kind !== "ok") continue;
    for (const utxo of read.utxos) {
      const spend = await store.txSpending(utxo.outRef);
      if (spend !== null && spend.slot > SIM_ORIGIN.point.slot + 1)
        return { outRef: utxo.outRef, slot: spend.slot };
    }
  }
  throw new Error("the scenario spends no queue output");
};

describe("committee shadow follower", () => {
  it("follows from the configured origin and records R3 at the tip without the protocol-init spend", async () => {
    const store = openStore();
    try {
      const { status } = await follow({
        store,
        script: { served: 0, opens: 0 },
        until: caughtUp,
      });
      expect(status).toMatchObject({
        schema: "committee-l1-follower-shadow-v1",
        state: "stopped",
        events: events.length,
        protocolInit: "pending",
        lastError: null,
      });
      expect(status.interventions.map((i) => i.reason)).toEqual([
        "origin_after_protocol_init",
      ]);
      const cursor = await store.cursor();
      expect(cursor?.point.slot).toBe(
        lastTip.point.kind === "point" ? Number(lastTip.point.slot) : -1,
      );
      expect(status.cursor).toEqual({
        slot: cursor!.point.slot,
        height: cursor!.height,
        generation: cursor!.generation,
      });
    } finally {
      await store.close();
    }
  });

  it("clears R3 once the hubOracleOneShot spend is in the facts", async () => {
    const first = openStore();
    let spent: Awaited<ReturnType<typeof spentQueueOutRef>>;
    try {
      await follow({
        store: first,
        script: { served: 0, opens: 0 },
        until: caughtUp,
      });
      spent = await spentQueueOutRef(first);
    } finally {
      await first.close();
    }
    const store = openStore();
    try {
      const { status, log } = await follow({
        store,
        script: { served: 0, opens: 0 },
        origin: simOrigin(spent.outRef),
        until: caughtUp,
      });
      // Raised at the tips before the spend, cleared by it.
      expect(
        log.some((line) => line.includes("origin_after_protocol_init")),
      ).toBe(true);
      expect(status).toMatchObject({
        state: "stopped",
        events: events.length,
        protocolInit: "seen",
        interventions: [],
      });
    } finally {
      await store.close();
    }
  });

  it("stops on a configured origin the node's chain does not extend (R4)", async () => {
    const store = openStore();
    try {
      const { status } = await follow({
        store,
        script: { served: 0, opens: 0 },
        origin: {
          origin: { slot: SIM_ORIGIN.point.slot, hash: Buffer.alloc(32, 0x0b) },
          hubOracleOneShot: UNSPENT,
        },
        until: (s) => s.state === "intervention",
      });
      expect(status.state).toBe("intervention");
      expect(status.interventions.map((i) => i.reason)).toEqual([
        "origin_not_on_chain",
      ]);
      expect(await store.cursor()).toBeNull();
    } finally {
      await store.close();
    }
  });

  it("maps a failed intersection to R4 on a fresh store and R2 on resume", async () => {
    const fresh = openStore();
    try {
      const { status } = await follow({
        store: fresh,
        script: {
          served: 0,
          opens: 0,
          intersectNotFound: { open: 0, resuming: false },
        },
        until: (s) => s.state === "intervention",
      });
      expect(status.interventions.map((i) => i.reason)).toEqual([
        "origin_not_on_chain",
      ]);
    } finally {
      await fresh.close();
    }
    const store = openStore();
    try {
      await follow({
        store,
        script: { served: 0, opens: 0 },
        until: caughtUp,
      });
      const { status } = await follow({
        store,
        script: {
          served: events.length,
          opens: 0,
          intersectNotFound: { open: 0, resuming: false },
        },
        until: (s) => s.state === "intervention",
      });
      expect(status.state).toBe("intervention");
      expect(status.interventions.map((i) => i.reason)).toEqual([
        "intersection_outside_history",
      ]);
    } finally {
      await store.close();
    }
  });

  it("refuses to resume a store initialized at another origin", async () => {
    const store = openStore();
    try {
      await follow({
        store,
        script: { served: 0, opens: 0 },
        until: caughtUp,
      });
      const script: Script = { served: events.length, opens: 0 };
      const { status } = await follow({
        store,
        script,
        origin: {
          origin: {
            slot: SIM_ORIGIN.point.slot + 1,
            hash: Buffer.alloc(32, 1),
          },
          hubOracleOneShot: UNSPENT,
        },
        until: (s) => s.state === "intervention",
      });
      expect(status.interventions.map((i) => i.reason)).toEqual([
        "origin_mismatch",
      ]);
      // It never opened a stream for the wrong origin.
      expect(script.opens).toBe(0);
    } finally {
      await store.close();
    }
  });

  it("backs off from a transport failure and resumes from its cursor", async () => {
    const store = openStore();
    try {
      const script: Script = {
        served: 0,
        opens: 0,
        failAfter: Math.floor(events.length / 2),
      };
      const { status, log } = await follow({
        store,
        script,
        until: caughtUp,
      });
      expect(script.opens).toBe(2);
      expect(status).toMatchObject({
        state: "stopped",
        events: events.length,
        lastError: null,
      });
      expect(log.filter((line) => line.startsWith("intervention "))).toEqual([
        expect.stringContaining("origin_after_protocol_init"),
      ]);
    } finally {
      await store.close();
    }
  });
});

describe("shadow follower gating", () => {
  const configured = async (): Promise<LoadedCommitteeConfig> => {
    const dir = await tempDir();
    const { manifestPath, deploymentInfoPath } = await writeConfigFiles(
      dir,
      libp2pManifest("01".repeat(32)),
    );
    const config = await loadCommitteeConfig({
      ...libp2pConfigEnv(dir, manifestPath, deploymentInfoPath),
      DA_SIGNER_INDEX: "0",
      DA_SIGNER_KEY_SOURCE: "hex:" + "00".repeat(32),
    });
    return {
      ...config,
      l1Origin: { slot: 1234, blockHash: "ab".repeat(32) },
      nativeLedger: {
        authorityNodeId: "node-0",
        socketPath: "/run/node.socket",
        nodeConfigPath: "/run/config.json",
        binaryPath: "/bin/transport",
      },
      localState: { kind: "database", url: "postgres://committee" },
      contractDeploymentInfo: {
        ...config.contractDeploymentInfo,
        hubOracleOneShot: { txHash: "cd".repeat(32), outputIndex: 2 },
      },
    };
  };
  const env = { [SHADOW_STATUS_ENV]: "/run/shadow.json" };

  it("is off unless the operator names the status file", async () => {
    const config = await configured();
    expect(shadowFollowerPlan(config, {})).toEqual({ kind: "off" });
    expect(shadowFollowerPlan(config, { [SHADOW_STATUS_ENV]: "" })).toEqual({
      kind: "off",
    });
  });

  it("runs from L1_ORIGIN, the local node and the Postgres local state", async () => {
    const config = await configured();
    expect(shadowFollowerPlan(config, env)).toEqual({
      kind: "run",
      statusPath: "/run/shadow.json",
      databaseUrl: "postgres://committee",
      socketPath: "/run/node.socket",
      binaryPath: "/bin/transport",
      networkMagic: config.cardanoL1Source.networkMagic,
      origin: {
        origin: { slot: 1234, hash: Buffer.from("ab".repeat(32), "hex") },
        hubOracleOneShot: {
          txHash: Buffer.from("cd".repeat(32), "hex"),
          index: 2,
        },
      },
    });
  });

  it("names what is missing", async () => {
    const config = await configured();
    const reason = (changed: LoadedCommitteeConfig): string => {
      const plan = shadowFollowerPlan(changed, env);
      return plan.kind === "unavailable" ? plan.reason : plan.kind;
    };
    expect(reason({ ...config, l1Origin: undefined })).toMatch(/L1_ORIGIN/u);
    expect(reason({ ...config, nativeLedger: undefined })).toMatch(
      /local node/u,
    );
    expect(
      reason({
        ...config,
        localState: { kind: "file", path: "/run/state.json" },
      }),
    ).toMatch(/Postgres/u);
    for (const hubOracleOneShot of [
      undefined,
      { txHash: "cd".repeat(31), outputIndex: 0 },
      { txHash: "cd".repeat(32), outputIndex: -1 },
    ])
      expect(
        reason({
          ...config,
          contractDeploymentInfo: {
            ...config.contractDeploymentInfo,
            hubOracleOneShot,
          },
        }),
      ).toMatch(/hubOracleOneShot/u);
  });
});
