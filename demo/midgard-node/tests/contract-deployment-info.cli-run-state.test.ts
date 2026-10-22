/**
 * The registered `init` and `reconcile deployment-manifest` commands finalize
 * the first deployment manifest with the L1 origin recorded in the run state
 * that `--run-state` names, not the default one, and read it before the L1
 * provider or the contracts are touched. Only the node config, the init
 * transaction and live deployment status, the local node's proof and the
 * provider are stubbed; the commands, the manifest program and the run-state
 * read are real.
 */
import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import type { L1Origin } from "@al-ft/midgard-core/l1-origin";
import type { FindOriginResult } from "@al-ft/midgard-l1-follower";
import { Cause, Effect, type Exit } from "effect";
import { describe, expect, it, onTestFinished, vi } from "vitest";

const { NONCE_TX, OTHER_NONCE_TX, INIT_TX } = vi.hoisted(() => ({
  NONCE_TX: "ef".repeat(32),
  OTHER_NONCE_TX: "0e".repeat(32),
  INIT_TX: "1a".repeat(32),
}));
const ORIGIN: L1Origin = { slot: 4_200, blockHash: "ab".repeat(32) };

const seen = vi.hoisted(() => ({
  /** What happened, in order: each proof asked and each provider touch. */
  events: [] as string[],
  run: undefined as Promise<Exit.Exit<unknown, unknown>> | undefined,
}));

/** A service that records, then refuses, any use but reaching `pass`. */
const untouchable = (
  name: string,
  pass: Readonly<Record<string, object>> = {},
): object =>
  new Proxy(
    {},
    {
      get: (_target, property) => {
        if (typeof property === "symbol") return undefined;
        if (Object.hasOwn(pass, property)) return pass[property];
        seen.events.push(`touched ${name}.${property}`);
        throw new Error(`${name}.${property} touched`);
      },
    },
  );

vi.mock("../src/runtime-env.js", () => ({ loadRuntimeDotenv: () => {} }));
// The init transaction confirms and the live deployment is complete.
vi.mock("../src/transactions/initialization.js", async (importOriginal) => {
  const original =
    await importOriginal<
      typeof import("../src/transactions/initialization.js")
    >();
  return {
    ...original,
    program: Effect.succeed(INIT_TX),
    fetchProtocolDeploymentStatus: () =>
      Effect.succeed({
        complete: true,
        missingComponents: [],
        hubOracleWitness: { txHash: INIT_TX, outputIndex: 0 },
      }),
  };
});
// The local node proves any origin it is asked about.
vi.mock(
  "../src/commands/contract-deployment-info.manifest-l1-origin.js",
  async (importOriginal) => {
    const original =
      await importOriginal<
        typeof import("../src/commands/contract-deployment-info.manifest-l1-origin.js")
      >();
    const found: FindOriginResult = {
      kind: "found",
      origin: { slot: 4_250, blockHash: "12".repeat(32) },
      nonceBlock: { slot: 4_260, blockHash: "34".repeat(32), height: 9 },
      txIndex: 0,
      depth: 12,
    };
    return {
      ...original,
      localNodeManifestL1OriginProver: () =>
        Effect.succeed((origin: L1Origin, nonceTxHash: string) => {
          seen.events.push(
            `prove ${origin.slot.toString()} for ${nonceTxHash}`,
          );
          return Promise.resolve(found);
        }),
    };
  },
);
vi.mock("../src/commands/cli-runtime.js", async (importOriginal) => {
  const original =
    await importOriginal<typeof import("../src/commands/cli-runtime.js")>();
  const { Effect: E } = await import("effect");
  const Services = await import("../src/services/index.js");
  const { IntentJournalWithoutFollower } = await import(
    "../src/services/intent-journal.js"
  );
  return {
    ...original,
    provideTxServices: (effect: Effect.Effect<unknown, unknown, never>) =>
      effect.pipe(
        E.provide(IntentJournalWithoutFollower),
        E.provideService(Services.NodeConfig, {
          NETWORK: "Preprod",
          HUB_ORACLE_ONE_SHOT_TX_HASH: NONCE_TX,
        } as never),
        E.provideService(
          Services.Lucid,
          untouchable("lucid", { api: untouchable("lucid.api") }) as never,
        ),
        E.provideService(
          Services.MidgardContracts,
          untouchable("contracts") as never,
        ),
      ),
    runCliEffect: (effect: Effect.Effect<unknown, unknown>) => {
      seen.run = E.runPromiseExit(effect);
    },
  };
});

import "../src/index.registration-4.js";

import {
  createDeploymentRunState,
  writeDeploymentRunStateAtomic,
} from "../src/e2e/run-state.js";
import { program } from "../src/index.registration.js";

/** A run state for hub-oracle nonce `txHash` with `ORIGIN` recorded. */
const writeRunState = (path: string, txHash: string) =>
  writeDeploymentRunStateAtomic(
    path,
    createDeploymentRunState({
      mode: "fresh",
      identity: {
        network: "Preprod",
        hubOracleOneShot: { txHash, outputIndex: 0 },
        l1Origin: ORIGIN,
      },
    }),
  );

const COMMANDS = [
  ["init", (out: string) => ["init", "--contract-deployment-info-output", out]],
  [
    "reconcile deployment-manifest",
    (out: string) => [
      "reconcile",
      "deployment-manifest",
      "--out",
      out,
      "--init-tx-hash",
      INIT_TX,
    ],
  ],
] as const;

/**
 * A deployment directory with no manifest yet: the run state `--run-state`
 * names, and a default run state (`MIDGARD_RUN_STATE_PATH`) for `defaultNonce`,
 * or none.
 */
const deployment = async (defaultNonce: string | undefined) => {
  const directory = await mkdtemp(join(tmpdir(), "midgard-cli-run-state-"));
  onTestFinished(() => rm(directory, { recursive: true, force: true }));
  const defaultPath = join(directory, "default-run-state.json");
  if (defaultNonce !== undefined)
    await writeRunState(defaultPath, defaultNonce);
  vi.stubEnv("MIDGARD_RUN_STATE_PATH", defaultPath);
  onTestFinished(() => {
    vi.unstubAllEnvs();
  });
  return {
    runStatePath: join(directory, "named-run-state.json"),
    out: join(directory, "contract-deployment-info.json"),
  };
};

/** Runs the registered command; returns its exit. */
const command = async (args: readonly string[]) => {
  seen.events = [];
  seen.run = undefined;
  await program.parseAsync(["node", "midgard-node", ...args]);
  return seen.run!;
};

const failure = (exit: Exit.Exit<unknown, unknown>): string =>
  exit._tag === "Failure" ? Cause.pretty(exit.cause) : "succeeded";

describe.each(COMMANDS)(
  "%s finalizing the first deployment manifest",
  (_name, args) => {
    it("R-2: proves the origin recorded in the run state --run-state names, before touching the L1 provider", async () => {
      const { runStatePath, out } = await deployment(undefined);
      await writeRunState(runStatePath, NONCE_TX);
      await command([...args(out), "--run-state", runStatePath]);
      expect(seen.events[0]).toBe(
        `prove ${ORIGIN.slot.toString()} for ${NONCE_TX}`,
      );
    });

    it("R-2: refuses the run state --run-state names when it belongs to another nonce, naming it, before touching the L1 provider", async () => {
      const { runStatePath, out } = await deployment(NONCE_TX);
      await writeRunState(runStatePath, OTHER_NONCE_TX);
      const exit = await command([...args(out), "--run-state", runStatePath]);
      expect(failure(exit)).toContain(
        `the deployment run state ${runStatePath} belongs to hub-oracle nonce ${OTHER_NONCE_TX}, not HUB_ORACLE_ONE_SHOT_TX_HASH ${NONCE_TX}`,
      );
      expect(seen.events).toEqual([]);
    });
  },
);
