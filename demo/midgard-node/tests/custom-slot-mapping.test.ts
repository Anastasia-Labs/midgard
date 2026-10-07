import { ogmiosSlotEvidenceUnavailableCause } from "@al-ft/midgard-core/ogmios-slot";
import { Effect, Logger } from "effect";
import { describe, expect, it } from "vitest";

import {
  CUSTOM_SLOT_MAPPING_ENVIRONMENT_KEY,
  type CustomSlotMappingEnvironment,
  resolveCustomSlotMapping,
  retryTransientSubmitSlotSnapshot,
} from "../src/custom-slot-mapping.js";
import { fetchLocalOgmiosSubmitSlotSnapshot } from "../src/l1-heads.js";

const OGMIOS = "http://127.0.0.1:1337";
// Whole seconds in the past, so wall-clock slots are exact.
const GENESIS_START_MS = Math.floor(Date.now() / 1_000) * 1_000 - 3_600_000;
const slotAt = (ms: number) => Math.floor((ms - GENESIS_START_MS) / 1_000);

const json = (body: unknown, status = 200) =>
  new Response(JSON.stringify(body), {
    status,
    headers: { "content-type": "application/json" },
  });

type Health = "fresh" | "stale" | "malformed";

/**
 * A fake local Ogmios. `health` answers successive `/health` polls (the last
 * entry repeats); `genesis` answers successive genesis queries likewise.
 */
const fakeOgmios = (options: {
  readonly health?: readonly Health[];
  readonly genesis?: ReadonlyArray<"ok" | "down" | "two-second-slots">;
}) => {
  const calls = { genesis: 0, health: 0, tip: 0 };
  const pick = <T>(list: readonly T[], index: number): T =>
    list[Math.min(index, list.length - 1)]!;
  const fetchImpl = async (url: string, init?: RequestInit) => {
    const now = Date.now();
    if (url.endsWith("/health")) {
      const state = pick(options.health ?? ["fresh"], calls.health);
      calls.health += 1;
      const updatedAt = state === "stale" ? now - 300_000 : now - 1_000;
      return json(
        state === "malformed"
          ? { networkSynchronization: 1 }
          : {
              connectionStatus: "connected",
              networkSynchronization: 1,
              lastKnownTip: { slot: slotAt(updatedAt) },
              lastTipUpdate: new Date(updatedAt).toISOString(),
            },
      );
    }
    const method = (JSON.parse(String(init?.body)) as { method: string })
      .method;
    if (method === "queryNetwork/tip") {
      calls.tip += 1;
      return json({ jsonrpc: "2.0", result: { slot: slotAt(now - 1_000) } });
    }
    const state = pick(options.genesis ?? ["ok"], calls.genesis);
    calls.genesis += 1;
    if (state === "down") {
      throw new TypeError("fetch failed");
    }
    return json({
      jsonrpc: "2.0",
      result: {
        networkMagic: 42,
        startTime: new Date(GENESIS_START_MS).toISOString(),
        slotLength: {
          milliseconds: state === "two-second-slots" ? 2_000 : 1_000,
        },
        activeSlotsCoefficient: "1/20",
      },
    });
  };
  return { calls, fetchImpl };
};

const environment = (isMainThread: boolean, shared?: unknown) => {
  const sets: unknown[] = [];
  const env: CustomSlotMappingEnvironment = {
    isMainThread,
    get: (key) =>
      key === CUSTOM_SLOT_MAPPING_ENVIRONMENT_KEY ? shared : undefined,
    set: (_key, value) => {
      sets.push(value);
    },
  };
  return { env, sets };
};

const run = <A, E>(effect: Effect.Effect<A, E>) => {
  const logs: string[] = [];
  const logger = Logger.make(({ message }) => {
    logs.push(Array.isArray(message) ? message.join(" ") : String(message));
  });
  return Effect.runPromise(
    Effect.either(effect).pipe(
      Effect.provide(Logger.replace(Logger.defaultLogger, logger)),
    ),
  ).then((result) => ({ result, logs }));
};

const FAST = { baseDelayMs: 1, maxDelayMs: 2 } as const;

const resolve = (
  ogmios: ReturnType<typeof fakeOgmios>,
  env: CustomSlotMappingEnvironment,
  custom = true,
) =>
  resolveCustomSlotMapping({
    ogmiosUrl: OGMIOS,
    timeoutMs: 1_000,
    custom,
    fetchImpl: ogmios.fetchImpl,
    retry: FAST,
    environment: env,
  });

const unready = (logs: readonly string[], reason: string) =>
  logs.filter((line) => line.includes(`unready: reason=${reason}`)).length;

describe("the node's Custom Lucid slot mapping", () => {
  it("waits out a 300 s-old tip, then builds the mapping exactly once", async () => {
    const ogmios = fakeOgmios({ health: ["stale", "stale", "stale", "fresh"] });
    const { env, sets } = environment(true);
    const { result, logs } = await run(resolve(ogmios, env));

    expect(result._tag).toBe("Right");
    const mapping = result._tag === "Right" ? result.right : undefined;
    expect(mapping).toMatchObject({
      ogmiosUrl: OGMIOS,
      slotConfig: {
        zeroTime: GENESIS_START_MS,
        zeroSlot: 0,
        slotLength: 1_000,
      },
      tipMaxAgeMs: 200_000,
    });
    expect(ogmios.calls.genesis).toBe(1);
    expect(ogmios.calls.health).toBe(4);
    expect(unready(logs, "ogmios_tip_stale")).toBe(3);
    expect(sets).toEqual([mapping]);
  });

  it("waits out an unreachable Ogmios before the genesis query lands", async () => {
    const ogmios = fakeOgmios({ genesis: ["down", "down", "ok"] });
    const { env, sets } = environment(true);
    const { result, logs } = await run(resolve(ogmios, env));

    expect(result._tag).toBe("Right");
    expect(ogmios.calls.genesis).toBe(3);
    expect(unready(logs, "ogmios_unreachable")).toBe(2);
    expect(sets).toHaveLength(1);
  });

  it("refuses a genesis slot length other than the profile's at once", async () => {
    const ogmios = fakeOgmios({ genesis: ["two-second-slots"] });
    const { env, sets } = environment(true);
    const { result, logs } = await run(resolve(ogmios, env));

    expect(result._tag).toBe("Left");
    expect(String(result._tag === "Left" ? result.left : "")).toMatch(
      /Custom slot length disagreement/u,
    );
    expect(ogmios.calls).toEqual({ genesis: 1, health: 0, tip: 0 });
    expect(logs.some((line) => line.includes("unready"))).toBe(false);
    expect(sets).toEqual([]);
  });

  it("refuses a malformed health answer at once", async () => {
    const ogmios = fakeOgmios({ health: ["malformed", "fresh"] });
    const { env, sets } = environment(true);
    const { result } = await run(resolve(ogmios, env));

    expect(result._tag).toBe("Left");
    expect(ogmios.calls.health).toBe(1);
    expect(sets).toEqual([]);
  });

  it("lets a worker inherit the main thread's mapping without a query", async () => {
    const main = fakeOgmios({});
    const mainEnv = environment(true);
    const { result: published } = await run(resolve(main, mainEnv.env));
    expect(published._tag).toBe("Right");

    const worker = fakeOgmios({ health: ["stale"] });
    const workerEnv = environment(false, mainEnv.sets[0]);
    const { result } = await run(resolve(worker, workerEnv.env));
    expect(result).toEqual(published);
    expect(worker.calls).toEqual({ genesis: 0, health: 0, tip: 0 });
    expect(workerEnv.sets).toEqual([]);
  });

  it("ignores a published mapping for another Ogmios", async () => {
    const worker = fakeOgmios({});
    const { env } = environment(false, {
      version: 1,
      ogmiosUrl: "http://elsewhere:1337",
      slotConfig: { zeroTime: 0, zeroSlot: 0, slotLength: 1_000 },
      tipMaxAgeMs: 200_000,
      genesisConfigurationSha256: "00",
    });
    const { result } = await run(resolve(worker, env));
    expect(result._tag).toBe("Right");
    // A worker without an inherited mapping reads genesis alone; the tip
    // health check stays with the main thread.
    expect(worker.calls).toEqual({ genesis: 1, health: 0, tip: 0 });
  });

  it("keeps only the tip-age bound on a named network", async () => {
    const ogmios = fakeOgmios({ health: ["stale"] });
    const { env } = environment(true);
    const { result } = await run(resolve(ogmios, env, false));
    expect(result._tag === "Right" ? result.right : undefined).toEqual({
      version: 1,
      ogmiosUrl: OGMIOS,
      tipMaxAgeMs: 200_000,
      genesisConfigurationSha256: expect.any(String),
    });
    expect(ogmios.calls.health).toBe(0);
  });
});

describe("the submit-slot snapshot at submit time", () => {
  const readOnce = (ogmios: ReturnType<typeof fakeOgmios>) => () =>
    fetchLocalOgmiosSubmitSlotSnapshot({
      ogmiosUrl: OGMIOS,
      fetchImpl: ogmios.fetchImpl,
      maxHealthAgeMs: 200_000,
    });
  const retry = { maxAttempts: 4, baseDelayMs: 1, maxDelayMs: 2 } as const;

  it("re-reads a stale tip and proceeds once it is fresh", async () => {
    const ogmios = fakeOgmios({ health: ["stale", "stale", "fresh"] });
    const { result } = await run(
      retryTransientSubmitSlotSnapshot(readOnce(ogmios), retry),
    );
    expect(result._tag).toBe("Right");
    expect(ogmios.calls.health).toBe(3);
    expect(ogmios.calls.tip).toBe(1);
  });

  it("still refuses a tip that stays stale past the bound", async () => {
    const ogmios = fakeOgmios({ health: ["stale"] });
    const { result } = await run(
      retryTransientSubmitSlotSnapshot(readOnce(ogmios), retry),
    );
    expect(result._tag).toBe("Left");
    expect(
      ogmiosSlotEvidenceUnavailableCause(
        result._tag === "Left" ? result.left : undefined,
      )?.reason,
    ).toBe("ogmios_tip_stale");
    expect(ogmios.calls.health).toBe(4);
    expect(ogmios.calls.tip).toBe(0);
  });

  it("refuses a malformed answer without re-reading", async () => {
    const ogmios = fakeOgmios({ health: ["malformed", "fresh"] });
    const { result } = await run(
      retryTransientSubmitSlotSnapshot(readOnce(ogmios), retry),
    );
    expect(result._tag).toBe("Left");
    expect(ogmios.calls.health).toBe(1);
  });
});
