/**
 * A Custom committee Lucid client waits out a local Ogmios that is
 * restarting, resyncing or between blocks, and builds on its genesis mapping
 * exactly once; a wrong network or a malformed answer still refuses at once.
 */
import { createServer, type IncomingMessage, type Server } from "node:http";
import type { AddressInfo } from "node:net";

import { ogmiosSlotEvidenceUnavailableCause } from "@al-ft/midgard-core/ogmios-slot";
import { afterEach, describe, expect, it } from "vitest";

import {
  assertOgmiosNetworkMagic,
  committeeLucidSlotOptions,
  DaCommitteeCustomSlotMappingError,
} from "../src/l1/lucid-network.js";

const MAGIC = 424_242;
const MAGIC_ID = "midgard-network-magic-preflight";

type Reply = "ok" | "unavailable" | "wrong-magic";
type Health = "fresh" | "stale" | "malformed";

const servers: Server[] = [];
afterEach(async () => {
  await Promise.all(
    servers
      .splice(0)
      .map((server) => new Promise((resolve) => server.close(resolve))),
  );
});

const readBody = (request: IncomingMessage): Promise<string> =>
  new Promise((resolve, reject) => {
    let body = "";
    request.on("data", (chunk: Buffer) => (body += chunk.toString("utf8")));
    request.on("end", () => resolve(body));
    request.on("error", reject);
  });

/** A scripted Ogmios: each list answers successive requests, last repeats. */
const startScriptedOgmios = async (script: {
  readonly magic?: readonly Reply[];
  readonly genesis?: readonly Reply[];
  readonly health?: readonly Health[];
}) => {
  let startTimeMs = Math.floor(Date.now() / 1_000) * 1_000 - 3_600_000;
  const slotAt = (ms: number) => Math.floor((ms - startTimeMs) / 1_000);
  const counts = { magic: 0, genesis: 0, health: 0, tip: 0 };
  const pick = <T>(
    list: readonly T[] | undefined,
    index: number,
    fallback: T,
  ) =>
    list === undefined || list.length === 0
      ? fallback
      : list[Math.min(index, list.length - 1)]!;
  const server = createServer((request, response) => {
    void (async () => {
      const reply = (status: number, body: unknown) => {
        response.writeHead(status, { "content-type": "application/json" });
        response.end(JSON.stringify(body));
      };
      const now = Date.now();
      if (request.method === "GET" && request.url === "/health") {
        const state = pick(script.health, counts.health, "fresh");
        counts.health += 1;
        const updatedAt = state === "stale" ? now - 300_000 : now - 1_000;
        return reply(
          200,
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
      const rpc = JSON.parse(await readBody(request)) as {
        readonly method: string;
        readonly id: string;
      };
      if (rpc.method === "queryNetwork/tip") {
        counts.tip += 1;
        return reply(200, { result: { slot: slotAt(now - 1_000) } });
      }
      const forMagic = rpc.id === MAGIC_ID;
      const state = forMagic
        ? pick(script.magic, counts.magic, "ok")
        : pick(script.genesis, counts.genesis, "ok");
      counts[forMagic ? "magic" : "genesis"] += 1;
      if (state === "unavailable") {
        return reply(503, { error: "restarting" });
      }
      return reply(200, {
        jsonrpc: "2.0",
        result: {
          networkMagic: state === "wrong-magic" ? 42 : MAGIC,
          startTime: new Date(startTimeMs).toISOString(),
          slotLength: { milliseconds: 1_000 },
          activeSlotsCoefficient: "1/20",
        },
      });
    })();
  });
  servers.push(server);
  await new Promise<void>((resolve) =>
    server.listen(0, "127.0.0.1", () => resolve()),
  );
  const { port } = server.address() as AddressInfo;
  return {
    url: `ws://127.0.0.1:${port.toString()}`,
    get startTimeMs() {
      return startTimeMs;
    },
    /** A chain reset under the same URL and magic: a later genesis start. */
    reset: () => {
      startTimeMs += 600_000;
    },
    counts,
  };
};

const waitHooks = () => {
  const logs: string[] = [];
  return {
    logs,
    wait: { log: (line: string) => logs.push(line), sleep: async () => {} },
  };
};

const options = (url: string, networkMagic = MAGIC) =>
  ({
    network: "Custom",
    route: { provider: "kupmios", ogmiosUrl: url },
    networkMagic,
  }) as const;

const reasons = (logs: readonly string[]) =>
  logs.map((line) => /reason=(\w+)/u.exec(line)?.[1]);

describe("committeeLucidSlotOptions on a transient Ogmios", () => {
  it("waits out a restart and a 300 s-old tip, then maps once", async () => {
    const ogmios = await startScriptedOgmios({
      magic: ["unavailable", "unavailable", "ok"],
      genesis: ["unavailable", "ok"],
      health: ["stale", "stale", "stale", "fresh"],
    });
    const { logs, wait } = waitHooks();

    await expect(
      committeeLucidSlotOptions(options(ogmios.url), wait),
    ).resolves.toEqual({
      slotConfig: {
        zeroTime: ogmios.startTimeMs,
        zeroSlot: 0,
        slotLength: 1_000,
      },
    });
    expect(ogmios.counts).toEqual({ magic: 3, genesis: 2, health: 4, tip: 1 });
    expect(reasons(logs)).toEqual([
      "ogmios_unreachable",
      "ogmios_unreachable",
      "ogmios_unreachable",
      "ogmios_tip_stale",
      "ogmios_tip_stale",
      "ogmios_tip_stale",
    ]);

    // The confirmed mapping is kept: a later client re-checks the magic and
    // the genesis, and consults no tip freshness.
    await expect(
      committeeLucidSlotOptions(options(ogmios.url), wait),
    ).resolves.toEqual({
      slotConfig: {
        zeroTime: ogmios.startTimeMs,
        zeroSlot: 0,
        slotLength: 1_000,
      },
    });
    expect(ogmios.counts).toEqual({ magic: 4, genesis: 3, health: 4, tip: 1 });
  });

  it("refuses an Ogmios swapped onto another network behind a kept mapping", async () => {
    const ogmios = await startScriptedOgmios({ magic: ["ok", "wrong-magic"] });
    const { wait } = waitHooks();
    await committeeLucidSlotOptions(options(ogmios.url), wait);

    await expect(
      committeeLucidSlotOptions(options(ogmios.url), wait),
    ).rejects.toThrow(/Ogmios network magic does not match/u);
    expect(ogmios.counts.magic).toBe(2);
  });

  it("maps a chain reset under the same URL afresh instead of keeping the old mapping", async () => {
    const ogmios = await startScriptedOgmios({});
    const { logs, wait } = waitHooks();
    const first = await committeeLucidSlotOptions(options(ogmios.url), wait);
    ogmios.reset();

    const second = await committeeLucidSlotOptions(options(ogmios.url), wait);

    expect(second.slotConfig?.zeroTime).toBe(ogmios.startTimeMs);
    expect(second.slotConfig?.zeroTime).not.toBe(first.slotConfig?.zeroTime);
    // The fresh mapping passed the submit-slot clock check again.
    expect(ogmios.counts.tip).toBe(2);
    expect(logs.some((line) => line.includes("mapping changed"))).toBe(true);
  });

  it("refuses a wrong network magic at once, with no retry", async () => {
    const ogmios = await startScriptedOgmios({ magic: ["wrong-magic", "ok"] });
    const { logs, wait } = waitHooks();
    const built = committeeLucidSlotOptions(options(ogmios.url), wait);

    await expect(built).rejects.toThrow(DaCommitteeCustomSlotMappingError);
    await expect(built).rejects.toThrow(
      /the network-magic check .* failed: Ogmios network magic does not match/u,
    );
    expect(ogmios.counts).toEqual({ magic: 1, genesis: 0, health: 0, tip: 0 });
    expect(logs).toEqual([]);
  });

  it("still refuses a wrong network magic once a restart clears", async () => {
    const ogmios = await startScriptedOgmios({
      magic: ["unavailable", "wrong-magic"],
    });
    const { logs, wait } = waitHooks();

    await expect(
      committeeLucidSlotOptions(options(ogmios.url), wait),
    ).rejects.toThrow(/Ogmios network magic does not match/u);
    expect(ogmios.counts.magic).toBe(2);
    expect(reasons(logs)).toEqual(["ogmios_unreachable"]);
  });

  it("refuses a malformed health answer at once, and keeps no mapping", async () => {
    const ogmios = await startScriptedOgmios({
      health: ["malformed", "fresh"],
    });
    const { logs, wait } = waitHooks();

    await expect(
      committeeLucidSlotOptions(options(ogmios.url), wait),
    ).rejects.toThrow(/missing connectionStatus/u);
    expect(ogmios.counts.health).toBe(1);
    expect(logs).toEqual([]);
    // A refusal is not kept: the next construction derives afresh.
    await expect(
      committeeLucidSlotOptions(options(ogmios.url), wait),
    ).resolves.toHaveProperty("slotConfig");
    expect(ogmios.counts.health).toBe(2);
  });
});

describe("assertOgmiosNetworkMagic", () => {
  const ok = (networkMagic: number) =>
    new Response(JSON.stringify({ result: { networkMagic } }), { status: 200 });

  it("classifies an unreachable Ogmios as transient, keeping its message", async () => {
    const refused = assertOgmiosNetworkMagic("http://127.0.0.1:1", MAGIC, () =>
      Promise.reject(new TypeError("fetch failed")),
    );
    await expect(refused).rejects.toThrow(
      "Ogmios network-magic preflight failed",
    );
    const error: unknown = await refused.catch((cause: unknown) => cause);
    expect(ogmiosSlotEvidenceUnavailableCause(error)?.reason).toBe(
      "ogmios_unreachable",
    );
  });

  it("keeps a client error, a JSON-RPC protocol error and a mismatch terminal", async () => {
    for (const answer of [
      new Response("{}", { status: 404 }),
      new Response(
        JSON.stringify({ error: { code: -32601, message: "no method" } }),
        { status: 200 },
      ),
      ok(42),
    ]) {
      const error: unknown = await assertOgmiosNetworkMagic(
        "http://127.0.0.1:1",
        MAGIC,
        async () => answer,
      ).catch((cause: unknown) => cause);
      expect(error).toBeInstanceOf(Error);
      expect(ogmiosSlotEvidenceUnavailableCause(error)).toBeUndefined();
    }
    await expect(
      assertOgmiosNetworkMagic("http://127.0.0.1:1", MAGIC, async () =>
        ok(MAGIC),
      ),
    ).resolves.toBeUndefined();
  });
});
