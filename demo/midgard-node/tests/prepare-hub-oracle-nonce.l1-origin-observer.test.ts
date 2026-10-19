import { readFileSync } from "node:fs";
import { mkdtemp, realpath, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { L1NodeTransport } from "@al-ft/l1-node-transport";
import { writeFakeSidecar } from "@al-ft/l1-node-transport/testing/fake-sidecar";
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { Effect } from "effect";
import { afterEach, describe, expect, it } from "vitest";

import {
  l1NodeChainObserver,
  l1NodeOriginSource,
  ORIGIN_SETTLE_ATTEMPTS,
  settledL1Origin,
} from "../src/commands/prepare-hub-oracle-nonce.l1-origin.js";

const handlerModule = fileURLToPath(
  new URL("./fixtures/origin-chain-handler.mjs", import.meta.url),
);

const T = { slot: 100, hash: "a0".repeat(32) };
const TIP = { ...T, blockNo: 40 };
const block = (n: number) => ({
  slot: 100 + n,
  hash: n.toString(16).padStart(2, "0").repeat(32),
});
const forward = (n: number) => ({ forward: block(n) });

type Script = { notFound: true } | { events: readonly unknown[] };

const opened: L1NodeTransport[] = [];
let directory: string | undefined;
afterEach(async () => {
  await Promise.all(opened.splice(0).map((transport) => transport.close()));
  if (directory !== undefined)
    await rm(directory, { recursive: true, force: true });
  directory = undefined;
});

/** A local node over the fake sidecar, and the opens it served from a block. */
const node = async (
  settles: readonly Script[],
  tip: "origin" | typeof TIP = TIP,
) => {
  directory ??= await realpath(
    await mkdtemp(join(tmpdir(), "midgard-nonce-origin-node-")),
  );
  const journal = join(directory, `journal-${opened.length.toString()}`);
  const transport = new L1NodeTransport({
    binaryPath: await writeFakeSidecar({
      path: join(directory, `sidecar-${opened.length.toString()}`),
      handlerModule,
      options: { tip, settles, journal },
    }),
    socketPath: join(directory, "node.socket"),
    networkMagic: 42,
    requestTimeoutMs: 10_000,
  });
  opened.push(transport);
  const opens = () => {
    try {
      return readFileSync(journal, "utf8").split("\n").filter(Boolean);
    } catch {
      return [];
    }
  };
  return { transport, opens };
};

const origin = { slot: T.slot, blockHash: T.hash };

describe("the local node's chain, as the origin record reads it", () => {
  it("reads the node's tip, and no tip while the chain holds no block", async () => {
    const live = await node([{ events: [] }]);
    await expect(l1NodeChainObserver(live.transport).tip()).resolves.toEqual(
      origin,
    );
    const empty = await node([{ events: [] }], "origin");
    await expect(
      l1NodeChainObserver(empty.transport).tip(),
    ).resolves.toBeUndefined();
  });

  it("settles once depth blocks extend the point, counting from the node's rollback to it", async () => {
    const { transport, opens } = await node([
      { events: [{ backward: T }, forward(1), forward(2), forward(3)] },
    ]);
    await expect(
      l1NodeChainObserver(transport).settle(origin, 3),
    ).resolves.toBe("settled");
    expect(opens()).toEqual([`open ${T.slot.toString()}.${T.hash}`]);
  });

  it("keeps the blocks a rollback above the point leaves, and counts on from them", async () => {
    const { transport } = await node([
      {
        events: [
          forward(1),
          forward(2),
          { backward: block(1) },
          forward(3),
          forward(4),
        ],
      },
    ]);
    await expect(
      l1NodeChainObserver(transport).settle(origin, 3),
    ).resolves.toBe("settled");
  });

  it("reports the point rolled back when the chain rolls back past it", async () => {
    const { transport } = await node([
      { events: [forward(1), { backward: "origin" }] },
    ]);
    await expect(
      l1NodeChainObserver(transport).settle(origin, 3),
    ).resolves.toBe("rolled_back");
  });

  it("reports the point rolled back when the node's chain no longer holds it", async () => {
    const { transport } = await node([{ notFound: true }]);
    await expect(
      l1NodeChainObserver(transport).settle(origin, 3),
    ).resolves.toBe("rolled_back");
  });

  it("D-f: over the node, a tip that left the chain is read again and settles on the next read", async () => {
    const { transport, opens } = await node([
      { notFound: true },
      { events: [forward(1), forward(2)] },
    ]);
    await expect(
      Effect.runPromise(
        settledL1Origin({
          observer: l1NodeChainObserver(transport),
          depth: 2,
        }),
      ),
    ).resolves.toEqual(origin);
    expect(opens()).toHaveLength(2);
  });

  it("D-f: over the node, a tip that never settles is refused after the bound", async () => {
    const { transport, opens } = await node([{ notFound: true }]);
    await expect(
      Effect.runPromise(
        settledL1Origin({
          observer: l1NodeChainObserver(transport),
          depth: 2,
        }),
      ),
    ).rejects.toThrow("no hub-oracle nonce was signed");
    expect(opens()).toHaveLength(ORIGIN_SETTLE_ATTEMPTS);
  });

  it("settles under the deployment's confirmation depth", async () => {
    const { transport } = await node([{ events: [] }]);
    expect(l1NodeOriginSource(transport).depth).toBe(
      DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth,
    );
  });
});
