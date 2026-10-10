import { mkdtemp, realpath, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import {
  type ChainPoint,
  type ChainSyncEvent,
  L1NodeTransport,
} from "@al-ft/l1-node-transport";
import { writeFakeSidecar } from "@al-ft/l1-node-transport/testing/fake-sidecar";
import { afterEach, beforeEach, describe, expect, it } from "vitest";

import { transportPoint } from "../../src/index.js";
import {
  buildForkSteps,
  forkCorpus,
  type ForkStep,
  runForkScenario,
  SIM_ORIGIN,
} from "../../src/testing/index.js";
import { FIXTURE_PROJECTION, openSqlite, SIM_K } from "../support/fork-sim.js";

const handlerModule = fileURLToPath(
  new URL("../fixtures/fork-sim-handler.mjs", import.meta.url),
);

const fakePoint = (point: ChainPoint): unknown =>
  point.kind === "origin"
    ? "origin"
    : { slot: Number(point.slot), hash: point.hash };

const wireEvent = (event: ChainSyncEvent): unknown => ({
  kind: event.kind,
  point: fakePoint(event.point),
  tip: {
    point: fakePoint(event.tip.point),
    blockNo: Number(event.tip.blockNo),
  },
  ...(event.kind === "roll_forward"
    ? {
        blockNo: Number(event.blockNo),
        blockType: event.blockType,
        prevHash: event.prevHash,
        block: Buffer.from(event.block).toString("hex"),
      }
    : {}),
});

let directory: string;
let transport: L1NodeTransport | undefined;
beforeEach(async () => {
  directory = await realpath(await mkdtemp(join(tmpdir(), "fork-sim-")));
});
afterEach(async () => {
  await transport?.close();
  transport = undefined;
  await rm(directory, { recursive: true, force: true });
});

// The scenario's events go through the real frame client and a fake
// sidecar, so the simulator's guarantees hold for what the transport
// actually delivers (bytes, points, sequence numbers, credit and acks).
describe("fork simulator through the L1 node transport", () => {
  it("delivers every shape's events intact and the store follows them", async () => {
    const entry = forkCorpus(SIM_K).find(
      (e) => e.name === "every shape in sequence",
    );
    if (entry === undefined) throw new Error("corpus entry missing");
    const origin = transportPoint(SIM_ORIGIN.point);
    const outcome = await runForkScenario(entry.scenario, {
      open: openSqlite,
      k: SIM_K,
      projections: [FIXTURE_PROJECTION],
      source: async (steps: readonly ForkStep[]) => {
        const binaryPath = await writeFakeSidecar({
          path: join(directory, "fake-sidecar"),
          handlerModule,
          options: {
            origin: { slot: Number(origin.slot), hash: origin.hash },
            events: steps.map((step) => wireEvent(step.event)),
          },
        });
        transport = new L1NodeTransport({
          binaryPath,
          socketPath: join(directory, "node.socket"),
          networkMagic: 42,
          requestTimeoutMs: 10_000,
          restartDelayMs: { initial: 50, max: 200 },
        });
        await transport.whenReady(10_000);
        const stream = transport.openChainSync({
          points: [origin],
          credit: 3,
          resume: false,
        });
        return {
          next: () => stream.next(),
          ack: (seq) => stream.ack(seq),
        };
      },
    });
    expect(outcome).toMatchObject({ ok: true });
    const { steps } = buildForkSteps(entry.scenario, [FIXTURE_PROJECTION]);
    expect(outcome.stats.events).toBe(steps.length);
    expect(outcome.stats.rollbacks).toBe(entry.scenario.episodes.length);
  });
});
