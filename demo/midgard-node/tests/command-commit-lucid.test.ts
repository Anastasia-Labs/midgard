/**
 * The commit worker's role factory builds its Lucid in the worker run's
 * scope, so the access stays open until the run ends; a Lucid provided out
 * of its own scope is the control.
 */
import * as LE from "@lucid-evolution/lucid";
import { Effect, Layer } from "effect";
import { describe, expect, it } from "vitest";

import { openL1Access } from "../src/l1-access.js";
import { l1SlotNow } from "../src/l1-heads.js";
import { NodeConfig } from "../src/services/config.js";
import type { NodeConfigDep } from "../src/services/config.node-config-dep.js";
import { L1Adapter } from "../src/services/l1-adapter.js";
import { Lucid } from "../src/services/lucid.js";
import { scopedCommitLucidFactory } from "../src/workers/commit-block-header.pending-user-event-counts-up-to.js";

const seeds = [0, 1, 2].map(() => LE.generateSeedPhrase());
const address = (seed: string) =>
  LE.walletFromSeed(seed, { network: "Custom" }).address;

const config = {
  NETWORK: "Custom",
  L1_OPERATOR_SEED_PHRASE: seeds[0],
  L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX: seeds[1],
  L1_REFERENCE_SCRIPT_SEED_PHRASE: seeds[2],
  L1_REFERENCE_SCRIPT_ADDRESS: address(seeds[2]!),
  L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS: address(seeds[2]!),
} as unknown as NodeConfigDep;

/** A role adapter whose access counts its closes. */
const closingAdapter = (closes: { count: number }) =>
  Layer.succeed(L1Adapter, {
    role: "follower",
    open: () =>
      Effect.acquireRelease(
        Effect.sync(() => {
          const point = { slot: 5, id: "00".repeat(32) };
          return openL1Access({
            kind: "follower",
            provider: new LE.Emulator([]),
            endpoint: "/ipc/node.socket",
            slotConfig: async () => LE.SLOT_CONFIG_NETWORK.Preview,
            tipSlot: async () => {
              if (closes.count > 0) throw new Error("the access is closed");
              return 5;
            },
            viewPoint: async () => point,
            synchronizedViewPoint: async () => point,
            submitSlotSnapshot: async () => {
              throw new Error("no submit slot in this test");
            },
            close: async () => {
              closes.count += 1;
            },
          });
        }),
        (access) => Effect.promise(access.close),
      ),
  });

const lucidOver = (adapter: Layer.Layer<L1Adapter>) =>
  Lucid.DefaultWithoutDependencies.pipe(
    Layer.provide(adapter),
    Layer.provide(Layer.succeed(NodeConfig, config)),
  );

describe("the commit worker's Lucid", () => {
  it("the commit worker's factory keeps the follower access open for the run", async () => {
    const closes = { count: 0 };
    const slot = await Effect.runPromise(
      Effect.gen(function* () {
        const factory = yield* scopedCommitLucidFactory(
          lucidOver(closingAdapter(closes)),
        );
        const lucid = yield* factory();
        return yield* l1SlotNow(lucid.api);
      }).pipe(Effect.scoped),
    );
    expect(slot).toBe(5);
    expect(closes.count).toBe(1);
  });

  it("a Lucid provided out of its own scope reads through a closed access (the control)", async () => {
    const closes = { count: 0 };
    const lucid = await Effect.runPromise(
      Effect.provide(Lucid, lucidOver(closingAdapter(closes))),
    );
    expect(closes.count).toBe(1);
    const slot = await Effect.runPromise(Effect.either(l1SlotNow(lucid.api)));
    expect(slot._tag).toBe("Left");
  });
});
