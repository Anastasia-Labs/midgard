import type { L1Origin } from "@al-ft/midgard-core/l1-origin";
import { it } from "@effect/vitest";
import { Effect } from "effect";
import { describe, expect } from "vitest";

import { nodeFollowerPlanFor } from "./helpers/node-follower-plan.js";

const ONE_SHOT = { txHash: Buffer.alloc(32, 0x0e), index: 0 };
/** The deployment's recorded origin T, and an operator override after it. */
const T: L1Origin = { slot: 4_200, blockHash: "ab".repeat(32) };
const OVERRIDE: L1Origin = { slot: 4_300, blockHash: "cd".repeat(32) };

const planFor = (l1Origin: L1Origin | null, recordedOrigin?: L1Origin) =>
  nodeFollowerPlanFor({ l1Origin, recordedOrigin, hubOracleOneShot: ONE_SHOT });

const point = (origin: L1Origin) => ({
  slot: origin.slot,
  hash: Buffer.from(origin.blockHash, "hex"),
});

describe("the origin the node's follower starts from", () => {
  it.effect(
    "C-a: with no L1_ORIGIN, follows from the manifest's recorded origin and checks the store against it",
    () =>
      Effect.gen(function* () {
        expect(yield* planFor(null, T)).toMatchObject({
          kind: "run",
          origin: { origin: point(T), recordedOrigin: point(T) },
        });
      }),
  );

  it.effect(
    "C-b: an L1_ORIGIN override is where the follower starts, still checked against the manifest's recorded origin",
    () =>
      Effect.gen(function* () {
        expect(yield* planFor(OVERRIDE, T)).toMatchObject({
          kind: "run",
          origin: { origin: point(OVERRIDE), recordedOrigin: point(T) },
        });
      }),
  );

  it.effect(
    "C-b: a manifest without l1Origin leaves L1_ORIGIN as the origin, with nothing to check it against",
    () =>
      Effect.gen(function* () {
        const plan = yield* planFor(OVERRIDE);
        expect(plan).toMatchObject({
          kind: "run",
          origin: { origin: point(OVERRIDE) },
        });
        expect(plan.kind === "run" && Object.keys(plan.origin)).toEqual([
          "origin",
          "hubOracleOneShot",
        ]);
      }),
  );

  it.effect(
    "C-b: neither L1_ORIGIN nor a recorded origin leaves the follower unconfigured",
    () =>
      Effect.gen(function* () {
        expect(yield* planFor(null)).toEqual({
          kind: "unconfigured",
          detail:
            "L1_ORIGIN is not set and the deployment manifest records no l1Origin",
        });
      }),
  );
});
