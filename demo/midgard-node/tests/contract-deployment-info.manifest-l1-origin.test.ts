import type { L1Origin } from "@al-ft/midgard-core/l1-origin";
import type { FindOriginResult } from "@al-ft/midgard-l1-follower";
import { it } from "@effect/vitest";
import { Effect, Exit } from "effect";
import { describe, expect } from "vitest";

import {
  buildDeploymentManifest,
  parseDeploymentManifest,
} from "../src/commands/contract-deployment-info.js";
import { provenManifestL1Origin } from "../src/commands/contract-deployment-info.manifest-l1-origin.js";
import { AlwaysSucceedsContract } from "../src/services/always-succeeds.js";
import {
  buildFinalizedContractDeploymentInfo,
  ONE_SHOT_TX_HASH,
  TEST_FINALIZED_MANIFEST_BUILD_CONTEXT,
  testReferenceScriptAuthPolicy,
} from "./contract-deployment-info.validator-hashes-by-path.js";
import { withRealEventHistoryForTest } from "./helpers/event-history.js";

const T: L1Origin = { slot: 4_200, blockHash: "ab".repeat(32) };
const LATER: L1Origin = { slot: 4_300, blockHash: "cd".repeat(32) };
const NOW = new Date("2026-10-18T00:00:00.000Z");

const deploymentInfo = Effect.gen(function* () {
  const contracts = withRealEventHistoryForTest(yield* AlwaysSucceedsContract, {
    txHash: ONE_SHOT_TX_HASH,
    outputIndex: 0,
  });
  return yield* buildFinalizedContractDeploymentInfo(
    contracts,
    testReferenceScriptAuthPolicy(
      contracts.referenceScriptAuth.policyId,
      contracts.referenceScriptAuth.mintingScriptCBOR,
    ),
  );
}).pipe(Effect.provide(AlwaysSucceedsContract.Default));

describe("the L1 origin a finalized manifest records", () => {
  it.effect(
    "M-a: the builder records the deployment's L1 origin, and the origin is part of the manifest id",
    () =>
      Effect.gen(function* () {
        const info = yield* deploymentInfo;
        const context = { ...TEST_FINALIZED_MANIFEST_BUILD_CONTEXT, now: NOW };
        const without = buildDeploymentManifest(info, context);
        const recorded = buildDeploymentManifest(info, {
          ...context,
          l1Origin: T,
        });
        expect(Object.keys(without)).not.toContain("l1Origin");
        expect(recorded.l1Origin).toEqual(T);
        expect(parseDeploymentManifest(recorded).l1Origin).toEqual(T);
        expect(recorded.manifestId).not.toBe(without.manifestId);
      }),
  );

  it.effect(
    "M-a: a rewrite keeps the existing manifest's origin, or its absence, so the manifest id never changes",
    () =>
      Effect.gen(function* () {
        const info = yield* deploymentInfo;
        const context = { ...TEST_FINALIZED_MANIFEST_BUILD_CONTEXT, now: NOW };
        const recorded = buildDeploymentManifest(info, {
          ...context,
          l1Origin: T,
        });
        const rewritten = buildDeploymentManifest(info, {
          ...context,
          existingManifest: recorded,
          l1Origin: LATER,
        });
        expect(rewritten.l1Origin).toEqual(T);
        expect(rewritten.manifestId).toBe(recorded.manifestId);

        const legacy = buildDeploymentManifest(info, context);
        const legacyRewritten = buildDeploymentManifest(info, {
          ...context,
          existingManifest: legacy,
          l1Origin: T,
        });
        expect(Object.keys(legacyRewritten)).not.toContain("l1Origin");
        expect(legacyRewritten.manifestId).toBe(legacy.manifestId);
      }),
  );
});

describe("the proof of a first finalized manifest's L1 origin", () => {
  const NONCE_TX = "EF".repeat(32);
  const found: FindOriginResult = {
    kind: "found",
    origin: { slot: 4_250, blockHash: "12".repeat(32) },
    nonceBlock: { slot: 4_260, blockHash: "34".repeat(32), height: 9 },
    txIndex: 0,
    depth: 12,
  };

  /** A prover answering `result`, and the calls it served. */
  const prover = (result: FindOriginResult) => {
    const calls: Array<readonly [L1Origin, string]> = [];
    return {
      calls,
      prove: (origin: L1Origin, nonceTxHash: string) => {
        calls.push([origin, nonceTxHash]);
        return Promise.resolve(result);
      },
    };
  };

  const failure = (exit: Exit.Exit<L1Origin, Error>): string => {
    expect(Exit.isFailure(exit)).toBe(true);
    return Exit.isFailure(exit) ? String(exit.cause) : "";
  };

  it.effect(
    "M-b: proves the recorded origin on the local node's chain, from the origin to the nonce's block",
    () =>
      Effect.gen(function* () {
        const node = prover(found);
        expect(
          yield* provenManifestL1Origin({
            recordedOrigin: T,
            nonceTxHash: NONCE_TX,
            prover: node.prove,
          }),
        ).toEqual(T);
        expect(node.calls).toEqual([[T, NONCE_TX.toLowerCase()]]);
      }),
  );

  it.effect.each([
    {
      name: "is not on the local node's chain",
      result: { kind: "from_not_on_chain" },
      reason: /not on the local node's chain/u,
    },
    {
      name: "does not lie before the nonce's block",
      result: { kind: "not_found", scannedTo: LATER },
      reason: /does not lie before the nonce's block/u,
    },
    {
      name: "cannot precede a nonce in the chain's first block",
      result: { kind: "no_preceding_point" },
      reason: /chain's first block/u,
    },
  ] as const)(
    "M-b: refuses an origin that $name, with no manifest written",
    ({ result, reason }) =>
      Effect.gen(function* () {
        const exit = yield* Effect.exit(
          provenManifestL1Origin({
            recordedOrigin: T,
            nonceTxHash: NONCE_TX,
            prover: prover(result).prove,
          }),
        );
        expect(failure(exit)).toMatch(reason);
      }),
  );

  it.effect(
    "M-b: refuses when the run state records no origin, without reading the chain",
    () =>
      Effect.gen(function* () {
        const node = prover(found);
        const exit = yield* Effect.exit(
          provenManifestL1Origin({
            recordedOrigin: undefined,
            nonceTxHash: NONCE_TX,
            prover: node.prove,
          }),
        );
        expect(failure(exit)).toMatch(/records no L1 origin/u);
        expect(node.calls).toEqual([]);
      }),
  );

  it.effect("M-b: refuses when no local node is configured", () =>
    Effect.gen(function* () {
      const exit = yield* Effect.exit(
        provenManifestL1Origin({
          recordedOrigin: T,
          nonceTxHash: NONCE_TX,
          prover: undefined,
        }),
      );
      expect(failure(exit)).toMatch(/no local node is configured/u);
    }),
  );
});
