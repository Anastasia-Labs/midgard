import { rmSync } from "node:fs";

import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it } from "vitest";

import {
  availabilityForeignSpendReaders,
  availabilityResponderOperations,
} from "../src/availability/factory.js";
import {
  dirs,
  journals,
  scene,
} from "./helpers/availability-sdk-read-scope.js";

afterEach(() => {
  journals.splice(0).forEach((journal) => journal.close());
  dirs
    .splice(0)
    .forEach((dir) => rmSync(dir, { recursive: true, force: true }));
});

describe("committee foreign-spend scope ownership", () => {
  it("forwards the actual observer scope through status, inputs and foreign-spend readers", async () => {
    const s = scene();
    const intent = SDK.inspectDaAvailabilitySignedIntent({
      deploymentIdentity: s.context.deploymentIdentity,
      actor: s.context.actor,
      headerHash: s.operation.headerHash,
      action: s.operation.action,
      signedCbor: s.tx.toTransaction().to_cbor_hex(),
    });
    const scope = SDK.createDaAvailabilityReadScope({ attemptTimeoutMs: 1000 });
    const point = {
      network: "Custom",
      slot: 1000,
      blockHash: "ab".repeat(32),
      providerSource: "fixture",
      observedAt: "fixture",
    };
    let passed: SDK.DaAvailabilityReadScope | undefined;
    let statusScope: SDK.DaAvailabilityReadScope | undefined;
    let inputScope: SDK.DaAvailabilityReadScope | undefined;
    const operations = availabilityResponderOperations({
      lucid: {
        transactionStatus: async () => {
          throw new Error("Unscoped status fallback was used");
        },
        utxosByOutRef: async () => {
          throw new Error("Unscoped inputs fallback was used");
        },
      } as unknown as LucidEvolution,
      readers: {
        currentPoint: async () => point,
        currentCursor: async () => ({
          sequence: 1,
          rollbackGeneration: 0,
          point,
        }),
        tipBlockNo: async () => 100,
        readTransactionStatus: async (txHash, readScope) => {
          statusScope = readScope;
          return { txHash, status: "not_found" };
        },
        readInputs: async (_refs, readScope) => {
          inputScope = readScope;
          return [];
        },
        resolveInclusion: async () => ({}),
        foreignSpend: {
          fetchSpend: async (_ref, readScope) => {
            passed = readScope;
            return undefined;
          },
          fetchAncestor: async () => {
            throw new Error("No spend to resolve");
          },
          readTransaction: async () => {
            throw new Error("No spend to resolve");
          },
        },
      },
      assertSourceHealthy: async () => {},
      context: s.context,
    });
    try {
      await operations.context.observe(intent, scope);
      expect(passed).toBe(scope);
      expect(statusScope).toBe(scope);
      expect(inputScope).toBe(scope);
    } finally {
      scope.close();
    }
  });

  it("cancels the owning Kupo spend request when the shared scope expires", async () => {
    const scope = SDK.createDaAvailabilityReadScope({ attemptTimeoutMs: 30 });
    let aborted = false;
    const readers = availabilityForeignSpendReaders({
      kupoUrl: "http://fixture",
      ogmiosUrl: "ws://fixture",
      sourceReadLimits: {
        requestRefusalMs: 1000,
        httpResponseBytes: 10000,
        webSocketMessageBytes: 10000,
        rawUtxos: 10,
      },
      fetchImpl: async (_url, init) =>
        new Promise((_resolve, reject) => {
          const signal = init?.signal;
          if (!signal)
            throw new Error("Scoped spend read omitted its owning signal");
          signal.addEventListener(
            "abort",
            () => {
              aborted = true;
              reject(signal.reason);
            },
            { once: true },
          );
        }),
    });
    try {
      await expect(
        readers.fetchSpend({ txHash: "ab".repeat(32), outputIndex: 0 }, scope),
      ).rejects.toThrow();
      expect(aborted).toBe(true);
    } finally {
      scope.close();
    }
  });
});
