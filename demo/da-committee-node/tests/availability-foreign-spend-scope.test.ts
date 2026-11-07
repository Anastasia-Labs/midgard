import { rmSync } from "node:fs";

import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it } from "vitest";

import { availabilityResponderOperations } from "../src/availability/factory.js";
import type { CommitteeAvailabilityReads } from "../src/l1/follower/availability-reads.js";
import {
  dirs,
  journals,
  scene,
} from "./helpers/availability-sdk-read-scope.js";
import { followerBoundary } from "./helpers/follower-boundary.js";

afterEach(() => {
  journals.splice(0).forEach((journal) => journal.close());
  dirs
    .splice(0)
    .forEach((dir) => rmSync(dir, { recursive: true, force: true }));
});

const fixture = () => {
  const s = scene();
  const intent = SDK.inspectDaAvailabilitySignedIntent({
    deploymentIdentity: s.context.deploymentIdentity,
    actor: s.context.actor,
    headerHash: s.operation.headerHash,
    action: s.operation.action,
    signedCbor: s.tx.toTransaction().to_cbor_hex(),
  });
  const consulted: string[] = [];
  const reads: CommitteeAvailabilityReads = {
    readBoundary: async () => {
      consulted.push("boundary");
      return followerBoundary({
        slot: 1000,
        blockHash: "ab".repeat(32),
        blockNo: 100,
      });
    },
    viewValid: async () => true,
    canonicalPoint: async () => null,
    submissionPoint: async () => null,
    landingPoint: async () => null,
    failedLanding: async () => undefined,
    intentPins: { add: async () => {}, bind: () => {} },
    foreignSpend: {
      fetchSpend: async () => {
        consulted.push("spend");
        return undefined;
      },
      fetchAncestor: async () => {
        throw new Error("No spend to resolve");
      },
      readTransaction: async () => {
        throw new Error("No spend to resolve");
      },
    },
  };
  const operations = availabilityResponderOperations({
    lucid: {
      transactionStatus: async (txHash: string) => {
        consulted.push("status");
        return { txHash, status: "not_found" };
      },
      utxosByOutRef: async () => {
        consulted.push("inputs");
        return [];
      },
    } as unknown as LucidEvolution,
    reads,
    assertSourceHealthy: async () => {},
    context: s.context,
  });
  return { intent, operations, consulted };
};

describe("committee observation on the follower's facts", () => {
  it("reads the boundary, status, inputs and the missing input's spend from the follower under the observer's scope", async () => {
    const f = fixture();
    const scope = SDK.createDaAvailabilityReadScope({ attemptTimeoutMs: 1000 });
    try {
      const observation = await f.operations.context.observe(f.intent, scope);
      expect(observation).not.toHaveProperty("foreignSpends");
      expect(f.consulted).toEqual(
        expect.arrayContaining(["boundary", "status", "inputs", "spend"]),
      );
    } finally {
      scope.close();
    }
  });

  it("reads nothing under a closed scope", async () => {
    const f = fixture();
    const scope = SDK.createDaAvailabilityReadScope({ attemptTimeoutMs: 1000 });
    scope.close();
    await expect(
      f.operations.context.observe(f.intent, scope),
    ).rejects.toThrow();
    expect(f.consulted).toEqual([]);
  });
});
