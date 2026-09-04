import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { fetchProtocolDeploymentStatusWithStartupRetry } from "../src/commands/listen-startup.js";
import type { ProtocolDeploymentStatus } from "../src/transactions/initialization.js";

const completeStatus = {
  hubOracleWitness: null,
  stateQueueTopology: {
    initialized: true,
    healthy: true,
    reason: undefined,
    policyUtxoCount: 1,
    parsedNodeCount: 1,
    invalidNodeCount: 0,
    rootCount: 1,
    tailCount: 1,
  },
  schedulerInitialized: true,
  registeredOperatorsInitialized: true,
  activeOperatorsInitialized: true,
  retiredOperatorsInitialized: true,
  fraudProofCatalogueInitialized: true,
  phasMembershipRewardAddress: "stake_test1uphas",
  phasMembershipScriptHash: "00".repeat(28),
  complete: true,
  empty: false,
  missingComponents: [],
} as unknown as ProtocolDeploymentStatus;

describe("fetchProtocolDeploymentStatusWithStartupRetry", () => {
  it("retries transient provider query failures before returning status", async () => {
    let attempts = 0;

    const status = await Effect.runPromise(
      fetchProtocolDeploymentStatusWithStartupRetry(
        () => {
          attempts += 1;
          if (attempts < 3) {
            return Effect.fail(
              new SDK.LucidError({
                message: "Failed to fetch hub-oracle witness UTxO(s)",
                cause: "kupo warming up",
              }),
            );
          }
          return Effect.succeed(completeStatus);
        },
        {
          maxAttempts: 5,
          retryDelayMs: 0,
        },
      ),
    );

    expect(status).toBe(completeStatus);
    expect(attempts).toBe(3);
  });

  it("does not retry deterministic protocol invariant failures", async () => {
    let attempts = 0;

    await expect(
      Effect.runPromise(
        fetchProtocolDeploymentStatusWithStartupRetry(
          () => {
            attempts += 1;
            return Effect.fail(
              new SDK.LucidError({
                message: "Expected at most one hub-oracle witness UTxO",
                cause: "duplicate witness tokens",
              }),
            );
          },
          {
            maxAttempts: 5,
            retryDelayMs: 0,
          },
        ),
      ),
    ).rejects.toMatchObject({
      message: "Expected at most one hub-oracle witness UTxO",
    });
    expect(attempts).toBe(1);
  });
});
