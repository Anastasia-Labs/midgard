import * as SDK from "@al-ft/midgard-sdk";
import { KupmiosError } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { fetchProtocolDeploymentStatusWithStartupRetry } from "../src/commands/listen-startup.js";
import {
  PROTOCOL_DEPLOYMENT_STATUS_UNAVAILABLE,
  StartupWaitingReporter,
} from "../src/services/startup-waiting.js";
import type { ProtocolDeploymentStatus } from "../src/transactions/initialization.js";

const completeStatus = {
  hubOracleWitness: null,
  stateQueueInitialized: true,
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
        { retryDelayMs: 0 },
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
          { retryDelayMs: 0 },
        ),
      ),
    ).rejects.toMatchObject({
      message: "Expected at most one hub-oracle witness UTxO",
    });
    expect(attempts).toBe(1);
  });
});

describe("fetchProtocolDeploymentStatusWithStartupRetry honours typed retryability", () => {
  /** Fails with `error` `failures` times, then returns the status. */
  const attemptsUntilOutcome = async (
    error: SDK.LucidError,
    failures = Number.POSITIVE_INFINITY,
  ) => {
    let attempts = 0;
    const reported: [string, readonly string[]][] = [];
    const outcome = await Effect.runPromise(
      Effect.either(
        fetchProtocolDeploymentStatusWithStartupRetry(
          () => {
            attempts += 1;
            return attempts <= failures
              ? Effect.fail(error)
              : Effect.succeed(completeStatus);
          },
          { retryDelayMs: 0 },
        ),
      ).pipe(
        Effect.locally(StartupWaitingReporter, (key, reasons) =>
          Effect.sync(() => {
            reported.push([key, reasons]);
          }),
        ),
      ),
    );
    return { attempts, outcome, reported };
  };

  it("does not retry a read wrapper around a Kupo refusal", async () => {
    const { attempts, outcome, reported } = await attemptsUntilOutcome(
      new SDK.LucidError({
        message: "Failed to fetch hub-oracle witness UTxO(s)",
        cause: new KupmiosError({
          protocol: "kupo",
          operation: "getUtxos",
          status: 400,
        }),
      }),
    );
    expect(outcome._tag).toBe("Left");
    expect(attempts).toBe(1);
    expect(reported).toEqual([]);
  });

  it("retries any wrapper around a retryable Kupo answer with no bound, waiting under its reason, until the read answers", async () => {
    const { attempts, outcome, reported } = await attemptsUntilOutcome(
      new SDK.LucidError({
        message: "Could not read the state-queue topology",
        cause: new KupmiosError({
          protocol: "kupo",
          operation: "getUtxos",
          status: 503,
        }),
      }),
      200,
    );
    expect(outcome).toMatchObject({ _tag: "Right", right: completeStatus });
    expect(attempts).toBe(201);
    expect(reported).toHaveLength(201);
    expect(
      new Set(reported.slice(0, -1).map(([, reasons]) => reasons[0])),
    ).toEqual(new Set([PROTOCOL_DEPLOYMENT_STATUS_UNAVAILABLE]));
    expect(reported.at(-1)).toEqual(["protocol_deployment_status", []]);
  });
});
