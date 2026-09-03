import { MIDGARD_CONSENSUS_PROFILE_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
import { runDaZstdStartupSelfTest } from "@al-ft/midgard-core/da-compression";
import { makeDeploymentMarkerV1 } from "@al-ft/midgard-core/deployment-manifest-identity-v1";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { readDaHardeningConfig } from "../src/da/hardening-config.js";
import {
  assertDaDeploymentIdentityCompatible,
  assertDaThresholdCompatible,
  runDaIdentityGatedStartupSequence,
} from "../src/da/startup.js";

describe("DA hardening startup and configuration", () => {
  it("runs the required native zstd startup round-trip", async () => {
    await expect(runDaZstdStartupSelfTest()).resolves.toBeUndefined();
  });

  it("fails closed when the transport threshold is below on-chain quorum", () => {
    expect(() => assertDaThresholdCompatible(1, 2n)).toThrow(
      /transport threshold is lower/,
    );
    expect(() => assertDaThresholdCompatible(2, 2n)).not.toThrow();
    expect(() => assertDaThresholdCompatible(3, 2n)).not.toThrow();
  });

  it("binds DA startup to the exact selected deployment manifest", () => {
    const manifestId = "ab".repeat(32);
    expect(() =>
      assertDaDeploymentIdentityCompatible(manifestId, {
        kind: "manifest",
        manifestId,
        deploymentMarker: makeDeploymentMarkerV1(manifestId),
        path: "/deployment.json",
        consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
      }),
    ).not.toThrow();
    expect(() =>
      assertDaDeploymentIdentityCompatible("cd".repeat(32), {
        kind: "manifest",
        manifestId,
        deploymentMarker: makeDeploymentMarkerV1(manifestId),
        path: "/deployment.json",
        consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
      }),
    ).toThrow(/identities do not match/);
    expect(() =>
      assertDaDeploymentIdentityCompatible(manifestId, {
        kind: "derived",
        consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
      }),
    ).toThrow(/requires a verified deployment-manifest contract source/);
  });

  it("rejects deployment mismatch before any later startup effect", async () => {
    const calls: string[] = [];
    const manifestId = "ab".repeat(32);
    const localPreflight = Effect.sync(() => {
      assertDaDeploymentIdentityCompatible("cd".repeat(32), {
        kind: "manifest",
        manifestId,
        deploymentMarker: makeDeploymentMarkerV1(manifestId),
        path: "/deployment.json",
        consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
      });
      throw new Error("deployment mismatch unexpectedly accepted");
    });

    await expect(
      Effect.runPromise(
        runDaIdentityGatedStartupSequence({
          localPreflight,
          initializeDatabase: Effect.sync(() => calls.push("database")).pipe(
            Effect.asVoid,
          ),
          initializeProtocol: Effect.sync(() =>
            calls.push("protocol-status-init-and-reference-scripts"),
          ).pipe(Effect.asVoid),
          providerAssertions: () =>
            Effect.sync(() =>
              calls.push("threshold-and-capability-probes"),
            ).pipe(Effect.asVoid),
        }),
      ),
    ).rejects.toThrow(/identities do not match/);
    expect(calls).toEqual([]);
  });

  it("parses strict rollout and retry controls from one canonical source", () => {
    expect(
      readDaHardeningConfig({
        MIDGARD_DA_PAYLOAD_ENVELOPE: "identity",
        MIDGARD_DA_ZSTD_LEVEL: "7",
        MIDGARD_DA_PUBLISH_CONCURRENCY: "4",
        MIDGARD_DA_PUBLISH_RECONCILE_INTERVAL_MS: "50",
        MIDGARD_DA_PUBLISH_RETRY_BACKOFF_MS: "10",
        MIDGARD_DA_PUBLISH_RETRY_BACKOFF_MAX_MS: "80",
      }),
    ).toEqual({
      envelopeMode: "identity",
      zstdLevel: 7,
      publishConcurrency: 4,
      reconcileIntervalMs: 50,
      retryBackoffMs: 10,
      retryBackoffMaxMs: 80,
    });
    expect(() =>
      readDaHardeningConfig({ MIDGARD_DA_PAYLOAD_ENVELOPE: "gzip" }),
    ).toThrow(/identity or zstd/);
    expect(() =>
      readDaHardeningConfig({
        MIDGARD_DA_PUBLISH_RETRY_BACKOFF_MS: "100",
        MIDGARD_DA_PUBLISH_RETRY_BACKOFF_MAX_MS: "10",
      }),
    ).toThrow(/must be >=/);
  });
});
