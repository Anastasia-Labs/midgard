import { describe, expect, it, vi } from "vitest";

import {
  assertWatcherProtocolParameterRuntimeAuthority,
  unsafeCreateWatcherProtocolParameterRuntimeAuthorityForTest,
} from "../../src/funding/prover-funding.js";
import { isWatcherL1TransientFailure } from "../../src/l1/transient-failure.js";
import { retryWatcherL1Transient } from "../../src/l1/transient-retry.js";
import {
  makeWatcherDeploymentAuthorityFixture,
  WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS,
} from "../support/deployment-authority-fixture.js";

const ogmiosParameters = (minFeeCoefficient = 44) => ({
  minFeeCoefficient,
  minFeeConstant: { ada: { lovelace: 155381 } },
  scriptExecutionPrices: { memory: "577/10000", cpu: "721/10000000" },
  minUtxoDepositCoefficient: 4310,
  collateralPercentage: 150,
  maxCollateralInputs: 3,
  maxTransactionSize: { bytes: 16384 },
  maxValueSize: { bytes: 5000 },
  maxExecutionUnitsPerTransaction: {
    memory: 16_500_000,
    cpu: 10_000_000_000,
  },
  minFeeReferenceScripts: {
    base: 15,
    range: 25_600,
    multiplier: 1.2,
  },
  maxReferenceScriptsSizePerTransaction: { bytes: 204_800 },
});

const response = (id: string, result: unknown): Response =>
  new Response(JSON.stringify({ jsonrpc: "2.0", id, result }), {
    status: 200,
    headers: { "content-type": "application/json" },
  });

describe("production prover protocol-parameter authority V1", () => {
  it("isolates mutable deployment-authority fixture clones around one admitted base", () => {
    const first = makeWatcherDeploymentAuthorityFixture();
    const second = makeWatcherDeploymentAuthorityFixture();
    const firstManifest = first.signedIdentity.manifest as Record<
      string,
      unknown
    >;
    const firstAppliedScripts = first.policy.appliedScriptHashes as Record<
      string,
      string
    >;

    firstManifest.network = "Mainnet";
    firstAppliedScripts.hubOracleMint = "ff".repeat(28);
    (
      first.contracts.hubOracleMint as unknown as Record<string, unknown>
    ).scriptHash = "ee".repeat(28);

    expect(second.signedIdentity.manifest.network).toBe("Preprod");
    expect(second.policy.appliedScriptHashes.hubOracleMint).not.toBe(
      firstAppliedScripts.hubOracleMint,
    );
    expect(second.contracts.hubOracleMint!.scriptHash).not.toBe(
      first.contracts.hubOracleMint!.scriptHash,
    );
    expect(first.result).toBe(second.result);
  });

  it("binds the signed snapshot to an exact live loopback Ogmios response", async () => {
    const deploymentIdentity = makeWatcherDeploymentAuthorityFixture().result;
    const fetchImpl = vi.fn(
      async (_url: string | URL | Request, init?: RequestInit) => {
        const request = JSON.parse(String(init?.body)) as {
          readonly id: string;
        };
        return response(request.id, ogmiosParameters());
      },
    ) as unknown as typeof fetch;

    const authority =
      await unsafeCreateWatcherProtocolParameterRuntimeAuthorityForTest({
        deploymentIdentity,
        ogmiosUrl: "http://127.0.0.1:1337",
        timeoutMs: 10_000,
        fetchImpl,
      });

    expect(authority).toMatchObject({
      deploymentFingerprint: deploymentIdentity.manifestId,
      source: "local_ogmios",
      sourceEndpoint: "http://127.0.0.1:1337",
      snapshot: WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS,
      snapshotDigest: expect.stringMatching(/^[0-9a-f]{64}$/u),
      authorityDigest: expect.stringMatching(/^[0-9a-f]{64}$/u),
    });
    expect(Object.isFrozen(authority)).toBe(true);
    expect(() =>
      assertWatcherProtocolParameterRuntimeAuthority(authority),
    ).not.toThrow();
    expect(() =>
      assertWatcherProtocolParameterRuntimeAuthority({
        ...authority,
      }),
    ).toThrow("not admitted");
  });

  it("rejects protocol drift, remote sources, and structural deployment identities", async () => {
    const deploymentIdentity = makeWatcherDeploymentAuthorityFixture().result;
    const fetchImpl = vi.fn(
      async (_url: string | URL | Request, init?: RequestInit) => {
        const request = JSON.parse(String(init?.body)) as {
          readonly id: string;
        };
        return response(request.id, ogmiosParameters(45));
      },
    ) as unknown as typeof fetch;
    const invoke = (
      overrides: Partial<{
        deploymentIdentity: typeof deploymentIdentity;
        ogmiosUrl: string;
      }> = {},
    ) =>
      unsafeCreateWatcherProtocolParameterRuntimeAuthorityForTest({
        deploymentIdentity: overrides.deploymentIdentity ?? deploymentIdentity,
        ogmiosUrl: overrides.ogmiosUrl ?? "http://127.0.0.1:1337",
        timeoutMs: 10_000,
        fetchImpl,
      });

    await expect(invoke()).rejects.toThrow("differ from the signed deployment");
    await expect(
      invoke({ ogmiosUrl: "https://provider.example/ogmios" }),
    ).rejects.toThrow("loopback");
    await expect(
      invoke({ deploymentIdentity: { ...deploymentIdentity } }),
    ).rejects.toThrow("invalid_field");
  });

  it("types an Ogmios that is down or busy as an L1 transient, so startup waits and then binds once", async () => {
    const deploymentIdentity = makeWatcherDeploymentAuthorityFixture().result;
    const outages: (() => Response)[] = [
      () => {
        throw new DOMException("The operation timed out.", "TimeoutError");
      },
      () => {
        throw new TypeError("fetch failed", {
          cause: Object.assign(new Error("connect ECONNREFUSED"), {
            code: "ECONNREFUSED",
          }),
        });
      },
      () => new Response("starting", { status: 503 }),
    ];
    const fetchImpl = vi.fn(
      async (_url: string | URL | Request, init?: RequestInit) => {
        const outage = outages.shift();
        if (outage !== undefined) return outage();
        const request = JSON.parse(String(init?.body)) as {
          readonly id: string;
        };
        return response(request.id, ogmiosParameters());
      },
    ) as unknown as typeof fetch;
    const retries: string[] = [];
    const authority = await retryWatcherL1Transient(
      () =>
        unsafeCreateWatcherProtocolParameterRuntimeAuthorityForTest({
          deploymentIdentity,
          ogmiosUrl: "http://127.0.0.1:1337",
          timeoutMs: 10_000,
          fetchImpl,
        }),
      { delayMs: () => 1, onRetry: (error) => retries.push(error.message) },
    );
    expect(authority.snapshot).toEqual(
      WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS,
    );
    expect(fetchImpl).toHaveBeenCalledTimes(4);
    expect(retries).toEqual([
      "prover funding Ogmios query timed out",
      "fetch failed",
      "prover funding Ogmios query failed with HTTP 503",
    ]);
  });

  it("keeps a refused protocol-parameter query hard", async () => {
    const deploymentIdentity = makeWatcherDeploymentAuthorityFixture().result;
    const failure =
      await unsafeCreateWatcherProtocolParameterRuntimeAuthorityForTest({
        deploymentIdentity,
        ogmiosUrl: "http://127.0.0.1:1337",
        timeoutMs: 10_000,
        fetchImpl: (async () =>
          new Response("bad request", { status: 400 })) as typeof fetch,
      }).catch((error: unknown) => error);
    expect(failure).toBeInstanceOf(Error);
    expect((failure as Error).message).toBe(
      "prover funding Ogmios query failed with HTTP 400",
    );
    expect(isWatcherL1TransientFailure(failure)).toBe(false);
  });
});
