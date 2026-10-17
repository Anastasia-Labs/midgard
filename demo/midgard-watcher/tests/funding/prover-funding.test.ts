import {
  CborTag,
  encodeCbor,
  TransportTimeoutError,
  TransportUnavailableError,
} from "@al-ft/l1-node-transport";
import { deriveDeploymentManifestCardanoProtocolParametersFromLedger } from "@al-ft/midgard-core/deployment-manifest-identity";
import { describe, expect, it, vi } from "vitest";

import {
  assertWatcherProtocolParameterRuntimeAuthority,
  createWatcherProtocolParameterRuntimeAuthority,
  refreshWatcherProtocolParameterRuntimeAuthority,
} from "../../src/funding/prover-funding.js";
import { WatcherProverFundingUnavailableError } from "../../src/funding/prover-funding-reservation.js";
import { isWatcherL1TransientFailure } from "../../src/l1/transient-failure.js";
import { retryWatcherL1Transient } from "../../src/l1/transient-retry.js";
import {
  makeWatcherDeploymentAuthorityFixture,
  WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS,
} from "../support/deployment-authority-fixture.js";
import {
  ledgerParameterQuery,
  ledgerProtocolParameters,
} from "../support/ledger-protocol-parameters.js";

const create = (
  query: () => Promise<Uint8Array>,
  deploymentIdentity = makeWatcherDeploymentAuthorityFixture().result,
) =>
  createWatcherProtocolParameterRuntimeAuthority({ deploymentIdentity, query });

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

  it("binds the live parameters to the node's protocol_params answer", async () => {
    const deploymentIdentity = makeWatcherDeploymentAuthorityFixture().result;
    const query = vi.fn(ledgerParameterQuery());

    const authority = await create(query, deploymentIdentity);

    expect(query).toHaveBeenCalledTimes(1);
    expect(authority).toMatchObject({
      deploymentFingerprint: deploymentIdentity.manifestId,
      source: "local_node",
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

  it("accepts legitimate parameter updates while rejecting structural deployment identities", async () => {
    const deploymentIdentity = makeWatcherDeploymentAuthorityFixture().result;
    const updated = await create(
      ledgerParameterQuery({ minFeeA: 45n }),
      deploymentIdentity,
    );
    expect(updated.snapshot.minFeeA).toBe("45");
    expect(updated.snapshotDigest).not.toBe(
      (await create(ledgerParameterQuery(), deploymentIdentity)).snapshotDigest,
    );
    await expect(
      create(ledgerParameterQuery(), { ...deploymentIdentity }),
    ).rejects.toThrow("invalid_field");
  });

  it("types a node that is down or restarting as an L1 transient, so startup waits and then binds once", async () => {
    const deploymentIdentity = makeWatcherDeploymentAuthorityFixture().result;
    const outages: (() => never)[] = [
      () => {
        throw new TransportUnavailableError(
          "sidecar_starting",
          "the sidecar is starting",
        );
      },
      () => {
        throw new TransportUnavailableError(
          "node_unreachable",
          "connect ECONNREFUSED",
        );
      },
      () => {
        throw new TransportTimeoutError("protocol_params timed out");
      },
    ];
    const query = vi.fn(async () => {
      const outage = outages.shift();
      if (outage !== undefined) outage();
      return ledgerProtocolParameters();
    });
    const retries: string[] = [];
    const authority = await retryWatcherL1Transient(
      () => create(query, deploymentIdentity),
      { delayMs: () => 1, onRetry: (error) => retries.push(error.message) },
    );
    expect(authority.snapshot).toEqual(
      WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS,
    );
    expect(query).toHaveBeenCalledTimes(4);
    expect(retries).toEqual(
      Array.from(
        { length: 3 },
        () => "Current local funding parameters are temporarily unavailable",
      ),
    );
  });

  it("keeps a failed query that is not a transport outage hard", async () => {
    const failure = await create(async () => {
      throw new Error("the node refused the query");
    }).catch((error: unknown) => error);
    expect(failure).toBeInstanceOf(Error);
    expect(failure).not.toBeInstanceOf(WatcherProverFundingUnavailableError);
    expect((failure as Error).message).toBe("the node refused the query");
    expect(isWatcherL1TransientFailure(failure)).toBe(false);
  });

  it("keeps an answer that is not Conway parameters hard", async () => {
    for (const bytes of [
      encodeCbor([1, 2, 3]),
      encodeCbor({ minFeeA: 44 }),
      Uint8Array.of(0xff),
    ]) {
      const failure = await create(async () => bytes).catch(
        (error: unknown) => error,
      );
      expect(failure).toBeInstanceOf(Error);
      expect(failure).not.toBeInstanceOf(WatcherProverFundingUnavailableError);
      expect(isWatcherL1TransientFailure(failure)).toBe(false);
    }
  });
});

describe("Conway ledger protocol parameters", () => {
  it("reads the funding fields at their Conway positions with exact reduced rationals", () => {
    expect(
      deriveDeploymentManifestCardanoProtocolParametersFromLedger(
        ledgerProtocolParameters({
          minFeeA: 47n,
          minFeeB: 155_382n,
          maxTxSize: 16_385n,
          coinsPerUtxoByte: 4_311n,
          priceMemory: [1154n, 20_000n],
          priceSteps: [722n, 10_000_000n],
          maxTxExUnits: [16_500_001n, 10_000_000_001n],
          maxValueSize: 5_001n,
          collateralPercentage: 151n,
          maxCollateralInputs: 4n,
          minFeeRefScriptCostPerByte: [30n, 2n],
        }),
      ),
    ).toEqual({
      minFeeA: "47",
      minFeeB: "155382",
      priceMemory: { numerator: "577", denominator: "10000" },
      priceSteps: { numerator: "361", denominator: "5000000" },
      coinsPerUtxoByte: "4311",
      collateralPercentage: "151",
      maxCollateralInputs: "4",
      maxTxSize: "16385",
      maxValueSize: "5001",
      maxTxExUnits: { memory: "16500001", steps: "10000000001" },
      referenceScriptFee: {
        base: { numerator: "15", denominator: "1" },
        range: "25600",
        multiplier: { numerator: "6", denominator: "5" },
        maximumSizeBytes: "204800",
      },
    });
  });

  it("reads a bignum-tagged integer and an untagged rational pair", () => {
    const custom = encodeCbor([
      new CborTag(2n, Uint8Array.of(0x2c)),
      155_381n,
      0n,
      16_384n,
      ...Array.from({ length: 10 }, () => 0n),
      4_310n,
      0n,
      [
        [577n, 10_000n],
        [721n, 10_000_000n],
      ],
      [16_500_000n, 10_000_000_000n],
      0n,
      5_000n,
      150n,
      3n,
      ...Array.from({ length: 8 }, () => 0n),
      new CborTag(30n, [15n, 1n]),
    ]);
    expect(
      deriveDeploymentManifestCardanoProtocolParametersFromLedger(custom),
    ).toEqual(WATCHER_TEST_CARDANO_PROTOCOL_PARAMETERS);
  });

  it("refuses a short array, a negative natural and a zero denominator", () => {
    expect(() =>
      deriveDeploymentManifestCardanoProtocolParametersFromLedger(
        encodeCbor(Array.from({ length: 30 }, () => 0n)),
      ),
    ).toThrow("PParams is not an array of at least 31 items");
    expect(() =>
      deriveDeploymentManifestCardanoProtocolParametersFromLedger(
        ledgerProtocolParameters({ minFeeA: -1n }),
      ),
    ).toThrow("minFeeA is negative");
    expect(() =>
      deriveDeploymentManifestCardanoProtocolParametersFromLedger(
        ledgerProtocolParameters({ priceMemory: [577n, 0n] }),
      ),
    ).toThrow("priceMemory is not a nonnegative rational");
  });
});

it("defers a temporary parameter-query outage without admitting malformed replies", async () => {
  let status = "live";
  const authority = await create(async () => {
    if (status === "outage")
      throw new TransportUnavailableError(
        "node_connection_lost",
        "the node closed the connection",
      );
    if (status === "malformed") return Uint8Array.of(0x01);
    return ledgerProtocolParameters();
  });
  status = "outage";
  const outage = await refreshWatcherProtocolParameterRuntimeAuthority(
    authority,
  ).catch((error: unknown) => error);
  expect(outage).toBeInstanceOf(WatcherProverFundingUnavailableError);
  expect(isWatcherL1TransientFailure(outage)).toBe(true);
  status = "malformed";
  await expect(
    refreshWatcherProtocolParameterRuntimeAuthority(authority),
  ).rejects.toThrow("PParams is not an array");
});
