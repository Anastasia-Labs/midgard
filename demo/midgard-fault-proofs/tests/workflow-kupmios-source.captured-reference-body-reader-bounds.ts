import "./workflow-kupmios-source.production-local-kupmios-raw-source-v1.js";

import { describe, expect, it, vi } from "vitest";

import {
  computeFraudProofReleaseEconomicsPolicyDigest,
  FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
  LOCAL_KUPMIOS_REFERENCE_ACQUISITION_BOUNDS,
  readAdmittedLocalKupmiosRawBlockAtPoint,
  readAdmittedLocalKupmiosReferenceBodiesAtPoint,
  validateVerifiedFraudProofReleaseEconomicsPolicy,
} from "../src/workflow/index.js";
import {
  ANCESTOR,
  chainPoint,
  DEPLOYMENT,
  KUP0_HEAD,
  RELEASE,
  TARGET,
} from "./workflow-kupmios-source.ogmios-boundary-socket.js";
import { sourceFixture } from "./workflow-kupmios-source.source-fixture.js";

describe("release-bound fraud-proof economics V1", () => {
  const testnetPolicy = {
    profile: "bounded-acceptance-v1",
    requiredBondLovelace: "900000000",
    slashingPenaltyLovelace: "500000000",
    fraudProverRewardLovelace: "400000000",
    inactivitySlashingPenaltyLovelace: "100000000",
    proverCollateralFloorLovelace: "5000000",
  } as const;

  it("admits the manifest-bound testnet profile", () => {
    expect(
      validateVerifiedFraudProofReleaseEconomicsPolicy({
        schemaVersion: FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
        deploymentIdentityDigest: DEPLOYMENT,
        blueprintHash: RELEASE,
        policyDigest:
          computeFraudProofReleaseEconomicsPolicyDigest(testnetPolicy),
        policy: testnetPolicy,
      }),
    ).toMatchObject({ policy: testnetPolicy });
  });

  it("rejects a caller-selected reward or substituted policy digest", () => {
    const policy = { ...testnetPolicy, fraudProverRewardLovelace: "1" };
    expect(() =>
      validateVerifiedFraudProofReleaseEconomicsPolicy({
        schemaVersion: FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
        deploymentIdentityDigest: DEPLOYMENT,
        blueprintHash: RELEASE,
        policyDigest: computeFraudProofReleaseEconomicsPolicyDigest(policy),
        policy,
      }),
    ).toThrow(/must equal|canonical launch profile/u);
  });

  it("rejects legacy or extended economics policy shapes", () => {
    const { proverCollateralFloorLovelace: _omitted, ...legacyPolicy } =
      testnetPolicy;
    for (const policy of [
      legacyPolicy,
      { ...testnetPolicy, extra: "forged" },
    ]) {
      expect(() =>
        validateVerifiedFraudProofReleaseEconomicsPolicy({
          schemaVersion: FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
          deploymentIdentityDigest: DEPLOYMENT,
          blueprintHash: RELEASE,
          policyDigest: "00".repeat(32),
          policy,
        } as never),
      ).toThrow(/must contain exactly/u);
    }
  });
});

describe("captured reference-body reader bounds", () => {
  it("owns cold caches per operation and preserves an existing target cache", async () => {
    const fixture = sourceFixture();
    const args = { source: fixture.source, point: chainPoint() };
    const first = await readAdmittedLocalKupmiosReferenceBodiesAtPoint(args);
    expect(first.targetBlock.transactions).toEqual([]);
    expect(first.creatingTransactionBodies).toEqual([]);
    expect(fixture.sockets).toHaveLength(1);
    await expect(
      readAdmittedLocalKupmiosReferenceBodiesAtPoint(args),
    ).resolves.toEqual(first);
    expect(fixture.sockets).toHaveLength(2);
    await readAdmittedLocalKupmiosRawBlockAtPoint(args);
    expect(fixture.sockets).toHaveLength(3);
    await expect(
      readAdmittedLocalKupmiosReferenceBodiesAtPoint(args),
    ).resolves.toEqual(first);
    await expect(
      readAdmittedLocalKupmiosRawBlockAtPoint(args),
    ).resolves.toEqual(first.targetBlock);
    expect(fixture.sockets).toHaveLength(3);
    expect(fixture.requests.some(({ url }) => url.includes("/matches/"))).toBe(
      false,
    );
    expect(
      fixture.requests.some(({ url }) => url === "http://127.0.0.1:1337"),
    ).toBe(false);
    for (const value of [
      first,
      first.targetBlock,
      first.targetBlock.point,
      first.targetBlock.kupoCheckpoint,
      first.targetBlock.transactions,
      first.creatingTransactionBodies,
    ])
      expect(Object.isFrozen(value)).toBe(true);
    await expect(
      readAdmittedLocalKupmiosReferenceBodiesAtPoint({
        ...args,
        source: { ...fixture.source },
      }),
    ).rejects.toThrow("admitted local Kupo/Ogmios source");
    Object.assign(fixture.source, {
      readBlockAtPoint: vi.fn(() => {
        throw new Error("substituted public method");
      }),
    });
    await expect(
      readAdmittedLocalKupmiosReferenceBodiesAtPoint(args),
    ).resolves.toEqual(first);
    expect(fixture.sockets.every(({ closeCount }) => closeCount === 1)).toBe(
      true,
    );
  });

  it.each([
    [
      LOCAL_KUPMIOS_REFERENCE_ACQUISITION_BOUNDS.targetTransactions + 1,
      "target transaction count",
    ],
    [
      LOCAL_KUPMIOS_REFERENCE_ACQUISITION_BOUNDS.inspectedMembers + 1,
      "inspected-member budget",
    ],
  ] as const)(
    "refuses %s transport entries before transaction decoding",
    async (count, reason) => {
      // Collection-shape data only; no CBOR or transaction is constructed.
      const fixture = sourceFixture({
        blockTransactions: Array.from({ length: count }, () => null),
      });
      await expect(
        readAdmittedLocalKupmiosReferenceBodiesAtPoint({
          source: fixture.source,
          point: chainPoint(),
        }),
      ).rejects.toThrow(reason);
      expect(fixture.sockets.every(({ closeCount }) => closeCount === 1)).toBe(
        true,
      );
    },
  );

  it.each([false, true])(
    "shares HTTP byte budget across responses (body-null fallback: %s)",
    async (fallback) => {
      const padding = " ".repeat(
        LOCAL_KUPMIOS_REFERENCE_ACQUISITION_BOUNDS.responseBytes / 2,
      );
      const fixture = sourceFixture({
        fetchOverride: async (url) => {
          const target = url.endsWith("/400");
          const text =
            padding +
            JSON.stringify({
              slot_no: target ? 400 : 380,
              header_hash: target ? TARGET : ANCESTOR,
            });
          const headers = {
            "x-most-recent-checkpoint": "990",
            etag: KUP0_HEAD,
          };
          if (!fallback) return new Response(text, { headers });
          const bytes = new TextEncoder().encode(text);
          const value = new Response(null, { headers });
          Object.defineProperty(value, "arrayBuffer", {
            value: async () => bytes.buffer,
          });
          return value;
        },
      });
      await expect(
        readAdmittedLocalKupmiosReferenceBodiesAtPoint({
          source: fixture.source,
          point: chainPoint(),
        }),
      ).rejects.toThrow(fallback ? "byte budget" : "byte bound");
      expect(fixture.requests).toHaveLength(2);
      expect(fixture.sockets).toHaveLength(0);
    },
    30_000,
  );

  it("shares HTTP/WS budget and counts text frames with unrecognized IDs", async () => {
    const padding = " ".repeat(
      LOCAL_KUPMIOS_REFERENCE_ACQUISITION_BOUNDS.responseBytes - 1024 * 1024,
    );
    const fixture = sourceFixture({
      fetchOverride: async (url) => {
        const target = url.endsWith("/400");
        return new Response(
          (target ? padding : "") +
            JSON.stringify({
              slot_no: target ? 400 : 380,
              header_hash: target ? TARGET : ANCESTOR,
            }),
          {
            headers: {
              "x-most-recent-checkpoint": "990",
              etag: KUP0_HEAD,
            },
          },
        );
      },
      socketBehavior: {
        responseText:
          " ".repeat(2 * 1024 * 1024) +
          JSON.stringify({ jsonrpc: "2.0", id: 999, result: null }),
      },
    });
    await expect(
      readAdmittedLocalKupmiosReferenceBodiesAtPoint({
        source: fixture.source,
        point: chainPoint(),
      }),
    ).rejects.toThrow("reference acquisition response byte budget");
    expect(fixture.sockets).toHaveLength(1);
    expect(fixture.sockets[0]!.closeCount).toBe(1);
  }, 30_000);

  it("cancels its actual pending reader and permits no later receipt read", async () => {
    const controller = new AbortController();
    const fixture = sourceFixture({
      signal: controller.signal,
      socketBehavior: { respond: false },
    });
    const pending = readAdmittedLocalKupmiosReferenceBodiesAtPoint({
      source: fixture.source,
      point: chainPoint(),
    });
    const outcome = pending.catch((error: unknown) => error);
    await fixture.socketCreated;
    controller.abort();
    expect(await outcome).toMatchObject({ name: "AbortError" });
    expect(fixture.sockets[0]!.closeCount).toBe(1);
    await expect(
      readAdmittedLocalKupmiosReferenceBodiesAtPoint({
        source: fixture.source,
        point: chainPoint(),
      }),
    ).rejects.toThrow("aborted");
  });
});
