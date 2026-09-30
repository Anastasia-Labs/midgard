import "@al-ft/midgard-core/deployment-manifest-identity";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "vitest";
import "../src/missing-native-script-tx/historical-script.js";
import "../src/workflow/historical-native-script-corpus.js";
import "../src/workflow/raw-l1-snapshot.js";
import "../src/workflow/release-finality-policy.js";
import "./missing-native-script-tx-historical-script.fixture.js";

import { describe, expect, it, vi } from "vitest";

import {
  admitHistoricalNativeScriptEvidence,
  createExternalHistoricalNativeScriptSourceRoster,
  HISTORICAL_NATIVE_SCRIPT_EVIDENCE_SCHEMA_VERSION,
  historicalNativeScriptBytes,
  requireHistoricalNativeScriptSourceRoster,
  resolveHistoricalNativeScriptEvidence,
} from "../src/missing-native-script-tx/historical-script.js";
import { createHistoricalNativeScriptProviderRoster } from "../src/workflow/historical-native-script-corpus.js";
import {
  APPLICATION_OVERLAY,
  DEPLOYMENT,
  fixture,
  inclusionPoint,
  point,
  releaseFinality,
  roster,
  source,
  throughPoint,
} from "./missing-native-script-tx-historical-script.fixture.js";

describe("missing-native-script authenticated historical resolver V1", () => {
  it("derives canonical script bytes from a final L1 reference-script publication", async () => {
    const { expectedScriptHash, response } = fixture();
    const history = source({ response });
    const installedRoster = roster("local_node", [history]);
    const evidence = await resolveHistoricalNativeScriptEvidence({
      roster: installedRoster,
      expectedScriptHash,
      throughPoint,
      releaseFinality,
      retainedDaCorroboratingScriptBytes: Buffer.from(
        response.scriptBytesHex,
        "hex",
      ),
    });
    expect(evidence).toMatchObject({
      schemaVersion: HISTORICAL_NATIVE_SCRIPT_EVIDENCE_SCHEMA_VERSION,
      deploymentIdentityDigest: DEPLOYMENT,
      expectedScriptHash,
      sourceMode: "local_node",
      applicationOverlayDigest: APPLICATION_OVERLAY,
      confirmationDepth: 30,
      sources: [
        {
          sourceId: "watcher-local-kupmios-history",
          operatorIdentitySha256: null,
        },
      ],
    });
    expect(Buffer.from(historicalNativeScriptBytes(evidence))).toEqual(
      Buffer.from(response.scriptBytesHex, "hex"),
    );
    const persisted: unknown = JSON.parse(JSON.stringify(evidence));
    expect(() => historicalNativeScriptBytes(persisted as never)).toThrow(
      "was not admitted",
    );
    const readmitted = await admitHistoricalNativeScriptEvidence({
      value: persisted,
      roster: installedRoster,
      expectedScriptHash,
      throughPoint,
      releaseFinality,
    });
    expect(Buffer.from(historicalNativeScriptBytes(readmitted))).toEqual(
      Buffer.from(response.scriptBytesHex, "hex"),
    );
    expect(evidence.evidenceDigest).toMatch(/^[0-9a-f]{64}$/u);
    expect(history.confirmCanonicalHistory).toHaveBeenCalledTimes(2);
  });

  it("accepts an authenticated publication prerequisite before release finality", async () => {
    const { expectedScriptHash, response } = fixture();
    const history = source({ response });
    const evidence = await resolveHistoricalNativeScriptEvidence({
      roster: roster("local_node", [history]),
      expectedScriptHash,
      throughPoint: response.inclusionPoint,
      releaseFinality,
    });
    expect(evidence.confirmationDepth).toBe(1);
    expect(evidence.finalityPolicyDigest).toBe(releaseFinality.policyDigest);
    expect(history.confirmCanonicalHistory).toHaveBeenCalledOnce();
  });

  it("rejects forged or context-substituted persisted evidence", async () => {
    const { expectedScriptHash, response } = fixture();
    const history = source({ response });
    const installedRoster = roster("local_node", [history]);
    const evidence = await resolveHistoricalNativeScriptEvidence({
      roster: installedRoster,
      expectedScriptHash,
      throughPoint,
      releaseFinality,
    });
    const cases: readonly unknown[] = [
      { ...evidence, unknown: true },
      { ...evidence, confirmationDepth: evidence.confirmationDepth + 1 },
      { ...evidence, evidenceDigest: "ff".repeat(32) },
      { ...evidence, throughPoint: point("201", "40", "66".repeat(32)) },
      { ...evidence, applicationOverlayDigest: "fe".repeat(32) },
      {
        ...evidence,
        sources: [
          {
            sourceId: evidence.sources[0]!.sourceId,
            operatorIdentitySha256: "55".repeat(32),
          },
        ],
      },
    ];
    for (const value of cases) {
      await expect(
        admitHistoricalNativeScriptEvidence({
          value,
          roster: installedRoster,
          expectedScriptHash,
          throughPoint,
          releaseFinality,
        }),
      ).rejects.toThrow();
    }
    await expect(
      admitHistoricalNativeScriptEvidence({
        value: evidence,
        roster: roster("external_providers", [
          source({
            sourceMode: "external_providers",
            sourceId: "provider-a",
            operatorIdentitySha256: "51".repeat(32),
            response,
          }),
          source({
            sourceMode: "external_providers",
            sourceId: "provider-b",
            operatorIdentitySha256: "52".repeat(32),
            response,
          }),
        ]),
        expectedScriptHash,
        throughPoint,
        releaseFinality,
      }),
    ).rejects.toThrow("schema/source mode mismatch");
  });

  it.each([
    ["advancing tip", point("220", "41", "61".repeat(32))],
    ["replaced tip", point("200", "39", "62".repeat(32))],
    ["rollback below the original tip", point("180", "35", "63".repeat(32))],
  ] as const)(
    "revalidates persisted publication at the current %s without resealing provenance",
    async (_label, currentPoint) => {
      const { expectedScriptHash, response } = fixture();
      const providers = ["a", "b"].map((id, index) =>
        source({
          sourceMode: "external_providers",
          sourceId: `provider-${id}`,
          operatorIdentitySha256: (index === 0 ? "51" : "52").repeat(32),
          response,
        }),
      );
      const installedRoster = roster("external_providers", providers);
      const original = await resolveHistoricalNativeScriptEvidence({
        roster: installedRoster,
        expectedScriptHash,
        throughPoint,
        releaseFinality,
      });
      for (const provider of providers)
        vi.mocked(provider.confirmCanonicalHistory).mockImplementation(
          async (request) => ({
            canonical: request.throughPoint.pointId === currentPoint.pointId,
            ...request,
          }),
        );
      const readmitted = await admitHistoricalNativeScriptEvidence({
        value: JSON.parse(JSON.stringify(original)),
        roster: installedRoster,
        expectedScriptHash,
        throughPoint: currentPoint,
        releaseFinality,
      });
      expect(readmitted).toEqual(original);
      expect(readmitted.evidenceDigest).toBe(original.evidenceDigest);
      expect(readmitted.throughPoint).toEqual(throughPoint);
      for (const provider of providers) {
        expect(
          provider.resolveReferenceScriptPublication,
        ).toHaveBeenLastCalledWith(
          expect.objectContaining({ throughPoint: currentPoint }),
        );
        expect(provider.confirmCanonicalHistory).toHaveBeenLastCalledWith({
          inclusionPoint,
          throughPoint: currentPoint,
        });
      }
      response.inclusionPoint = point("110", "11", "65".repeat(32));
      await expect(
        admitHistoricalNativeScriptEvidence({
          value: original,
          roster: installedRoster,
          expectedScriptHash,
          throughPoint: currentPoint,
          releaseFinality,
        }),
      ).rejects.toThrow("changed after live roster reconfirmation");
      response.inclusionPoint = inclusionPoint;
      vi.mocked(providers[1]!.confirmCanonicalHistory).mockImplementation(
        async (request) => ({ canonical: false, ...request }),
      );
      await expect(
        admitHistoricalNativeScriptEvidence({
          value: original,
          roster: installedRoster,
          expectedScriptHash,
          throughPoint: currentPoint,
          releaseFinality,
        }),
      ).rejects.toThrow("rolled back during resolution");
      await expect(
        admitHistoricalNativeScriptEvidence({
          value: original,
          roster: installedRoster,
          expectedScriptHash,
          throughPoint: point("90", "9", "64".repeat(32)),
          releaseFinality,
        }),
      ).rejects.toThrow("publication after the boundary");
    },
  );

  it("rejects substituted preimages, outrefs, boundaries, and DA corroboration", async () => {
    const { expectedScriptHash, response } = fixture();
    const cases = [
      { ...response, scriptBytesHex: "00" },
      { ...response, publicationOutRef: `${"ff".repeat(32)}#0` },
      { ...response, inclusionBlockTransactionIds: ["ff".repeat(32)] },
      { ...response, inclusionPoint: point("201", "40", "44".repeat(32)) },
    ];
    for (const candidate of cases) {
      await expect(
        resolveHistoricalNativeScriptEvidence({
          roster: roster("local_node", [source({ response: candidate })]),
          expectedScriptHash,
          throughPoint,
          releaseFinality,
        }),
      ).rejects.toThrow();
    }
    await expect(
      resolveHistoricalNativeScriptEvidence({
        roster: roster("local_node", [source({ response })]),
        expectedScriptHash,
        throughPoint,
        releaseFinality,
        retainedDaCorroboratingScriptBytes: Uint8Array.from([0]),
      }),
    ).rejects.toThrow("corroboration differs from authenticated L1 history");
  });

  it("requires exact independent-provider agreement in external mode", async () => {
    const { expectedScriptHash, response } = fixture();
    const providerA = source({
      sourceMode: "external_providers",
      sourceId: "provider-a",
      operatorIdentitySha256: "51".repeat(32),
      response,
    });
    const providerB = source({
      sourceMode: "external_providers",
      sourceId: "provider-b",
      operatorIdentitySha256: "52".repeat(32),
      response,
    });
    await expect(
      resolveHistoricalNativeScriptEvidence({
        roster: roster("external_providers", [providerA, providerB]),
        expectedScriptHash,
        throughPoint,
        releaseFinality,
      }),
    ).resolves.toMatchObject({
      sourceMode: "external_providers",
      sources: [{ sourceId: "provider-a" }, { sourceId: "provider-b" }],
    });

    expect(() =>
      roster("external_providers", [
        providerA,
        source({
          sourceMode: "external_providers",
          sourceId: "provider-b",
          operatorIdentitySha256: "51".repeat(32),
          response,
        }),
      ]),
    ).toThrow("providers are not independent");

    await expect(
      resolveHistoricalNativeScriptEvidence({
        roster: roster("external_providers", [
          providerA,
          source({
            sourceMode: "external_providers",
            sourceId: "provider-c",
            operatorIdentitySha256: "53".repeat(32),
            response: {
              ...response,
              publicationOutRef: `${response.publicationOutRef.slice(0, -1)}1`,
            },
          }),
        ]),
        expectedScriptHash,
        throughPoint,
        releaseFinality,
      }),
    ).rejects.toThrow();
  });

  it("admits only the immutable concrete provider roster and revalidates it on restart", async () => {
    const { expectedScriptHash, response } = fixture();
    const providerRoster = createHistoricalNativeScriptProviderRoster({
      deploymentFingerprint: DEPLOYMENT,
      providers: [
        {
          sourceId: "provider-a",
          operatorIdentitySha256: "51".repeat(32),
          authorityEndpoint: "https://provider-a.example.test",
        },
        {
          sourceId: "provider-b",
          operatorIdentitySha256: "52".repeat(32),
          authorityEndpoint: "https://provider-b.example.test",
        },
      ],
    });
    let substituteOnRestart = false;
    vi.stubGlobal(
      "fetch",
      vi.fn(async (input: string | URL | Request, init?: RequestInit) => {
        const url = new URL(
          typeof input === "string"
            ? input
            : input instanceof URL
              ? input.toString()
              : input.url,
        );
        const body = JSON.parse(String(init?.body)) as Record<string, unknown>;
        if (url.pathname.endsWith("/canonicality")) {
          return new Response(
            JSON.stringify({
              canonical: true,
              inclusionPoint: body.inclusionPoint,
              throughPoint: body.throughPoint,
            }),
            { status: 200 },
          );
        }
        const sourceId = url.hostname.startsWith("provider-a")
          ? "provider-a"
          : "provider-b";
        const operatorIdentitySha256 =
          sourceId === "provider-a" ? "51".repeat(32) : "52".repeat(32);
        return new Response(
          JSON.stringify({
            ...response,
            sourceMode: "external_providers",
            sourceId,
            operatorIdentitySha256,
            deploymentIdentityDigest: body.deploymentIdentityDigest,
            blueprintHash: body.blueprintHash,
            finalityPolicyDigest: body.finalityPolicyDigest,
            expectedScriptHash: body.expectedScriptHash,
            throughPoint: body.throughPoint,
            ...(substituteOnRestart
              ? {
                  publicationOutRef: `${response.publicationOutRef.slice(0, -1)}1`,
                }
              : {}),
          }),
          { status: 200 },
        );
      }),
    );
    try {
      const installedRoster = createExternalHistoricalNativeScriptSourceRoster({
        providerRoster,
        releaseFinality,
      });
      expect(
        requireHistoricalNativeScriptSourceRoster(
          installedRoster,
          releaseFinality,
        ),
      ).toBe(installedRoster);
      expect(() =>
        createExternalHistoricalNativeScriptSourceRoster({
          providerRoster: { ...providerRoster },
          releaseFinality,
        }),
      ).toThrow(/admitted immutable provider roster/u);
      expect(() =>
        requireHistoricalNativeScriptSourceRoster(
          roster("external_providers", [
            source({
              sourceMode: "external_providers",
              sourceId: "provider-a",
              operatorIdentitySha256: "51".repeat(32),
              response,
            }),
            source({
              sourceMode: "external_providers",
              sourceId: "provider-b",
              operatorIdentitySha256: "52".repeat(32),
              response,
            }),
          ]),
          releaseFinality,
        ),
      ).toThrow(/not a concrete production authority/u);

      const evidence = await resolveHistoricalNativeScriptEvidence({
        roster: installedRoster,
        expectedScriptHash,
        throughPoint,
        releaseFinality,
      });
      substituteOnRestart = true;
      await expect(
        admitHistoricalNativeScriptEvidence({
          value: JSON.parse(JSON.stringify(evidence)),
          roster: installedRoster,
          expectedScriptHash,
          throughPoint,
          releaseFinality,
        }),
      ).rejects.toThrow();
    } finally {
      vi.unstubAllGlobals();
    }
  });
});
