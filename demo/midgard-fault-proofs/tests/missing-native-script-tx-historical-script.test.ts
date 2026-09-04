import { missingNativeScriptTxVersionedScriptHash } from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  keyHashToCredential,
} from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import {
  admitHistoricalNativeScriptEvidence,
  createExternalHistoricalNativeScriptSourceRoster,
  HISTORICAL_NATIVE_SCRIPT_EVIDENCE_SCHEMA_VERSION,
  HISTORICAL_NATIVE_SCRIPT_SOURCE,
  historicalNativeScriptBytes,
  type HistoricalNativeScriptSource,
  requireHistoricalNativeScriptSourceRoster,
  resolveHistoricalNativeScriptEvidence,
  unsafeCreateHistoricalNativeScriptSourceRosterForTest,
} from "../src/missing-native-script-tx/historical-script.js";
import { createHistoricalNativeScriptProviderRoster } from "../src/workflow/historical-native-script-corpus.js";
import {
  computeFraudProofRawL1PointId,
  type FraudProofRawL1Point,
} from "../src/workflow/raw-l1-snapshot.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  type VerifiedFraudProofReleaseFinalityPolicy,
} from "../src/workflow/release-finality-policy.js";

const DEPLOYMENT = "11".repeat(32);
const RELEASE = "22".repeat(32);
const APPLICATION_OVERLAY = "23".repeat(32);
const policy = {
  confirmationDepth: 30,
  automaticRecoveryMaxDepth: 2160,
  deepRollbackPolicy: "automated_rewind_replay_incident-v1",
} as const;
const releaseFinality: VerifiedFraudProofReleaseFinalityPolicy = {
  schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  deploymentIdentityDigest: DEPLOYMENT,
  releaseIdentityDigest: RELEASE,
  policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
  policy,
};

const point = (
  slot: string,
  blockNo: string,
  blockHash: string,
): FraudProofRawL1Point => ({
  slot,
  blockNo,
  blockHash,
  pointId: computeFraudProofRawL1PointId({ slot, blockNo, blockHash }),
});

const inclusionPoint = point("100", "10", "31".repeat(32));
const throughPoint = point("200", "39", "32".repeat(32));

const fixture = () => {
  const native = CML.NativeScript.new_script_all(CML.NativeScriptList.new());
  const scriptBytesHex = native.to_canonical_cbor_hex();
  const expectedScriptHash = missingNativeScriptTxVersionedScriptHash(
    Buffer.from(scriptBytesHex, "hex"),
  );
  const output = CML.TransactionOutput.new(
    CML.Address.from_bech32(
      credentialToAddress("Preview", keyHashToCredential("41".repeat(28))),
    ),
    CML.Value.from_coin(3_000_000n),
    undefined,
    CML.Script.new_native(native),
  );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(output);
  const body = CML.TransactionBody.new(
    CML.TransactionInputList.new(),
    outputs,
    170_000n,
  );
  const txHash = CML.hash_transaction(body).to_hex();
  const response = {
    schemaVersion: HISTORICAL_NATIVE_SCRIPT_EVIDENCE_SCHEMA_VERSION,
    deploymentIdentityDigest: DEPLOYMENT,
    releaseIdentityDigest: RELEASE,
    finalityPolicyDigest: releaseFinality.policyDigest,
    expectedScriptHash,
    sourceMode: "local_node" as const,
    sourceId: "watcher-local-kupmios-history",
    operatorIdentitySha256: null,
    scriptBytesHex,
    publicationOutRef: `${txHash}#0`,
    publicationOutputCbor: output.to_canonical_cbor_hex(),
    publicationTransactionBodyCbor: body.to_canonical_cbor_hex(),
    publicationTransactionIndex: 0,
    inclusionBlockTransactionIds: [txHash],
    inclusionPoint,
    throughPoint,
  };
  return { expectedScriptHash, response };
};

const source = ({
  sourceMode = "local_node",
  sourceId = "watcher-local-kupmios-history",
  operatorIdentitySha256 = null,
  response = fixture().response,
}: {
  readonly sourceMode?: "local_node" | "external_providers";
  readonly sourceId?: string;
  readonly operatorIdentitySha256?: string | null;
  readonly response?: Readonly<Record<string, unknown>>;
} = {}): HistoricalNativeScriptSource => ({
  sourceVersion: HISTORICAL_NATIVE_SCRIPT_SOURCE,
  sourceMode,
  sourceId,
  operatorIdentitySha256,
  resolveReferenceScriptPublication: vi.fn(async (request) => ({
    ...response,
    sourceMode,
    sourceId,
    operatorIdentitySha256,
    deploymentIdentityDigest: request.deploymentIdentityDigest,
    releaseIdentityDigest: request.releaseIdentityDigest,
    finalityPolicyDigest: request.finalityPolicyDigest,
    expectedScriptHash: request.expectedScriptHash,
    throughPoint: request.throughPoint,
  })),
  confirmCanonicalHistory: vi.fn(
    async ({
      inclusionPoint: confirmedInclusion,
      throughPoint: confirmedThrough,
    }) => ({
      canonical: true,
      inclusionPoint: confirmedInclusion,
      throughPoint: confirmedThrough,
    }),
  ),
});

const roster = (
  sourceMode: "local_node" | "external_providers",
  sources: readonly HistoricalNativeScriptSource[],
) =>
  unsafeCreateHistoricalNativeScriptSourceRosterForTest({
    sourceMode,
    sources,
    applicationOverlayDigest: APPLICATION_OVERLAY,
    releaseFinality,
  });

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
    await expect(
      admitHistoricalNativeScriptEvidence({
        value: evidence,
        roster: installedRoster,
        expectedScriptHash,
        throughPoint: point("201", "40", "66".repeat(32)),
        releaseFinality,
      }),
    ).rejects.toThrow("changed the pinned historical boundary");
  });

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
            releaseIdentityDigest: body.releaseIdentityDigest,
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
