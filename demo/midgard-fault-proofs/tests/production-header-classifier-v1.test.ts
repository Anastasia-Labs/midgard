import {
  decodeMidgardNativeTxProofFieldLengthsV1,
  encodeMidgardNativeTxProofFieldLengthsV1,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import type { RetainedDaPayloadSource } from "../src/transition-trace/fetch.js";
import {
  COMPLETE_CANONICAL_REPLAY_V1,
  type CompleteCanonicalReplayV1,
  createCompleteCanonicalReplayUnionV1,
  DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY_V1,
  FIELD_PREIMAGE_LENGTH_MISMATCH_COMPLETE_CANONICAL_REPLAY_V1,
  MINT_DECLARED_ASSET_LIMIT_COMPLETE_CANONICAL_REPLAY_V1,
  NETWORK_ID_COMPLETE_CANONICAL_REPLAY_V1,
  NO_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY_V1,
  OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_COMPLETE_CANONICAL_REPLAY_V1,
  PROTECTED_OUTPUT_SIGNER_MISSING_COMPLETE_CANONICAL_REPLAY_V1,
  RESOLVED_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY_V1,
} from "../src/workflow/complete-replay-v1.js";
import {
  authenticatedStateQueueObservationDigestV1,
  classifyProductionHeaderV1,
  createProductionHeaderClassifierV1,
  productionHeaderDecisionReplayContextV1,
  type ProductionHeaderDecisionV1,
  requireRunnableProductionHeaderFaultV1,
} from "../src/workflow/production-header-classifier-v1.js";
import {
  computeFraudProofReleaseFinalityPolicyDigestV1,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY_V1,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_V1_SCHEMA_VERSION,
  type FraudProofReleaseFinalityAuthorityV1,
} from "../src/workflow/release-finality-policy-v1.js";
import {
  authenticatedHeaderObservationV1,
  buildCanonicalBlockFixtureV1,
  buildFixtureTransactionV1,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture-v1.js";

const DEPLOYMENT_FINGERPRINT = "d1".repeat(32);
const RELEASE_FINALITY_POLICY = {
  confirmationDepth: 30,
  automaticRecoveryMaxDepth: 2160,
  deepRollbackPolicy: "automated_rewind_replay_incident-v1",
} as const;

const finalityAuthority = (): FraudProofReleaseFinalityAuthorityV1 => ({
  authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY_V1,
  verifyForWorkflow: async () => ({
    schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_V1_SCHEMA_VERSION,
    deploymentIdentityDigest: DEPLOYMENT_FINGERPRINT,
    releaseIdentityDigest: "e1".repeat(32),
    policyDigest: computeFraudProofReleaseFinalityPolicyDigestV1(
      RELEASE_FINALITY_POLICY,
    ),
    policy: RELEASE_FINALITY_POLICY,
  }),
});

const retainedDaSource = ({
  payloadEnvelopeCbor,
  fetched,
}: {
  readonly payloadEnvelopeCbor: Buffer;
  readonly fetched: { count: number };
}): RetainedDaPayloadSource => ({
  sourceId: "libp2p",
  fetchPayloadByHeaderHash: async () => {
    fetched.count += 1;
    return {
      ok: true,
      provenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: "libp2p/peer-a",
        grade: "security",
      },
      sourceId: "libp2p",
      sourcePeerId: "peer-a",
      payloadEnvelopeCbor,
      attempts: [],
    };
  },
});

describe("production authenticated-header classifier V1", () => {
  it("admits only closed replay unions in canonical catalogue order", async () => {
    expect(() =>
      createCompleteCanonicalReplayUnionV1([
        NETWORK_ID_COMPLETE_CANONICAL_REPLAY_V1,
        DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY_V1,
      ]),
    ).toThrow("canonical catalogue order");

    const forged: CompleteCanonicalReplayV1 = {
      replayVersion: COMPLETE_CANONICAL_REPLAY_V1,
      launchScope: ["doubleSpend"],
      replay: async () => {
        throw new Error("must not run");
      },
    };
    await expect(
      createProductionHeaderClassifierV1({
        deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
        replayer: forged,
        releaseFinalityAuthority: finalityAuthority(),
      }),
    ).rejects.toThrow("closed canonical replay bundle");

    await expect(
      createProductionHeaderClassifierV1({
        deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
        replayer: RESOLVED_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY_V1,
        releaseFinalityAuthority: finalityAuthority(),
      }),
    ).rejects.toThrow(
      "resolved-output complete replay requires an admitted historical replay authority",
    );
  });

  it("fetches public DA once and mints an opaque exact fault selection", async () => {
    const sharedInput = outRefCbor(71, 0n);
    const fixture = await buildCanonicalBlockFixtureV1({
      transactions: [
        buildFixtureTransactionV1({ spendInputs: [sharedInput], fee: 1n }),
        buildFixtureTransactionV1({ spendInputs: [sharedInput], fee: 2n }),
      ],
    });
    const observation = authenticatedHeaderObservationV1(fixture);
    const classifier = await createProductionHeaderClassifierV1({
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      replayer: DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY_V1,
      releaseFinalityAuthority: finalityAuthority(),
    });
    const authenticatedObservationDigest =
      await authenticatedStateQueueObservationDigestV1({
        observation,
        minimumConfirmationDepth: 30,
      });
    const fetched = { count: 0 };
    const decision = await classifyProductionHeaderV1({
      classifier,
      observation,
      authenticatedObservationDigest,
      sources: [
        retainedDaSource({
          payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
          fetched,
        }),
      ],
    });
    expect(fetched.count).toBe(1);
    expect(decision).toMatchObject({
      decision: "fault_detected",
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      headerHash: fixture.headerHash,
      category: "doubleSpend",
      violationId: "double-spend",
      position: "1",
      authenticatedObservationDigest,
    });
    expect(decision.decisionDigest).toMatch(/^[0-9a-f]{64}$/u);
    expect(requireRunnableProductionHeaderFaultV1(decision)).toBe(decision);
  });

  it("routes an authenticated raw field-length defect with one DA fetch", async () => {
    const canonical = buildFixtureTransactionV1({
      spendInputs: [],
      fee: 1n,
    });
    const lengths = [
      ...decodeMidgardNativeTxProofFieldLengthsV1(
        Buffer.from(canonical.source.source.field_preimage_lengths_cbor, "hex"),
      ),
    ];
    lengths[0] = lengths[0]! + 1;
    const source: SDK.L2TransactionSourceV1 = {
      ...canonical.source,
      source: {
        ...canonical.source.source,
        field_preimage_lengths_cbor:
          encodeMidgardNativeTxProofFieldLengthsV1(lengths).toString("hex"),
      },
    };
    const fixture = await buildCanonicalBlockFixtureV1({
      transactions: [
        {
          ...canonical,
          source,
          sourceValueBytes: Buffer.from(
            Data.to(source as never, SDK.L2TransactionSourceV1Schema as never),
            "hex",
          ),
        },
      ],
    });
    const observation = authenticatedHeaderObservationV1(fixture);
    const classifier = await createProductionHeaderClassifierV1({
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      replayer: FIELD_PREIMAGE_LENGTH_MISMATCH_COMPLETE_CANONICAL_REPLAY_V1,
      releaseFinalityAuthority: finalityAuthority(),
    });
    const authenticatedObservationDigest =
      await authenticatedStateQueueObservationDigestV1({
        observation,
        minimumConfirmationDepth: 30,
      });
    const fetched = { count: 0 };
    const decision = await classifyProductionHeaderV1({
      classifier,
      observation,
      authenticatedObservationDigest,
      sources: [
        retainedDaSource({
          payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
          fetched,
        }),
      ],
    });
    expect(fetched.count).toBe(1);
    expect(decision).toMatchObject({
      decision: "fault_detected",
      category: "fieldPreimageLengthMismatch",
      violationId: "field-preimage-length-mismatch",
      position: "0",
    });
  });

  it("routes an accepted untagged transaction with observers before canonical parsing", async () => {
    const transaction = buildFixtureTransactionV1({
      spendInputs: [],
      fee: 1n,
      networkId: 255n,
      requiredObservers: [Buffer.alloc(28, 0x42)],
    });
    const fixture = await buildCanonicalBlockFixtureV1({
      transactions: [transaction],
    });
    const observation = authenticatedHeaderObservationV1(fixture);
    const classifier = await createProductionHeaderClassifierV1({
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      replayer:
        OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_COMPLETE_CANONICAL_REPLAY_V1,
      releaseFinalityAuthority: finalityAuthority(),
    });
    const authenticatedObservationDigest =
      await authenticatedStateQueueObservationDigestV1({
        observation,
        minimumConfirmationDepth: 30,
      });
    const fetched = { count: 0 };
    const decision = await classifyProductionHeaderV1({
      classifier,
      observation,
      authenticatedObservationDigest,
      sources: [
        retainedDaSource({
          payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
          fetched,
        }),
      ],
    });
    expect(fetched.count).toBe(1);
    expect(decision).toMatchObject({
      decision: "fault_detected",
      category: "observersForbiddenOnUntaggedNetwork",
      violationId: "observers-forbidden-on-untagged-network",
      position: "0",
    });
  });

  it("preserves catalogue priority across accepted raw replay routes", async () => {
    const declaredAssetLimitCrossing = Buffer.concat([
      Buffer.from([0x82, 0x58, 0x1c]),
      Buffer.alloc(28, 0x31),
      Buffer.from([0xb9, 0x40, 0x01, 0x00]),
    ]);
    const transaction = buildFixtureTransactionV1({
      spendInputs: [],
      fee: 1n,
      networkId: 255n,
      requiredObservers: [Buffer.alloc(28, 0x42)],
      mintPolicyItems: [declaredAssetLimitCrossing],
    });
    const fixture = await buildCanonicalBlockFixtureV1({
      transactions: [transaction],
    });
    const observation = authenticatedHeaderObservationV1(fixture);
    const classifier = await createProductionHeaderClassifierV1({
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      replayer: createCompleteCanonicalReplayUnionV1([
        MINT_DECLARED_ASSET_LIMIT_COMPLETE_CANONICAL_REPLAY_V1,
        OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_COMPLETE_CANONICAL_REPLAY_V1,
      ]),
      releaseFinalityAuthority: finalityAuthority(),
    });
    const authenticatedObservationDigest =
      await authenticatedStateQueueObservationDigestV1({
        observation,
        minimumConfirmationDepth: 30,
      });
    const decision = await classifyProductionHeaderV1({
      classifier,
      observation,
      authenticatedObservationDigest,
      sources: [
        retainedDaSource({
          payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
          fetched: { count: 0 },
        }),
      ],
    });
    expect(decision).toMatchObject({
      decision: "fault_detected",
      category: "mintDeclaredAssetLimit",
      violationId: "mint-declared-asset-limit",
    });
  });

  it("keeps signer-family positions on the authenticated transaction frontier", async () => {
    const protectedOutput = encodeMidgardTxOutput({
      address: Buffer.concat([Buffer.from([0x68]), Buffer.alloc(28, 0x42)]),
      value: { lovelace: 2_000_000n, assets: new Map() },
    });
    const faulty = buildFixtureTransactionV1({
      spendInputs: [],
      outputs: [protectedOutput],
      fee: 2n,
    });
    const earlierHealthy = Array.from({ length: 256 }, (_, index) =>
      buildFixtureTransactionV1({
        spendInputs: [],
        fee: BigInt(index + 10),
      }),
    ).find((transaction) => transaction.txId < faulty.txId);
    expect(earlierHealthy).toBeDefined();
    const fixture = await buildCanonicalBlockFixtureV1({
      transactions: [earlierHealthy!, faulty],
    });
    const observation = authenticatedHeaderObservationV1(fixture);
    const classifier = await createProductionHeaderClassifierV1({
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      replayer: PROTECTED_OUTPUT_SIGNER_MISSING_COMPLETE_CANONICAL_REPLAY_V1,
      releaseFinalityAuthority: finalityAuthority(),
    });
    const authenticatedObservationDigest =
      await authenticatedStateQueueObservationDigestV1({
        observation,
        minimumConfirmationDepth: 30,
      });
    const decision = await classifyProductionHeaderV1({
      classifier,
      observation,
      authenticatedObservationDigest,
      sources: [
        retainedDaSource({
          payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
          fetched: { count: 0 },
        }),
      ],
    });
    expect(decision).toMatchObject({
      decision: "fault_detected",
      category: "protectedOutputSignerMissing",
      violationId: "protected-output-signer-missing",
      position: "1",
    });
  });

  it("keeps healthy decisions non-runnable and rejects structural forgeries", async () => {
    const fixture = await buildCanonicalBlockFixtureV1({ transactions: [] });
    const observation = authenticatedHeaderObservationV1(fixture);
    const classifier = await createProductionHeaderClassifierV1({
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      replayer: DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY_V1,
      releaseFinalityAuthority: finalityAuthority(),
    });
    const authenticatedObservationDigest =
      await authenticatedStateQueueObservationDigestV1({
        observation,
        minimumConfirmationDepth: 30,
      });
    const decision = await classifyProductionHeaderV1({
      classifier,
      observation,
      authenticatedObservationDigest,
      sources: [
        retainedDaSource({
          payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
          fetched: { count: 0 },
        }),
      ],
    });
    expect(decision.decision).toBe("healthy");
    expect(() => requireRunnableProductionHeaderFaultV1(decision)).toThrow(
      "only fault_detected",
    );
    expect(() =>
      requireRunnableProductionHeaderFaultV1({
        ...decision,
        decision: "fault_detected",
        category: "doubleSpend",
        violationId: "double-spend",
        detectionId: "forged",
        position: "0",
      } as ProductionHeaderDecisionV1),
    ).toThrow("not module-admitted");
  });

  it("rejects an L1 source digest substitution before retained-DA I/O", async () => {
    const fixture = await buildCanonicalBlockFixtureV1({ transactions: [] });
    const observation = authenticatedHeaderObservationV1(fixture);
    const classifier = await createProductionHeaderClassifierV1({
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      replayer: DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY_V1,
      releaseFinalityAuthority: finalityAuthority(),
    });
    const fetched = { count: 0 };
    await expect(
      classifyProductionHeaderV1({
        classifier,
        observation,
        authenticatedObservationDigest: "00".repeat(32),
        sources: [
          retainedDaSource({
            payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
            fetched,
          }),
        ],
      }),
    ).rejects.toThrow("observation digest differs");
    expect(fetched.count).toBe(0);
  });

  it("returns unprovable without predecessor authority and admits a freshly fetched predecessor", async () => {
    const predecessor = await buildCanonicalBlockFixtureV1({
      transactions: [],
      utxos: [
        {
          key: outRefCbor(65, 0n),
          value: encodeMidgardTxOutput({
            address: Buffer.concat([
              Buffer.from([0x60]),
              Buffer.alloc(28, 0x42),
            ]),
            value: { lovelace: 2_000_000n, assets: new Map() },
          }),
        },
      ],
    });
    const current = await buildCanonicalBlockFixtureV1({
      transactions: [],
      prevHeaderHash: predecessor.headerHash,
      prevUtxosRoot: predecessor.header.utxosRoot,
    });
    const observation = authenticatedHeaderObservationV1(current);
    const predecessorObservation = authenticatedHeaderObservationV1(
      predecessor,
      {
        chainPoint: { slot: 4241n, blockHash: "06".repeat(32) },
      },
    );
    const classifier = await createProductionHeaderClassifierV1({
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      replayer: NO_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY_V1,
      releaseFinalityAuthority: finalityAuthority(),
    });
    const authenticatedObservationDigest =
      await authenticatedStateQueueObservationDigestV1({
        observation,
        minimumConfirmationDepth: 30,
      });
    const payloads = new Map([
      [current.headerHash, current.payloadEnvelopeCbor],
      [predecessor.headerHash, predecessor.payloadEnvelopeCbor],
    ]);
    const fetched = { count: 0 };
    const source: RetainedDaPayloadSource = {
      sourceId: "libp2p",
      fetchPayloadByHeaderHash: async (headerHash) => {
        fetched.count += 1;
        const payloadEnvelopeCbor = payloads.get(headerHash);
        if (payloadEnvelopeCbor === undefined) {
          return { ok: false, sourceId: "libp2p", attempts: [] };
        }
        return {
          ok: true,
          provenance: {
            trustClass: "public_or_permissionless_da",
            sourceId: "libp2p/peer-a",
            grade: "security",
          },
          sourceId: "libp2p",
          sourcePeerId: "peer-a",
          payloadEnvelopeCbor,
          attempts: [],
        };
      },
    };

    const unavailable = await classifyProductionHeaderV1({
      classifier,
      observation,
      authenticatedObservationDigest,
      sources: [source],
    });
    expect(unavailable).toMatchObject({
      decision: "unprovable",
      reason: "predecessor_context_unavailable",
    });
    expect(
      productionHeaderDecisionReplayContextV1(unavailable),
    ).toBeUndefined();
    expect(fetched.count).toBe(1);

    const admitted = await classifyProductionHeaderV1({
      classifier,
      observation,
      authenticatedObservationDigest,
      sources: [source],
      predecessorObservation,
    });
    expect(admitted.decision).toBe("healthy");
    expect(
      productionHeaderDecisionReplayContextV1(admitted)?.predecessor,
    ).toBeDefined();
    expect(fetched.count).toBe(3);
  });
});
