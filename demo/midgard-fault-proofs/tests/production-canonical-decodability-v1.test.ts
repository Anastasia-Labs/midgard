import {
  decodeMidgardNativeTxCanonicalV1,
  decodeSingleCbor,
  deriveMidgardNativeTxFaultEvidenceMaterialV1,
  encodeCbor,
} from "@al-ft/midgard-core";
import {
  type L2TransactionSourceV1,
  L2TransactionSourceV1 as L2TransactionSourceV1Codec,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayloadV1 } from "../src/evidence/canonical-block-evidence-v1.js";
import { canonicalDecodabilityRawBlockEvidenceFromVerifiedPayloadV1 } from "../src/evidence/canonical-decodability-raw-evidence-v1.js";
import type { RetainedDaPayloadSource } from "../src/transition-trace/fetch.js";
import { CANONICAL_DECODABILITY_COMPLETE_CANONICAL_REPLAY_V1 } from "../src/workflow/complete-replay-v1.js";
import {
  admitProductionCanonicalDecodabilityArtifactV1,
  prepareProductionCanonicalDecodabilityArtifactV1,
} from "../src/workflow/production-canonical-decodability-v1.js";
import {
  authenticatedStateQueueObservationDigestV1,
  classifyProductionHeaderV1,
  createProductionHeaderClassifierV1,
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
  type FixtureTransactionV1,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture-v1.js";

const DEPLOYMENT_FINGERPRINT = "d7".repeat(32);
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
    releaseIdentityDigest: "e7".repeat(32),
    policyDigest: computeFraudProofReleaseFinalityPolicyDigestV1(
      RELEASE_FINALITY_POLICY,
    ),
    policy: RELEASE_FINALITY_POLICY,
  }),
});

const malformedOutputsTransaction = (): FixtureTransactionV1 => {
  const valid = buildFixtureTransactionV1({
    spendInputs: [outRefCbor(0x52, 0n)],
    fee: 1n,
  });
  const outer = decodeSingleCbor(valid.canonicalCbor);
  if (!Array.isArray(outer) || !Array.isArray(outer[1])) {
    throw new Error("fixture native transaction changed outer wire");
  }
  const body = outer[1];
  const canonicalCbor = encodeCbor([
    outer[0],
    // A scalar is not the canonical list envelope required for outputs.
    [...body.slice(0, 2), Buffer.from([0]), ...body.slice(3)],
    outer[2],
    outer[3],
  ]);
  expect(() => decodeMidgardNativeTxCanonicalV1(canonicalCbor)).toThrow(
    "outputs is not a canonical §5.1 field preimage",
  );
  const material = deriveMidgardNativeTxFaultEvidenceMaterialV1(canonicalCbor);
  const txId = material.transactionId.toString("hex");
  const source: L2TransactionSourceV1 = {
    tx_id: txId,
    source: {
      compact_cbor: material.proofSource.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        material.proofSource.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        material.proofSource.fieldPreimageLengthsCbor.toString("hex"),
    },
  };
  return {
    txId,
    canonicalCbor,
    compactCbor: material.proofSource.compactCbor,
    source,
    sourceValueBytes: Buffer.from(
      Data.to(source, L2TransactionSourceV1Codec),
      "hex",
    ),
  };
};

const preparedArtifact = async () => {
  const fixture = await buildCanonicalBlockFixtureV1({
    transactions: [
      buildFixtureTransactionV1({
        spendInputs: [outRefCbor(0x51, 0n)],
        fee: 1n,
      }),
      malformedOutputsTransaction(),
    ],
  });
  const observation = authenticatedHeaderObservationV1(fixture);
  const daProvenance = {
    trustClass: "public_or_permissionless_da",
    sourceId: "libp2p/production-canonical-decodability-test",
    grade: "security",
  } as const;
  await expect(
    canonicalBlockEvidenceFromVerifiedPayloadV1({
      observation,
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
      daProvenance,
    }),
  ).rejects.toMatchObject({ code: "authenticatedCommittedFieldDefect" });
  const evidence =
    await canonicalDecodabilityRawBlockEvidenceFromVerifiedPayloadV1({
      observation,
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
      daProvenance,
    });
  return {
    fixture,
    observation,
    evidence,
    artifact: await prepareProductionCanonicalDecodabilityArtifactV1(evidence),
  };
};

describe("production canonical-decodability public-evidence workflow V1", () => {
  it("selects the exact malformed field and replays its source-root membership", async () => {
    const { artifact, evidence } = await preparedArtifact();
    const admitted =
      await admitProductionCanonicalDecodabilityArtifactV1(artifact);
    expect(artifact).toMatchObject({
      selectedTransactionIndex: evidence.selected.transactionIndex,
      selectedFieldIndex: 2,
    });
    expect(artifact.selectedVerdict).not.toBe(0);
    expect(admitted.committedPreimage.toString("hex")).toBe("00");
    expect(admitted.txInclusion.nativeTxId).toBe(
      artifact.transactions[artifact.selectedTransactionIndex]?.nodeTxId,
    );
  });

  it("rejects substituted roots, proofs, fields, and verdicts", async () => {
    const { artifact } = await preparedArtifact();
    await expect(
      admitProductionCanonicalDecodabilityArtifactV1({
        ...artifact,
        transactionsPhasRoot: "ff".repeat(32),
      }),
    ).rejects.toThrow("transactions PHAS root changed");
    await expect(
      admitProductionCanonicalDecodabilityArtifactV1({
        ...artifact,
        txMembershipProofCbor: "d87980",
      }),
    ).rejects.toThrow("transaction proof changed");
    await expect(
      admitProductionCanonicalDecodabilityArtifactV1({
        ...artifact,
        selectedFieldIndex: 1,
      }),
    ).rejects.toThrow("selected verdict changed");
    await expect(
      admitProductionCanonicalDecodabilityArtifactV1({
        ...artifact,
        selectedVerdict: artifact.selectedVerdict + 1,
      }),
    ).rejects.toThrow("selected verdict changed");
  });

  it("routes the authenticated raw defect through one-DA-fetch header classification", async () => {
    const { evidence, fixture, observation } = await preparedArtifact();
    const classifier = await createProductionHeaderClassifierV1({
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      replayer: CANONICAL_DECODABILITY_COMPLETE_CANONICAL_REPLAY_V1,
      releaseFinalityAuthority: finalityAuthority(),
    });
    const authenticatedObservationDigest =
      await authenticatedStateQueueObservationDigestV1({
        observation,
        minimumConfirmationDepth: 30,
      });
    let fetchCount = 0;
    const source: RetainedDaPayloadSource = {
      sourceId: "libp2p",
      fetchPayloadByHeaderHash: async () => {
        fetchCount += 1;
        return {
          ok: true,
          provenance: {
            trustClass: "public_or_permissionless_da",
            sourceId: "libp2p/q17-peer",
            grade: "security",
          },
          sourceId: "libp2p",
          sourcePeerId: "q17-peer",
          payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
          attempts: [],
        };
      },
    };
    const decision = await classifyProductionHeaderV1({
      classifier,
      observation,
      authenticatedObservationDigest,
      sources: [source],
    });
    expect(fetchCount).toBe(1);
    expect(decision).toMatchObject({
      decision: "fault_detected",
      category: "canonicalDecodability",
      violationId: "canonical-decodability",
      position: evidence.selected.transactionIndex.toString(),
    });
  });
});
