import {
  decodeMidgardNativeTxCanonical,
  decodeSingleCbor,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  encodeCbor,
} from "@al-ft/midgard-core";
import {
  type L2TransactionSource,
  L2TransactionSource as L2TransactionSourceCodec,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence-v1.js";
import { canonicalDecodabilityRawBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-decodability-raw-evidence-v1.js";
import type { RetainedDaPayloadSource } from "../src/transition-trace/fetch.js";
import { CANONICAL_DECODABILITY_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay-v1.js";
import {
  admitCanonicalDecodabilityArtifact,
  prepareCanonicalDecodabilityArtifact,
} from "../src/workflow/production-canonical-decodability-v1.js";
import {
  authenticatedStateQueueObservationDigest,
  classifyHeader,
  createHeaderClassifier,
} from "../src/workflow/production-header-classifier-v1.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  type FraudProofReleaseFinalityAuthority,
} from "../src/workflow/release-finality-policy-v1.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  type FixtureTransaction,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture-v1.js";

const DEPLOYMENT_FINGERPRINT = "d7".repeat(32);
const RELEASE_FINALITY_POLICY = {
  confirmationDepth: 30,
  automaticRecoveryMaxDepth: 2160,
  deepRollbackPolicy: "automated_rewind_replay_incident-v1",
} as const;

const finalityAuthority = (): FraudProofReleaseFinalityAuthority => ({
  authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  verifyForWorkflow: async () => ({
    schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
    deploymentIdentityDigest: DEPLOYMENT_FINGERPRINT,
    releaseIdentityDigest: "e7".repeat(32),
    policyDigest: computeFraudProofReleaseFinalityPolicyDigest(
      RELEASE_FINALITY_POLICY,
    ),
    policy: RELEASE_FINALITY_POLICY,
  }),
});

const malformedOutputsTransaction = (): FixtureTransaction => {
  const valid = buildFixtureTransaction({
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
  expect(() => decodeMidgardNativeTxCanonical(canonicalCbor)).toThrow(
    "outputs is not a canonical §5.1 field preimage",
  );
  const material = deriveMidgardNativeTxFaultEvidenceMaterial(canonicalCbor);
  const txId = material.transactionId.toString("hex");
  const source: L2TransactionSource = {
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
      Data.to(source, L2TransactionSourceCodec),
      "hex",
    ),
  };
};

const preparedArtifact = async () => {
  const fixture = await buildCanonicalBlockFixture({
    transactions: [
      buildFixtureTransaction({
        spendInputs: [outRefCbor(0x51, 0n)],
        fee: 1n,
      }),
      malformedOutputsTransaction(),
    ],
  });
  const observation = authenticatedHeaderObservation(fixture);
  const daProvenance = {
    trustClass: "public_or_permissionless_da",
    sourceId: "libp2p/production-canonical-decodability-test",
    grade: "security",
  } as const;
  await expect(
    canonicalBlockEvidenceFromVerifiedPayload({
      observation,
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
      daProvenance,
    }),
  ).rejects.toMatchObject({ code: "authenticatedCommittedFieldDefect" });
  const evidence =
    await canonicalDecodabilityRawBlockEvidenceFromVerifiedPayload({
      observation,
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
      daProvenance,
    });
  return {
    fixture,
    observation,
    evidence,
    artifact: await prepareCanonicalDecodabilityArtifact(evidence),
  };
};

describe("production canonical-decodability public-evidence workflow V1", () => {
  it("selects the exact malformed field and replays its source-root membership", async () => {
    const { artifact, evidence } = await preparedArtifact();
    const admitted = await admitCanonicalDecodabilityArtifact(artifact);
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
      admitCanonicalDecodabilityArtifact({
        ...artifact,
        transactionsPhasRoot: "ff".repeat(32),
      }),
    ).rejects.toThrow("transactions PHAS root changed");
    await expect(
      admitCanonicalDecodabilityArtifact({
        ...artifact,
        txMembershipProofCbor: "d87980",
      }),
    ).rejects.toThrow("transaction proof changed");
    await expect(
      admitCanonicalDecodabilityArtifact({
        ...artifact,
        selectedFieldIndex: 1,
      }),
    ).rejects.toThrow("selected verdict changed");
    await expect(
      admitCanonicalDecodabilityArtifact({
        ...artifact,
        selectedVerdict: artifact.selectedVerdict + 1,
      }),
    ).rejects.toThrow("selected verdict changed");
  });

  it("routes the authenticated raw defect through one-DA-fetch header classification", async () => {
    const { evidence, fixture, observation } = await preparedArtifact();
    const classifier = await createHeaderClassifier({
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      replayer: CANONICAL_DECODABILITY_COMPLETE_CANONICAL_REPLAY,
      releaseFinalityAuthority: finalityAuthority(),
    });
    const authenticatedObservationDigest =
      await authenticatedStateQueueObservationDigest({
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
    const decision = await classifyHeader({
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
