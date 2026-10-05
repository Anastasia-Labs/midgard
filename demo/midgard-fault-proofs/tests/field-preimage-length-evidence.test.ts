import * as SDK from "@al-ft/midgard-sdk";
import { type UTxO } from "@lucid-evolution/lucid";
import { expect, it, vi } from "vitest";

import { fetchFraudProofEvidence } from "../src/evidence/fraud-proof-evidence.js";
import {
  planFieldPreimageLengthCarriage,
  resolveFieldPreimageLengthCarriage,
} from "../src/field-preimage-length-mismatch/carriage.js";
import {
  detectAuthenticatedFieldPreimageLengthEvidence,
  detectFieldPreimageLengthCompleteReplay,
  fieldPreimageLengthEvidenceFromCanonicalBlock,
  fieldPreimageLengthEvidenceFromVerifiedPayload,
} from "../src/field-preimage-length-mismatch/evidence.js";
import { forcedFieldPreimageLengthRawFinding } from "../src/field-preimage-length-mismatch/evidence-forced.js";
import {
  fieldLengthForcedFixture,
  forcedLengthMismatchFixture,
  normalMismatchWithMalformedForcedFixture,
  opaqueForcedLengthMismatchFixture,
} from "./field-preimage-length-evidence.fixture.js";
import { bound, outRef } from "./field-preimage-length-recovery.bound.js";
import {
  authenticatedObservation,
  reconstruct,
  retainedSource,
} from "./transition-trace-challenger.build-payload-fixture.js";

it("routes a committed forced wrong acceptance after full reconstruction refuses its forged length vector", async () => {
  const fixture = await forcedLengthMismatchFixture();
  await expect(reconstruct(fixture)).rejects.toMatchObject({
    code: "malformedPayload",
    message: expect.stringContaining("forced_transactions["),
  });
  const routed = await fetchFraudProofEvidence({
    observation: authenticatedObservation(fixture),
    sources: [retainedSource(fixture)],
  });
  expect(routed.kind).toBe("field_preimage_length_mismatch");
  if (routed.kind !== "field_preimage_length_mismatch")
    throw new Error("expected field-length route");
  const evidence = routed.evidence;
  expect(evidence.prepared).toMatchObject({
    sourceKind: "forced",
    direction: "wrongfulAcceptance",
    fieldIndex: 0,
  });
  expect(evidence.prepared.declaredLength).toBe(
    evidence.prepared.actualLength + 1,
  );
  expect(evidence.stageEvidence).toMatchObject({
    forcedDirection: 0n,
    forcedHeader: fixture.header,
    forcedMembership: {
      root: fixture.header.forcedTransactionsRoot,
      value: { verdict: "ForcedTxValid" },
    },
  });
  const direct = await detectAuthenticatedFieldPreimageLengthEvidence({
    observation: authenticatedObservation(fixture),
    sources: [retainedSource(fixture)],
  });
  expect(direct.prepared).toEqual(evidence.prepared);
  const workflow = (
    await bound(fixture.headerHash, {
      value: {
        kind: "not_started",
        stateQueueBlockOutRef: outRef("66"),
      },
    })
  ).workflow;
  const planned = planFieldPreimageLengthCarriage({ workflow, evidence });
  expect(planned.sourceKind).toBe(1n);
});

it("does not manufacture forced evidence under a different counted root", async () => {
  const fixture = await forcedLengthMismatchFixture();
  const observation = authenticatedObservation(fixture);
  await expect(
    forcedFieldPreimageLengthRawFinding({
      headerHash: fixture.headerHash,
      header: {
        ...fixture.header,
        forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      },
      body: fixture.payload.block_body,
    }),
  ).rejects.toThrow("forced source root differs from the L1 header");
  await expect(
    fieldPreimageLengthEvidenceFromVerifiedPayload({
      observation: {
        ...observation,
        header: {
          ...observation.header,
          forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        },
      },
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    }),
  ).rejects.toThrow("authenticated L1 header");
});

it("prepares the classified subject among multiple real forced contradictions", async () => {
  const fixture = await fieldLengthForcedFixture(true);
  const routed = await fetchFraudProofEvidence({
    observation: authenticatedObservation(fixture),
    sources: [retainedSource(fixture)],
  });
  if (routed.kind !== "canonical_block")
    throw new Error("expected canonical block");
  const detections = detectFieldPreimageLengthCompleteReplay(routed.evidence);
  expect(detections).toHaveLength(2);
  const first = await fieldPreimageLengthEvidenceFromCanonicalBlock(
    routed.evidence,
  );
  const second = await fieldPreimageLengthEvidenceFromCanonicalBlock(
    routed.evidence,
    detections[1],
  );
  expect(first.stageEvidence.forcedMembership?.key).toEqual(
    routed.evidence.reconstruction.forcedTransactions[0]!.key,
  );
  expect(second.stageEvidence.forcedMembership?.key).toEqual(
    routed.evidence.reconstruction.forcedTransactions[1]!.key,
  );
  expect(first.prepared.evidenceDigest).not.toBe(
    second.prepared.evidenceDigest,
  );
  await expect(
    fieldPreimageLengthEvidenceFromCanonicalBlock(routed.evidence, {
      ...detections[1]!,
      subjectEventKeyCbors: detections[0]!.subjectEventKeyCbors,
    }),
  ).rejects.toThrow("0 exact forced findings");
});

it("proves a normal mismatch independently of a malformed committed forced vector", async () => {
  const fixture = await normalMismatchWithMalformedForcedFixture();
  await expect(reconstruct(fixture)).rejects.toMatchObject({
    code: "malformedPayload",
    message: expect.stringContaining("transactions["),
  });
  const routed = await fetchFraudProofEvidence({
    observation: authenticatedObservation(fixture),
    sources: [retainedSource(fixture)],
  });
  if (routed.kind !== "field_preimage_length_mismatch")
    throw new Error("expected field-length route");
  expect(routed.evidence.prepared).toMatchObject({
    sourceKind: "normal",
    direction: "wrongfulAcceptance",
    fieldIndex: 0,
  });
  expect(routed.evidence.stageEvidence.acceptedInclusion?.nativeTxId).toBe(
    fixture.payload.block_body.transactions[0]![0],
  );
});

it("retains both branch failures when malformed forced data and truthful normal lengths prove no fault", async () => {
  const fixture = await normalMismatchWithMalformedForcedFixture(false);
  const failure = await fieldPreimageLengthEvidenceFromVerifiedPayload({
    observation: authenticatedObservation(fixture),
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
  }).catch((error: unknown) => error);
  expect(failure).toBeInstanceOf(AggregateError);
  if (!(failure instanceof AggregateError))
    throw new Error("expected both branch failures");
  expect(failure.errors).toHaveLength(2);
  expect(String(failure.errors[1])).toContain("0 exact accepted findings");
});

it.each([
  [2, "Inline"],
  [14_337, "RawUtxo"],
  [15_149, "Certified"],
] as const)(
  "preserves opaque forced preimage bytes through full DA preparation and %s-byte %s carriage",
  async (length, tier) => {
    const preimage = Buffer.alloc(length);
    preimage.set(Buffer.from("8180", "hex"));
    const fixture = await opaqueForcedLengthMismatchFixture(preimage);
    const routed = await fetchFraudProofEvidence({
      observation: authenticatedObservation(fixture),
      sources: [retainedSource(fixture)],
    });
    if (routed.kind !== "field_preimage_length_mismatch")
      throw new Error("expected field-length route");
    const evidence = routed.evidence;
    expect(evidence.prepared).toMatchObject({
      sourceKind: "forced",
      actualLength: length,
      declaredLength: length + 1,
      preimageHex: preimage.toString("hex"),
    });
    const workflow = (
      await bound(fixture.headerHash, {
        value: { kind: "not_started", stateQueueBlockOutRef: outRef("66") },
      })
    ).workflow;
    const planned = planFieldPreimageLengthCarriage({ workflow, evidence });
    expect(planned.plan.tier).toBe(tier);
    expect(planned.preimage).toEqual(preimage);
    const publications: UTxO[] = planned.plan.publications.map(
      (publication, index) => ({
        address: workflow.config.signer.address,
        txHash: "67".repeat(32),
        outputIndex: index,
        assets: { lovelace: 2_000_000n },
        datum: SDK.fieldPreimagePublicationDatumCbor(publication.bytes),
      }),
    );
    const certificate: UTxO[] =
      tier === "Certified"
        ? [
            {
              address: workflow.config.signer.address,
              txHash: "68".repeat(32),
              outputIndex: 0,
              assets: {
                lovelace: 2_000_000n,
                [workflow.config.contracts.fieldPreimageCertificate.policyId +
                SDK.FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX]: 1n,
              },
              datum: SDK.deriveFieldPreimageCertification(planned.plan)
                .datumCbor,
            },
          ]
        : [];
    vi.mocked(workflow.config.lucid.utxosAt).mockResolvedValue([
      ...publications,
      ...certificate,
    ]);
    const resolved = await resolveFieldPreimageLengthCarriage({
      workflow,
      evidence,
    });
    expect(resolved.carriageReferences).toHaveLength(
      publications.length + certificate.length,
    );
    const claim = resolved.claimResolver(resolved.carriageReferences);
    if (!("BodyFieldClaim" in claim))
      throw new Error("expected body field claim");
    expect(Object.keys(claim.BodyFieldClaim.carriage)).toEqual([tier]);
    if (tier === "Inline")
      expect(claim.BodyFieldClaim.carriage).toEqual({
        Inline: { preimage: preimage.toString("hex") },
      });
    const mutated = Buffer.from(preimage);
    mutated[0] = 0;
    expect(() =>
      planFieldPreimageLengthCarriage({
        workflow,
        evidence: {
          ...evidence,
          prepared: {
            ...evidence.prepared,
            preimageHex: mutated.toString("hex"),
          },
        },
      }),
    ).toThrow("authenticated commitment or length");
  },
);
