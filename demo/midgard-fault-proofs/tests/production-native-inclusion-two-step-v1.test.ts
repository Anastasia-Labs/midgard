import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  adjudicateMidgardNativeTxFullV1Validity,
  computeMidgardNativeTxIdV1,
  deriveMidgardNativeTxFaultEvidenceMaterialV1,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardSpendInputItemV1,
  midgardFieldCommitmentV1,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayloadV1 } from "../src/evidence/canonical-block-evidence-v1.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import { eventKeyFingerprint } from "../src/transition-trace/reconstruct.js";
import { classifyCanonicalBlockViolationsV1 } from "../src/workflow/classification-v1.js";
import {
  INVALID_RANGE_COMPLETE_CANONICAL_REPLAY_V1,
  ZERO_INPUT_COMPLETE_CANONICAL_REPLAY_V1,
} from "../src/workflow/complete-replay-v1.js";
import {
  admitProductionNativeInclusionTwoStepArtifactV1,
  prepareProductionNativeInclusionTwoStepArtifactV1,
  PRODUCTION_NATIVE_INCLUSION_TWO_STEP_ARTIFACT_V1,
} from "../src/workflow/production-native-inclusion-two-step-v1.js";
import {
  prepareZeroInputEvidenceV1,
  ZeroInputVerdictSubjectV1Schema,
} from "../src/zero-input/family-v1.js";
import { ZeroInputForcedSourcePayloadV1Schema } from "../src/zero-input/schemas-v1.js";
import {
  authenticatedHeaderObservationV1,
  buildCanonicalBlockFixtureV1,
  buildFixtureTransactionV1,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture-v1.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";

const evidenceFor = async () => {
  const fixture = await buildCanonicalBlockFixtureV1({
    transactions: [
      buildFixtureTransactionV1({
        spendInputs: [outRefCbor(12, 0n)],
        fee: 1n,
      }),
      buildFixtureTransactionV1({
        spendInputs: [outRefCbor(13, 0n)],
        fee: 2n,
        validityIntervalStart: 1n,
      }),
      buildFixtureTransactionV1({ spendInputs: [], fee: 3n }),
    ],
  });
  return await canonicalBlockEvidenceFromVerifiedPayloadV1({
    observation: authenticatedHeaderObservationV1(fixture),
    payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "libp2p/production-native-inclusion-test",
      grade: "security",
    },
  });
};

describe("production invalid-range/zero-input public-evidence artifacts V1", () => {
  it("prepares the exact classified invalid-range transaction and replays its MPF proof", async () => {
    const evidence = await evidenceFor();
    const replay =
      await INVALID_RANGE_COMPLETE_CANONICAL_REPLAY_V1.replay(evidence);
    const classification = await classifyCanonicalBlockViolationsV1({
      evidence,
      detections: replay.detections,
    });
    if (
      classification.decision !== "fault_detected" ||
      classification.category !== "invalidRange"
    ) {
      throw new Error("fixture did not classify as invalidRange");
    }
    const artifact = await prepareProductionNativeInclusionTwoStepArtifactV1({
      category: "invalidRange",
      evidence,
      classification,
    });
    expect(artifact).toMatchObject({
      category: "invalidRange",
      position: Number(classification.selected.position),
      blockSlot: "0",
      violationReason: "starts-after-block-slot",
    });
    expect(admitProductionNativeInclusionTwoStepArtifactV1(artifact)).toEqual(
      expect.objectContaining({ artifact }),
    );
  });

  it("prepares the exact classified zero-input transaction", async () => {
    const evidence = await evidenceFor();
    const replay =
      await ZERO_INPUT_COMPLETE_CANONICAL_REPLAY_V1.replay(evidence);
    const classification = await classifyCanonicalBlockViolationsV1({
      evidence,
      detections: replay.detections,
    });
    if (
      classification.decision !== "fault_detected" ||
      classification.category !== "zeroInput"
    ) {
      throw new Error("fixture did not classify as zeroInput");
    }
    const artifact = await prepareProductionNativeInclusionTwoStepArtifactV1({
      category: "zeroInput",
      evidence,
      classification,
    });
    expect(artifact).toMatchObject({
      category: "zeroInput",
      position: Number(classification.selected.position),
      blockSlot: null,
      violationReason: null,
    });
    expect(admitProductionNativeInclusionTwoStepArtifactV1(artifact)).toEqual(
      expect.objectContaining({ artifact }),
    );
  });

  it("rejects substituted roots, proofs, detection identities, and family fields", async () => {
    const evidence = await evidenceFor();
    const replay =
      await INVALID_RANGE_COMPLETE_CANONICAL_REPLAY_V1.replay(evidence);
    const classification = await classifyCanonicalBlockViolationsV1({
      evidence,
      detections: replay.detections,
    });
    if (
      classification.decision !== "fault_detected" ||
      classification.category !== "invalidRange"
    ) {
      throw new Error("fixture did not classify as invalidRange");
    }
    const artifact = await prepareProductionNativeInclusionTwoStepArtifactV1({
      category: "invalidRange",
      evidence,
      classification,
    });
    expect(() =>
      admitProductionNativeInclusionTwoStepArtifactV1({
        ...artifact,
        transactionsPhasRoot: "ff".repeat(32),
      }),
    ).toThrow("does not open its PHAS root");
    expect(() =>
      admitProductionNativeInclusionTwoStepArtifactV1({
        ...artifact,
        txMembershipProofCbor: "d87980",
      }),
    ).toThrow();
    expect(() =>
      admitProductionNativeInclusionTwoStepArtifactV1({
        ...artifact,
        detectionId: `${artifact.detectionId}-substituted`,
      }),
    ).toThrow("does not re-derive its selected violation");
    expect(() =>
      admitProductionNativeInclusionTwoStepArtifactV1({
        ...artifact,
        category: "zeroInput",
        blockSlot: null,
        violationReason: null,
      }),
    ).toThrow("zero-input artifact does not re-derive");
  });

  it("admits only a re-derived forced EmptyInputs subject and rejects caller authority", async () => {
    const input = encodeMidgardSpendInputItemV1({
      txId: Buffer.from("71".repeat(32), "hex"),
      outputIndex: 0,
    });
    const invalid = adjudicateMidgardNativeTxFullV1Validity(
      makeNativeTx({ spendInputCbors: [input], fee: 0n }),
      "TxIsInvalid",
    );
    const material = deriveMidgardNativeTxFaultEvidenceMaterialV1(
      encodeMidgardNativeTxCanonicalV1(invalid),
    );
    const transactionId = computeMidgardNativeTxIdV1(invalid).toString("hex");
    const key = { transactionId: "72".repeat(32), outputIndex: 0n };
    const leaf = {
      tx_id: transactionId,
      source: {
        compact_cbor: material.proofSource.compactCbor.toString("hex"),
        witness_set_compact_cbor:
          material.proofSource.witnessSetCompactCbor.toString("hex"),
        field_preimage_lengths_cbor:
          material.proofSource.fieldPreimageLengthsCbor.toString("hex"),
      },
      verdict: { ForcedTxInvalid: { reason: "EmptyInputs" } },
    } as const;
    const keyBytes = Buffer.from(
      Data.to(key as never, SDK.OutputReferenceSchema as never),
      "hex",
    );
    const valueBytes = Buffer.from(
      Data.to(leaf as never, SDK.ForcedInclusionTxV1Schema as never),
      "hex",
    );
    const root = await buildCountedRoot(SDK.ROOT_DOMAINS.forcedTransactionsV1, [
      { key: keyBytes, value: valueBytes },
    ]);
    const store = new Store(undefined);
    await store.ready();
    const trie = new Trie(store);
    await trie.insert(keyBytes, valueBytes);
    const proof = await trie.prove(keyBytes);
    const membership = {
      domain: root.domain,
      root: root.root,
      phas_root: root.phasRoot,
      count: root.count,
      key,
      value: leaf,
      proof: Data.from(proof.toCBOR().toString("hex"), SDK.ProofSchema),
    };
    const base = await buildCanonicalBlockFixtureV1({ transactions: [] });
    const header = {
      ...base.header,
      forcedTransactionsRoot: root.root,
      forcedTransactionCount: 1n,
    };
    const boundHeaderHash = await Effect.runPromise(
      SDK.hashBlockHeaderV1(header),
    );
    const subject = SDK.forcedVerdictSubjectV1({
      transactionId,
      sourceKey: key,
      rejectionReason: "EmptyInputs",
    });
    const field = material.fieldPreimages[0]!;
    const evidence = prepareZeroInputEvidenceV1({
      finding: { subject },
      inputFieldPreimage: field,
      committedFieldHashHex: midgardFieldCommitmentV1(field).toString("hex"),
    });
    const artifact = {
      schemaVersion: PRODUCTION_NATIVE_INCLUSION_TWO_STEP_ARTIFACT_V1,
      category: "zeroInput",
      headerHash: boundHeaderHash,
      detectionId: `zero-input:forced:0:${transactionId}`,
      position: 0,
      blockSlot: null,
      violationReason: null,
      nativeTxId: transactionId,
      nativeTxCompactCbor: leaf.source.compact_cbor,
      l2TransactionSourceCbor: Data.to(
        { tx_id: transactionId, source: leaf.source } as never,
        SDK.L2TransactionSourceV1 as never,
      ),
      transactionsPhasRoot: "00".repeat(32),
      txMembershipProofCbor: "",
      sourceKind: "forced",
      subjectCbor: Data.to(
        subject as never,
        ZeroInputVerdictSubjectV1Schema as never,
      ),
      inputFieldPreimageCbor: evidence.inputFieldPreimageCbor,
      inputFieldCommitment: evidence.inputFieldCommitment,
      forcedSourceCbor: Data.to(
        { header, membership, direction: 1n } as never,
        ZeroInputForcedSourcePayloadV1Schema as never,
      ),
    } as const;
    expect(admitProductionNativeInclusionTwoStepArtifactV1(artifact)).toEqual(
      expect.objectContaining({
        artifact,
        inclusion: null,
        zeroInputEvidence: expect.objectContaining({ inputCount: 1 }),
      }),
    );
    expect(() =>
      admitProductionNativeInclusionTwoStepArtifactV1({
        ...artifact,
        verdict: "caller-supplied",
      }),
    ).toThrow("missing or unknown fields");
    expect(() =>
      admitProductionNativeInclusionTwoStepArtifactV1({
        ...artifact,
        subjectCbor: Data.to(
          SDK.acceptedVerdictSubjectV1(transactionId) as never,
          ZeroInputVerdictSubjectV1Schema as never,
        ),
      }),
    ).toThrow(/injected its verdict subject|source changed/u);
    const forcedSource = Data.from(
      artifact.forcedSourceCbor,
      ZeroInputForcedSourcePayloadV1Schema as never,
    ) as {
      membership: typeof membership;
      header: typeof header;
      direction: bigint;
    };
    expect(() =>
      admitProductionNativeInclusionTwoStepArtifactV1({
        ...artifact,
        forcedSourceCbor: Data.to(
          {
            ...forcedSource,
            membership: {
              ...forcedSource.membership,
              value: {
                ...forcedSource.membership.value,
                verdict: { ForcedTxInvalid: { reason: "NetworkIdMismatch" } },
              },
            },
          } as never,
          ZeroInputForcedSourcePayloadV1Schema as never,
        ),
      }),
    ).toThrow("changed authenticated leaf");

    const canonical = await canonicalBlockEvidenceFromVerifiedPayloadV1({
      observation: authenticatedHeaderObservationV1(base),
      payloadEnvelopeCbor: base.payloadEnvelopeCbor,
      daProvenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: "libp2p/forced-zero-input-authority-test",
        grade: "security",
      },
    });
    const eventKey = {
      ForcedTransactionEventKey: { tx_order_id: key },
    } as const;
    const fingerprint = eventKeyFingerprint(eventKey);
    const forcedEntry = {
      key,
      value: leaf,
      keyBytes,
      valueBytes,
      fullTransactionCbor: encodeMidgardNativeTxCanonicalV1(invalid),
    };
    const block = {
      ...canonical,
      header,
      headerHash: boundHeaderHash,
      reconstruction: {
        ...canonical.reconstruction,
        header,
        headerHash: boundHeaderHash,
        forcedTransactions: [forcedEntry],
        sourceEvents: [
          {
            phase: "ForcedTransaction",
            eventKey,
            fingerprint,
            entry: forcedEntry,
          },
        ],
        sourceEventsByFingerprint: new Map([
          [
            fingerprint,
            {
              phase: "ForcedTransaction",
              eventKey,
              fingerprint,
              entry: forcedEntry,
            },
          ],
        ]),
        rootData: {
          ...canonical.reconstruction.rootData,
          forcedTransactions: root,
        },
      },
    } as never;
    const detection = {
      detectionId: `zero-input:forced:0:${transactionId}`,
      headerHash: boundHeaderHash,
      violationId: "zero-input",
      position: 0n,
      diagnostic: "authenticated wrongful EmptyInputs rejection",
    };
    const prepared = await prepareProductionNativeInclusionTwoStepArtifactV1({
      category: "zeroInput",
      evidence: block,
      classification: {
        schemaVersion: "midgard-fraud-proof-classification-v1",
        decision: "fault_detected",
        headerHash: boundHeaderHash,
        category: "zeroInput",
        selected: detection,
        detections: [detection],
        unprovableGaps: [],
      },
    });
    expect(prepared).toMatchObject({
      detectionId: detection.detectionId,
      sourceKind: "forced",
      subjectCbor: artifact.subjectCbor,
      inputFieldCommitment: artifact.inputFieldCommitment,
    });
    expect(admitProductionNativeInclusionTwoStepArtifactV1(prepared)).toEqual(
      expect.objectContaining({
        inclusion: null,
        zeroInputEvidence: expect.objectContaining({ inputCount: 1 }),
      }),
    );
  });
});
