import {
  decodeSingleCbor,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  encodeCbor,
  encodeMidgardNativeTxCanonical,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import { acceptedVerdictSubject } from "@al-ft/midgard-sdk";
import { type Script } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { prepareFieldItemWidthEvidence } from "../src/field-item-width-illegal/field-item-width-illegal.js";
import {
  deriveFieldItemWidthIllegalAuthenticatedSource,
  deriveFieldItemWidthIllegalEvidenceFromCanonicalBlock,
  detectFieldItemWidthIllegalCompleteReplay,
  FIELD_ITEM_WIDTH_ILLEGAL_FAMILY_DEFINITION,
  prepareFieldItemWidthIllegalRecoveryMaterial,
} from "../src/field-item-width-illegal/workflow.js";
import {
  buildTrieView,
  encodeL2TransactionSourceValue,
} from "../src/prepare-double-spend.js";
import { committedFieldShapeScenarioMaterial } from "./support/committed-field-shape-emulator.js";
import { buildInvalidForcedTransitionTraceFixture } from "./support/submit-init-emulator-fixtures.js";

const script = (byte: string): Script => ({
  type: "PlutusV3",
  script: byte.repeat(8),
});

const fieldCarriage = async (
  prepared: ReturnType<
    typeof deriveFieldItemWidthIllegalEvidenceFromCanonicalBlock
  >,
  source: { nativeTxCompactCbor: string; witnessSetCompactCbor: string },
) => {
  const requirement =
    await FIELD_ITEM_WIDTH_ILLEGAL_FAMILY_DEFINITION.fieldCarriage![0]!.requirementForAction(
      {
        runtime: {
          config: {
            signer: { paymentKeyHash: "a".repeat(56) },
            referenceScripts: {},
          },
          material: { require: () => ({ evidence: prepared, source }) },
        },
        binding: {
          fieldPreimageCertificate: {
            policyId: "b".repeat(56),
            mintingScript: script("1"),
          },
        },
      } as never,
      { action: { input: { stage: "step_02" } }, artifact: {} } as never,
    );
  if (requirement === null || "planned" in requirement) return requirement;
  throw new Error("expected a planned field-carriage requirement");
};

describe("fieldItemWidthIllegal manifest-bound production workflow", () => {
  it.each([
    [-1n, 0n],
    [4n, -1n],
    [1n << 80n, 0n],
    [4n, 1n << 80n],
    [4n, 0n],
  ])(
    "public replay and recovery preserve exact forced coordinate %s/%s",
    async (fieldIndex, itemIndex) => {
      const fixture = await buildInvalidForcedTransitionTraceFixture({
        operatorVkey: "b".repeat(56),
        now: 1_900_000_000_000,
        fieldItemWidthIllegalCoordinate: { fieldIndex: 2, itemIndex: 0 },
      });
      const original = fixture.reconstruction.forcedTransactions[0]!;
      const block = {
        headerHash: fixture.reconstruction.headerHash,
        transactions: [],
        reconstruction: {
          ...fixture.reconstruction,
          forcedTransactions: [
            {
              ...original,
              value: {
                ...original.value,
                verdict: {
                  ForcedTxInvalid: {
                    reason: {
                      FieldItemWidthIllegal: {
                        field_index: fieldIndex,
                        item_index: itemIndex,
                      },
                    },
                  },
                },
              },
            },
          ],
        },
      };
      expect(
        detectFieldItemWidthIllegalCompleteReplay(block as never),
      ).toHaveLength(1);
      const derived = deriveFieldItemWidthIllegalEvidenceFromCanonicalBlock(
        block as never,
      );
      expect(derived.fieldIndex).toBe(fieldIndex);
      expect(derived.itemIndex).toBe(itemIndex);
      const carriage = await fieldCarriage(derived, {
        nativeTxCompactCbor: original.value.submitted_source.compact_cbor,
        witnessSetCompactCbor:
          original.value.submitted_source.witness_set_compact_cbor,
      });
      if (fieldIndex < 0n || fieldIndex > 7n || itemIndex < 0n)
        expect(carriage).toBeNull();
      else
        expect(carriage?.planned.preimage.toString("hex")).toBe(
          derived.fieldPreimageHex,
        );
    },
  );

  it("public replay detects an accepted forced transaction's malformed fixed wrapper", async () => {
    const fixture = await buildInvalidForcedTransitionTraceFixture({
      operatorVkey: "b".repeat(56),
      now: 1_900_000_000_000,
      fieldItemWidthIllegalCoordinate: { fieldIndex: 2, itemIndex: 0 },
    });
    const original = fixture.reconstruction.forcedTransactions[0]!;
    const envelope = decodeSingleCbor(
      original.fullTransactionCbor,
    ) as unknown[];
    const body = envelope[1] as unknown[];
    // Same fixed extent as a canonical outref, with a bad committed wrapper.
    body[0] = Buffer.from("815726825820" + "11".repeat(32) + "190000", "hex");
    const bytes = encodeCbor(envelope);
    const material = deriveMidgardForcedTxFaultEvidenceMaterial(bytes);
    const block = {
      headerHash: fixture.reconstruction.headerHash,
      transactions: [],
      reconstruction: {
        ...fixture.reconstruction,
        forcedTransactions: [
          {
            ...original,
            fullTransactionCbor: bytes,
            value: {
              ...original.value,
              tx_id: material.transactionId.toString("hex"),
              verdict: "ForcedTxValid",
              submitted_source: {
                compact_cbor: material.proofSource.compactCbor.toString("hex"),
                witness_set_compact_cbor:
                  material.proofSource.witnessSetCompactCbor.toString("hex"),
                field_preimage_lengths_cbor:
                  material.proofSource.fieldPreimageLengthsCbor.toString("hex"),
              },
            },
          },
        ],
      },
    };
    expect(
      detectFieldItemWidthIllegalCompleteReplay(block as never),
    ).toHaveLength(1);
    const derived = deriveFieldItemWidthIllegalEvidenceFromCanonicalBlock(
      block as never,
    );
    expect(derived.fieldIndex).toBe(0n);
    expect(derived.fieldPreimageHex).toBe(
      material.fieldPreimages[0]!.toString("hex"),
    );
  });

  it("recovers accepted malformed input bytes without a ledger input decoder", async () => {
    const base = committedFieldShapeScenarioMaterial(
      "field-item-width-illegal",
    ).fullTx!;
    const envelope = decodeSingleCbor(
      encodeMidgardNativeTxCanonical(base),
    ) as unknown[];
    const body = envelope[1] as unknown[];
    body[0] = Buffer.from("815726825820" + "11".repeat(32) + "190000", "hex");
    const bytes = encodeCbor(envelope);
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(bytes);
    const nodeTxId = material.transactionId.toString("hex");
    const l2TransactionSourceCbor = encodeL2TransactionSourceValue({
      txId: nodeTxId,
      proofSource: material.proofSource,
    });
    const trie = await buildTrieView([
      {
        key: Buffer.from(nodeTxId, "hex"),
        value: Buffer.from(l2TransactionSourceCbor, "hex"),
      },
    ]);
    const block = {
      transactions: [
        { nodeTxId, txCbor: bytes.toString("hex"), l2TransactionSourceCbor },
      ],
      reconstruction: {
        forcedTransactions: [],
        rootData: { transactions: { phasRoot: trie.root } },
      },
      inclusionRootAuthentication: { sourceValuePhasRoot: trie.root },
    };
    const derived = deriveFieldItemWidthIllegalEvidenceFromCanonicalBlock(
      block as never,
    );
    expect(derived.fieldIndex).toBe(0n);
    const recovered = await deriveFieldItemWidthIllegalAuthenticatedSource({
      block: block as never,
      evidence: derived,
    });
    expect(recovered.acceptedInclusion?.l2TransactionSourceCbor).toBe(
      l2TransactionSourceCbor,
    );
    expect(recovered.nativeTxCompactCbor).toBe(
      material.proofSource.compactCbor.toString("hex"),
    );
    const carriage = await fieldCarriage(derived, recovered);
    expect(carriage?.planned.preimage).toEqual(material.fieldPreimages[0]);
  });

  it("public carriage preserves a bad witness wrapper under its authenticated witness hash", async () => {
    const base = committedFieldShapeScenarioMaterial(
      "field-item-width-illegal",
    ).fullTx!;
    const envelope = decodeSingleCbor(
      encodeMidgardNativeTxCanonical(base),
    ) as unknown[];
    (envelope[1] as unknown[])[2] = Buffer.from("80", "hex");
    const field = Buffer.from(
      "815864825820" + "11".repeat(32) + "5840" + "22".repeat(64),
      "hex",
    );
    (envelope[2] as unknown[])[0] = field;
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(
      encodeCbor(envelope),
    );
    const prepared = prepareFieldItemWidthEvidence({
      finding: {
        subject: acceptedVerdictSubject(material.transactionId.toString("hex")),
        fieldIndex: 7n,
        itemIndex: 0n,
      },
      fieldPreimage: field,
      committedFieldHashHex: midgardFieldCommitment(field).toString("hex"),
    });
    const source = {
      nativeTxCompactCbor: material.proofSource.compactCbor.toString("hex"),
      witnessSetCompactCbor:
        material.proofSource.witnessSetCompactCbor.toString("hex"),
    };
    const carriage = await fieldCarriage(prepared, source);
    expect(carriage?.planned.preimage).toEqual(field);
    if (carriage === null || !("witnessSetHash" in carriage.planned))
      throw new Error("expected an authenticated witness field opening");
    expect(carriage.planned.witnessSetHash).toBe(
      Buffer.from(material.compact.transactionWitnessSetHash).toString("hex"),
    );
    await expect(
      fieldCarriage(prepared, { ...source, witnessSetCompactCbor: "00" }),
    ).rejects.toThrow();
  });

  it("recovers the selected public detection when a block has two faulty transactions", async () => {
    const base = committedFieldShapeScenarioMaterial(
      "field-item-width-illegal",
    ).fullTx!;
    const first = encodeMidgardNativeTxCanonical(base);
    const envelope = decodeSingleCbor(first) as unknown[];
    (envelope[1] as unknown[])[3] = base.body.fee + 1n;
    const transactions = [first, encodeCbor(envelope)].map((bytes) => {
      const material = deriveMidgardNativeTxFaultEvidenceMaterial(bytes);
      const nodeTxId = material.transactionId.toString("hex");
      return {
        nodeTxId,
        txCbor: bytes.toString("hex"),
        l2TransactionSourceCbor: encodeL2TransactionSourceValue({
          txId: nodeTxId,
          proofSource: material.proofSource,
        }),
      };
    });
    const trie = await buildTrieView(
      transactions.map((transaction) => ({
        key: Buffer.from(transaction.nodeTxId, "hex"),
        value: Buffer.from(transaction.l2TransactionSourceCbor, "hex"),
      })),
    );
    const block = {
      headerHash: "a".repeat(56),
      transactions,
      reconstruction: {
        forcedTransactions: [],
        rootData: { transactions: { phasRoot: trie.root } },
      },
      inclusionRootAuthentication: { sourceValuePhasRoot: trie.root },
    };
    const detections = detectFieldItemWidthIllegalCompleteReplay(
      block as never,
    );
    expect(detections).toHaveLength(2);
    const selected = detections[1]!;
    const recovered = await prepareFieldItemWidthIllegalRecoveryMaterial(
      block as never,
      selected.detectionId,
    );
    expect(recovered.evidence.subject.transaction_id).toBe(
      transactions[1]!.nodeTxId,
    );
    expect(recovered.detectionId).toBe(selected.detectionId);
    await expect(
      prepareFieldItemWidthIllegalRecoveryMaterial(
        block as never,
        "substituted",
      ),
    ).rejects.toThrow(/exact findings/u);
  });
});
