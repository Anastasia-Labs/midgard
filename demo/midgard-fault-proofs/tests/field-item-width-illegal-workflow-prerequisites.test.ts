import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeSingleCbor,
  encodeCbor,
} from "@al-ft/midgard-core";
import {
  deriveMidgardForcedTxFaultEvidenceMaterial,
  encodeMidgardForcedTxCanonical,
} from "@al-ft/midgard-core/codec/forced";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import {
  deriveFieldItemWidthIllegalAuthenticatedSource,
  deriveFieldItemWidthIllegalEvidenceFromCanonicalBlock,
  detectFieldItemWidthIllegalCompleteReplay,
  prepareFieldItemWidthIllegalRecoveryMaterial,
} from "../src/field-item-width-illegal/workflow.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import {
  COMMITTED_FIELD_SHAPE_COMPLETE_CANONICAL_REPLAY,
  createCompleteCanonicalReplayUnion,
  FIELD_ITEM_WIDTH_ILLEGAL_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import { CanonicalReplayPrerequisiteError } from "../src/workflow/replay-prerequisite.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";

const forcedBytes = () => {
  const normal = buildFixtureTransaction({
    spendInputs: [outRefCbor(0x31, 0n)],
    outputs: [Buffer.from("00", "hex")],
    fee: 1n,
  });
  return encodeMidgardForcedTxCanonical(
    decodeMidgardNativeTxFullFromCanonicalCbor(normal.canonicalCbor),
  );
};

/** Inner fields remain raw; every subject passes real retained-DA admission. */
const admittedForcedSubjects = async (
  sources: readonly {
    bytes: Buffer;
    verdict: SDK.ForcedInclusionTxV1["verdict"];
  }[],
) => {
  const base = await buildCanonicalBlockFixture({ transactions: [] });
  const forced: SDK.DaPayloadEntry[] = [];
  const preimages: SDK.DaPayloadEntry[] = [];
  const steps: SDK.DaPayloadEntry[] = [];
  const events: SDK.DaPayloadEntry[] = [];
  const validations: SDK.DaPayloadEntry[] = [];
  const subjects = sources.map(({ bytes, verdict }, index) => {
    const material = deriveMidgardForcedTxFaultEvidenceMaterial(bytes);
    const key = { transactionId: "42".repeat(32), outputIndex: BigInt(index) };
    const event = { ForcedTransactionEventKey: { tx_order_id: key } };
    const leaf: SDK.ForcedInclusionTxV1 = {
      tx_id: material.transactionId.toString("hex"),
      submitted_source: {
        compact_cbor: material.proofSource.compactCbor.toString("hex"),
        witness_set_compact_cbor:
          material.proofSource.witnessSetCompactCbor.toString("hex"),
        field_preimage_lengths_cbor:
          material.proofSource.fieldPreimageLengthsCbor.toString("hex"),
      },
      verdict,
    };
    const keyCbor = Data.to(key, SDK.OutputReference);
    const eventCbor = Data.to(event, SDK.EventKey);
    forced.push([keyCbor, Data.to(leaf, SDK.ForcedInclusionTxV1)]);
    preimages.push([keyCbor, bytes.toString("hex")]);
    steps.push([
      Data.to(BigInt(index)),
      Data.to(
        {
          schema_version: 1n,
          step_index: BigInt(index),
          event_key: event,
          phase: "ForcedTransaction",
          pre_utxos_root: base.header.prevUtxosRoot,
          post_utxos_root: base.header.utxosRoot,
        },
        SDK.TransitionStep,
      ),
    ]);
    events.push([
      eventCbor,
      Data.to(
        { step_index: BigInt(index), phase: "ForcedTransaction" },
        SDK.EventToStepValue,
      ),
    ]);
    validations.push([
      eventCbor,
      Data.to(
        {
          schema_version: 1n,
          machine_version: 1n,
          trace_root: "1a".repeat(32),
          step_count: 1n,
          initial_state_hash: "1b".repeat(32),
          terminal_state_hash: "1c".repeat(32),
          verdict: verdict === "ForcedTxValid" ? "Accepted" : "Rejected",
          rejection_code_hash:
            verdict === "ForcedTxValid" ? "00".repeat(32) : "1d".repeat(32),
        },
        SDK.ValidationTraceDescriptor,
      ),
    ]);
    return { key, eventCbor, leaf };
  });
  const sorted = (entries: SDK.DaPayloadEntry[]) =>
    entries.sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0));
  const root = async (domain: SDK.RootDomain, entries: SDK.DaPayloadEntry[]) =>
    (
      await buildCountedRoot(
        domain,
        entries.map(([key, value]) => ({
          key: Buffer.from(key, "hex"),
          value: Buffer.from(value, "hex"),
        })),
      )
    ).root;
  const subjectCount = BigInt(sources.length);
  const counts = {
    ...base.payload.block_body.counts,
    forcedTransactionCount: subjectCount,
    totalEventCount: subjectCount,
    transitionStepCount: subjectCount,
    validationTraceCount: subjectCount,
  };
  const header: SDK.Header = {
    ...base.header,
    ...counts,
    // The header pins the validation-trace pair to the empty tree; the body
    // keeps the built members and their count.
    validationTraceCount: 0n,
    forcedTransactionsRoot: await root(
      SDK.ROOT_DOMAINS.forcedTransactionsV1,
      forced,
    ),
    transitionTraceRoot: await root(SDK.ROOT_DOMAINS.transitionTrace, steps),
    eventToStepRoot: await root(SDK.ROOT_DOMAINS.eventToStep, events),
    validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const payload: SDK.DaPayload = {
    ...base.payload,
    block_body: {
      ...base.payload.block_body,
      header,
      header_hash: headerHash,
      counts,
      forced_transactions: sorted(forced),
      forced_transaction_preimages: sorted(preimages),
      transition_trace: sorted(steps),
      event_to_step: sorted(events),
      validation_traces: sorted(validations),
    },
  };
  const block = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation({ header, headerHash }),
    payloadEnvelopeCbor: await wrapDaPayload(SDK.encodeDaPayload(payload), {
      mode: "identity",
    }),
    daProvenance: {
      trustClass: "public_or_permissionless_da",
      sourceId: "fiwi-prerequisite-regression",
      grade: "security",
    },
  });
  return { block, subjects };
};

const admittedPair = async (
  malformedFirst: boolean,
  malformedVerdict: SDK.ForcedInclusionTxV1["verdict"],
) => {
  const legal = forcedBytes();
  const envelope = decodeSingleCbor(legal) as unknown[];
  (envelope[1] as unknown[])[0] = Buffer.from("81", "hex");
  const sources = [
    { bytes: encodeCbor(envelope), verdict: malformedVerdict },
    {
      bytes: legal,
      verdict: {
        ForcedTxInvalid: {
          reason: {
            FieldItemWidthIllegal: { field_index: 2n, item_index: 0n },
          },
        },
      },
    },
  ];
  const { block, subjects } = await admittedForcedSubjects(
    malformedFirst ? sources : [...sources].reverse(),
  );
  return {
    block,
    malformed: subjects[malformedFirst ? 0 : 1]!,
    independent: subjects[malformedFirst ? 1 : 0]!,
  };
};

const union = createCompleteCanonicalReplayUnion([
  COMMITTED_FIELD_SHAPE_COMPLETE_CANONICAL_REPLAY,
  FIELD_ITEM_WIDTH_ILLEGAL_COMPLETE_CANONICAL_REPLAY,
]);

describe("FIWI independent retained-DA subjects", () => {
  it.each([
    [true, "ForcedTxValid"],
    [false, "ForcedTxValid"],
    [true, { ForcedTxInvalid: { reason: "NetworkIdMismatch" } }],
    [false, { ForcedTxInvalid: { reason: "NetworkIdMismatch" } }],
    [
      true,
      {
        ForcedTxInvalid: {
          reason: {
            FieldItemWidthIllegal: { field_index: 0n, item_index: 0n },
          },
        },
      },
    ],
    [
      false,
      {
        ForcedTxInvalid: {
          reason: {
            FieldItemWidthIllegal: { field_index: 0n, item_index: 0n },
          },
        },
      },
    ],
  ] as const)(
    "preserves a provable subject with malformed-first=%s verdict=%o",
    async (malformedFirst, verdict) => {
      const { block, malformed, independent } = await admittedPair(
        malformedFirst,
        verdict,
      );
      expect(block.grade).toBe("security");
      expect(block.reconstruction.forcedTransactions).toHaveLength(2);
      let prerequisite: CanonicalReplayPrerequisiteError | undefined;
      try {
        detectFieldItemWidthIllegalCompleteReplay(block);
      } catch (error) {
        expect(error).toBeInstanceOf(CanonicalReplayPrerequisiteError);
        if (!(error instanceof CanonicalReplayPrerequisiteError)) throw error;
        prerequisite = error;
      }
      expect(prerequisite?.failures).toEqual([
        {
          headerHash: block.headerHash,
          eventKeyCbor: malformed.eventCbor,
          prerequisite: "representable_field_shape",
        },
      ]);
      expect(prerequisite?.detections).toHaveLength(1);
      const detection = prerequisite!.detections[0]!;
      expect(detection.subjectEventKeyCbors).toEqual([independent.eventCbor]);
      if (malformedFirst) {
        // The current committed-field-shape member has no forced-source arm.
        // A later finding cannot cover an earlier unopened event.
        await expect(union.replay(block)).rejects.toBeInstanceOf(
          CanonicalReplayPrerequisiteError,
        );
      } else {
        const decision = await union.replay(block);
        expect(decision.detections).toEqual([detection]);
      }
      const recovered = await prepareFieldItemWidthIllegalRecoveryMaterial(
        block,
        detection.detectionId,
      );
      expect(recovered.evidence.subject.transaction_id).toBe(
        independent.leaf.tx_id,
      );
      expect(recovered.source.forcedMembership?.key).toEqual(independent.key);
      expect(recovered.source.nativeTxCompactCbor).toBe(
        independent.leaf.submitted_source.compact_cbor,
      );
      expect(recovered.source.witnessSetCompactCbor).toBe(
        independent.leaf.submitted_source.witness_set_compact_cbor,
      );
      await expect(
        deriveFieldItemWidthIllegalAuthenticatedSource({
          block,
          evidence: {
            ...recovered.evidence,
            subject: {
              ...recovered.evidence.subject,
              source_key: SDK.encodeProofThreadForcedSourceKey({
                ...independent.key,
                outputIndex: 99n,
              }).toString("hex"),
            },
          },
        }),
      ).rejects.toThrow(/forced subject disappeared/u);
      await expect(
        prepareFieldItemWidthIllegalRecoveryMaterial(block, "substituted"),
      ).rejects.toThrow(/exact findings/u);
      for (const field of [
        "tx_id",
        "compact_cbor",
        "witness_set_compact_cbor",
        "field_preimage_lengths_cbor",
      ] as const) {
        const corrupted = {
          ...block,
          reconstruction: {
            ...block.reconstruction,
            forcedTransactions: block.reconstruction.forcedTransactions.map(
              (entry) =>
                entry.value.tx_id !== malformed.leaf.tx_id
                  ? entry
                  : {
                      ...entry,
                      value:
                        field === "tx_id"
                          ? { ...entry.value, tx_id: "ff".repeat(32) }
                          : {
                              ...entry.value,
                              submitted_source: {
                                ...entry.value.submitted_source,
                                [field]: "00",
                              },
                            },
                    },
            ),
          },
        };
        expect(() =>
          detectFieldItemWidthIllegalCompleteReplay(corrupted),
        ).toThrow(/differs from its authenticated leaf/u);
        await expect(
          prepareFieldItemWidthIllegalRecoveryMaterial(
            corrupted,
            detection.detectionId,
          ),
        ).rejects.toThrow(/differs from its authenticated leaf/u);
      }
    },
  );
});

describe("FIWI independent fields within one retained-DA subject", () => {
  it.each([
    [false, true],
    [false, false],
    [true, true],
    [true, false],
  ] as const)(
    "preserves an independent item with malformed-earlier-field=%s illegal-wrapper=%s",
    async (malformedEarlierField, illegalWrapper) => {
      const envelope = decodeSingleCbor(forcedBytes()) as unknown[];
      const body = envelope[1] as unknown[];
      const input = Buffer.from(
        `81${illegalWrapper ? "57" : "58"}26825820${"11".repeat(32)}190000`,
        "hex",
      );
      body[0] = malformedEarlierField ? Buffer.from("81", "hex") : input;
      if (malformedEarlierField) body[1] = input;
      body[8] = Buffer.from("80ff", "hex");
      const { block, subjects } = await admittedForcedSubjects([
        {
          bytes: encodeCbor(envelope),
          verdict: {
            ForcedTxInvalid: {
              reason: {
                FieldItemWidthIllegal: { field_index: 5n, item_index: 0n },
              },
            },
          },
        },
      ]);
      expect(block.grade).toBe("security");
      expect(block.reconstruction.forcedTransactions).toHaveLength(1);
      if (!illegalWrapper) {
        expect(() => detectFieldItemWidthIllegalCompleteReplay(block)).toThrow(
          CanonicalReplayPrerequisiteError,
        );
        expect(() =>
          deriveFieldItemWidthIllegalEvidenceFromCanonicalBlock(block),
        ).toThrow(CanonicalReplayPrerequisiteError);
        return;
      }
      const detections = detectFieldItemWidthIllegalCompleteReplay(block);
      expect(detections).toHaveLength(1);
      expect(detections[0]!.subjectEventKeyCbors).toEqual([
        subjects[0]!.eventCbor,
      ]);
      expect((await union.replay(block)).detections).toEqual(detections);
      const recovered = await prepareFieldItemWidthIllegalRecoveryMaterial(
        block,
        detections[0]!.detectionId,
      );
      expect(recovered.evidence.fieldIndex).toBe(
        malformedEarlierField ? 1n : 0n,
      );
      expect(recovered.evidence.itemIndex).toBe(0n);
      expect(recovered.evidence.fieldPreimageHex).toBe(input.toString("hex"));
      expect(recovered.evidence.observation).toEqual({
        ItemLegality: { item_is_legal: false },
      });
      expect(recovered.source.forcedMembership?.key).toEqual(subjects[0]!.key);
      expect(recovered.source.nativeTxCompactCbor).toBe(
        subjects[0]!.leaf.submitted_source.compact_cbor,
      );
    },
  );
});

describe("FIWI correctly rejected prefixes", () => {
  it.each([false, true])(
    "preserves later findings with unresolved-earlier-field=%s",
    async (malformedEarlierField) => {
      const envelope = decodeSingleCbor(forcedBytes()) as unknown[];
      const body = envelope[1] as unknown[];
      const input = Buffer.from(`815726825820${"11".repeat(32)}190000`, "hex");
      body[0] = malformedEarlierField ? Buffer.from("81", "hex") : input;
      if (malformedEarlierField) body[1] = input;
      body[8] = Buffer.from("80ff", "hex");
      const later = decodeSingleCbor(forcedBytes()) as unknown[];
      (later[1] as unknown[])[0] = input;
      const { block, subjects } = await admittedForcedSubjects([
        {
          bytes: encodeCbor(envelope),
          verdict: {
            ForcedTxInvalid: {
              reason: {
                FieldItemWidthIllegal: {
                  field_index: malformedEarlierField ? 1n : 0n,
                  item_index: 0n,
                },
              },
            },
          },
        },
        { bytes: encodeCbor(later), verdict: "ForcedTxValid" },
      ]);
      const detections = (() => {
        try {
          return detectFieldItemWidthIllegalCompleteReplay(block);
        } catch (error) {
          expect(malformedEarlierField).toBe(true);
          if (!(error instanceof CanonicalReplayPrerequisiteError)) throw error;
          expect(error.failures).toEqual([
            {
              headerHash: block.headerHash,
              eventKeyCbor: subjects[0]!.eventCbor,
              prerequisite: "representable_field_shape",
            },
          ]);
          return error.detections;
        }
      })();
      expect(detections).toHaveLength(1);
      expect(detections[0]!.subjectEventKeyCbors).toEqual([
        subjects[1]!.eventCbor,
      ]);
      if (malformedEarlierField) {
        await expect(union.replay(block)).rejects.toBeInstanceOf(
          CanonicalReplayPrerequisiteError,
        );
      } else {
        expect((await union.replay(block)).detections).toEqual(detections);
      }
      const recovered = await prepareFieldItemWidthIllegalRecoveryMaterial(
        block,
        detections[0]!.detectionId,
      );
      expect(recovered.evidence.fieldIndex).toBe(0n);
      expect(recovered.evidence.itemIndex).toBe(0n);
      expect(recovered.evidence.fieldPreimageHex).toBe(input.toString("hex"));
      expect(recovered.source.forcedMembership?.key).toEqual(subjects[1]!.key);
      expect(recovered.source.nativeTxCompactCbor).toBe(
        subjects[1]!.leaf.submitted_source.compact_cbor,
      );
    },
  );
});
