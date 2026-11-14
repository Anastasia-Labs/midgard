import { readFileSync } from "node:fs";

import {
  encodeMidgardFieldPreimage,
  encodeMidgardRedeemerWitnessItem,
} from "@al-ft/midgard-core";
import {
  decodeMidgardForcedTxFullFromCanonicalCbor,
  deriveMidgardForcedTxProofSource,
  encodeMidgardForcedTxCanonical,
} from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import type { CanonicalBlockEvidence } from "../src/evidence/canonical-block-evidence.js";
import { deriveFieldItemWidthIllegalAuthenticatedSource } from "../src/field-item-width-illegal/workflow.js";
import { deriveOutputReferenceScriptDecodingAuthenticatedSource } from "../src/output-reference-script-decoding/authenticated-workflow.js";
import { deriveProtectedOutputSignerMissingAuthenticatedSource } from "../src/protected-output-signer-missing/authenticated-workflow.js";
import {
  admitRedeemerWorkflowArtifact,
  prepareRedeemerCanonicityWorkflowArtifact,
} from "../src/redeemer-canonicity/runtime.js";
import { deriveResolvedOutputNonCanonicalAuthenticatedSource } from "../src/resolved-output-non-canonical/authenticated-workflow.js";
import { deriveSpendInputSignerMissingAuthenticatedSource } from "../src/spend-input-signer-missing/authenticated-workflow.js";
import { deriveTransactionOutputNonCanonicalAuthenticatedSource } from "../src/transaction-output-non-canonical/workflow.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import {
  type DecodedForcedTransactionEntry,
  eventKeyFingerprint,
  type SourceEventRecord,
} from "../src/transition-trace/reconstruct.js";
import { deriveWitnessScriptDecodingAuthenticatedSource } from "../src/witness-script-decoding/workflow.js";
import { makeHeader } from "./support/emulator/header-fixtures.js";

const vector = JSON.parse(
  readFileSync(
    new URL(
      "../../midgard-core/tests/fixtures/forced-submission-vector.json",
      import.meta.url,
    ),
    "utf8",
  ),
) as { canonical: string; transactionId: string };

const families = [
  {
    name: "spend input signer",
    derive: deriveSpendInputSignerMissingAuthenticatedSource,
    reason: { SpendInputSignerMissing: { input_index: 0n } },
  },
  {
    name: "protected output signer",
    derive: deriveProtectedOutputSignerMissingAuthenticatedSource,
    reason: { ProtectedOutputSignerMissing: { output_index: 0n } },
  },
  {
    name: "output reference script",
    derive: deriveOutputReferenceScriptDecodingAuthenticatedSource,
    reason: { OutputReferenceScriptMalformed: { output_index: 0n } },
  },
  {
    name: "resolved output",
    derive: deriveResolvedOutputNonCanonicalAuthenticatedSource,
    reason: {
      InputSpentOutputNonCanonical: { source_kind: 0n, input_index: 0n },
    },
  },
  {
    name: "field item width",
    derive: deriveFieldItemWidthIllegalAuthenticatedSource,
    reason: { FieldItemWidthIllegal: { field_index: 0n, item_index: 0n } },
  },
  {
    name: "transaction output",
    derive: deriveTransactionOutputNonCanonicalAuthenticatedSource,
    reason: { OutputNonCanonical: { output_index: 0n } },
  },
  {
    name: "witness script",
    derive: deriveWitnessScriptDecodingAuthenticatedSource,
    reason: { WitnessScriptHeaderMalformed: { script_index: 0n } },
  },
] as const;

// These source factories consume an already authenticated reconstruction. Build
// its actual two-leaf counted root, so selection also has to produce a real
// membership proof for the requested order. The evidence's family-specific
// predicate data is deliberately outside this source-authentication boundary.
const fixture = async (
  reason: SDK.RejectionReason,
  redeemerFields?: readonly Buffer[],
) => {
  const original = decodeMidgardForcedTxFullFromCanonicalCbor(
    Buffer.from(vector.canonical, "hex"),
  );
  const entries: DecodedForcedTransactionEntry[] = [0n, 1n].map(
    (outputIndex) => {
      const fullTransactionCbor = encodeMidgardForcedTxCanonical({
        version: original.version,
        body: original.body,
        witnessSet: {
          ...original.witnessSet,
          redeemerTxWitsPreimageCbor:
            redeemerFields?.[Number(outputIndex)] ??
            Buffer.from(outputIndex === 0n ? "80" : "814100", "hex"),
        },
      });
      const source = deriveMidgardForcedTxProofSource(
        decodeMidgardForcedTxFullFromCanonicalCbor(fullTransactionCbor),
      );
      const key = { transactionId: "aa".repeat(32), outputIndex };
      const value: SDK.ForcedInclusionTxV1 = {
        tx_id: vector.transactionId,
        submitted_source: {
          compact_cbor: source.compactCbor.toString("hex"),
          witness_set_compact_cbor:
            source.witnessSetCompactCbor.toString("hex"),
          field_preimage_lengths_cbor:
            source.fieldPreimageLengthsCbor.toString("hex"),
        },
        verdict: { ForcedTxInvalid: { reason } },
      };
      return {
        key,
        value,
        fullTransactionCbor,
        keyBytes: SDK.encodeProofThreadForcedSourceKey(key),
        valueBytes: Buffer.from(
          Data.to<SDK.ForcedInclusionTxV1>(
            value,
            SDK.ForcedInclusionTxV1Schema as never,
          ),
          "hex",
        ),
      };
    },
  );
  const root = await buildCountedRoot(
    SDK.ROOT_DOMAINS.forcedTransactionsV1,
    entries.map(({ keyBytes, valueBytes }) => ({
      key: keyBytes,
      value: valueBytes,
    })),
  );
  const events: SourceEventRecord[] = entries.map((entry) => {
    const eventKey = { ForcedTransactionEventKey: { tx_order_id: entry.key } };
    return {
      phase: "ForcedTransaction",
      eventKey,
      fingerprint: eventKeyFingerprint(eventKey),
      entry,
    };
  });
  const block = {
    header: {
      ...makeHeader("11".repeat(28), 0),
      forcedTransactionsRoot: root.root,
    },
    headerHash: "22".repeat(28),
    transactions: [],
    reconstruction: {
      forcedTransactions: entries,
      rootData: { forcedTransactions: root },
      sourceEventsByFingerprint: new Map(
        events.map((event) => [event.fingerprint, event]),
      ),
    },
  } as unknown as CanonicalBlockEvidence;
  return { block, entries, root };
};

describe.each(families)(
  "$name exact forced order source",
  ({ derive, reason }) => {
    it("selects each same-body order's own witness bytes and membership", async () => {
      const { block, entries, root } = await fixture(reason);
      expect(entries[0]!.value.tx_id).toBe(entries[1]!.value.tx_id);
      expect(entries[0]!.value.submitted_source).not.toEqual(
        entries[1]!.value.submitted_source,
      );
      for (const entry of entries) {
        const subject = SDK.forcedVerdictSubject({
          transactionId: entry.value.tx_id,
          sourceKey: entry.key,
          rejectionReason: reason,
        });
        const source = await derive({
          block,
          evidence: { subject, finding: { subject } } as never,
        });
        expect(source.forcedMembership).toMatchObject({
          root: root.root,
          count: 2n,
          key: entry.key,
          value: entry.value,
        });
        expect(source.forcedMembership!.proof).toBeDefined();
        expect(source.nativeTxCompactCbor).toBe(
          entry.value.submitted_source.compact_cbor,
        );
        expect(source.witnessSetCompactCbor).toBe(
          entry.value.submitted_source.witness_set_compact_cbor,
        );
      }
    });

    it("refuses a foreign order key despite a matching transaction ID and reason", async () => {
      const { block, entries } = await fixture(reason);
      const subject = SDK.forcedVerdictSubject({
        transactionId: entries[0]!.value.tx_id,
        sourceKey: { ...entries[0]!.key, outputIndex: 2n },
        rejectionReason: reason,
      });
      await expect(
        derive({ block, evidence: { subject, finding: { subject } } as never }),
      ).rejects.toThrow(/forced subject disappeared/);
    });
  },
);

it("retains the selected same-body forced order through redeemer workflow restart", async () => {
  const reason = { RedeemerMalformed: { redeemer_index: 0n } };
  // The first order is honestly rejected; the second has the same body but
  // canonical witness data, so only its rejection can be challenged.
  const fields = ["1800", "00"].map((data) =>
    encodeMidgardFieldPreimage([
      encodeMidgardRedeemerWitnessItem({
        purpose: "Spend",
        index: 0n,
        redeemerCbor: Buffer.from(data, "hex"),
        executionUnits: { memory: 1n, steps: 2n },
      }),
    ]),
  );
  const { block, entries } = await fixture(reason, fields);
  expect(entries[0]!.value.tx_id).toBe(entries[1]!.value.tx_id);
  const artifact = await prepareRedeemerCanonicityWorkflowArtifact(block);
  const admitted = admitRedeemerWorkflowArtifact(
    JSON.parse(JSON.stringify(artifact)),
  );
  expect(admitted.accepted).toBeNull();
  expect(admitted.evidence.canonical).toBe(true);
  expect(admitted.evidence.subject.source_kind).toBe(1n);
  expect(admitted.evidence.subject.source_key).toBe(
    entries[1]!.keyBytes.toString("hex"),
  );
  expect(admitted.nativeTxCompactCbor).toBe(
    entries[1]!.value.submitted_source.compact_cbor,
  );
  expect(admitted.witnessSetCompactCbor).toBe(
    entries[1]!.value.submitted_source.witness_set_compact_cbor,
  );
  expect(admitted.forced?.membership).toMatchObject({
    key: entries[1]!.key,
    value: entries[1]!.value,
  });
});
