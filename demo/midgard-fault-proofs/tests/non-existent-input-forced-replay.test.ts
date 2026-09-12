import {
  computeHash28,
  computeMidgardNativeTxId,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import { encodeMidgardForcedTxCanonical } from "@al-ft/midgard-core/codec/forced";
import { deriveMidgardForcedTxProofSource } from "@al-ft/midgard-core/codec/forced";
import { materializeMidgardForcedTxFromCanonical } from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import {
  admitNonExistentInputForcedArtifact,
  nonExistentInputForcedArtifact,
} from "../src/non-existent-input/artifact.js";
import { prepareNonExistentInputWrongfulRejection } from "../src/non-existent-input/wrongful-rejection.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import { classifyCanonicalBlockViolations } from "../src/workflow/classification.js";
import {
  admitCompleteCanonicalReplayPredecessor,
  NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  reencodeFixturePayload,
} from "./helpers/canonical-block-evidence-fixture.js";
import { makeNativeTx } from "./support/emulator/native-tx.js";
import { buildInvalidForcedTransitionTraceFixture } from "./support/submit-init-emulator-fixtures.js";

const provenance = {
  trustClass: "public_or_permissionless_da",
  sourceId: "test/non-existent-input-retained-da",
  grade: "security",
} as const;
const fixture = async (present: boolean, index: bigint, consumed = false) => {
  const base = await buildInvalidForcedTransitionTraceFixture({
    operatorVkey: "aa".repeat(28),
    now: 1_000_000,
  });
  const key = encodeMidgardSpendInputItem({
    txId: Buffer.alloc(32, 0x55),
    outputIndex: 0,
  });
  const output = Buffer.from(
    "a200581d70aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0",
    "hex",
  );
  const predecessorFixture = await buildCanonicalBlockFixture({
    transactions: [],
    utxos: present ? [{ key, value: output }] : [],
  });
  const native = materializeMidgardForcedTxFromCanonical(
    makeNativeTx({ spendInputCbors: [key], fee: 0n }),
  );
  const proofSource = deriveMidgardForcedTxProofSource(native);
  const leaf = {
    tx_id: computeMidgardNativeTxId(native).toString("hex"),
    submitted_source: {
      compact_cbor: proofSource.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        proofSource.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        proofSource.fieldPreimageLengthsCbor.toString("hex"),
    },
    verdict: {
      ForcedTxInvalid: {
        reason: { InputNotFound: { source_kind: 0n, input_index: index } },
      },
    },
  } as const;
  const sourceKey = Data.to(
    base.eventKey.ForcedTransactionEventKey.tx_order_id,
    SDK.OutputReference,
  );
  const sourceValue = Data.to(leaf, SDK.ForcedInclusionTxV1);
  const entries: SDK.DaPayloadEntry[] = [[sourceKey, sourceValue]];
  const preimages: SDK.DaPayloadEntry[] = [
    [
      sourceKey,
      encodeMidgardForcedTxCanonical(
        materializeMidgardForcedTxFromCanonical(native),
      ).toString("hex"),
    ],
  ];
  const finalRoot = consumed
    ? SDK.EMPTY_MERKLE_TREE_ROOT
    : predecessorFixture.header.utxosRoot;
  const targetIndex = consumed ? 1n : 0n;
  const step = {
    ...base.reconstruction.transitionTrace[0]!.value,
    step_index: targetIndex,
    pre_utxos_root: finalRoot,
    post_utxos_root: finalRoot,
  };
  const traces: SDK.DaPayloadEntry[] = [
    [Data.to(targetIndex), Data.to(step, SDK.TransitionStep)],
  ];
  const events: SDK.DaPayloadEntry[] = [
    [
      Data.to(base.eventKey, SDK.EventKey),
      Data.to(
        { step_index: targetIndex, phase: "ForcedTransaction" },
        SDK.EventToStepValue,
      ),
    ],
  ];
  const validations = [
    ...base.reconstruction.payload.block_body.validation_traces,
  ];
  if (consumed) {
    const priorNative = makeNativeTx({ spendInputCbors: [key], fee: 1n });
    const priorSource = deriveMidgardForcedTxProofSource(
      materializeMidgardForcedTxFromCanonical(priorNative),
    );
    const priorKey = { transactionId: "aa".repeat(32), outputIndex: 0n };
    const priorEvent: SDK.EventKey = {
      ForcedTransactionEventKey: { tx_order_id: priorKey },
    };
    const priorKeyCbor = Data.to(priorKey, SDK.OutputReference);
    entries.unshift([
      priorKeyCbor,
      Data.to(
        {
          tx_id: computeMidgardNativeTxId(priorNative).toString("hex"),
          submitted_source: {
            compact_cbor: priorSource.compactCbor.toString("hex"),
            witness_set_compact_cbor:
              priorSource.witnessSetCompactCbor.toString("hex"),
            field_preimage_lengths_cbor:
              priorSource.fieldPreimageLengthsCbor.toString("hex"),
          },
          verdict: "ForcedTxValid",
        },
        SDK.ForcedInclusionTxV1,
      ),
    ]);
    preimages.unshift([
      priorKeyCbor,
      encodeMidgardForcedTxCanonical(priorNative).toString("hex"),
    ]);
    traces.unshift([
      Data.to(0n),
      Data.to(
        {
          ...step,
          step_index: 0n,
          event_key: priorEvent,
          pre_utxos_root: predecessorFixture.header.utxosRoot,
        },
        SDK.TransitionStep,
      ),
    ]);
    events.unshift([
      Data.to(priorEvent, SDK.EventKey),
      Data.to(
        { step_index: 0n, phase: "ForcedTransaction" },
        SDK.EventToStepValue,
      ),
    ]);
    const descriptor = Data.from(
      validations[0]![1],
      SDK.ValidationTraceDescriptor,
    );
    validations.unshift([
      Data.to(priorEvent, SDK.EventKey),
      Data.to(
        { ...descriptor, verdict: "Accepted" },
        SDK.ValidationTraceDescriptor,
      ),
    ]);
  }
  const rootFor = async (domain: SDK.RootDomain, rows: SDK.DaPayloadEntry[]) =>
    buildCountedRoot(
      domain,
      rows.map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
    );
  const root = await rootFor(SDK.ROOT_DOMAINS.forcedTransactionsV1, entries);
  const traceRoot = await rootFor(SDK.ROOT_DOMAINS.transitionTrace, traces);
  const eventRoot = await rootFor(SDK.ROOT_DOMAINS.eventToStep, events);
  const validationRoot = await rootFor(
    SDK.ROOT_DOMAINS.validationTraces,
    validations,
  );
  const counts = {
    ...base.reconstruction.payload.block_body.counts,
    forcedTransactionCount: BigInt(entries.length),
    totalEventCount: BigInt(entries.length),
    transitionStepCount: BigInt(entries.length),
    validationTraceCount: BigInt(entries.length),
  };
  const header = {
    ...base.header,
    ...counts,
    prevHeaderHash: predecessorFixture.headerHash,
    prevUtxosRoot: predecessorFixture.header.utxosRoot,
    utxosRoot: finalRoot,
    forcedTransactionsRoot: root.root,
    transitionTraceRoot: traceRoot.root,
    eventToStepRoot: eventRoot.root,
    validationTracesRoot: validationRoot.root,
  };
  const headerHash = computeHash28(SDK.encodeHeaderCbor(header)).toString(
    "hex",
  );
  const payload = {
    ...base.reconstruction.payload,
    block_body: {
      ...base.reconstruction.payload.block_body,
      header,
      header_hash: headerHash,
      counts,
      utxos: consumed ? [] : predecessorFixture.payload.block_body.utxos,
      forced_transactions: entries,
      forced_transaction_preimages: preimages,
      transition_trace: traces,
      event_to_step: events,
      validation_traces: validations,
    },
  } as SDK.DaPayload;
  const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(predecessorFixture, {
      header,
      headerHash,
    }),
    payloadEnvelopeCbor: await reencodeFixturePayload(payload),
    daProvenance: provenance,
  });
  const context = {
    predecessor: await admitCompleteCanonicalReplayPredecessor({
      value: {
        observation: authenticatedHeaderObservation(predecessorFixture),
        payloadEnvelopeCborHex:
          predecessorFixture.payloadEnvelopeCbor.toString("hex"),
        daProvenance: provenance,
      },
      currentEvidence: evidence,
      minimumConfirmationDepth: 1,
    }),
  };
  const predecessor = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(predecessorFixture),
    payloadEnvelopeCbor: predecessorFixture.payloadEnvelopeCbor,
    daProvenance: provenance,
  });
  return { evidence, context, predecessor };
};

describe("nonExistentInput installed forced replay", () => {
  it("uses the forced event pre-state after an earlier forced spend", async () => {
    const { evidence, context, predecessor } = await fixture(true, 0n, true);
    expect(evidence.header.prevUtxosRoot).not.toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
    expect(
      (
        await NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY.replay(
          evidence,
          context,
        )
      ).detections,
    ).toHaveLength(0);
    await expect(
      prepareNonExistentInputWrongfulRejection({
        block: evidence,
        forcedIndex: 1,
        predecessor,
      }),
    ).rejects.toThrow(/honest/);
  });

  it.each([0n, -1n, 1n])(
    "classifies exact committed index %s and readmits after JSON restart",
    async (index) => {
      const { evidence, context, predecessor } = await fixture(true, index);
      const decision =
        await NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY.replay(
          evidence,
          context,
        );
      expect(decision.detections).toHaveLength(1);
      expect(decision.detections[0]!.violationId).toBe(
        "non-existent-input-wrongful-rejection",
      );
      const classification = await classifyCanonicalBlockViolations({
        evidence,
        detections: decision.detections,
        minimumConfirmationDepth: 1,
      });
      expect(classification.decision).toBe("fault_detected");
      const prepared = await prepareNonExistentInputWrongfulRejection({
        block: evidence,
        forcedIndex: 0,
        predecessor,
      });
      expect(
        await admitNonExistentInputForcedArtifact(
          JSON.parse(JSON.stringify(nonExistentInputForcedArtifact(prepared))),
        ),
      ).toEqual(prepared);
    },
  );
  it("refuses an honest missing-input rejection and missing prior ledger", async () => {
    const honest = await fixture(false, 0n);
    expect(
      (
        await NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY.replay(
          honest.evidence,
          honest.context,
        )
      ).detections,
    ).toHaveLength(0);
    await expect(
      prepareNonExistentInputWrongfulRejection({
        block: honest.evidence,
        forcedIndex: 0,
        predecessor: honest.predecessor,
      }),
    ).rejects.toThrow(/honest/);
    const wrong = await fixture(true, 0n);
    await expect(
      prepareNonExistentInputWrongfulRejection({
        block: wrong.evidence,
        forcedIndex: 0,
      }),
    ).rejects.toThrow(/predecessor/);
  });
});
