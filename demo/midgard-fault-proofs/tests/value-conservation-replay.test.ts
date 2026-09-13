import { computeHash28, computeMidgardNativeTxId } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import { prepareValueConservationArtifact } from "../src/value-not-preserved/replay.js";
import {
  admitCompleteCanonicalReplayPredecessor,
  VALUE_NOT_PRESERVED_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  reencodeFixturePayload,
} from "./helpers/canonical-block-evidence-fixture.js";
import { buildDecodingBlockFixture } from "./support/native-script-decoding-emulator.js";
import {
  buildValueNotPreservedFixture,
  vnpOutput,
  vnpOutRef,
  vnpValue,
} from "./support/value-not-preserved-emulator.js";
const provenance = {
  trustClass: "public_or_permissionless_da",
  sourceId: "test/value-conservation-retained-da",
  grade: "security",
} as const;
const fixture = async (forced: boolean, balanced: boolean) => {
  const value = await buildValueNotPreservedFixture({
    spentInputs: [
      { input: vnpOutRef("11", 0), spentValue: vnpValue(5_000_000n) },
    ],
    outputs: [
      vnpOutput({ value: vnpValue(balanced ? 4_000_000n : 4_000_001n) }),
    ],
  });
  const predecessorFixture = await buildCanonicalBlockFixture({
    transactions: [],
    utxos: value.ledger.spentInputs.map((spent) => ({
      key: Buffer.from(SDK.encodeMidgardTxInputCanonical(spent.input)),
      value: Buffer.from(spent.outputCbor, "hex"),
    })),
  });
  const base = await buildDecodingBlockFixture({
    operatorVkey: "aa".repeat(28),
    startTime: 1_000_000n,
    priorLedgerRoot: predecessorFixture.header.utxosRoot,
    subject: forced
      ? {
          kind: "forced",
          nativeTx: value.nativeTx,
          orderKey: { transactionId: "bb".repeat(32), outputIndex: 0n },
          verdict: { ForcedTxInvalid: { reason: "ValueNotPreserved" } },
        }
      : { kind: "normal", nativeTx: value.nativeTx },
  });
  const header = {
    ...base.header,
    prevHeaderHash: predecessorFixture.headerHash,
    prevUtxosRoot: predecessorFixture.header.utxosRoot,
    utxosRoot: predecessorFixture.header.utxosRoot,
  };
  const headerHash = computeHash28(SDK.encodeHeaderCbor(header)).toString(
    "hex",
  );
  const payload: SDK.DaPayload = {
    ...base.reconstruction.payload,
    block_body: {
      ...base.reconstruction.payload.block_body,
      header,
      header_hash: headerHash,
      utxos: predecessorFixture.payload.block_body.utxos,
    },
  };
  const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(predecessorFixture, {
      header,
      headerHash,
    }),
    payloadEnvelopeCbor: await reencodeFixturePayload(payload),
    daProvenance: provenance,
  });
  const predecessor = await canonicalBlockEvidenceFromVerifiedPayload({
    observation: authenticatedHeaderObservation(predecessorFixture),
    payloadEnvelopeCbor: predecessorFixture.payloadEnvelopeCbor,
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
  return { evidence, predecessor, context };
};
describe("value conservation installed canonical replay", () => {
  it.each([true, false])(
    "authenticates predecessor and detects only the contradiction, forced=%s",
    async (forced) => {
      for (const balanced of [true, false]) {
        const { evidence, predecessor, context } = await fixture(
          forced,
          balanced,
        );
        const decision =
          await VALUE_NOT_PRESERVED_COMPLETE_CANONICAL_REPLAY.replay(
            evidence,
            context,
          );
        expect(decision.detections).toHaveLength(forced === balanced ? 1 : 0);
        expect(
          await prepareValueConservationArtifact({
            block: evidence,
            predecessor,
            sourceIndex: 0,
            forced,
          }),
        ).toEqual(forced === balanced ? expect.anything() : null);
        await expect(
          prepareValueConservationArtifact({
            block: evidence,
            sourceIndex: 0,
            forced,
          }),
        ).rejects.toThrow("authenticated predecessor ledger unavailable");
      }
    },
  );
});

it.each([true, false])(
  "replays an earlier forced spend into the selected pre-state, forced=%s",
  async (forced) => {
    const prior = await buildValueNotPreservedFixture({
      spentInputs: [
        { input: vnpOutRef("11", 0), spentValue: vnpValue(5_000_000n) },
      ],
      outputs: [
        {
          ...vnpOutput({ value: vnpValue(4_000_000n) }),
          address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x99)]),
        },
      ],
    });
    const next = await buildValueNotPreservedFixture({
      spentInputs: [
        {
          input: {
            tx_id: computeMidgardNativeTxId(prior.nativeTx).toString("hex"),
            output_index: 0n,
          },
          spentValue: vnpValue(4_000_000n),
        },
      ],
      outputs: [
        vnpOutput({ value: vnpValue(forced ? 3_000_000n : 3_000_001n) }),
      ],
    });
    const ledgerFixture = async (value: typeof prior) =>
      await buildCanonicalBlockFixture({
        transactions: [],
        utxos: value.ledger.spentInputs.map((spent) => ({
          key: Buffer.from(SDK.encodeMidgardTxInputCanonical(spent.input)),
          value: Buffer.from(spent.outputCbor, "hex"),
        })),
      });
    const before = await ledgerFixture(prior);
    const after = await ledgerFixture(next);
    const first = await buildDecodingBlockFixture({
      operatorVkey: "aa".repeat(28),
      startTime: 1_000_000n,
      priorLedgerRoot: before.header.utxosRoot,
      subject: {
        kind: "forced",
        nativeTx: prior.nativeTx,
        orderKey: { transactionId: "aa".repeat(32), outputIndex: 0n },
        verdict: "ForcedTxValid",
      },
    });
    const second = await buildDecodingBlockFixture({
      operatorVkey: "aa".repeat(28),
      startTime: 1_000_000n,
      priorLedgerRoot: after.header.utxosRoot,
      subject: forced
        ? {
            kind: "forced",
            nativeTx: next.nativeTx,
            orderKey: { transactionId: "bb".repeat(32), outputIndex: 0n },
            verdict: { ForcedTxInvalid: { reason: "ValueNotPreserved" } },
          }
        : { kind: "normal", nativeTx: next.nativeTx },
    });
    const predecessor = await canonicalBlockEvidenceFromVerifiedPayload({
      observation: authenticatedHeaderObservation(before),
      payloadEnvelopeCbor: before.payloadEnvelopeCbor,
      daProvenance: provenance,
    });
    for (const corruptPriorEffect of [false, true]) {
      const a = first.reconstruction.payload.block_body;
      const b = second.reconstruction.payload.block_body;
      const merge = (
        left: readonly SDK.DaPayloadEntry[],
        right: readonly SDK.DaPayloadEntry[],
      ): SDK.DaPayloadEntry[] =>
        [...left, ...right].sort(([l], [r]) => l.localeCompare(r));
      const firstStep = Data.from(
        a.transition_trace[0]![1],
        SDK.TransitionStepSchema,
      );
      const secondStep = Data.from(
        b.transition_trace[0]![1],
        SDK.TransitionStepSchema,
      );
      const transition_trace: SDK.DaPayloadEntry[] = [
        [
          Data.to(0n),
          Data.to(
            {
              ...firstStep,
              post_utxos_root: corruptPriorEffect
                ? before.header.utxosRoot
                : after.header.utxosRoot,
            },
            SDK.TransitionStepSchema,
          ),
        ],
        [
          Data.to(1n),
          Data.to({ ...secondStep, step_index: 1n }, SDK.TransitionStepSchema),
        ],
      ];
      const event_to_step = merge(
        a.event_to_step,
        b.event_to_step.map(([key, value]) => [
          key,
          Data.to(
            { ...Data.from(value, SDK.EventToStepValueSchema), step_index: 1n },
            SDK.EventToStepValueSchema,
          ),
        ]),
      );
      const forced_transactions = merge(
        a.forced_transactions,
        b.forced_transactions,
      );
      const validation_traces = merge(a.validation_traces, b.validation_traces);
      const root = async (
        domain: Parameters<typeof buildCountedRoot>[0],
        entries: readonly SDK.DaPayloadEntry[],
      ) =>
        (
          await buildCountedRoot(
            domain,
            entries.map(([key, value]) => ({
              key: Buffer.from(key, "hex"),
              value: Buffer.from(value, "hex"),
            })),
          )
        ).root;
      const header = {
        ...second.header,
        prevHeaderHash: before.headerHash,
        prevUtxosRoot: before.header.utxosRoot,
        utxosRoot: after.header.utxosRoot,
        forcedTransactionCount: BigInt(forced_transactions.length),
        totalEventCount: 2n,
        transitionStepCount: 2n,
        validationTraceCount: 2n,
        forcedTransactionsRoot: await root(
          SDK.ROOT_DOMAINS.forcedTransactionsV1,
          forced_transactions,
        ),
        transitionTraceRoot: await root(
          SDK.ROOT_DOMAINS.transitionTrace,
          transition_trace,
        ),
        eventToStepRoot: await root(
          SDK.ROOT_DOMAINS.eventToStep,
          event_to_step,
        ),
        validationTracesRoot: await root(
          SDK.ROOT_DOMAINS.validationTraces,
          validation_traces,
        ),
      };
      const headerHash = computeHash28(SDK.encodeHeaderCbor(header)).toString(
        "hex",
      );
      const payload: SDK.DaPayload = {
        ...second.reconstruction.payload,
        block_body: {
          ...b,
          header,
          header_hash: headerHash,
          utxos: after.payload.block_body.utxos,
          counts: {
            ...b.counts,
            forcedTransactionCount: BigInt(forced_transactions.length),
            totalEventCount: 2n,
            transitionStepCount: 2n,
            validationTraceCount: 2n,
          },
          forced_transactions,
          forced_transaction_preimages: merge(
            a.forced_transaction_preimages,
            b.forced_transaction_preimages,
          ),
          transition_trace,
          event_to_step,
          validation_traces,
        },
      };
      const block = await canonicalBlockEvidenceFromVerifiedPayload({
        observation: authenticatedHeaderObservation(before, {
          header,
          headerHash,
        }),
        payloadEnvelopeCbor: await reencodeFixturePayload(payload),
        daProvenance: provenance,
      });
      const result = prepareValueConservationArtifact({
        block,
        predecessor,
        sourceIndex: forced ? 1 : 0,
        forced,
      });
      if (corruptPriorEffect)
        await expect(result).rejects.toMatchObject({
          name: "CanonicalReplayPrerequisiteError",
          failures: [
            {
              headerHash: block.headerHash,
              eventKeyCbor: Data.to(
                Data.from(transition_trace[0]![1], SDK.TransitionStep)
                  .event_key,
                SDK.EventKey,
              ),
              prerequisite: "prior_transition_effect",
            },
          ],
          detections: [],
        });
      else {
        const artifact = await result;
        expect(artifact).not.toBeNull();
        const step = Data.from(artifact!.transitionCbor, SDK.IndexedTraceProof);
        expect(step.value.step_index).toBe(1n);
        expect(step.value.pre_utxos_root).toBe(after.header.utxosRoot);
        expect(step.value.pre_utxos_root).not.toBe(before.header.utxosRoot);
      }
    }
  },
);
