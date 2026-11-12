/**
 * What an event-ledger refusal means to a family that reads a block's
 * ledgers through the shared walk (native-script-decoding, and the families
 * built on its prior ledger), and which family's finding discharges it:
 *
 * - a rejected normal step changes nothing, and is itself convicted by
 *   l2TxMistag (tests/l2-tx-mistag.test.ts:116);
 * - an absent spend input wants that step's spend-input proof, which
 *   non-existent-input gives (tests/predecessor-replay.test.ts:137);
 * - an absent withdrawal input wants the same, which withdrawal-mistag or
 *   double-withdraw gives at its leaf
 *   (tests/replay-prerequisite.withdrawal-replay-prerequisites.ts:73,93);
 * - a step out of phase order wants its transition proof, which the
 *   transition-trace phase band discharges
 *   (tests/transition-trace-phase-band.test.ts:115);
 * - an output the ledger already holds is refused by name.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { makeOutput } from "@al-ft/midgard-validation/tests/validation-fixtures";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { createEventLedgerWalk } from "../src/event-ledger.js";
import { canonicalBlockEvidenceFromVerifiedPayload } from "../src/evidence/canonical-block-evidence.js";
import { ledgerEntriesRoot, type LedgerEntry } from "../src/ledger-at.js";
import { nativeScriptDecodingPriorLedger } from "../src/native-script-decoding/replay.js";
import { detectTransitionTraceFaults } from "../src/transition-trace/detect.js";
import { transitionTraceCanonicalDetections } from "../src/transition-trace/replay-authority.canonical-detections.js";
import { classifyCanonicalBlockViolations } from "../src/workflow/classification.js";
import {
  type CompleteCanonicalReplay,
  completeCanonicalReplayPredecessorLedger,
  createCompleteCanonicalReplayUnion,
  DOUBLE_WITHDRAW_COMPLETE_CANONICAL_REPLAY,
  L2_TX_MISTAG_COMPLETE_CANONICAL_REPLAY,
  NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY,
  WITHDRAWAL_MISTAG_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.js";
import {
  assertReplayPrerequisiteCovered,
  CanonicalReplayPrerequisiteError,
} from "../src/workflow/replay-prerequisite.js";
import {
  authenticatedHeaderObservation,
  buildFixtureTransaction,
  type FixtureTransactionInput,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";
import { committedL2BlockFixture } from "./helpers/committed-l2-block-fixture.js";
import {
  hex,
  outRef,
  undecodableNativeOutput,
  withdrawalOf,
} from "./helpers/event-ledger-seeded-blocks.js";
import { inputExistenceChronologyFixture } from "./helpers/input-existence-chronology-fixture.js";
import { buildForcedAndL2Block } from "./support/transition-trace-phase-band.forced-and-l2-block.js";

const NATIVE: LedgerEntry = {
  outRef: outRefCbor(0xa1, 0n),
  output: undecodableNativeOutput,
};
const SPENT: LedgerEntry = {
  outRef: outRefCbor(0xa2, 0n),
  output: makeOutput(2_000_000n),
};
const OTHER: LedgerEntry = {
  outRef: outRefCbor(0xa3, 0n),
  output: makeOutput(3_000_000n),
};

type Step = Readonly<{
  input: FixtureTransactionInput;
  /** Whether the operator's committed root applies the step's effects. */
  applied: boolean;
}>;

/** The steps over `predecessor`, committed with the operator's roots. */
const block = async (
  steps: readonly Step[],
  predecessor: readonly LedgerEntry[] = [NATIVE, SPENT, OTHER],
) => {
  const txs = steps.map(({ input }) => buildFixtureTransaction(input));
  const ledger = new Map(
    predecessor.map((entry) => [hex(entry.outRef), entry]),
  );
  const roots = [await ledgerEntriesRoot(predecessor)];
  for (const [index, { input, applied }] of steps.entries()) {
    if (applied) {
      for (const key of input.spendInputs) ledger.delete(hex(key));
      for (const [outputIndex, output] of (input.outputs ?? []).entries()) {
        const key = outRef(txs[index]!.txId, outputIndex);
        ledger.set(hex(key), { outRef: key, output });
      }
    }
    roots.push(await ledgerEntriesRoot([...ledger.values()]));
  }
  const fixture = await committedL2BlockFixture({
    predecessorEntries: predecessor,
    txs,
    steps: txs.map((_, index) => ({
      pre: roots[index]!,
      post: roots[index + 1]!,
    })),
  });
  return {
    ...fixture,
    txs,
    key: (index: number): SDK.EventKey => ({
      L2TransactionEventKey: { tx_id: txs[index]!.txId },
    }),
    /** Step `index`'s transactions-root position, which findings name. */
    position: (index: number) =>
      BigInt(
        fixture.evidence.reconstruction.transactions.findIndex(
          ({ txId }) => txId === txs[index]!.txId,
        ),
      ),
    walk: () =>
      createEventLedgerWalk({
        block: fixture.evidence,
        predecessor: completeCanonicalReplayPredecessorLedger(fixture)!,
      }),
    replay: async (members: readonly CompleteCanonicalReplay[]) => {
      const replayer =
        members.length === 1
          ? members[0]!
          : createCompleteCanonicalReplayUnion(members);
      const { detections } = await replayer.replay(
        fixture.evidence,
        fixture.context,
      );
      return {
        detections,
        classification: await classifyCanonicalBlockViolations({
          evidence: fixture.evidence,
          detections,
          minimumConfirmationDepth: 1,
        }),
      };
    },
  };
};

/** An accepted transaction whose reference input carries a native script
 * that does not decode: a native-script-decoding wrongful acceptance. */
const nativeFault = (spend: Buffer = SPENT.outRef): Step => ({
  input: {
    spendInputs: [spend],
    referenceInputs: [NATIVE.outRef],
    outputs: [makeOutput(1_000_000n)],
    fee: 7n,
  },
  applied: true,
});

const rejected = (applied: boolean): Step => ({
  input: { validity: "TxIsInvalid", spendInputs: [OTHER.outRef], fee: 5n },
  applied,
});

/** The distinct prerequisites a replay leaves undischarged: each candidate
 * of a refused event names its own. */
const prerequisites = async (run: () => Promise<unknown>) => {
  const error = await run().then(
    () => undefined,
    (cause: unknown) => cause,
  );
  if (!(error instanceof CanonicalReplayPrerequisiteError))
    throw new Error(`expected a prerequisite, got ${String(error)}`);
  const distinct = new Map(
    error.failures.map(({ eventKeyCbor, prerequisite }) => [
      `${eventKeyCbor}:${prerequisite}`,
      { eventKey: Data.from(eventKeyCbor, SDK.EventKey), prerequisite },
    ]),
  );
  return [...distinct.values()];
};

describe("a rejected normal step in the event ledger", () => {
  it("changes nothing, so a later native fault stays challengeable, and l2TxMistag convicts the step", async () => {
    const fixture = await block([rejected(false), nativeFault()]);
    expect(
      (await fixture.replay([NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY]))
        .detections,
    ).toMatchObject([
      {
        detectionId: `native-script-decoding:0:${fixture.position(1)}:1:0`,
      },
    ]);
    expect(
      (await fixture.replay([L2_TX_MISTAG_COMPLETE_CANONICAL_REPLAY]))
        .detections,
    ).toMatchObject([
      { violationId: "l2-tx-mistag", position: fixture.position(0) },
    ]);
    const { detections, classification } = await fixture.replay([
      NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
      L2_TX_MISTAG_COMPLETE_CANONICAL_REPLAY,
    ]);
    expect(detections).toHaveLength(2);
    expect(classification).toMatchObject({
      decision: "fault_detected",
      selected: { violationId: "l2-tx-mistag", position: fixture.position(0) },
    });
  });

  it("gives a rejected step the ledger before it, which its no-op leaves to the next step", async () => {
    const fixture = await block([rejected(false), nativeFault()]);
    const walk = fixture.walk();
    const own = await walk.before(fixture.key(0), 0n);
    expect(own.utxosRoot).toBe(fixture.evidence.header.prevUtxosRoot);
    expect(own.outputs.has(hex(OTHER.outRef))).toBe(true);
    expect((await walk.before(fixture.key(1), 1n)).utxosRoot).toBe(
      own.utxosRoot,
    );
  });

  it("refuses a rejected step whose committed root changed, and l2TxMistag's finding at it discharges the refusal", async () => {
    const fixture = await block([rejected(true), nativeFault()]);
    await expect(
      fixture.walk().before(fixture.key(1), 1n),
    ).rejects.toMatchObject({ reason: "post_root_changed", step: 0n });
    expect(
      await prerequisites(() =>
        fixture.replay([NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY]),
      ),
    ).toEqual([
      { eventKey: fixture.key(0), prerequisite: "prior_transition_effect" },
    ]);
    expect(
      (
        await fixture.replay([
          NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
          L2_TX_MISTAG_COMPLETE_CANONICAL_REPLAY,
        ])
      ).classification,
    ).toMatchObject({
      decision: "fault_detected",
      selected: { violationId: "l2-tx-mistag", position: fixture.position(0) },
    });
  });
});

describe("an output the event ledger already holds", () => {
  it("is refused by name at its step, never overwritten", async () => {
    const creator: FixtureTransactionInput = {
      spendInputs: [SPENT.outRef],
      outputs: [makeOutput(4_000_000n)],
      fee: 3n,
    };
    const held: LedgerEntry = {
      outRef: outRef(buildFixtureTransaction(creator).txId),
      output: makeOutput(5_000_000n),
    };
    const fixture = await block(
      [{ input: creator, applied: true }, nativeFault(OTHER.outRef)],
      [NATIVE, SPENT, OTHER, held],
    );
    await expect(
      fixture.walk().before(fixture.key(1), 1n),
    ).rejects.toMatchObject({
      reason: "transaction_output_already_present",
      step: 0n,
    });
    expect(
      await prerequisites(() =>
        fixture.replay([NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY]),
      ),
    ).toEqual([
      { eventKey: fixture.key(0), prerequisite: "prior_transition_effect" },
    ]);
  });
});

describe("an absent input in the event ledger", () => {
  it("wants the step's spend-input proof, which non-existent-input gives", async () => {
    const fixture = await block([
      {
        input: { spendInputs: [outRefCbor(0xaf, 0n)], fee: 3n },
        applied: true,
      },
      nativeFault(),
    ]);
    await expect(
      fixture.walk().before(fixture.key(1), 1n),
    ).rejects.toMatchObject({ reason: "transaction_input_absent", step: 0n });
    expect(
      await prerequisites(() =>
        fixture.replay([NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY]),
      ),
    ).toEqual([
      { eventKey: fixture.key(0), prerequisite: "present_spend_input" },
    ]);
    expect(
      (
        await fixture.replay([
          NON_EXISTENT_INPUT_COMPLETE_CANONICAL_REPLAY,
          NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
        ])
      ).classification,
    ).toMatchObject({
      decision: "fault_detected",
      selected: {
        violationId: "non-existent-input",
        position: fixture.position(0),
      },
    });
  });

  it("wants a payable withdrawal's spend-input proof, which a withdrawal finding at its leaf gives", async () => {
    const withdrawal = withdrawalOf(0x5f, true).committed;
    const fixture = await inputExistenceChronologyFixture(
      "withdrawal_untouched",
      0,
      undefined,
      withdrawal,
    );
    const [failure] = await prerequisites(() =>
      nativeScriptDecodingPriorLedger(
        fixture.evidence.reconstruction,
        fixture.targetStep,
        fixture.predecessor,
      ),
    );
    expect(failure).toEqual({
      eventKey: withdrawal.eventKey,
      prerequisite: "present_spend_input",
    });
    const detections = [
      ...(
        await WITHDRAWAL_MISTAG_COMPLETE_CANONICAL_REPLAY.replay(
          fixture.evidence,
          fixture.context,
        )
      ).detections,
      ...(
        await DOUBLE_WITHDRAW_COMPLETE_CANONICAL_REPLAY.replay(fixture.evidence)
      ).detections,
    ];
    expect(() =>
      assertReplayPrerequisiteCovered(
        fixture.evidence,
        {
          headerHash: fixture.evidence.headerHash,
          eventKeyCbor: Data.to(failure!.eventKey, SDK.EventKey),
          prerequisite: failure!.prerequisite,
        },
        detections,
      ),
    ).not.toThrow();
  });
});

describe("a step out of phase order in the event ledger", () => {
  it("wants the misplaced step's transition proof, which the phase band discharges", async () => {
    const fixture = await buildForcedAndL2Block({ forcedLast: true });
    const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
      observation: authenticatedHeaderObservation(fixture),
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
      daProvenance: {
        trustClass: "public_or_permissionless_da",
        sourceId: "test/event-ledger-phase-order",
        grade: "security",
      },
    });
    const [l2, forced] = [0n, 1n].map(
      (index) => evidence.reconstruction.traceByStepIndex.get(index)!.value,
    );
    expect([l2!.phase, forced!.phase]).toEqual([
      "L2Transaction",
      "ForcedTransaction",
    ]);
    await expect(
      createEventLedgerWalk({ block: evidence }).before(forced!.event_key, 1n),
    ).rejects.toMatchObject({ reason: "phase_order_changed", step: 0n });
    const [failure] = await prerequisites(() =>
      nativeScriptDecodingPriorLedger(evidence.reconstruction, 1n),
    );
    expect(failure).toEqual({
      eventKey: l2!.event_key,
      prerequisite: "prior_transition_effect",
    });
    const band = transitionTraceCanonicalDetections(
      evidence,
      await detectTransitionTraceFaults(evidence.reconstruction),
    );
    expect(band.map(({ detectionId }) => detectionId)).toEqual([
      "transition-trace:0:eventToStepMismatch",
      "transition-trace:1:eventToStepMismatch",
    ]);
    expect(() =>
      assertReplayPrerequisiteCovered(
        evidence,
        {
          headerHash: evidence.headerHash,
          eventKeyCbor: Data.to(failure!.eventKey, SDK.EventKey),
          prerequisite: failure!.prerequisite,
        },
        band,
      ),
    ).not.toThrow();
  });
});
