/**
 * The ledger value-not-preserved replays a block's earlier events into is
 * the shared event-ledger walk every other family reads, and a refused walk
 * means here what it means to them (tests/event-ledger.prerequisites.test.ts):
 *
 * - an output whose native script does not decode keeps the descriptor the
 *   ledger root commits for it;
 * - an absent input wants that step's spend-input proof, and an output the
 *   ledger already holds that step's transition proof;
 * - a selected event whose committed pre-state the earlier steps do not
 *   leave is refused by the walk itself.
 *
 * A transaction whose own output, or spent output, does not decode as a
 * transaction output has no value the conservation fold can read. The family
 * declines it, and the family that convicts that output discharges the
 * decline: output-reference-script-decoding for an output the transaction
 * creates, at or before the spend, and resolved-output-non-canonical for an
 * output the predecessor ledger holds.
 *
 * A forced transaction the operator rejected for its value is declined the
 * same way, and no family yet convicts what it would mean: an admitted order
 * carries no output reference script, and no forced arm convicts a rejection
 * whose reason ranks after the spent output's fault. The decline stays
 * undischarged, so the union fails closed on it.
 */
import * as SDK from "@al-ft/midgard-sdk/core";
import { makeOutput } from "@al-ft/midgard-validation/tests/validation-fixtures";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { isReplayLocalFault } from "../src/block-replay.faults.js";
import { type LedgerEntry } from "../src/ledger-at.js";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  type Ledger,
  LedgerNodeMissing,
} from "../src/ledger-store/index.js";
import { keyValuePhasProof } from "../src/transition-trace/phas.js";
import {
  prepareValueConservationArtifact,
  valueConservationInputs,
} from "../src/value-not-preserved/replay.js";
import { completeCanonicalReplayPredecessorStoreLedger } from "../src/workflow/complete-replay.admit-complete-canonical-replay-predecessor.js";
import { VALUE_NOT_PRESERVED_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.create-complete-canonical-replay-union.js";
import {
  NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  OUTPUT_REFERENCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  RESOLVED_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.resolved-output-non-canonical-complete-canonical-replay.js";
import {
  buildFixtureTransaction,
  type FixtureTransactionInput,
  outRefCbor,
} from "./helpers/canonical-block-evidence-fixture.js";
import {
  block,
  NATIVE,
  OTHER,
  prerequisites,
  SPENT,
  type Step,
} from "./helpers/event-ledger-committed-steps.js";
import { forcedRejection } from "./helpers/event-ledger-forced-step.js";
import { referenceLedgerBefore } from "./helpers/event-ledger-reference.js";
import {
  outRef,
  undecodableNativeOutput,
} from "./helpers/event-ledger-seeded-blocks.js";
import { oracleLedgerRoot } from "./support/ledger-root-oracle.js";

/** Spends `SPENT` (2 ADA) into 3 ADA: an accepted imbalance. */
const inflation: Step = {
  input: {
    spendInputs: [SPENT.outRef],
    outputs: [makeOutput(3_000_000n)],
    fee: 7n,
  },
  applied: true,
};

/** Spends `OTHER` (3 ADA) into an output and its fee: balanced. */
const balanced: FixtureTransactionInput = {
  spendInputs: [OTHER.outRef],
  outputs: [makeOutput(2_999_993n)],
  fee: 7n,
};

/** Creates an output whose native reference script does not decode. */
const creator: Step = {
  input: {
    spendInputs: [SPENT.outRef],
    outputs: [undecodableNativeOutput, makeOutput(1_000_000n)],
    fee: 7n,
  },
  applied: true,
};

const VNP = VALUE_NOT_PRESERVED_COMPLETE_CANONICAL_REPLAY;

const findings = async (
  fixture: Awaited<ReturnType<typeof block>>,
  members = [VNP],
) =>
  (await fixture.replay(members)).detections.map(
    ({ violationId, position }) => ({ violationId, position }),
  );

describe("value-not-preserved's event ledger", () => {
  it("opens a predecessor ledger holding an output whose native script does not decode", async () => {
    const fixture = await block([inflation]);
    expect(await findings(fixture)).toEqual([
      { violationId: "value-not-preserved", position: fixture.position(0) },
    ]);
  });

  it("wants an earlier step's spend-input proof when its input is absent", async () => {
    const fixture = await block([
      {
        input: { spendInputs: [outRefCbor(0xaf, 0n)], fee: 3n },
        applied: true,
      },
      inflation,
    ]);
    expect(await prerequisites(() => fixture.replay([VNP]))).toEqual([
      { eventKey: fixture.key(0), prerequisite: "present_spend_input" },
    ]);
  });

  it("wants an earlier step's transition proof when its output is already held", async () => {
    const held: LedgerEntry = {
      outRef: outRef(buildFixtureTransaction(balanced).txId),
      output: makeOutput(5_000_000n),
    };
    const fixture = await block(
      [{ input: balanced, applied: true }, inflation],
      [NATIVE, SPENT, OTHER, held],
    );
    expect(await prerequisites(() => fixture.replay([VNP]))).toEqual([
      { eventKey: fixture.key(0), prerequisite: "prior_transition_effect" },
    ]);
  });

  it("is refused by the walk when the selected event's pre-state is not the earlier steps' result", async () => {
    const fixture = await block([
      { input: balanced, applied: true },
      { ...inflation, pre: await oracleLedgerRoot([NATIVE]) },
    ]);
    await expect(fixture.replay([VNP])).rejects.toThrow(
      "event prior ledger changed",
    );
  });
});

describe("value-not-preserved declines an output that does not decode", () => {
  it("in the transaction's own outputs, which output-reference-script-decoding convicts", async () => {
    const fixture = await block([creator]);
    expect(await prerequisites(() => fixture.replay([VNP]))).toEqual([
      { eventKey: fixture.key(0), prerequisite: "representable_field_shape" },
    ]);
    expect(
      await findings(fixture, [
        VNP,
        OUTPUT_REFERENCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
      ]),
    ).toEqual([
      {
        violationId: "output-reference-script-malformed",
        position: fixture.position(0),
      },
    ]);
  });

  it("spent after an earlier step created it, which the conviction of that step discharges", async () => {
    const created = buildFixtureTransaction(creator.input).txId;
    const fixture = await block([
      creator,
      {
        input: {
          spendInputs: [outRef(created, 0)],
          outputs: [makeOutput(3_000_000n)],
          fee: 7n,
        },
        applied: true,
      },
    ]);
    const declined = await prerequisites(() => fixture.replay([VNP]));
    expect(declined).toHaveLength(2);
    expect(declined).toEqual(
      expect.arrayContaining(
        [0, 1].map((index) => ({
          eventKey: fixture.key(index),
          prerequisite: "representable_field_shape",
        })),
      ),
    );
    expect(
      await findings(fixture, [
        VNP,
        OUTPUT_REFERENCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
      ]),
    ).toEqual([
      {
        violationId: "output-reference-script-malformed",
        position: fixture.position(0),
      },
    ]);
  });

  it("spent from the predecessor ledger, which resolved-output-non-canonical convicts", async () => {
    const fixture = await block([
      {
        input: {
          spendInputs: [NATIVE.outRef],
          outputs: [makeOutput(3_000_000n)],
          fee: 7n,
        },
        applied: true,
      },
    ]);
    expect(await prerequisites(() => fixture.replay([VNP]))).toEqual([
      { eventKey: fixture.key(0), prerequisite: "representable_field_shape" },
    ]);
    expect(
      await findings(fixture, [
        VNP,
        RESOLVED_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
      ]),
    ).toEqual([
      {
        violationId: "resolved-output-non-canonical",
        position: fixture.position(0),
      },
    ]);
  });
});

describe("value-not-preserved's spent inputs", () => {
  it("are read and proved against the ledger before their transaction", async () => {
    // Step 0 spends `SPENT` and creates the output step 1 spends with
    // `OTHER`: the ledger before step 1 differs from the predecessor.
    const creating: FixtureTransactionInput = {
      spendInputs: [SPENT.outRef],
      outputs: [makeOutput(1_999_993n)],
      fee: 7n,
    };
    const spent = [
      OTHER.outRef,
      outRef(buildFixtureTransaction(creating).txId),
    ].sort(Buffer.compare);
    const fixture = await block([
      { input: creating, applied: true },
      {
        input: {
          spendInputs: spent,
          outputs: [makeOutput(6_000_000n)],
          fee: 7n,
        },
        applied: true,
      },
    ]);
    const predecessor = completeCanonicalReplayPredecessorStoreLedger(fixture)!;
    const before = await referenceLedgerBefore({
      block: fixture.evidence,
      predecessor,
      eventKey: fixture.key(1),
      stepIndex: 1n,
    });
    expect(before.utxosRoot).not.toBe(predecessor.root);
    const artifact = await prepareValueConservationArtifact({
      block: fixture.evidence,
      predecessor,
      sourceIndex: Number(fixture.position(1)),
      forced: false,
    });
    expect(artifact?.inputs).toStrictEqual(
      await Promise.all(
        spent.map(async (key) => {
          const { value } = before.trie.entries.find((entry) =>
            entry.key.equals(key),
          )!;
          return {
            outputCbor: before.outputs
              .get(key.toString("hex"))!
              .toString("hex"),
            proofCbor: Data.to(
              await keyValuePhasProof(before.trie, key, value),
              SDK.Proof,
            ),
          };
        }),
      ),
    );
  });

  it.each([
    [
      "reading the spent inputs",
      async (ledger: Ledger, fixture: Awaited<ReturnType<typeof block>>) =>
        await prepareValueConservationArtifact({
          block: fixture.evidence,
          predecessor: ledger,
          sourceIndex: Number(fixture.position(0)),
          forced: false,
        }),
    ],
    [
      "proving the spent inputs",
      async (ledger: Ledger) =>
        await valueConservationInputs(ledger, [
          { key: SPENT.outRef, output: SPENT.output },
        ]),
    ],
  ] as const)(
    "is this process's local fault %s when the store no longer holds the ledger",
    async (_, run) => {
      // The transaction is step 0, so it reads the predecessor itself, which
      // `retain` of nothing deletes.
      const fixture = await block([inflation]);
      const predecessor =
        completeCanonicalReplayPredecessorStoreLedger(fixture)!;
      const store = createLedgerStore(createMemoryLedgerStorage());
      const ledger = await store.admit(predecessor.entries(), predecessor.root);
      await store.retain(new Set());
      const error: unknown = await run(ledger, fixture).catch(
        (cause: unknown) => cause,
      );
      expect(isReplayLocalFault(error)).toBe(true);
      expect((error as { cause: unknown }).cause).toBeInstanceOf(
        LedgerNodeMissing,
      );
    },
  );
});

describe("value-not-preserved declines a forced rejection whose output does not decode", () => {
  const declined = async (
    fixture: Awaited<ReturnType<typeof forcedRejection>>,
    members: Parameters<typeof fixture.replay>[0],
  ) =>
    expect(await prerequisites(() => fixture.replay(members))).toEqual([
      { eventKey: fixture.key, prerequisite: "representable_field_shape" },
    ]);

  it("in its own outputs, which no admitted order carries", async () => {
    const fixture = await forcedRejection({ input: creator.input });
    await declined(fixture, [VNP]);
    await declined(fixture, [
      VNP,
      OUTPUT_REFERENCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
    ]);
  });

  it("spent from the predecessor ledger", async () => {
    const fixture = await forcedRejection({
      input: {
        spendInputs: [NATIVE.outRef],
        outputs: [makeOutput(3_000_000n)],
        fee: 7n,
      },
    });
    await declined(fixture, [VNP]);
    await declined(fixture, [
      NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
      VNP,
      RESOLVED_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
    ]);
  });
});
