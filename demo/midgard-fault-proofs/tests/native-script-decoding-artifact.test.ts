/**
 * The native script decoding artifact's predecessor witness: a block whose
 * first transaction spends `SPENT`, and whose second spends `OTHER` and
 * references `NATIVE`, a native script that does not decode (a wrongful
 * acceptance), over a predecessor holding outputs neither touches.
 */
import * as SDKFraudProof from "@al-ft/midgard-sdk/fraud-proof";
import { makeOutput } from "@al-ft/midgard-validation/tests/validation-fixtures";
import { describe, expect, it } from "vitest";

import type { LedgerEntry } from "../src/ledger-at.js";
import {
  admitNativeScriptDecodingWorkflowArtifact,
  prepareNativeScriptDecodingWorkflowArtifact,
} from "../src/native-script-decoding/artifact.js";
import { prepareNativeScriptDecodingReplay } from "../src/native-script-decoding/replay.js";
import { completeCanonicalReplayPredecessorStoreLedger } from "../src/workflow/complete-replay.admit-complete-canonical-replay-predecessor.js";
import { NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.resolved-output-non-canonical-complete-canonical-replay.js";
import type { WorkflowJsonObject } from "../src/workflow/identity.js";
import {
  block,
  NATIVE,
  OTHER,
  SPENT,
  type Step,
} from "./helpers/event-ledger-committed-steps.js";
import { testLedgerEntries } from "./support/ledger-store-conformance.js";
import {
  droppedWitnesses,
  expectWitnessRefused,
  predecessorWitnessOf,
  tamperedWitnesses,
  withPredecessorWitness,
} from "./support/predecessor-witness-artifact.js";

const nativeFault = (spent: Buffer): Step => ({
  input: {
    spendInputs: [spent],
    referenceInputs: [NATIVE.outRef],
    outputs: [makeOutput(1_000_000n)],
    fee: 7n,
  },
  applied: true,
});

const AFTER_A_SPEND: readonly Step[] = [
  { input: { spendInputs: [SPENT.outRef], fee: 7n }, applied: true },
  nativeFault(OTHER.outRef),
];

const predecessorWith = (others: number): LedgerEntry[] => [
  NATIVE,
  SPENT,
  OTHER,
  ...testLedgerEntries(2000, others),
];

/** The committed block and the artifact the workflow writes for its one
 * detection. */
const scene = async (
  steps: readonly Step[],
  predecessor: readonly LedgerEntry[],
) => {
  const fixture = await block(steps, predecessor);
  const { classification } = await fixture.replay([
    NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  ]);
  if (classification.decision !== "fault_detected")
    throw new Error("the scene has no detection");
  const artifact = await prepareNativeScriptDecodingWorkflowArtifact({
    evidence: fixture.evidence,
    classification,
    replayContext: fixture.context,
  });
  return {
    fixture,
    // What the workflow store holds: JSON written and read back.
    artifact: JSON.parse(JSON.stringify(artifact)) as WorkflowJsonObject,
  };
};

type Prepared = NonNullable<
  Awaited<ReturnType<typeof prepareNativeScriptDecodingReplay>>
>;

/** A prepared finding with the bind step's descriptor proof taken. */
const comparable = async ({ ledgerTrie, current, ...rest }: Prepared) => ({
  ...rest,
  headerHash: current.headerHash,
  rootHex: ledgerTrie?.rootHex,
  proof: await ledgerTrie?.prove(
    Buffer.from(
      SDKFraudProof.encodeMidgardTxInputCanonical(
        rest.subjectFieldInputs[Number(rest.coordinate.outpointCursor)]!,
      ),
    ),
  ),
});

describe("native script decoding artifact predecessor witness", () => {
  it.each([0, 40])(
    "admits and prepares what the full ledger prepares, over %s other outputs",
    async (others) => {
      const { fixture, artifact } = await scene(
        AFTER_A_SPEND,
        predecessorWith(others),
      );
      const admitted =
        await admitNativeScriptDecodingWorkflowArtifact(artifact);
      const expected = await prepareNativeScriptDecodingReplay({
        current: fixture.evidence.reconstruction,
        predecessor: await completeCanonicalReplayPredecessorStoreLedger({
          evidence: fixture.evidence,
          context: fixture.context,
        }),
        coordinate: admitted.coordinate,
      });
      expect(admitted.ledgerTrie).not.toBeNull();
      expect(await comparable(admitted)).toEqual(await comparable(expected!));
    },
  );

  it("refuses every tampered and every dropped node", async () => {
    const { artifact } = await scene(AFTER_A_SPEND, predecessorWith(40));
    const witness = predecessorWitnessOf(artifact);
    expect(witness.nodes.length).toBeGreaterThan(1);
    // Admission, then the proof the bind step takes.
    const use = async (changed: typeof witness) =>
      await comparable(
        await admitNativeScriptDecodingWorkflowArtifact(
          withPredecessorWitness(artifact, changed),
        ),
      );
    for (const [index, tampered] of tamperedWitnesses(witness).entries())
      await expectWitnessRefused(
        async () => await use(tampered),
        `tampered node ${index.toString()}`,
      );
    for (const [index, dropped] of droppedWitnesses(witness).entries())
      await expectWitnessRefused(
        async () => await use(dropped),
        `dropped node ${index.toString()}`,
      );
  });

  it("refuses the witness a sibling block over the same predecessor wrote", async () => {
    const predecessor = predecessorWith(40);
    const own = await scene(AFTER_A_SPEND, predecessor);
    const sibling = await scene([nativeFault(OTHER.outRef)], predecessor);
    expect(sibling.fixture.evidence.headerHash).not.toBe(
      own.fixture.evidence.headerHash,
    );
    expect(sibling.fixture.evidence.header.prevUtxosRoot).toBe(
      own.fixture.evidence.header.prevUtxosRoot,
    );
    await comparable(
      await admitNativeScriptDecodingWorkflowArtifact(own.artifact),
    );
    // Both read `NATIVE`; only the own block's walk to its faulty step spends
    // `SPENT`, which the sibling's witness does not hold.
    await expectWitnessRefused(
      async () =>
        await comparable(
          await admitNativeScriptDecodingWorkflowArtifact(
            withPredecessorWitness(
              own.artifact,
              predecessorWitnessOf(sibling.artifact),
            ),
          ),
        ),
      "sibling witness",
    );
  });

  it("refuses the previous schema version", async () => {
    const { artifact } = await scene(AFTER_A_SPEND, predecessorWith(0));
    const { predecessorWitness: _, ...rest } = artifact;
    for (const previous of [
      {
        ...rest,
        schemaVersion: "midgard-native-script-decoding-workflow-artifact-v2",
        predecessorLedger: [],
      },
      {
        ...artifact,
        schemaVersion: "midgard-native-script-decoding-workflow-artifact-v2",
      },
    ])
      await expect(
        admitNativeScriptDecodingWorkflowArtifact(
          previous as WorkflowJsonObject,
        ),
      ).rejects.toThrow("nativeScriptDecoding artifact shape changed");
  });

  it("refuses a null witness for a block that starts from a non-empty ledger", async () => {
    const { artifact } = await scene(AFTER_A_SPEND, predecessorWith(0));
    await expect(
      admitNativeScriptDecodingWorkflowArtifact({
        ...artifact,
        predecessorWitness: null,
      }),
    ).rejects.toThrow("the block starts from a non-empty ledger");
  });

  it("writes a witness that does not grow with the predecessor ledger", async () => {
    const nodes = async (others: number) =>
      predecessorWitnessOf(
        (await scene(AFTER_A_SPEND, predecessorWith(others))).artifact,
      ).nodes.length;
    const small = await nodes(64);
    const large = await nodes(1024);
    // Sixteen times the outputs add one trie level to each path the
    // preparation reads and proves: the witness grows by that level, a whole
    // ledger in the artifact would grow sixteenfold.
    expect(large).toBeLessThan(small * 4);
  });
});
