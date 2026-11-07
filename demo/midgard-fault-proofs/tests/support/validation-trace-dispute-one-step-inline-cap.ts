/** The validation-trace dispute runner at a phase step whose semantic
 * resolver reads one item of a field, with the field grown one unit at a
 * time: by default an item, or as the caller grows it. At the largest field
 * within the phase's inline limit the prepare and semantic moves carry the
 * field whole, inline, and the runner drives the step to the award, its
 * prepare move within the transaction size limit less its reserve even when
 * our wallet pays it; one more unit would pass it. Past the limit the
 * runner publishes the field at our key address first (past one
 * publication, under the field-preimage certificate), the semantic move
 * reads it by reference, the step is driven to the award all the same, and
 * each publication is spent back by key once that move is final
 * (validation-trace-dispute-one-step-phase-drive.ts). Beside a co-shape
 * that grows what the step's evidence carries beside the field, the field
 * at the limit goes by reference, and the largest within the limit less
 * those bytes goes inline, its prepare move still within the limit. */
import {
  decodeMidgardFieldPreimage,
  MIDGARD_CHUNK_BYTES_K,
} from "@al-ft/midgard-core/codec/native-tx-field-access";
import { Data } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import type { ValidationTracePhase } from "../../src/validation-trace-dispute/one-step-phase.js";
import {
  expectReclaimedByKey,
  landed,
  ownReadsOf,
  spenderOf,
} from "./validation-trace-dispute-observe-copies.js";
import type { defendTwoThreads } from "./validation-trace-dispute-one-step-case.js";
import {
  phaseDrive,
  type PhaseEvidence,
} from "./validation-trace-dispute-one-step-phase-drive.js";
import { playThreads } from "./validation-trace-dispute-one-step-play.js";
import { reportFit } from "./validation-trace-dispute-scripted-defender.js";

/** The longest field our raw-datum publication carries in one output; a
 * longer one is published under the field-preimage certificate. */
const RAW_PUBLICATION_MAX_BYTES = 15_000;

/** The ledger's transaction size limit, and the reserve a move keeps under
 * it. */
const MAX_TX_SIZE = 16_384;
const SIZE_RESERVE = 512;

/** The tier-1 inline carriage limit of a field's preimage. */
const TIER1_MAX_PREIMAGE_BYTES = 14_336;

/** The bytes of a field's CBOR array header over `items` items. */
const headerBytes = (items: number) => (items < 24 ? 1 : items < 256 ? 2 : 3);

type Installs = Pick<
  Parameters<typeof defendTwoThreads>[0],
  | "failingReferenceInputs"
  | "failingSigners"
  | "failingScript"
  | "failingRedeemerData"
  | "failingNative"
>;

/** A failing transaction grown elsewhere beside the field (`installs`):
 * it moves the step `steps` states on, and the step's moves there carry at
 * most `besideBytes` more beside the field than where the limit was
 * measured, in its evidence and its state indices. */
type CoShape = Readonly<{
  label: string;
  installs: Installs;
  steps: number;
  besideBytes: number;
}>;

/** How the field grows: an item of `itemBytes` bytes at a time, or a
 * `unit` at a time, its preimage `fieldBytes(units)` bytes at `units`
 * units. */
type Growth =
  | { readonly itemBytes: number }
  | {
      readonly unit: string;
      readonly fieldBytes: (units: number) => number;
    };

/**
 * The rows of a step of `phase` whose semantic resolver `semantic` (held at
 * `at`) reads an item of a field that grows as `growth` says: the failing
 * transaction `installs(units)` carries the field at `units` units, our
 * trace reads the item at the step after its state `stepOf(units)`
 * (`evidence`), and the phase's inline limit is `limit` bytes. `field`
 * names the field in the rows. Each of `overAt`, a label and a byte count
 * over the limit, adds a row of the largest field within that count.
 */
export const describeInlineCap = ({
  field,
  phase,
  semantic,
  at,
  evidence,
  limit,
  stepOf,
  installs,
  overAt = [],
  coShape,
  ...growth
}: {
  readonly field: string;
  readonly phase: ValidationTracePhase;
  readonly semantic: number;
  readonly at: Parameters<typeof phaseDrive>[0]["at"];
  readonly evidence: (units: number) => PhaseEvidence;
  readonly limit: number;
  readonly stepOf: (units: number) => number;
  readonly installs: (units: number) => Installs;
  readonly overAt?: readonly (readonly [label: string, bytes: number])[];
  readonly coShape?: CoShape;
} & Growth) => {
  const { unit, fieldBytes } =
    "itemBytes" in growth
      ? {
          unit: "item",
          fieldBytes: (items: number) =>
            items * growth.itemBytes + headerBytes(items),
        }
      : growth;
  const fits = (items: number) => fieldBytes(items) <= limit;
  let largest = 1;
  while (fits(largest + 1)) largest += 1;
  /** The most units whose field is within `bytes`. */
  const within = (bytes: number) => {
    let units = largest;
    while (fieldBytes(units + 1) <= bytes) units += 1;
    return units;
  };
  const tier1 = within(TIER1_MAX_PREIMAGE_BYTES);
  const prepareLabel = `PrepareSelected ${phase}`;
  const verifyLabel = `Verify ${phase} ${semantic.toString()}`;

  describe(`validation-trace dispute runner: a step of ${phase}, its ${field} at the inline ${phase} limit`, () => {
    // A raised per-byte fee prices the prepare move over the per-move cap,
    // so our wallet pays it: its largest shape, with the wallet's input and
    // change output.
    const drive = phaseDrive({
      from: stepOf(largest),
      phase,
      semantic,
      evidence: evidence(largest),
      at,
      minFeeA: 80,
      ...installs(largest),
    });

    beforeAll(drive.setUp, 1_800_000);

    it(
      "routes the step to the semantic resolver, committing the transition beside the item and its field",
      drive.routes,
      300_000,
    );

    it(
      "moves the thread through the step's resolution to the award, and closes it",
      drive.awards,
      1_200_000,
    );

    it(`lands the prepare move within the transaction size limit less its reserve, and one more ${unit} would pass it`, () => {
      const bytes = drive.bytesOf(prepareLabel);
      expect(bytes).toBeLessThanOrEqual(MAX_TX_SIZE - SIZE_RESERVE);
      expect(
        bytes + fieldBytes(largest + 1) - fieldBytes(largest),
      ).toBeGreaterThan(MAX_TX_SIZE - SIZE_RESERVE);
      expect(drive.bytesOf(verifyLabel)).toBeLessThanOrEqual(bytes);
    });

    it(
      "pays each move from its thread when its fee fits under the cap, and from our wallet when it does not",
      drive.pays,
    );
  });

  if (coShape !== undefined)
    describeCoShape({
      phase,
      semantic,
      at,
      evidence,
      stepOf,
      installs,
      coShape,
      fieldBytes,
      largest,
      field,
      limit,
    });

  describe.each<readonly [label: string, units: number]>([
    [`one ${unit} over the inline ${phase} limit`, largest + 1],
    ["at the inline carriage's largest", tier1],
    ...overAt.map(([label, bytes]) => [label, within(bytes)] as const),
  ])(
    `validation-trace dispute runner: a step of ${phase}, its ${field} %s`,
    (_, units) => {
      const bytes = fieldBytes(units);
      const drive = phaseDrive({
        from: stepOf(units),
        phase,
        semantic,
        evidence: evidence(units),
        at,
        ...(bytes > RAW_PUBLICATION_MAX_BYTES ? { certifies: true } : {}),
        ...installs(units),
      });

      beforeAll(drive.setUp, 1_800_000);

      it(
        "moves the thread through the step's resolution to the award, and closes it",
        drive.awards,
        1_200_000,
      );

      it(`reads the field by reference: the semantic move reads our publication${bytes > MIDGARD_CHUNK_BYTES_K ? "s" : ""} of its ${bytes.toString()} bytes, and neither move carries it`, () => {
        const { scene } = drive.dispute();
        const reads = ownReadsOf(scene, drive.cborOf(verifyLabel));
        const chunks = reads.map(({ datumCbor }) =>
          Buffer.from(Data.from<string>(datumCbor), "hex"),
        );
        expect(chunks).toHaveLength(Math.ceil(bytes / MIDGARD_CHUNK_BYTES_K));
        expect(chunks.reduce((total, chunk) => total + chunk.length, 0)).toBe(
          bytes,
        );
        if (chunks.length === 1 && "itemBytes" in growth)
          expect(decodeMidgardFieldPreimage(chunks[0]!)).toHaveLength(units);
        expect(ownReadsOf(scene, drive.cborOf(prepareLabel))).toStrictEqual([]);
        for (const label of [prepareLabel, verifyLabel]) {
          expect(drive.bytesOf(label)).toBeLessThan(bytes);
          expect(drive.bytesOf(label)).toBeLessThanOrEqual(
            MAX_TX_SIZE - SIZE_RESERVE,
          );
        }
        const publishes = landed(scene).filter(({ hash }) =>
          reads.some(({ outRef }) => outRef.startsWith(`${hash}#`)),
        );
        for (const { cbor } of publishes)
          expect(cbor.length / 2).toBeLessThanOrEqual(MAX_TX_SIZE);
        reportFit(
          `Publish ${phase} field (${bytes.toString()} bytes)`,
          publishes.map(({ cbor }) => cbor),
        );
        reportFit(`${verifyLabel} reading ${bytes.toString()} bytes`, [
          drive.cborOf(verifyLabel),
        ]);
      });

      it("spends each publication back to our key address by key once the semantic move is final", async () => {
        const dispute = drive.dispute();
        const { scene, runner, a, b } = dispute;
        const reads = ownReadsOf(scene, drive.cborOf(verifyLabel));
        await playThreads({
          scene,
          runner,
          sides: [a, b],
          until: async () =>
            reads.every(({ outRef }) => spenderOf(scene, outRef) !== undefined),
          limit: 40,
        });
        for (const { outRef } of reads)
          expectReclaimedByKey(scene, spenderOf(scene, outRef)!);
      }, 600_000);

      it(
        "pays each move from its thread when its fee fits under the cap, and from our wallet when it does not",
        drive.pays,
      );
    },
  );
};

/** The rows of the step `describeInlineCap` describes beside `coShape`:
 * the field at the limit, which goes by reference, and the largest within
 * the limit less what the co-shape adds beside it, which goes inline. */
const describeCoShape = ({
  phase,
  semantic,
  at,
  evidence,
  stepOf,
  installs,
  coShape,
  fieldBytes,
  largest,
  field,
  limit,
}: Pick<
  Parameters<typeof describeInlineCap>[0],
  | "phase"
  | "semantic"
  | "at"
  | "evidence"
  | "stepOf"
  | "installs"
  | "field"
  | "limit"
> & {
  readonly coShape: CoShape;
  readonly fieldBytes: (units: number) => number;
  readonly largest: number;
}) => {
  let inline = largest;
  while (inline > 1 && fieldBytes(inline) > limit - coShape.besideBytes)
    inline -= 1;
  const prepareLabel = `PrepareSelected ${phase}`;
  const verifyLabel = `Verify ${phase} ${semantic.toString()}`;
  describe.each<readonly [label: string, units: number, byReference: boolean]>([
    [`at the inline ${phase} limit`, largest, true],
    [
      `within the inline ${phase} limit less what it adds beside the field`,
      inline,
      false,
    ],
  ])(
    `validation-trace dispute runner: a step of ${phase}, its ${field} beside ${coShape.label}, %s`,
    (_, units, byReference) => {
      const drive = phaseDrive({
        from: stepOf(units) + coShape.steps,
        phase,
        semantic,
        evidence: evidence(units),
        at,
        minFeeA: 80,
        ...installs(units),
        ...coShape.installs,
      });

      beforeAll(drive.setUp, 1_800_000);

      it(
        "moves the thread through the step's resolution to the award, and closes it",
        drive.awards,
        1_200_000,
      );

      it(`${byReference ? "reads the field by reference" : "carries the field inline"}, each move within the transaction size limit less its reserve even when our wallet pays it`, () => {
        const { scene } = drive.dispute();
        const reads = ownReadsOf(scene, drive.cborOf(verifyLabel));
        expect(reads).toHaveLength(byReference ? 1 : 0);
        expect(ownReadsOf(scene, drive.cborOf(prepareLabel))).toStrictEqual([]);
        for (const label of [prepareLabel, verifyLabel])
          expect(drive.bytesOf(label)).toBeLessThanOrEqual(
            MAX_TX_SIZE - SIZE_RESERVE,
          );
        if (!byReference)
          expect(drive.bytesOf(prepareLabel)).toBeGreaterThan(
            fieldBytes(units),
          );
        reportFit(
          `${prepareLabel} beside ${coShape.label} (${fieldBytes(units).toString()} bytes)`,
          [drive.cborOf(prepareLabel)],
        );
      });
    },
  );
};
