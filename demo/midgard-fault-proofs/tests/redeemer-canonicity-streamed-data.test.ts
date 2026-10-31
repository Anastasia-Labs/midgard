import { writeFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

import {
  REDEEMER_DATA_CHUNK_BYTES,
  redeemerCanonicityEvidenceCloses,
} from "../src/redeemer-canonicity/index.js";
import {
  dataStep,
  mint,
  openAcceptedThread,
  readToVerdict,
  recordedCanonical,
  refuseDataSeams,
  refuseFinalize,
} from "./redeemer-canonicity-streamed-data.open-thread.js";
import {
  densePlutusData,
  type DensePlutusDataShape,
} from "./support/dense-plutus-data.js";
import type { CompleteSignedTransactionMeasurement } from "./support/emulator/measurement.js";

/** The largest redeemer data a field-8 item can carry. */
const MAXIMUM_REDEEMER_DATA_BYTES = 32_743;

/** Below this, a data step's execution budget is never at risk. */
const STEP_MEMORY_CAP = 13_200_000n;
const STEP_CPU_CAP = 8_000_000_000n;

const fit: [string, CompleteSignedTransactionMeasurement][] = [];

const progress = (message: string): void => {
  if (process.env.MIDGARD_PRINT_FIT === "1")
    console.info(`[redeemer-canonicity-streamed] ${message}`);
};

const hex = (text: string) => Buffer.from(text, "hex");

describe("redeemer-canonicity streamed data verdict", () => {
  it("convicts the acceptance of redeemer data whose indefinite map sits five levels deep", async () => {
    const data = hex("9f9f9f9fbf0101ffffffffff");
    const opened = await openAcceptedThread(data);
    expect(opened.evidence.canonical).toBe(false);
    expect(redeemerCanonicityEvidenceCloses(opened.evidence)).toBe(true);
    const read = await readToVerdict(opened, opened.threadOutRef);
    expect(await recordedCanonical(opened, read.threadOutRef)).toBe(false);
    const minted = await mint(opened, read.threadOutRef);
    expect(minted.result.fraudProofUnit).toBeTruthy();
  }, 600_000);

  it("convicts the acceptance of redeemer data whose only flaw lies past its first 30,000 bytes", async () => {
    const flawAt = 1 + 455 * 66;
    const data = Buffer.concat([
      hex("9f"),
      ...Array.from({ length: 455 }, () =>
        Buffer.concat([hex("5840"), Buffer.alloc(64, 0x5a)]),
      ),
      hex("1800"),
      hex("ff"),
    ]);
    expect(flawAt).toBeGreaterThan(30_000);
    expect(data.subarray(flawAt, flawAt + 2)).toEqual(hex("1800"));
    const opened = await openAcceptedThread(data);
    expect(opened.evidence.canonical).toBe(false);
    expect(opened.evidence.carriage).toBe("Certified");
    const read = await readToVerdict(opened, opened.threadOutRef);
    expect(read.steps).toBe(opened.evidence.dataPlan!.steps.length);
    expect(read.steps).toBeGreaterThan(1);
    expect(await recordedCanonical(opened, read.threadOutRef)).toBe(false);
    const minted = await mint(opened, read.threadOutRef);
    expect(minted.result.fraudProofUnit).toBeTruthy();
  }, 900_000);

  it("refuses a false acceptance claim against canonical redeemer data nested 4,000 lists deep, and every forged data step on the way", async () => {
    const data = Buffer.concat([
      Buffer.alloc(4_000, 0x9f),
      hex("00"),
      Buffer.alloc(4_000, 0xff),
    ]);
    const opened = await openAcceptedThread(data);
    expect(opened.evidence.canonical).toBe(true);
    expect(redeemerCanonicityEvidenceCloses(opened.evidence)).toBe(false);
    const plan = opened.evidence.dataPlan!;
    progress(`4,000 levels: ${plan.steps.length.toString()} data steps`);
    // Forge at the opening checkpoint, then again once frames are hidden
    // below the revealed ones, and at the verdict.
    const opening = await refuseDataSeams(opened, opened.threadOutRef);
    // The opening window runs into the span's second chunk.
    expect(opening.windowLength).toBeGreaterThan(REDEEMER_DATA_CHUNK_BYTES);
    const first = await dataStep(opened, opened.threadOutRef);
    expect(first.result.finished).toBe(false);
    const deep = await refuseDataSeams(opened, first.result.nextThreadOutRef);
    expect(deep.stack.length).toBeGreaterThan(deep.revealedCount);
    let current = first.result.nextThreadOutRef;
    for (let index = 1; index < plan.steps.length - 1; index += 1)
      current = (await dataStep(opened, current)).result.nextThreadOutRef;
    const last = await refuseDataSeams(opened, current);
    expect(last.next.kind).toBe("finish");
    const verdict = await dataStep(opened, current);
    expect(verdict.result.finished).toBe(true);
    await refuseFinalize(opened, verdict.result.nextThreadOutRef);
  }, 1_800_000);

  it("refuses a false acceptance claim against canonical 32,743-byte redeemer data in each of the densest shapes, reading each to its verdict, and measures the costliest data step", async () => {
    let costliest: CompleteSignedTransactionMeasurement | undefined;
    let costliestShape = "";
    const counts: Record<string, number> = {};
    // Each data step's measured memory beside the planner's estimate.
    const perStep: Record<string, [string, number][]> = {};
    for (const shape of [
      "map-chain",
      "generic-constructors",
      "flat-integers",
      "chunked-bytes",
    ] as const satisfies readonly DensePlutusDataShape[]) {
      const data = densePlutusData(shape, MAXIMUM_REDEEMER_DATA_BYTES);
      expect(data).toHaveLength(MAXIMUM_REDEEMER_DATA_BYTES);
      const opened = await openAcceptedThread(data);
      expect(opened.evidence.canonical).toBe(true);
      expect(opened.evidence.carriage).toBe("Certified");
      for (const [index, envelope] of opened.envelope.entries())
        fit.push([
          `${shape}-envelope-${index.toString()}`,
          envelope.measurement,
        ]);
      progress(
        `${shape}: ${opened.evidence.dataPlan!.steps.length.toString()} data steps planned`,
      );
      const planned = opened.evidence.dataPlan!.steps;
      const stepMemory: [string, number][] = [];
      perStep[shape] = stepMemory;
      const read = await readToVerdict(opened, opened.threadOutRef, (m) => {
        const estimate = planned[stepMemory.length]!.estimatedMemory;
        stepMemory.push([m.executionMemory.toString(), estimate]);
        // Never above the planned estimate, which the planner keeps below
        // one transaction's budget.
        expect(m.executionMemory).toBeLessThanOrEqual(BigInt(estimate));
        expect(m.executionMemory).toBeLessThanOrEqual(STEP_MEMORY_CAP);
        expect(m.executionSteps).toBeLessThanOrEqual(STEP_CPU_CAP);
        expect(m.l1ByteMargin).toBeGreaterThan(0);
        if (
          costliest === undefined ||
          m.executionMemory > costliest.executionMemory
        ) {
          costliest = m;
          costliestShape = shape;
        }
      });
      expect(read.steps).toBe(opened.evidence.dataPlan!.steps.length);
      counts[shape] = read.steps;
      progress(`${shape}: ${read.steps.toString()} data steps taken`);
      await refuseFinalize(opened, read.threadOutRef);
    }
    progress(
      `costliest data step: ${costliestShape} ${costliest!.executionMemory.toString()} mem ${costliest!.executionSteps.toString()} cpu; steps ${JSON.stringify(counts)}`,
    );
    fit.push(["redeemer_canonicity_w1_step_max", costliest!]);
    const output = process.env.MIDGARD_FIT_OUT;
    if (output !== undefined)
      await writeFile(
        output,
        JSON.stringify(
          {
            costliestShape,
            dataSteps: counts,
            perStep,
            rows: fit.map(([name, measurement]) => ({
              name,
              signedBytes: measurement.completeSignedBytes,
              memoryUnits: measurement.executionMemory.toString(),
              cpuUnits: measurement.executionSteps.toString(),
            })),
          },
          null,
          2,
        ),
      );
  }, 7_200_000);
});
