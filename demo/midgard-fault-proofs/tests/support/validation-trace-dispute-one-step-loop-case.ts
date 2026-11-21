/** The validation-trace dispute runner at a step of a staged semantics group
 * of the CEK core (arm 31: the staged product loop, 9; the serialise loop,
 * 10; the staged conversion, 11; the writeBits loop, 12), or at a builtin
 * failure past group 7's roots (arm 36: the writeBits loop, or the staged
 * failure hop), on the emulator
 * (validation-trace-dispute-one-step-staged-drive.ts): from Init, the runner
 * stages what the step reads (its operands, or a serialiseData step's
 * result) at our staging address, plays the thread through the binder, the
 * step's arm chain, a loop arm once per hop of its loop, and the settle to
 * the award, each move within the transaction limits, and spends the
 * staging outputs back by our key once the thread has settled. A wrong post
 * state at the step is convicted. The same transaction in a correct block,
 * which stamps it TxIsInvalid, starts no workflow and signs nothing: a
 * correct post state is not convicted. */
import { CML, coreToTxOutput } from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it, vi } from "vitest";

import type { ValidationTraceArmHolder } from "../../src/validation-trace-dispute/one-step-holders.js";
import { honestBlock } from "./validation-trace-dispute-max-shape-case.js";
import {
  armLabels,
  lastFailureStep,
  lastStepOfGroup,
  stagedBytesOf,
  stagedDrive,
} from "./validation-trace-dispute-one-step-staged-drive.js";
import { reportFit } from "./validation-trace-dispute-scripted-defender.js";
import { stagedScript } from "./validation-trace-dispute-staged-scripts.js";

/** The arms of group 5's chain before its stage. */
const ROOTS_TO_BUDGET = [
  "semanticRoots",
  "semanticResult",
  "semanticBudget",
] as const satisfies readonly ValidationTraceArmHolder[];

/** The stage of each staged semantics group this case drives. */
const STAGES = {
  9: "stagedProduct",
  10: "semanticSerialiseLoop",
  11: "stagedConversion",
  12: "stagedWriteBitsLoop",
} as const satisfies Readonly<Record<number, ValidationTraceArmHolder>>;

/** The stage group 7's roots route each builtin failure this case drives
 * to. */
const FAILURE_STAGES = {
  writeBits: "stagedWriteBitsLoop",
  staged: "stagedFailure",
} as const satisfies Readonly<Record<string, ValidationTraceArmHolder>>;

/** The step a case drives: the last of a staged semantics group, or the
 * builtin failure past group 7's roots. */
export type LoopCaseStep =
  | { readonly group: keyof typeof STAGES }
  | { readonly failure: keyof typeof FAILURE_STAGES };

export type LoopCaseDrive = ReturnType<typeof stagedDrive>;

/** The arms before the stage of `step`'s chain, its stage, the step, and
 * the chain's name. */
const routeOf = (step: LoopCaseStep) =>
  "group" in step
    ? {
        before: ROOTS_TO_BUDGET,
        stage: STAGES[step.group],
        from: lastStepOfGroup(step.group),
        chain: `the group-${step.group.toString()} chain`,
      }
    : {
        before: ["semanticFailureRoots"] as const,
        stage: FAILURE_STAGES[step.failure],
        from: lastFailureStep,
        chain: "group 7's roots",
      };

/**
 * The rows of the step `step` in the run of the failing script `hex`, run
 * under `exUnits` when given: what the driver stages for it is
 * `stagedBytes` long, and its stage takes `hops` hops. `more` adds rows
 * over the convicting drive.
 */
export const loopCase = ({
  hex,
  exUnits,
  hops,
  stagedBytes,
  more,
  ...step
}: LoopCaseStep & {
  readonly hex: string;
  readonly exUnits?: readonly [bigint, bigint];
  readonly hops: number;
  readonly stagedBytes: readonly number[];
  readonly more?: (drive: LoopCaseDrive) => void;
}) => {
  const { before, stage, from, chain } = routeOf(step);
  const arms: readonly ValidationTraceArmHolder[] = [
    ...before,
    ...Array<ValidationTraceArmHolder>(hops).fill(stage),
  ];
  const drive = stagedDrive({
    hex,
    from,
    arms,
    ...(exUnits === undefined ? {} : { exUnits }),
    // The case may close before every staging output is spent back: the
    // block's removal then goes first, and the spend backs follow it.
    withRemoval: true,
  });
  const staging = stagedBytes.length > 0;

  /** The signed transaction `hash` the runner landed. */
  const signedOf = (hash: string) =>
    drive.dispute().scene.l1.signedCbors.get(hash)!;

  /** Our staging outputs the ledger shows, in ledger order. */
  const stagings = async () =>
    (await drive.stagedOnL1()).map(({ outRef }) => outRef).sort();

  describe("a wrong post state is convicted", () => {
    beforeAll(drive.setUp, 3_600_000);
    afterAll(() => drive.dispute()?.recorder?.restore());

    it(`moves the thread from the PrepareSelected move through the binder, ${chain}, its stage ${hops.toString()} times, and the settle to the award within the transaction limits, each hop through the last arm reading every staging output and the settle none, and closes it`, async () => {
      expect(
        stagedBytesOf(await drive.stepWitness()).map(({ length }) => length),
      ).toEqual(stagedBytes);
      const runs = await drive.runs();
      expect(runs.length > 0).toBe(staging);
      await drive.play(async () => (await stagings()).length >= runs.length);
      const live = await stagings();
      await drive.awards();
      expect(live).toHaveLength(runs.length);
      for (const label of [
        "PrepareSelected core",
        "VerifyCoreStep",
        ...armLabels(arms),
        "Settle core",
      ]) {
        const hash = CML.hash_transaction(
          CML.Transaction.from_cbor_hex(drive.cborOf(label)).body(),
        ).to_hex();
        expect(
          drive.readsOf(hash).filter((outRef) => live.includes(outRef)),
          label,
        ).toEqual(label === "Settle core" ? [] : live);
        console.info(
          `fee ${label}: ${drive.feeOf(label).toString()} lovelace, ${drive.bytesOf(label).toString()} bytes`,
        );
      }
      if (!staging) return;
      reportFit(
        "Staging",
        [...new Set(live.map((outRef) => outRef.split("#")[0]!))].map(signedOf),
      );
      console.info(
        `staging: ${runs.length.toString()} outputs for ${stagedBytes.join("+")} bytes`,
      );
    }, 7_200_000);

    it("pays each move from its thread when its fee fits under the cap, and from our wallet when it does not", async () =>
      await drive.pays());

    if (staging)
      it("spends each staging output back to our wallet by our key once the thread has settled", async () => {
        const live = await stagings();
        const { scene } = drive.dispute();
        // The removal dates its validity window from the application clock,
        // which on the emulator is the emulator's.
        const clock = vi
          .spyOn(Date, "now")
          .mockImplementation(() => scene.h.emulator.now());
        try {
          await drive.play(async () => {
            for (const outRef of live)
              if ((await drive.live(outRef)) !== undefined) return false;
            return true;
          }, 60);
        } finally {
          clock.mockRestore();
        }
        const reclaims = await Promise.all(live.map(drive.spenderOf));
        expect(reclaims.every((txHash) => txHash !== undefined)).toBe(true);
        for (const txHash of reclaims as string[]) {
          const body = CML.Transaction.from_cbor_hex(signedOf(txHash)).body();
          expect(body.inputs().len()).toBe(1);
          expect(coreToTxOutput(body.outputs().get(0)).address).toBe(
            scene.h.proverSigner.address,
          );
        }
      }, 1_200_000);

    more?.(drive);
  });

  describe("a correct post state is not convicted", () => {
    honestBlock({ failingScript: stagedScript(hex) });
  });
};
