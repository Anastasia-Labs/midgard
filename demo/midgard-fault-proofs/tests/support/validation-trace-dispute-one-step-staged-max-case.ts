/** The validation-trace dispute runner at a staged builtin step (arm 31)
 * whose staged operands are as long as staging takes, on the emulator
 * (validation-trace-dispute-one-step-staged-drive.ts): from Init, the runner
 * stages the step's operands at our staging address, plays the thread
 * through the binder, the staged chain and the settle to the award, each
 * move within the transaction limits, and spends the staging outputs back
 * by our key once the thread has settled. A wrong post state at the step is
 * convicted. The same transaction in a correct block, which stamps it
 * TxIsInvalid, starts no workflow and signs nothing: a correct post state
 * is not convicted. */
import { CML, coreToTxOutput } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import {
  confirmedIntents,
  VALIDATION_TRACE_STAGING_RECLAIM,
} from "../../src/validation-trace-dispute/one-step-staging-journal.js";
import { honestBlock } from "./validation-trace-dispute-max-shape-case.js";
import {
  STAGED_BYTES_ARMS,
  type STAGED_INTEGER_ARMS,
  stagedDrive,
} from "./validation-trace-dispute-one-step-staged-drive.js";
import { reportFit } from "./validation-trace-dispute-scripted-defender.js";
import { stagedScript } from "./validation-trace-dispute-staged-scripts.js";

/**
 * The rows of the staged step of builtin `tag` in the run of the failing
 * script `hex`, run under `exUnits` when given, whose staged operands are
 * `operandBytes` long and whose arm chain is `arms`.
 */
export const stagedMaxCase = ({
  hex,
  exUnits,
  tag,
  operandBytes,
  arms = STAGED_BYTES_ARMS,
}: {
  readonly hex: string;
  readonly exUnits?: readonly [bigint, bigint];
  readonly tag: bigint;
  readonly operandBytes: readonly number[];
  readonly arms?: typeof STAGED_BYTES_ARMS | typeof STAGED_INTEGER_ARMS;
}) => {
  const drive = stagedDrive({
    hex,
    arms,
    ...(exUnits === undefined ? {} : { exUnits }),
  });

  /** The signed transaction `hash` the runner landed. */
  const signedOf = (hash: string) =>
    drive.dispute().scene.l1.signedCbors.get(hash)!;

  /** Our staging outputs, in ledger order. */
  const stagings = () =>
    drive
      .stagings()
      .stagings.map(({ outRef }) => outRef)
      .sort();

  describe("a wrong post state is convicted", () => {
    beforeAll(drive.setUp, 1_800_000);

    it("stages each run of the step's operands at our staging address and moves the thread from the PrepareSelected move through the staged chain to the award within the transaction limits, each hop through the last arm reading every staging output and the settle none, and closes it", async () => {
      const witness = await drive.witness();
      expect(witness.tag).toBe(tag);
      expect(witness.stagedOperands.map(({ length }) => length)).toEqual(
        operandBytes,
      );
      const runs = await drive.runs();
      await drive.play(
        async () => drive.stagings().stagings.length >= runs.length,
      );
      const live = stagings();
      await drive.awards();
      expect(live).toHaveLength(runs.length);
      for (const label of [
        "PrepareSelected core",
        "VerifyCoreStep",
        ...arms.map((holder) => `VerifyArm ${holder}`),
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
      const staged = [
        ...new Set(live.map((outRef) => outRef.split("#")[0]!)),
      ].map(signedOf);
      reportFit("Staging", staged);
      const deposit = live.reduce((sum, outRef) => {
        const [hash, index] = outRef.split("#");
        return (
          sum +
          CML.Transaction.from_cbor_hex(signedOf(hash!))
            .body()
            .outputs()
            .get(Number(index))
            .amount()
            .coin()
        );
      }, 0n);
      console.info(
        `staging: ${runs.length.toString()} runs, ${deposit.toString()} lovelace held`,
      );
    }, 3_600_000);

    it("pays each move from its thread when its fee fits under the cap, and from our wallet when it does not", async () =>
      await drive.pays());

    it("spends each staging output back to our wallet by our key once the thread has settled", async () => {
      const live = stagings();
      await drive.play(
        async () =>
          live.every((outRef) => drive.stagings().reclaimed.has(outRef)),
        60,
      );
      const { scene } = drive.dispute();
      const reclaims = confirmedIntents(
        drive.entries(),
        VALIDATION_TRACE_STAGING_RECLAIM,
      );
      expect(
        reclaims.map(({ actionInput }) => String(actionInput.consumes)).sort(),
      ).toEqual(live);
      for (const { txHash } of reclaims) {
        const body = CML.Transaction.from_cbor_hex(signedOf(txHash)).body();
        expect(body.inputs().len()).toBe(1);
        expect(coreToTxOutput(body.outputs().get(0)).address).toBe(
          scene.h.proverSigner.address,
        );
      }
      reportFit(
        "Staging reclaim",
        reclaims.map(({ txHash }) => signedOf(txHash)),
      );
    }, 1_200_000);
  });

  describe("a correct post state is not convicted", () => {
    honestBlock({ failingScript: stagedScript(hex) });
  });
};
