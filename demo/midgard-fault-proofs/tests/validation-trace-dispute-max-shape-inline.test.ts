/** The validation-trace dispute runner from Init to conviction at the
 * deepest trace admission takes, the failing transaction's redeemer field at
 * the inline observe limit and the operator's thread at the trace's last CEK
 * compute step (validation-trace-dispute-max-shape-case.ts). The same
 * transaction in an honest block, which stamps it TxIsInvalid, starts no
 * workflow and signs nothing. */
import type { UTxO } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import { VALIDATION_TRACE_INLINE_OBSERVE_MAX_PREIMAGE_BYTES } from "../src/validation-trace-dispute/one-step-canonical-decode.js";
import { ADMITTED_TX_EX_UNITS } from "./helpers/plutus-execution-block-fixture.js";
import {
  deepestCase,
  LONGEST_FAILING_SCRIPT,
  TIMEOUT,
} from "./support/validation-trace-dispute-max-shape-case.js";
import {
  type Scene,
  setUpScene,
} from "./support/validation-trace-dispute-open-scene.js";
import { redeemerDataForFieldPreimage } from "./support/validation-trace-dispute-redeemer-field.js";
import { makeRunner } from "./support/validation-trace-dispute-runner.js";

const outRefOf = (utxo: UTxO) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

describe("validation-trace dispute runner at the deepest trace admission takes", () => {
  deepestCase({
    fieldBytes: VALIDATION_TRACE_INLINE_OBSERVE_MAX_PREIMAGE_BYTES,
    group: 0,
  });

  describe("the same transaction in an honest block", () => {
    let scene: Scene;

    beforeAll(async () => {
      scene = await setUpScene(undefined, {
        failingScript: LONGEST_FAILING_SCRIPT,
        failingExUnits: ADMITTED_TX_EX_UNITS,
        failingRedeemerData: redeemerDataForFieldPreimage(
          VALIDATION_TRACE_INLINE_OBSERVE_MAX_PREIMAGE_BYTES,
          ADMITTED_TX_EX_UNITS,
        ),
        failingStampedInvalid: true,
      });
    }, TIMEOUT);

    it(
      "starts no workflow and signs nothing",
      async () => {
        const unspent = () =>
          Object.values(scene.h.emulator.ledger)
            .filter(({ spent }) => !spent)
            .map(({ utxo }) => outRefOf(utxo));
        const signed = new Set(scene.l1.signedCbors.keys());
        const before = unspent();
        const result = await makeRunner(scene).runOnce(scene.failing);
        expect(result.kind).toBe("no_fault_detected");
        expect(
          [...scene.l1.signedCbors.keys()].filter((hash) => !signed.has(hash)),
        ).toStrictEqual([]);
        expect(unspent()).toStrictEqual(before);
      },
      TIMEOUT,
    );
  });
});
