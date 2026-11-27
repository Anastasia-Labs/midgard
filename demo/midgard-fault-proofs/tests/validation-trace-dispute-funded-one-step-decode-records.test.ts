/** The decode records a validation-trace dispute makes before Init, on a
 * deployment that drives CEK script selections, under the watcher's prover
 * funding: the watcher records the decode of the script our trace selects
 * and then opens the case.
 *
 * The category is not installed until its defender exists; this file
 * stands in for that install so the production classifier mints the
 * objective's actuation permit. */
import { beforeAll, describe, expect, it } from "vitest";

import { VALIDATION_TRACE_CASE_OPEN } from "../src/validation-trace-dispute/adapter.js";
import type { ValidationTraceOneStepReferences } from "../src/validation-trace-dispute/transactions.js";
import { drainProver } from "./support/installed-prover-wallet.js";
import {
  type AdapterWrap,
  openFundedDispute,
  turnUntil,
} from "./support/validation-trace-dispute-funded-runner.js";
import {
  FUNDED,
  TIMEOUT,
} from "./support/validation-trace-dispute-funded-verify-source.js";
import {
  publishOneStepReferences,
  publishSelectionReferences,
} from "./support/validation-trace-dispute-one-step.js";
import {
  type Scene,
  setUpScene,
} from "./support/validation-trace-dispute-open-scene.js";

let scene: Scene;
let oneStep: ValidationTraceOneStepReferences;

/** A watcher over the scene, with `options`. */
const open = async (
  options: Omit<
    NonNullable<Parameters<typeof openFundedDispute>[1]>,
    "oneStep"
  > = {},
) => await openFundedDispute(scene, { ...options, oneStep });

/** Records the stage of each action a watcher preflights into `order`. */
const recordOrder =
  (order: string[]): AdapterWrap =>
  (adapter) => ({
    ...adapter,
    preflight: async (context) => {
      const preflight = await adapter.preflight(context);
      order.push(String(context.action.input.stage));
      return preflight;
    },
  });

describe("validation-trace dispute decode records before Init under the prover funding", () => {
  beforeAll(async () => {
    // Published before the scene's blocks, so the publications take none of
    // the source verification's window from the failing block's end.
    let references: ValidationTraceOneStepReferences | undefined;
    scene = await setUpScene(async (contracts) => {
      references = {
        ...(await publishOneStepReferences(contracts)),
        cekSelection: await publishSelectionReferences(contracts),
      };
    });
    oneStep = references!;
    await drainProver(scene.h as never, FUNDED);
  }, 1_800_000);

  it(
    "records the decode of the script our trace selects, then opens the case",
    async () => {
      const order: string[] = [];
      const dispute = await open();
      const { result, history } = await turnUntil(
        dispute,
        VALIDATION_TRACE_CASE_OPEN,
        { wrap: recordOrder(order), limit: 30 },
      );
      expect(result.kind, history.join("\n")).toBe("pending");
      const finish = order.indexOf("script_decode_finish");
      expect(order[0]).toBe("script_decode_stage");
      expect(finish).toBeGreaterThan(0);
      expect(order.indexOf("init")).toBe(finish + 1);
    },
    TIMEOUT,
  );
});
