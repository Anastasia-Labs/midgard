/** The decode records a validation-trace dispute makes before Init, on a
 * deployment that drives CEK script selections, under the watcher's prover
 * funding. A watcher with no publication observer refuses to run the
 * dispute at all, signing nothing, and the funding permit of one whose
 * roster does not govern the script-decode contract refuses the decode
 * record that pays to it: the workflow stalls there, signing nothing more,
 * and the case does not open. The same watcher, observing publications and
 * governing the contract, records the decode of the script our trace
 * selects and then opens the case.
 *
 * The category is not installed until its defender exists; this file
 * stands in for that install so the production classifier mints the
 * objective's actuation permit. */
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { VALIDATION_TRACE_CASE_OPEN } from "../src/validation-trace-dispute/adapter.js";
import type { ValidationTraceOneStepReferences } from "../src/validation-trace-dispute/transactions.js";
import { SCRIPT_DECODE_SPEND_ROLE } from "../src/workflow/script-decode-prerequisite.js";
import { drainProver } from "./support/installed-prover-wallet.js";
import {
  type AdapterWrap,
  type FundedDispute,
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
const opened: FundedDispute[] = [];

/** A watcher over the scene, with `options`. */
const open = async (
  options: Omit<
    NonNullable<Parameters<typeof openFundedDispute>[1]>,
    "oneStep"
  > = {},
) => {
  const dispute = await openFundedDispute(scene, { ...options, oneStep });
  opened.push(dispute);
  return dispute;
};

/** The prover wallet's outputs, by out-ref. */
const walletOutRefs = async () =>
  (await scene.h.emulator.getUtxos(scene.h.proverSigner.address))
    .map(({ txHash, outputIndex }) => `${txHash}#${outputIndex.toString()}`)
    .sort();

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

describe("validation-trace dispute decode records before Init under the prover funding permit", () => {
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

  afterAll(async () => {
    for (const dispute of opened) await dispute.close();
  });

  it("refuses to run the dispute without a publication observer, signing nothing", async () => {
    const before = await walletOutRefs();
    const dispute = await open({ publications: false });
    await expect(dispute.turn()).rejects.toThrow(
      /^validation-trace script-decode records need a publication observer$/u,
    );
    expect(await walletOutRefs()).toStrictEqual(before);
  }, 600_000);

  it(
    "stalls the decode record whose output the roster does not govern, under a roster without the script-decode contract: it signs nothing more and the case does not open",
    async () => {
      const dispute = await open({
        roster: (hashes) =>
          Object.fromEntries(
            Object.entries(hashes).filter(
              ([name]) => name !== SCRIPT_DECODE_SPEND_ROLE,
            ),
          ),
      });
      const order: string[] = [];
      const reasons: string[] = [];
      let stalledAt: string[] | undefined;
      for (let turn = 0; turn < 6; turn += 1) {
        const result = await dispute.turn(recordOrder(order));
        reasons.push(
          "reason" in result ? `${result.kind}: ${result.reason}` : result.kind,
        );
        if (result.kind === "stalled") stalledAt ??= await walletOutRefs();
      }
      const stalls = reasons.filter((reason) => reason.startsWith("stalled"));
      expect(stalls.length, reasons.join("\n")).toBeGreaterThan(0);
      for (const reason of stalls)
        expect(reason).toMatch(
          /^stalled: funding reservation failed for script-decode-bind:.*funding output escapes the governed contract roster$/u,
        );
      expect(order).not.toContain("init");
      expect(
        reasons.some((reason) => reason.includes(VALIDATION_TRACE_CASE_OPEN)),
      ).toBe(false);
      expect(await walletOutRefs()).toStrictEqual(stalledAt);
    },
    TIMEOUT,
  );

  it(
    "records the decode of the script our trace selects, then opens the case, once the watcher observes publications and governs the contract",
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
