/** The validation-trace dispute runner among outputs other parties place at
 * its addresses, through the shared own-outputs harness: copies of its
 * pending claim's datum (exact, and in other shapes) at its contract
 * before VerifySource, and copies of its case and bond datums at their
 * contracts once the case is open. The runner spends and references only
 * its own journaled outputs: VerifySource consumes the claim Open made, no
 * transaction of ours touches a planted output, every plant stays live, and
 * running again over the open case submits nothing. A token-bearing coin at
 * the prover's address is the funding permit's to keep out of coin
 * selection; validation-trace-dispute-funded.test.ts runs it there. */
import { beforeAll, describe, expect, it } from "vitest";

import {
  confirmedValidationTraceStages,
  parseValidationTraceAction,
} from "../src/validation-trace-dispute/actions.js";
import { VALIDATION_TRACE_CASE_OPEN } from "../src/validation-trace-dispute/adapter.js";
import { parseOwnOutRef } from "../src/workflow/own-outputs.js";
import {
  type LedgerWatch,
  type OwnOutputRole,
  plantCopies,
  plantShapes,
  watchLedger,
} from "./support/own-outputs-installed.js";
import {
  type Scene,
  setUpScene,
} from "./support/validation-trace-dispute-open-scene.js";
import {
  makeRunner,
  runUntil,
} from "./support/validation-trace-dispute-runner.js";

let scene: Scene;
let watch: LedgerWatch;

/** Every look-alike of our output at `outRef`, as `role`: other shapes and
 * exact copies of its inline datum, at its own address. */
const plantLookalikes = async (role: OwnOutputRole["role"], outRef: string) => {
  const [output] = await scene.h.emulator.getUtxosByOutRef([
    parseOwnOutRef(outRef),
  ]);
  if (typeof output?.datum !== "string")
    throw new Error(`no inline datum at ${outRef}`);
  const lookalike = { role, address: output.address, datums: [output.datum] };
  await plantShapes(watch, scene.h.funderLucid, lookalike);
  scene.h.emulator.awaitBlock(1);
  await plantCopies(watch, scene.h.funderLucid, lookalike);
  scene.h.emulator.awaitBlock(1);
};

/** No transaction of ours spent or read a planted output; each one the
 * other party left unspent is live. */
const expectPlantsUntouched = async () => {
  const planted = watch.plantedOutRefs();
  expect(planted.size).toBeGreaterThan(0);
  for (const transaction of watch.ours())
    expect(
      [...transaction.inputs, ...transaction.referenceInputs].filter((outRef) =>
        planted.has(outRef),
      ),
    ).toStrictEqual([]);
  const live = await scene.h.emulator.getUtxosByOutRef(
    [...planted].map(parseOwnOutRef),
  );
  // The other party's own change is its to spend in later plants.
  const spentByOthers = new Set(
    watch.accepted
      .filter(({ planted }) => planted)
      .flatMap(({ inputs }) => inputs),
  );
  for (const outRef of planted)
    if (!spentByOthers.has(outRef))
      expect(
        live.some(
          (utxo) => `${utxo.txHash}#${utxo.outputIndex.toString()}` === outRef,
        ),
        outRef,
      ).toBe(true);
};

describe("validation-trace dispute runner among look-alike outputs", () => {
  beforeAll(async () => {
    scene = await setUpScene();
    watch = watchLedger(scene.h.emulator);
  }, 1_800_000);

  it("VerifySources its own pending claim and keeps its own case, never a planted look-alike", async () => {
    // Before VerifySource is built, the pending claim Open made is copied
    // at its contract. The orchestrator retries a failed preflight, so a
    // failed plant is kept.
    let openClaim: string | undefined;
    let planted: unknown = "not attempted";
    const runner = makeRunner(scene, {
      wrap: (adapter) => ({
        ...adapter,
        preflight: async (context) => {
          const { stage, consumes } = parseValidationTraceAction(
            context.action,
          );
          if (openClaim === undefined && stage === "verify_source") {
            openClaim = consumes!;
            try {
              await plantLookalikes("validation_trace_opener", openClaim);
              planted = true;
            } catch (error) {
              planted = error;
            }
          }
          return await adapter.preflight(context);
        },
      }),
    });

    const { result, history } = await runUntil(
      runner,
      scene.failing,
      VALIDATION_TRACE_CASE_OPEN,
    );
    expect(planted).toBe(true);
    if (!("entries" in result)) throw new Error(history.join("\n"));
    const verify = confirmedValidationTraceStages(result.entries).find(
      ({ action }) => action.stage === "verify_source",
    )!;
    expect(openClaim).toBeDefined();
    const open = confirmedValidationTraceStages(result.entries).find(
      ({ action }) => action.stage === "open",
    )!;
    expect(verify.action.consumes).toBe(open.outputs[0]!.outRef);
    expect(openClaim).toBe(open.outputs[0]!.outRef);
    const [caseClaim, bondClaim] = verify.outputs;

    await plantLookalikes("validation_trace_case", caseClaim!.outRef);
    await plantLookalikes("validation_trace_bond", bondClaim!.outRef);

    const ours = watch.ours().length;
    const again = await runner.runOnce(scene.failing);
    expect(again.kind).toBe("pending");
    if (again.kind === "pending")
      expect(again.reason.startsWith(VALIDATION_TRACE_CASE_OPEN)).toBe(true);
    if (!("entries" in again)) throw new Error("no workflow journal");
    expect(
      again.entries.filter(({ event }) => event.kind === "submission_intent"),
    ).toHaveLength(3);
    expect(watch.ours()).toHaveLength(ours);
    const still = confirmedValidationTraceStages(again.entries).find(
      ({ action }) => action.stage === "verify_source",
    )!;
    expect(still.outputs.map(({ outRef }) => outRef)).toStrictEqual([
      caseClaim!.outRef,
      bondClaim!.outRef,
    ]);
    await expectPlantsUntouched();
  }, 1_800_000);
});
