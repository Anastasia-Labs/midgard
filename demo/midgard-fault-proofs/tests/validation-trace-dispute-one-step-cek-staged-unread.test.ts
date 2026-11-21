/** The validation-trace dispute runner's spend back of a staged builtin
 * step's staging outputs (arm 31) that no move read, on the emulator
 * (support/validation-trace-dispute-one-step-staged-drive.ts), with each
 * transaction final only `RELEASE_DEPTH` blocks under the tip: the runner
 * stages each run of the step's operands while the thread stands at the
 * PrepareSelected move's holder, and past the case's duration another party
 * closes the case before that move lands. The runner spends each staging
 * output it sees back to our wallet, one transaction each, a runner started
 * again mid-way leaving each spent back exactly once, and takes the case's ending waiting on
 * no spend back's finality and on no grace for a staging output the
 * provider does not show; the ending's proof-materials sweep, which reads
 * our proof-materials address rather than out-refs, takes that one back. */
import * as SDK from "@al-ft/midgard-sdk/da";
import { CML, coreToTxOutput, Data, type UTxO } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import { VALIDATION_TRACE_STAGING_OUTSTANDING } from "../src/validation-trace-dispute/one-step-staging-sweep.js";
import { validationTraceCaseExpiryMs } from "../src/validation-trace-dispute/threads.js";
import type { FraudProofWorkflowRunResult } from "../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import { landWith } from "./support/validation-trace-dispute-defender-moves.js";
import { defenderWonCaseClose } from "./support/validation-trace-dispute-defender-won-close.js";
import {
  awaitLastSlotAtOrBefore,
  liveCase,
} from "./support/validation-trace-dispute-one-step-case.js";
import { stagedDrive } from "./support/validation-trace-dispute-one-step-staged-drive.js";
import {
  hideFromProvider,
  onLedger,
} from "./support/validation-trace-dispute-one-step-staging-ledger.js";

/** How many blocks under the tip a transaction is final. */
const RELEASE_DEPTH = 4;

const drive = stagedDrive({ releaseDepth: RELEASE_DEPTH });

const RECLAIM = "validation-trace-reclaim-staging:";

/** Our spend backs the dispute's processes sent, in order. */
const reclaimSends = () =>
  drive
    .sent()
    .flatMap(({ actionId, txHash }) =>
      actionId.startsWith(RECLAIM)
        ? [{ consumes: actionId.slice(RECLAIM.length), txHash }]
        : [],
    );

/** The staging outputs our spend backs spend, each once, in the order
 * first sent. */
const reclaimed = () => [
  ...new Set(reclaimSends().map(({ consumes }) => consumes)),
];

/** Our spend back of `outRef` that the ledger shows landed: one of those
 * we sent for it. */
const expectSpentBackOnce = async (outRef: string) => {
  const spender = await drive.spenderOf(outRef);
  expect(spender, outRef).toBeDefined();
  expect(
    reclaimSends()
      .filter(({ consumes }) => consumes === outRef)
      .map(({ txHash }) => txHash),
    outRef,
  ).toContain(spender);
  return spender!;
};

/** The staging outputs, as the ledger held them once staged. */
let stagings: UTxO[] = [];
/** Each run's outcome. */
const history: string[] = [];
/** Ends the provider's blind spot over the last staging output. */
let hidden: (() => void) | undefined;

const outRefOf = (utxo: Pick<UTxO, "txHash" | "outputIndex">) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/** The out-refs the signed transaction `cbor` spends. */
const inputsOf = (cbor: string): readonly string[] => {
  const inputs = CML.Transaction.from_cbor_hex(cbor).body().inputs();
  return Array.from({ length: inputs.len() }, (_, index) => {
    const input = inputs.get(index);
    return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
  });
};

/** Runs the runner until `until` holds over the run; the run, and the tip
 * it observed. */
const runUntil = async (
  until: (result: FraudProofWorkflowRunResult) => boolean | Promise<boolean>,
  limit = 20,
) => {
  const { scene } = drive.dispute();
  for (let run = 0; run < limit; run += 1) {
    const observedAt = scene.h.emulator.blockHeight;
    const result = await drive.dispute().runner.runOnce(scene.failing);
    history.push(
      "reason" in result ? `${result.kind}: ${result.reason}` : result.kind,
    );
    if (await until(result)) return { result, observedAt };
  }
  throw new Error(`the runner did not get there:\n${history.join("\n")}`);
};

describe("validation-trace dispute runner: spending back a staged step's staging outputs no move read once another party closes the case", () => {
  beforeAll(drive.setUp, 1_800_000);

  it("stages each run of the step's operands while the thread stands at the PrepareSelected move's holder", async () => {
    const runs = await drive.runs();
    expect(runs.length).toBeGreaterThan(1);
    await drive.play(
      async () => (await drive.stagings()).length >= runs.length,
    );
    expect((await drive.placeOfB())?.holder).toBe("prepare");
    const { scene } = drive.dispute();
    const staged = await drive.stagings();
    stagings = staged.map(({ outRef }) => {
      const [txHash, index] = outRef.split("#");
      return scene.h.emulator.ledger[`${txHash!}${index!}`]!.utxo;
    });
    expect(stagings.map(outRefOf)).toStrictEqual(
      staged.map(({ outRef }) => outRef),
    );
    expect(reclaimSends()).toStrictEqual([]);
  }, 1_200_000);

  it("spends each staging output it sees back to our wallet once another party closes the case, one transaction each, a runner started again mid-way leaving each spent back exactly once", async () => {
    const { scene, caseUnit, b, bondOutRef } = drive.dispute();
    const caseUtxo = await liveCase({ scene, caseUnit });
    awaitLastSlotAtOrBefore(
      scene,
      validationTraceCaseExpiryMs(Data.from(caseUtxo.datum!, SDK.Case)) +
        20_000,
    );
    const [bond] = await scene.h.emulator.getUtxosByOutRef([
      {
        txHash: bondOutRef.split("#")[0]!,
        outputIndex: Number(bondOutRef.split("#")[1]),
      },
    ]);
    await landWith(
      b.defender.lucid,
      await defenderWonCaseClose({
        scene,
        lucid: b.defender.lucid,
        caseUtxo,
        bond: bond!,
      }),
    );
    // The last staging output stays out of the provider's view for now.
    hidden = hideFromProvider(scene.h.proverLucid, outRefOf(stagings.at(-1)!));

    await runUntil(() => reclaimSends().length >= 1);
    drive.restart();
    const seen = stagings.slice(0, -1);
    await runUntil(() => seen.every((utxo) => !onLedger(scene, utxo)));
    expect(reclaimed()).toStrictEqual(seen.map(outRefOf));
    for (const utxo of seen) {
      const reclaim = await expectSpentBackOnce(outRefOf(utxo));
      const body = CML.Transaction.from_cbor_hex(
        scene.l1.signedCbors.get(reclaim)!,
      ).body();
      expect(body.inputs().len()).toBe(1);
      expect(body.outputs().len()).toBe(1);
      expect(coreToTxOutput(body.outputs().get(0)).address).toBe(
        scene.h.proverSigner.address,
      );
    }
    // No spend back of the dispute takes the output the provider does not
    // show.
    expect(reclaimed()).not.toContain(outRefOf(stagings.at(-1)!));
  }, 1_200_000);

  it("takes the case's ending waiting on no spend back's finality and on no grace for the staging output the provider does not show, and the ending's proof-materials sweep takes that output back by our key", async () => {
    const { scene } = drive.dispute();
    const last = stagings.at(-1)!;
    const from = history.length;
    const { result, observedAt } = await runUntil(
      (run) => run.kind === "stalled",
    );
    expect("reason" in result && result.reason).toContain(
      "validation-trace case is closed without this workflow's close",
    );
    expect(
      history
        .slice(from)
        .some((run) => run.includes(VALIDATION_TRACE_STAGING_OUTSTANDING)),
      history.join("\n"),
    ).toBe(false);
    // The ending came before the latest spend back was final at release
    // depth.
    const seen = stagings.slice(0, -1);
    const blocks: number[] = [];
    for (const utxo of seen) {
      expect(onLedger(scene, utxo)).toBe(false);
      blocks.push(
        await drive.blockOf((await drive.spenderOf(outRefOf(utxo)))!),
      );
    }
    expect(observedAt - Math.max(...blocks)).toBeLessThan(RELEASE_DEPTH);

    // The ended workflow's proof-materials sweep, which finds our
    // materials by our address rather than by out-ref, took the output the
    // provider does not show back to our wallet, once; the dispute sent no
    // spend back of it. The sweep goes out in front of the ending, not
    // through the dispute's adapter, so the dispute's sends never hold it.
    hidden?.();
    hidden = undefined;
    expect(onLedger(scene, last)).toBe(false);
    const sweep = await drive.spenderOf(outRefOf(last));
    expect(sweep).toBeDefined();
    expect(drive.sent().map(({ txHash }) => txHash)).not.toContain(sweep);
    expect(
      [...scene.l1.signedCbors.values()].filter((cbor) =>
        inputsOf(cbor).includes(outRefOf(last)),
      ),
    ).toHaveLength(1);
    expect(
      coreToTxOutput(
        CML.Transaction.from_cbor_hex(scene.l1.signedCbors.get(sweep!)!)
          .body()
          .outputs()
          .get(0),
      ).address,
    ).toBe(scene.h.proverSigner.address);
    expect(reclaimed()).toStrictEqual(seen.map(outRefOf));
    for (const utxo of seen) await expectSpentBackOnce(outRefOf(utxo));
  }, 600_000);
});
