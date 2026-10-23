/** The validation-trace dispute runner's spend back of a staged builtin
 * step's staging outputs (arm 31) that no move read, on the emulator
 * (support/validation-trace-dispute-one-step-staged-drive.ts), with each
 * transaction final only `RELEASE_DEPTH` blocks under the tip: the runner
 * stages each run of the step's operands while the thread stands at the
 * PrepareSelected move's holder, and past the case's duration another party
 * closes the case before that move lands. The runner spends each staging
 * output back to our wallet, one transaction each, a runner started again
 * mid-way spending none twice, and takes the case's ending only once every
 * spend back is final at release depth and no staging output the provider
 * does not show may still be live. */
import * as SDK from "@al-ft/midgard-sdk";
import { CML, coreToTxOutput, Data, type UTxO } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import { VALIDATION_TRACE_STAGING_RECLAIM } from "../src/validation-trace-dispute/one-step-staging-journal.js";
import { VALIDATION_TRACE_STAGING_OUTSTANDING } from "../src/validation-trace-dispute/one-step-staging-sweep.js";
import { validationTraceCaseExpiryMs } from "../src/validation-trace-dispute/threads.js";
import type { FraudProofWorkflowJournalEntry } from "../src/workflow/journal.js";
import type { FraudProofWorkflowRunResult } from "../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import { landWith } from "./support/validation-trace-dispute-defender-moves.js";
import { defenderWonCaseClose } from "./support/validation-trace-dispute-defender-won-close.js";
import {
  awaitLastSlotAtOrBefore,
  liveCase,
} from "./support/validation-trace-dispute-one-step-case.js";
import {
  entriesOf,
  stagedDrive,
} from "./support/validation-trace-dispute-one-step-staged-drive.js";
import {
  hideFromProvider,
  onLedger,
} from "./support/validation-trace-dispute-one-step-staging-ledger.js";

/** How many blocks under the tip a transaction is final. */
const RELEASE_DEPTH = 4;

const drive = stagedDrive({ releaseDepth: RELEASE_DEPTH });

/** Our spend backs the journal shows submitted, in order. */
const reclaimIntents = (entries: readonly FraudProofWorkflowJournalEntry[]) =>
  entries.flatMap(({ event }) =>
    event.kind === "submission_intent" &&
    event.actionInput.schemaVersion === VALIDATION_TRACE_STAGING_RECLAIM
      ? [
          {
            actionId: event.actionId,
            consumes: event.actionInput.consumes as string,
            txHash: event.txHash,
          },
        ]
      : [],
  );

/** The staging outputs, as the ledger held them once staged. */
let stagings: UTxO[] = [];
/** The latest journal, and each run's outcome. */
let entries: readonly FraudProofWorkflowJournalEntry[] = [];
const history: string[] = [];
/** Ends the provider's blind spot over the last staging output. */
let hidden: (() => void) | undefined;

const outRefOf = (utxo: Pick<UTxO, "txHash" | "outputIndex">) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

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
    entries = entriesOf(result);
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
    await drive.play(async () => drive.stagings().length >= runs.length);
    expect((await drive.placeOfB())?.holder).toBe("prepare");
    const { scene } = drive.dispute();
    stagings = drive.stagings().map(({ outRef }) => {
      const [txHash, index] = outRef.split("#");
      return scene.h.emulator.ledger[`${txHash!}${index!}`]!.utxo;
    });
    expect(stagings.map(outRefOf)).toStrictEqual(
      drive.stagings().map(({ outRef }) => outRef),
    );
    expect(reclaimIntents(drive.entries())).toStrictEqual([]);
  }, 1_200_000);

  it("spends each staging output it sees back to our wallet once another party closes the case, one transaction each, a runner started again mid-way spending none twice", async () => {
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

    await runUntil(() => reclaimIntents(entries).length >= 1);
    drive.restart();
    const seen = stagings.slice(0, -1);
    await runUntil(() => seen.every((utxo) => !onLedger(scene, utxo)));
    const intents = reclaimIntents(entries);
    expect(intents.map(({ consumes }) => consumes)).toStrictEqual(
      seen.map(outRefOf),
    );
    for (const utxo of seen) {
      const reclaim = await drive.spenderOf(outRefOf(utxo));
      expect(reclaim, outRefOf(utxo)).toBe(
        intents.find(({ consumes }) => consumes === outRefOf(utxo))!.txHash,
      );
      const body = CML.Transaction.from_cbor_hex(
        scene.l1.signedCbors.get(reclaim!)!,
      ).body();
      expect(body.inputs().len()).toBe(1);
      expect(body.outputs().len()).toBe(1);
      expect(coreToTxOutput(body.outputs().get(0)).address).toBe(
        scene.h.proverSigner.address,
      );
    }
    expect(onLedger(scene, stagings.at(-1)!)).toBe(true);
  }, 1_200_000);

  it("takes the case's ending only once every spend back is final at release depth and the staging output the provider does not show has come back", async () => {
    const { scene } = drive.dispute();
    const last = stagings.at(-1)!;
    const unseen = `${VALIDATION_TRACE_STAGING_OUTSTANDING}: ${outRefOf(last)} is not yet visible to the provider`;
    // Once every spend back so far is final at release depth, the output
    // the provider does not show still holds the ending.
    await runUntil((run) => run.kind === "pending" && run.reason === unseen);
    for (let run = 0; run < RELEASE_DEPTH + 2; run += 1) {
      const { result } = await runUntil(() => true, 1);
      expect(result.kind === "pending" && result.reason).toBe(unseen);
    }
    expect(onLedger(scene, last)).toBe(true);
    hidden?.();
    hidden = undefined;

    const { result, observedAt } = await runUntil(
      (run) => run.kind === "stalled",
    );
    expect("reason" in result && result.reason).toContain(
      "validation-trace case is closed without this workflow's close",
    );
    // One spend back for each staging output, each final at release depth
    // when the ending was taken.
    const intents = reclaimIntents(entries);
    expect(intents.map(({ consumes }) => consumes)).toStrictEqual(
      stagings.map(outRefOf),
    );
    for (const [index, utxo] of stagings.entries()) {
      expect(onLedger(scene, utxo)).toBe(false);
      const reclaim = await drive.spenderOf(outRefOf(utxo));
      expect(reclaim).toBe(intents[index]!.txHash);
      expect(
        observedAt - (await drive.blockOf(reclaim!)),
      ).toBeGreaterThanOrEqual(RELEASE_DEPTH);
    }
  }, 600_000);
});
