/** The validation-trace dispute runner at a staged builtin step (arm 31,
 * semantics group 7) whose operand is past the direct carriage bound, on the
 * emulator (support/validation-trace-dispute-one-step-staged-drive.ts): the
 * runner stages each run of the step's operands at our staging address,
 * holds the PrepareSelected move until every run is at confirmation depth,
 * plays the thread through the staged chain to the award with each hop up
 * to the settle reading the staging outputs, and spends them back once no
 * thread needs them. */
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, coreToTxOutput, Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import { confirmedValidationTraceStages } from "../src/validation-trace-dispute/actions.js";
import { parseValidationTraceDisputeArtifact } from "../src/validation-trace-dispute/artifact.js";
import { captureValidationTraceThreadMove } from "../src/validation-trace-dispute/move-dispatch.js";
import {
  type ValidationTraceOneStepHolder,
  validationTraceOneStepHolder,
} from "../src/validation-trace-dispute/one-step-holders.js";
import { VALIDATION_TRACE_STAGING_SHALLOW } from "../src/validation-trace-dispute/one-step-staging.js";
import {
  confirmedIntents,
  VALIDATION_TRACE_STAGING_RECLAIM,
} from "../src/validation-trace-dispute/one-step-staging-journal.js";
import { VALIDATION_TRACE_ONE_STEP_REQUIRED } from "../src/validation-trace-dispute/threads.js";
import type { JournalJsonObject } from "../src/workflow/journal.js";
import { WorkflowActionUnbuiltError } from "../src/workflow/orchestrator.unbuilt-actions.js";
import { sceneCaseHistory } from "./support/validation-trace-dispute-case-history.js";
import { liveCase } from "./support/validation-trace-dispute-one-step-case.js";
import {
  stagedDrive,
  stagingIntents,
} from "./support/validation-trace-dispute-one-step-staged-drive.js";

const DEPTH = DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth;

const drive = stagedDrive();

const outRefOf = (utxo: UTxO) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/** The moves the runner landed whose thread output pays to `holder`. */
const movesTo = (holder: ValidationTraceOneStepHolder) => {
  const { scene, deployment, prepared } = drive.dispute();
  const address = validationTraceOneStepHolder(deployment, holder).address;
  return prepared.filter(({ stage, hash, cbor }) => {
    if (stage !== "resolve" || !scene.l1.signedCbors.has(hash)) return false;
    const outputs = CML.Transaction.from_cbor_hex(cbor).body().outputs();
    return Array.from(
      { length: outputs.len() },
      (_, index) => coreToTxOutput(outputs.get(index)).address,
    ).includes(address);
  });
};

/** The staging outputs live now, in the order of the step's runs. */
const liveStagings = async () => {
  const runs = await drive.runs();
  const { stagings } = drive.stagings();
  const live: string[] = [];
  for (const { datumCbor } of runs)
    for (const staging of [...stagings].reverse())
      if (
        staging.datumCbor === datumCbor &&
        (await drive.live(staging.outRef)) !== undefined
      ) {
        live.push(staging.outRef);
        break;
      }
  return live;
};

/** An output of our wallet holding a datum and half its ada, which no move
 * may fund from. */
let datumOutput: string;
/** The staging output spent before the PrepareSelected move. */
let spent: string;

describe("validation-trace dispute runner: staging the operands of a staged builtin step and driving it to the award", () => {
  beforeAll(async () => {
    await drive.setUp();
    const { scene } = drive.dispute();
    const lucid = scene.h.proverLucid;
    scene.h.proverSigner.selectWallet(lucid);
    const funds = (await lucid.wallet().getUtxos()).reduce(
      (sum, utxo) => sum + (utxo.assets.lovelace ?? 0n),
      0n,
    );
    const signed = await (
      await lucid
        .newTx()
        .pay.ToAddressWithData(
          scene.h.proverSigner.address,
          { kind: "inline", value: Data.to(42n) },
          { lovelace: funds / 2n },
        )
        .complete()
    ).sign
      .withWallet()
      .complete();
    datumOutput = `${await signed.submit()}#0`;
    scene.h.emulator.awaitBlock(1);
    expect(await drive.live(datumOutput)).toBeDefined();
  }, 1_800_000);

  it("refuses the PrepareSelected move over a staging that does not name each run, as the move's recorded one-step-required outcome for our workflow alone", async () => {
    // One run, which reads the journal our workflow has so far.
    await drive.play(async () => true, 1);
    const { scene, deployment, material, caseUnit } = drive.dispute();
    const place = await drive.placeOfB();
    expect(place?.holder).toBe("prepare");
    const prepared = drive
      .entries()
      .find(({ event }) => event.kind === "prepared");
    if (prepared?.event.kind !== "prepared") throw new Error("no artifact");
    const artifact = parseValidationTraceDisputeArtifact(
      prepared.event.artifact.familyArtifact as JournalJsonObject,
    );
    const opened = confirmedValidationTraceStages(drive.entries()).find(
      ({ action }) => action.stage === "open",
    )!;
    const caseUtxo = await liveCase(drive.dispute());
    const refused = await captureValidationTraceThreadMove({
      caseDatum: Data.from(caseUtxo.datum!, SDK.Case),
      stage: "resolve",
      move: {
        lucid: scene.h.proverLucid,
        signer: scene.h.proverSigner,
        deployment,
        artifact,
        threadOutRef: outRefOf(place!.utxo),
        caseUnit,
        caseOpenerOutRef: opened.outputs[0]!.outRef,
        material,
        // One run too few for the step's staged operands.
        staging: async () => [[]],
      },
      caseOutRef: outRefOf(caseUtxo),
      history: {
        caseHistory: sceneCaseHistory(scene),
        headerHash: scene.failing.evidence.headerHash,
        caseUnit,
      },
    }).catch((cause: unknown) => cause);
    expect(refused).toBeInstanceOf(WorkflowActionUnbuiltError);
    expect((refused as Error).message).toContain(
      VALIDATION_TRACE_ONE_STEP_REQUIRED,
    );
    expect(outRefOf((await drive.placeOfB())!.utxo)).toBe(
      outRefOf(place!.utxo),
    );
  }, 300_000);

  it("stages each run of the step's operands once at our staging address, a runner started again over the journal mid-staging staging none twice", async () => {
    const runs = await drive.runs();
    expect(runs.length).toBeGreaterThan(1);
    await drive.play(async () => drive.stagings().stagings.length >= 1);
    expect(drive.stagings().stagings.length).toBeLessThan(runs.length);
    drive.restart();
    await drive.play(
      async () => drive.stagings().stagings.length >= runs.length,
    );
    const { stagings } = drive.stagings();
    expect(stagings).toHaveLength(runs.length);
    expect(stagingIntents(drive.entries())).toHaveLength(runs.length);
    expect(stagings.map(({ actionId }) => actionId.endsWith(":0"))).toEqual(
      runs.map(() => true),
    );
    expect(stagings.map(({ datumCbor }) => datumCbor).sort()).toEqual(
      runs.map(({ datumCbor }) => datumCbor).sort(),
    );
    for (const { outRef, datumCbor } of stagings) {
      const utxo = await drive.live(outRef);
      expect(utxo?.address).toBe(drive.stagingAddress());
      expect(utxo?.datum).toBe(datumCbor);
    }
    expect((await drive.placeOfB())?.holder).toBe("prepare");
  }, 600_000);

  it("stages a run again when its staging output is no longer live, and holds the PrepareSelected move until every run is at confirmation depth", async () => {
    const { scene } = drive.dispute();
    const lucid = scene.h.proverLucid;
    const [first] = drive.stagings().stagings;
    spent = first!.outRef;
    scene.h.proverSigner.selectWallet(lucid);
    const unsigned = await Effect.runPromise(
      SDK.buildUnsignedMidgardCekStagingReclaimProgram(lucid, {
        outputs: [(await drive.live(spent))!],
        pins: new SDK.MidgardCekStagingPins(),
      }),
    );
    await (await unsigned.sign.withWallet().complete()).submit();
    scene.h.emulator.awaitBlock(1);
    expect(await drive.live(spent)).toBeUndefined();

    const history = await drive.play(
      async () => (await drive.placeOfB())?.holder !== "prepare",
    );
    expect(
      history.some((run) => run.includes(VALIDATION_TRACE_STAGING_SHALLOW)),
    ).toBe(true);
    const intents = stagingIntents(drive.entries());
    const runs = await drive.runs();
    expect(intents).toHaveLength(runs.length + 1);
    expect(intents).toContain(first!.actionId.replace(/:0$/, ":1"));

    const live = await liveStagings();
    expect(live).toHaveLength(runs.length);
    expect(live).not.toContain(spent);
    const selected = movesTo("core");
    expect(selected.length).toBeGreaterThan(0);
    const stagedAt = await Promise.all(
      live.map(async (outRef) => await drive.blockOf(outRef.split("#")[0]!)),
    );
    for (const move of selected) {
      const reads = drive.readsOf(move.hash);
      expect(reads.filter((outRef) => live.includes(outRef))).toEqual(live);
      expect(reads).not.toContain(spent);
      const landed = await drive.blockOf(move.hash);
      for (const block of stagedAt)
        expect(landed - block).toBeGreaterThanOrEqual(DEPTH);
    }
  }, 600_000);

  it("moves the thread through the staged chain to the award, each hop from the PrepareSelected move through the last arm reading the staging outputs and the settle none, and closes it", async () => {
    await drive.awards();
    const live = await liveStagings();
    for (const label of [
      "PrepareSelected core",
      "VerifyCoreStep",
      "VerifyArm semanticRoots",
      "VerifyArm semanticResult",
      "VerifyArm semanticBudget",
      "VerifyArm stagedBytes",
      "Settle core",
    ]) {
      const hash = CML.hash_transaction(
        CML.Transaction.from_cbor_hex(drive.cborOf(label)).body(),
      ).to_hex();
      const reads = drive
        .readsOf(hash)
        .filter((outRef) => live.includes(outRef));
      expect(reads, label).toEqual(label === "Settle core" ? [] : live);
      console.info(
        `fee ${label}: ${drive.feeOf(label).toString()} lovelace, ${drive.bytesOf(label).toString()} bytes`,
      );
    }
  }, 1_200_000);

  it("pays each move from its thread when its fee fits under the cap, and from our wallet when it does not", async () =>
    await drive.pays());

  it("spends each staging output back to our wallet by our key once every thread that read it has settled, funding no move from it or from a datum-bearing output of our wallet", async () => {
    const live = await liveStagings();
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
    ).toEqual([...live].sort());
    const settled = await Promise.all(
      movesTo("award").map(async ({ hash }) => await drive.blockOf(hash)),
    );
    for (const { txHash } of reclaims) {
      const body = CML.Transaction.from_cbor_hex(
        scene.l1.signedCbors.get(txHash)!,
      ).body();
      expect(body.inputs().len()).toBe(1);
      expect(body.outputs().len()).toBe(1);
      expect(coreToTxOutput(body.outputs().get(0)).address).toBe(
        scene.h.proverSigner.address,
      );
      const landed = await drive.blockOf(txHash);
      for (const block of settled) expect(landed).toBeGreaterThan(block);
    }
    expect(await drive.live(datumOutput)).toBeDefined();
  }, 1_200_000);
});
