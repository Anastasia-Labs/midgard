/** The validation-trace dispute runner at a staged builtin step (arm 31,
 * semantics group 7) whose operand is past the direct carriage bound, on the
 * emulator (support/validation-trace-dispute-one-step-staged-drive.ts): the
 * runner stages each run of the step's operands at our staging address,
 * holds the PrepareSelected move until every run is at confirmation depth,
 * plays the thread through the staged chain to the award with each hop up
 * to the settle reading the staging outputs, and spends them back once no
 * thread needs them. */
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk/da";
import * as SDKFraudProof from "@al-ft/midgard-sdk/fraud-proof";
import { CML, coreToTxOutput, Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import { VALIDATION_TRACE_DISPUTE_ACTION } from "../src/validation-trace-dispute/actions.js";
import {
  encodeValidationTraceDisputeArtifact,
  parseValidationTraceDisputeArtifact,
} from "../src/validation-trace-dispute/artifact.js";
import { captureValidationTraceThreadMove } from "../src/validation-trace-dispute/move-dispatch.js";
import {
  type ValidationTraceOneStepHolder,
  validationTraceOneStepHolder,
} from "../src/validation-trace-dispute/one-step-holders.js";
import { VALIDATION_TRACE_STAGING_SHALLOW } from "../src/validation-trace-dispute/one-step-staging.js";
import { VALIDATION_TRACE_ONE_STEP_REQUIRED } from "../src/validation-trace-dispute/threads.js";
import { FraudProofL1UnavailableError } from "../src/workflow/l1-source.js";
import type { FraudProofWorkflowRunResult } from "../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import { WorkflowActionUnbuiltError } from "../src/workflow/orchestrator.unbuilt-actions.js";
import { sceneCaseHistory } from "./support/validation-trace-dispute-case-history.js";
import { liveCase } from "./support/validation-trace-dispute-one-step-case.js";
import {
  isPendingFor,
  withLedgerParameters,
} from "./support/validation-trace-dispute-one-step-play.js";
import { stagedDrive } from "./support/validation-trace-dispute-one-step-staged-drive.js";

const DEPTH = DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth;

/** A transaction size limit under which the PrepareSelected move (2,670
 * bytes) does not build and a spend back of one staging output does. */
const NO_PREPARE_SELECTED = 2_000;

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

/** The staging outputs live now, one for each of the step's runs, in
 * ledger order (the order a transaction reads them in). */
const liveStagings = async () => {
  const runs = await drive.runs();
  const stagings = await drive.stagedOnL1();
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
  return live.sort();
};

/** Runs `body` while the L1 follower our runner reads depths from knows
 * each transaction of `hashes` landed but cannot say how deep it stands
 * (from the first time `lost` holds on): it reports the transaction
 * confirmed with no count of confirmations, as it does for a block it has
 * applied whose place on the canonical chain it does not know. */
const withDepthUnknown = async <A>(
  hashes: readonly string[],
  body: () => Promise<A>,
  lost: () => boolean = () => true,
): Promise<A> => {
  const config = drive.dispute().scene.h.proverLucid.config();
  const provider = config.provider!;
  const status = provider.getTransactionStatus!.bind(provider);
  const unknown = new Set(hashes);
  config.provider = new Proxy(provider, {
    get: (target, key) => {
      if (key === "getTransactionStatus")
        return async (hash: string) => {
          const found = await status(hash);
          if (found.status !== "confirmed" || !unknown.has(hash) || !lost())
            return found;
          const { confirmations: _, ...confirmation } = found.confirmation;
          return { ...found, confirmation };
        };
      const value: unknown = Reflect.get(target, key, target);
      return typeof value === "function" ? value.bind(target) : value;
    },
  });
  try {
    return await body();
  } finally {
    config.provider = provider;
  }
};

/** An output at our proof-materials address holding a datum and half our
 * ada: it is no coin of the funding address, so no move may fund from it. */
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
          scene.h.proverSigner.proofMaterialsAddress,
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
    // One run of the runner over the dispute as it stands.
    await drive.play(async () => true, 1);
    const { scene, deployment, material, caseUnit } = drive.dispute();
    const place = await drive.placeOfB();
    expect(place?.holder).toBe("prepare");
    // The artifact the watcher prepares from the failing block's claim.
    const artifact = parseValidationTraceDisputeArtifact(
      encodeValidationTraceDisputeArtifact({
        headerHash: scene.failing.evidence.headerHash,
        material,
      }),
    );
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

  it("stages each run of the step's operands once at our staging address, a fresh process started mid-staging staging none twice", async () => {
    const runs = await drive.runs();
    expect(runs.length).toBeGreaterThan(1);
    // The run before may have landed the first staging already.
    if ((await drive.stagedOnL1()).length === 0)
      await drive.play(async () => (await drive.stagedOnL1()).length >= 1);
    expect((await drive.stagedOnL1()).length).toBeLessThan(runs.length);
    drive.restart();
    await drive.play(
      async () => (await drive.stagedOnL1()).length >= runs.length,
    );
    expect(await drive.stagedOnL1()).toHaveLength(runs.length);
    // One landed staging for each run, across both processes, each under
    // its own action.
    const stagings = await drive.stagings();
    expect(stagings).toHaveLength(runs.length);
    expect(drive.stagingSends()).toHaveLength(runs.length);
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

  it("waits on the L1 source when it cannot say how deep a staging output stands, staging nothing again and moving no thread", async () => {
    const { scene } = drive.dispute();
    const staged = await drive.stagedOnL1();
    const place = await drive.placeOfB();
    const run = withDepthUnknown(
      staged.map(({ outRef }) => outRef.split("#")[0]!),
      async () => await drive.dispute().runner.runOnce(scene.failing),
    );
    await expect(run).rejects.toBeInstanceOf(FraudProofL1UnavailableError);
    expect(await drive.stagedOnL1()).toEqual(staged);
    expect(outRefOf((await drive.placeOfB())!.utxo)).toBe(
      outRefOf(place!.utxo),
    );
  }, 300_000);

  it("keeps each staging output no move has read while the PrepareSelected move does not build, spending none back", async () => {
    const { scene } = drive.dispute();
    const runs = await drive.runs();
    let held: FraudProofWorkflowRunResult | undefined;
    await withLedgerParameters(
      scene,
      { maxTxSize: NO_PREPARE_SELECTED },
      async () =>
        await drive.play(async (result) => {
          if (!isPendingFor(result, VALIDATION_TRACE_ONE_STEP_REQUIRED))
            return false;
          held = result;
          return true;
        }, 12),
    );
    expect(held?.kind === "pending" && held.reason).toContain(
      "the resolve move on thread",
    );
    // No spend back was sent, and no run was staged again.
    expect(
      drive
        .sent()
        .filter(({ actionId }) =>
          actionId.startsWith("validation-trace-reclaim-staging:"),
        ),
    ).toEqual([]);
    expect(await drive.stagings()).toHaveLength(runs.length);
    const staged = await drive.stagedOnL1();
    expect(staged).toHaveLength(runs.length);
    for (const { outRef } of staged)
      expect(await drive.live(outRef)).toBeDefined();
    expect((await drive.placeOfB())?.holder).toBe("prepare");
  }, 600_000);

  it("waits on the L1 source when it cannot say how deep a staging output stands as the PrepareSelected move builds", async () => {
    const { scene } = drive.dispute();
    const staged = await drive.stagedOnL1();
    const hashes = staged.map(({ outRef }) => outRef.split("#")[0]!);
    // The follower loses the depth once the move's build starts.
    let lost = false;
    drive.restart({
      wrap: (adapter) => ({
        ...adapter,
        preflight: async (context) => {
          lost ||=
            context.action.input.schemaVersion ===
            VALIDATION_TRACE_DISPUTE_ACTION;
          return await adapter.preflight(context);
        },
      }),
    });
    const place = await drive.placeOfB();
    const run = withDepthUnknown(
      hashes,
      async () => await drive.dispute().runner.runOnce(scene.failing),
      () => lost,
    ).finally(() => drive.restart());
    await expect(run).rejects.toBeInstanceOf(FraudProofL1UnavailableError);
    expect(lost).toBe(true);
    expect(await drive.stagedOnL1()).toEqual(staged);
    expect(outRefOf((await drive.placeOfB())!.utxo)).toBe(
      outRefOf(place!.utxo),
    );
  }, 300_000);

  it("stages a run again when its staging output is no longer live, and holds the PrepareSelected move until every run is at confirmation depth", async () => {
    const { scene } = drive.dispute();
    const lucid = scene.h.proverLucid;
    const [first] = await drive.stagings();
    spent = first!.outRef;
    scene.h.proverSigner.selectWallet(lucid);
    const unsigned = await Effect.runPromise(
      SDKFraudProof.buildUnsignedMidgardCekStagingReclaimProgram(lucid, {
        outputs: [(await drive.live(spent))!],
        pins: new SDKFraudProof.MidgardCekStagingPins(),
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
    // The run is staged again with the same datum. The process started
    // after the first staging landed, so it counts the run's generations
    // from none and sends it under the first one's action.
    const stagings = await drive.stagings();
    const runs = await drive.runs();
    expect(stagings).toHaveLength(runs.length + 1);
    const again = stagings.filter(
      ({ datumCbor }) => datumCbor === first!.datumCbor,
    );
    expect(again).toHaveLength(2);
    expect(again.map(({ actionId }) => actionId)).toStrictEqual([
      first!.actionId,
      first!.actionId,
    ]);
    expect(again[1]!.outRef).not.toBe(spent);

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
    await drive.play(async () => {
      for (const outRef of live)
        if ((await drive.live(outRef)) !== undefined) return false;
      return true;
    }, 60);
    const { scene } = drive.dispute();
    const reclaims = await Promise.all(live.map(drive.spenderOf));
    const settled = await Promise.all(
      movesTo("award").map(async ({ hash }) => await drive.blockOf(hash)),
    );
    for (const txHash of reclaims) {
      expect(txHash).toBeDefined();
      if (txHash === undefined) continue;
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
