/** The validation-trace dispute's one-step resolution through the
 * canonical-decode step after state 8, an item of a redeemer field one byte
 * over the inline observe limit, under the watcher's prover funding. A
 * funded watcher opens our case; the scene's operator defends it with a
 * trace agreeing with ours up to state 8 and reveals from it; the watcher
 * bisects the thread and resolves it through the item's stages. Before the
 * observe move the watcher publishes a bytes-only copy of the field at its
 * key address from its funding; spent outside the watcher before the move
 * lands, the copy is published again once its publication is past the
 * recovery depth, and the move reads the new copy. The thread is then
 * awarded and closes into our case. Our funding reservation holds plain
 * coins only, so the copy, which carries a datum, is not among them: the
 * watcher leaves it at our key address, and the convicted block's removal
 * waits undisturbed. The copy is spent back by key where our signer is not
 * restricted to a reservation
 * (validation-trace-dispute-one-step-canonical-decode-8-observe-copy.test.ts).
 *
 * The category is not installed until its defender exists; this file
 * stands in for that install so the production classifier mints the
 * objective's actuation permit. */
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { VALIDATION_TRACE_CASE_OPEN } from "../src/validation-trace-dispute/adapter.js";
import { VALIDATION_TRACE_INLINE_OBSERVE_MAX_PREIMAGE_BYTES } from "../src/validation-trace-dispute/one-step-canonical-decode.js";
import { VALIDATION_TRACE_CASE_CONVICTED } from "../src/validation-trace-dispute/removal.js";
import type { ValidationTraceOneStepReferences } from "../src/validation-trace-dispute/transactions.js";
import { parseOwnOutRef } from "../src/workflow/own-outputs.js";
import { recordCrossBlockRawEmulator } from "./support/cross-block-raw-emulator.js";
import { drainProver } from "./support/installed-prover-wallet.js";
import { sceneOperator } from "./support/validation-trace-dispute-defend.js";
import type { CapturedMove } from "./support/validation-trace-dispute-funded-moves.js";
import { fundedOneStepTurns } from "./support/validation-trace-dispute-funded-one-step-turns.js";
import {
  type FundedDispute,
  openFundedDispute,
  turnUntil,
} from "./support/validation-trace-dispute-funded-runner.js";
import {
  caseAndBond,
  FUNDED,
  TIMEOUT,
} from "./support/validation-trace-dispute-funded-verify-source.js";
import {
  copiesOf,
  landed,
  outRefsOf,
  readersOf,
  spendByKey,
  spenderOf,
} from "./support/validation-trace-dispute-observe-copies.js";
import {
  publishCanonicalDecodeReferences,
  publishOneStepReferences,
} from "./support/validation-trace-dispute-one-step.js";
import {
  confirmedOneStepMoves,
  playThreads,
  type Side,
} from "./support/validation-trace-dispute-one-step-play.js";
import {
  failingMaterial,
  type Scene,
  setUpScene,
} from "./support/validation-trace-dispute-open-scene.js";
import { redeemerDataForFieldPreimage } from "./support/validation-trace-dispute-redeemer-field.js";
import { recordedRemoval } from "./support/validation-trace-dispute-removal-l1.js";
import {
  defendScripted,
  scriptedTrace,
  threadUnitOf,
} from "./support/validation-trace-dispute-scripted-defender.js";

const BYTES = VALIDATION_TRACE_INLINE_OBSERVE_MAX_PREIMAGE_BYTES + 1;

let scene: Scene;
let dispute: FundedDispute;
let side: Side;
const moves: CapturedMove[] = [];

const { turns, place, playTo } = fundedOneStepTurns({
  scene: () => scene,
  dispute: () => dispute,
  side: () => side,
  moves,
});

/** Plays the watcher's turns until `done` holds. */
const playUntil = async (done: () => Promise<boolean> | boolean, limit = 20) =>
  await playThreads({
    scene,
    runner: turns(),
    sides: [side],
    until: async () => await done(),
    limit,
  });

describe("validation-trace dispute one-step resolution through a canonical-decode item of a field one byte over the inline observe limit under the prover funding permit", () => {
  beforeAll(async () => {
    // Before the scene, so the raw L1 view has the whole history.
    const recorder = recordCrossBlockRawEmulator();
    let oneStep: ValidationTraceOneStepReferences | undefined;
    scene = await setUpScene(
      async (contracts) => {
        oneStep = {
          ...(await publishOneStepReferences(contracts)),
          canonicalDecode: await publishCanonicalDecodeReferences(contracts),
        };
      },
      { failingRedeemerData: redeemerDataForFieldPreimage(BYTES) },
    );
    const removal = await recordedRemoval(scene, recorder);
    // The L1 view stops here, before our case: the convicted block's
    // removal waits on it.
    removal.holdView();
    await removal
      .l1For(scene.failing.evidence.headerHash)
      .observe({ headerHash: scene.failing.evidence.headerHash });
    await drainProver(scene.h as never, FUNDED);
    dispute = await openFundedDispute(scene, {
      oneStep: oneStep!,
      removal: removal.removal,
      removalScripts: removal.published,
      liveParameters: true,
    });
    const opened = await turnUntil(dispute, VALIDATION_TRACE_CASE_OPEN);
    if (opened.result.kind !== "pending")
      throw new Error(`the case did not open: ${opened.history.join("\n")}`);
    const { caseOutput } = await caseAndBond(scene, opened.result);
    const defender = sceneOperator(scene);
    const trace = scriptedTrace(await failingMaterial(scene), 8);
    const threadOutRef = await defendScripted(
      scene,
      caseOutput,
      defender,
      trace,
    );
    const [thread] = await scene.h.emulator.getUtxosByOutRef([
      parseOwnOutRef(threadOutRef),
    ]);
    side = { defender, trace, unit: threadUnitOf(scene, thread!) };
  }, 1_800_000);

  afterAll(async () => {
    await dispute?.close();
  });

  it(
    "publishes a bytes-only copy of the field at our key address from our funding before the observe move",
    async () => {
      await playTo("canonicalDecodeItemObserve");
      await playUntil(() => copiesOf(scene, BYTES).length > 0, 6);
      const [copy] = copiesOf(scene, BYTES);
      const publish = landed(scene).find(
        ({ hash }) => hash === copy!.split("#")[0],
      )!;
      // No script runs, and only our wallet's coins are spent.
      expect(publish.tx.witness_set().redeemers()).toBeUndefined();
      const spent = await scene.h.emulator.getUtxosByOutRef(
        outRefsOf(publish.tx.body().inputs()).map(parseOwnOutRef),
      );
      expect(spent).toStrictEqual([]);
      expect(readersOf(scene, copy!)).toStrictEqual([]);
      expect((await place())?.holder).toBe("canonicalDecodeItemObserve");
    },
    TIMEOUT,
  );

  it(
    "publishes the copy again once a spend of it outside the watcher is past the recovery depth, and the observe move reads the new copy",
    async () => {
      const [first] = copiesOf(scene, BYTES);
      await spendByKey(scene, first!);
      // Time passes until the publication's transaction is past the recovery
      // depth, its output still gone.
      scene.h.emulator.awaitBlock(
        DEPLOYMENT_MANIFEST_L1_FINALITY.automaticRecoveryMaxDepth,
      );
      await playUntil(
        async () => (await place())?.holder === "canonicalDecodeItemProof",
      );
      const copies = copiesOf(scene, BYTES);
      expect(copies).toHaveLength(2);
      expect(copies[0]).toBe(first);
      expect(readersOf(scene, first!)).toStrictEqual([]);
      expect(readersOf(scene, copies[1]!)).toHaveLength(1);
    },
    TIMEOUT,
  );

  it(
    "settles the step, is awarded and closes into our case",
    async () => {
      const { history } = await playThreads({
        scene,
        runner: turns(),
        sides: [side],
        until: async (run) =>
          "entries" in run &&
          confirmedOneStepMoves(run.entries).some(
            ({ stage }) => stage === "close_awarded",
          ),
        limit: 20,
      });
      expect(await place(), history.join("\n")).toBeUndefined();
    },
    TIMEOUT,
  );

  it(
    "leaves the copy, which our funding reservation does not hold, at our key address while the removal waits",
    async () => {
      const [, copy] = copiesOf(scene, BYTES);
      const { result, history } = await turnUntil(
        dispute,
        VALIDATION_TRACE_CASE_CONVICTED,
      );
      expect(result.kind, history.join("\n")).toBe("pending");
      expect(spenderOf(scene, copy!)).toBeUndefined();
      const [utxo] = await scene.h.emulator.getUtxosByOutRef([
        parseOwnOutRef(copy!),
      ]);
      expect(utxo?.address).toBe(scene.h.proverSigner.address);
    },
    TIMEOUT,
  );
});
