/** Our staging outputs (arm 31) under the watcher's prover funding once the
 * challenged header has rolled back, over the scene of
 * support/validation-trace-dispute-funded-one-step-staging.ts: our workflow
 * staged each run of the step and played its thread past the
 * PrepareSelected move, whose moves read the staging, when the header rolls
 * back. Every later turn runs under the reconciliation-only permit the
 * watcher derives from the workflow it recorded, and each transaction is
 * final only `RELEASE_DEPTH` blocks under the tip.
 *
 * While the thread stands past the PrepareSelected move, each staging
 * output stays where it is. Once the case's duration has passed and its
 * defender closes it, each comes back to our wallet, one transaction each,
 * and the case's ending is taken only once each spend back is final at
 * release depth. */
import { CML, coreToTxOutput } from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { VALIDATION_TRACE_STAGING_OUTSTANDING } from "../src/validation-trace-dispute/one-step-staging-sweep.js";
import type { FraudProofWorkflowRunResult } from "../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import { topUpProver } from "./support/installed-prover-wallet.js";
import { landWith } from "./support/validation-trace-dispute-defender-moves.js";
import { defenderWonCaseClose } from "./support/validation-trace-dispute-defender-won-close.js";
import {
  fundedStagingScene,
  runs,
} from "./support/validation-trace-dispute-funded-one-step-staging.js";
import { outRefOf } from "./support/validation-trace-dispute-funded-one-step-turns.js";
import {
  ADA,
  TIMEOUT,
} from "./support/validation-trace-dispute-funded-verify-source.js";
import {
  awaitLastSlotAtOrBefore,
  liveCase,
} from "./support/validation-trace-dispute-one-step-case.js";
import { landedSpenderOf } from "./support/validation-trace-dispute-one-step-staged-drive.js";

/** How many blocks under the tip a transaction is final, once the header
 * has rolled back. */
const RELEASE_DEPTH = 4;
let releaseDepth = 0;

const { at, turns, place, playTo, open, stagedOutputs } = fundedStagingScene({
  releaseDepth: () => releaseDepth,
});
/** The staging outputs, as the ledger held them once the thread moved past
 * the PrepareSelected move. */
let staged: string[] = [];

const describeRun = (result: FraudProofWorkflowRunResult) =>
  "reason" in result ? `${result.kind}: ${result.reason}` : result.kind;

const live = async () => (await stagedOutputs()).map(outRefOf).sort();

describe("validation-trace dispute staging outputs under reconciliation-only funding after a header rollback, while our thread stands past the PrepareSelected move", () => {
  beforeAll(open, 1_800_000);

  afterAll(async () => {
    await at.dispute?.close();
  });

  it(
    "keeps each staging output under the reconciliation-only permit while the thread stands past the PrepareSelected move",
    async () => {
      await topUpProver(at.scene.h as never, 400n * ADA);
      await playTo("core");
      staged = await live();
      expect(staged).toHaveLength(runs.length);
      await at.dispute.rollBackHeader();
      releaseDepth = RELEASE_DEPTH;
      const history: string[] = [];
      for (let turn = 0; turn < RELEASE_DEPTH + 4; turn += 1)
        history.push(describeRun(await turns().runOnce()));
      expect(await live(), history.join("\n")).toStrictEqual(staged);
      expect((await place())?.holder, history.join("\n")).toBe("core");
    },
    TIMEOUT,
  );

  it(
    "spends each back to our wallet once the case's duration has passed and its defender closed it, and takes the ending only once each spend back is final at release depth",
    async () => {
      const { scene } = at;
      awaitLastSlotAtOrBefore(scene, at.caseExpiresAtMs + 20_000);
      const { lucid } = at.side.defender;
      await landWith(
        lucid,
        await defenderWonCaseClose({
          scene,
          lucid,
          caseUtxo: await liveCase({ scene, caseUnit: at.caseUnit }),
          bond: at.bond,
        }),
      );
      const history: FraudProofWorkflowRunResult[] = [];
      let observedAt = 0;
      for (let turn = 0; turn < 30; turn += 1) {
        observedAt = scene.h.emulator.blockHeight;
        const result = await turns().runOnce();
        history.push(result);
        if (result.kind !== "pending") break;
      }
      const ending = history.at(-1)!;
      expect(ending.kind, history.map(describeRun).join("\n")).toBe("stalled");
      expect("reason" in ending && ending.reason).toContain(
        "validation-trace case is closed without this workflow's close",
      );
      expect(
        history.some(
          (result) =>
            result.kind === "pending" &&
            result.reason.startsWith(VALIDATION_TRACE_STAGING_OUTSTANDING),
        ),
      ).toBe(true);
      expect(await live()).toStrictEqual([]);
      for (const outRef of staged) {
        const reclaim = await landedSpenderOf(
          scene.l1.signedCbors,
          scene.h.emulator,
          outRef,
        );
        expect(reclaim, outRef).toBeDefined();
        const body = CML.Transaction.from_cbor_hex(
          scene.l1.signedCbors.get(reclaim!)!,
        ).body();
        expect(body.inputs().len()).toBe(1);
        expect(body.outputs().len()).toBe(1);
        expect(coreToTxOutput(body.outputs().get(0)).address).toBe(
          scene.h.proverSigner.address,
        );
        const status = await scene.h.emulator.getTransactionStatus(reclaim!);
        expect(status.status).toBe("confirmed");
        if (status.status !== "confirmed") return;
        expect(
          observedAt - status.confirmation.blockHeight!,
          outRef,
        ).toBeGreaterThanOrEqual(RELEASE_DEPTH);
      }
    },
    TIMEOUT,
  );
});
