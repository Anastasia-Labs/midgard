/** The validation-trace dispute's case of ours verified by another party, under
 * the prover funding permit, on the emulator. Our funded watcher opens our
 * claim and signs our VerifySource, and its process stops before
 * broadcasting it; another party then runs VerifySource over our pending
 * claim first. The next process supersedes our VerifySource, whose wallet
 * inputs it still holds as a superseded attempt's, and plays the case our
 * Open opened: once the scene's operator defends it, our reveal, timeout and
 * thread close each build and land, spending only the thread (and case)
 * their validators fix, never those wallet inputs, and the case then closes
 * Convicted, spending the case and bond the other party's VerifySource
 * created.
 *
 * The category is not installed until its defender exists; this file
 * stands in for that install so the production classifier mints the
 * objective's actuation permit. */
import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { VALIDATION_TRACE_CASE_OPEN } from "../src/validation-trace-dispute/adapter.js";
import { validationTraceCaseOrigin } from "../src/validation-trace-dispute/case-origin.js";
import { VALIDATION_TRACE_CASE_CONVICTED } from "../src/validation-trace-dispute/removal.js";
import { VALIDATION_TRACE_THREAD_AWAITING } from "../src/validation-trace-dispute/threads.js";
import { parseOwnOutRef } from "../src/workflow/own-outputs.js";
import { recordCrossBlockRawEmulator } from "./support/cross-block-raw-emulator.js";
import { drainProver } from "./support/installed-prover-wallet.js";
import { sceneOperator } from "./support/validation-trace-dispute-defend.js";
import {
  outRefOf,
  verifiedByAnotherParty,
} from "./support/validation-trace-dispute-foreign-verify.js";
import {
  confirmedMoves,
  moveIntents,
} from "./support/validation-trace-dispute-funded-moves.js";
import {
  openFundedDispute,
  turnUntil,
} from "./support/validation-trace-dispute-funded-runner.js";
import {
  FUNDED,
  TIMEOUT,
} from "./support/validation-trace-dispute-funded-verify-source.js";
import { setUpScene } from "./support/validation-trace-dispute-open-scene.js";
import { recordedRemoval } from "./support/validation-trace-dispute-removal-l1.js";
import {
  defendScripted,
  liveThread,
  revealAsDefender,
  scriptedTrace,
  threadOf,
  threadUnitOf,
} from "./support/validation-trace-dispute-scripted-defender.js";

describe("validation-trace dispute case of ours verified by another party", () => {
  it(
    "plays our Open's case to its conviction after another party verified it first, its superseded VerifySource's wallet inputs never forced into a move",
    async () => {
      // Before the scene, so the raw L1 view has the whole history.
      const recorder = recordCrossBlockRawEmulator();
      const scene = await setUpScene();
      const { h } = scene;
      const removal = await recordedRemoval(scene, recorder);
      // The L1 view stops here, before our record.
      removal.holdView();
      await removal
        .l1For(scene.failing.evidence.headerHash)
        .observe({ headerHash: scene.failing.evidence.headerHash });
      await drainProver(h as never, FUNDED);
      const dispute = await openFundedDispute(scene, {
        removal: removal.removal,
      });
      try {
        const { stoppedHex, held, material, caseOutput, bond } =
          await verifiedByAnotherParty(scene, dispute);

        // The next process finds the case our Open opened.
        await dispute.restart();
        const opened = await turnUntil(dispute, VALIDATION_TRACE_CASE_OPEN, {
          limit: 40,
        });
        expect(opened.result.kind, opened.history.join("\n")).toBe("pending");
        expect(
          confirmedMoves(opened.result).map(({ stage }) => stage),
        ).toStrictEqual(["init", "open"]);

        // The operator defends, reveals, then falls silent.
        const defender = sceneOperator(scene);
        const trace = scriptedTrace(
          material,
          Math.floor(Number(material.stepCount) / 3),
        );
        const threadOutRef = await defendScripted(
          scene,
          caseOutput,
          defender,
          trace,
        );
        const [thread] = await h.emulator.getUtxosByOutRef([
          parseOwnOutRef(threadOutRef),
        ]);
        const unit = threadUnitOf(scene, thread!);
        await revealAsDefender(scene, defender, thread!, trace);
        const pass = async () => {
          const live = (await liveThread(scene, unit))!;
          const deadline = Number(
            threadOf(live).turn_started_at + threadOf(live).defender_clock_ms,
          );
          const prover = h.proverLucid;
          h.emulator.awaitSlot(
            prover.unixTimeToSlot(deadline) - prover.currentSlot() + 2,
          );
        };
        const revealed = await turnUntil(
          dispute,
          VALIDATION_TRACE_THREAD_AWAITING,
          {
            limit: 40,
          },
        );
        expect(revealed.result.kind, revealed.history.join("\n")).toBe(
          "pending",
        );
        await pass();
        const closed = await turnUntil(
          dispute,
          VALIDATION_TRACE_CASE_CONVICTED,
          { limit: 40 },
        );
        expect(closed.result.kind, closed.history.join("\n")).toBe("pending");

        const stages = confirmedMoves(closed.result);
        expect(stages.map(({ stage }) => stage)).toStrictEqual([
          "init",
          "open",
          "reveal",
          "timeout",
          "close_thread",
          "close",
        ]);
        // Our stopped VerifySource never landed, and no move spent one of
        // the wallet inputs it held.
        const stoppedHash = CML.hash_transaction(
          CML.Transaction.from_cbor_hex(stoppedHex).body(),
        ).to_hex();
        expect(
          (await h.emulator.getTransactionStatus(stoppedHash)).status,
        ).not.toBe("confirmed");
        for (const stage of ["reveal", "timeout", "close-thread"] as const)
          for (const { txHash } of moveIntents(closed.result, stage)) {
            const signed = scene.l1.signedCbors.get(txHash);
            if (signed === undefined) continue;
            const inputs = CML.Transaction.from_cbor_hex(signed)
              .body()
              .inputs();
            for (let index = 0; index < inputs.len(); index += 1) {
              const input = inputs.get(index);
              expect(held).not.toContain(
                `${input.transaction_id().to_hex()}#${input.index().toString()}`,
              );
            }
          }
        // The case token's L1 history, its thread closed, still names the
        // other party's VerifySource as where the case and bond began.
        const caseUnit = Object.keys(caseOutput.assets).find((key) =>
          key.startsWith(scene.chain.case.policyId),
        )!;
        expect(
          validationTraceCaseOrigin({
            transactions: await dispute.caseHistory.transactions({
              headerHash: scene.failing.evidence.headerHash,
              caseUnit,
            }),
            caseUnit,
          }),
        ).toStrictEqual({
          caseOutRef: outRefOf(caseOutput),
          bondOutRef: outRefOf(bond),
        });
        // The case's close spent that bond.
        expect(
          await h.emulator.getUtxosByOutRef([parseOwnOutRef(outRefOf(bond))]),
        ).toStrictEqual([]);
        for (const stage of ["reveal", "timeout", "close-thread"] as const) {
          const landed = moveIntents(closed.result, stage).filter(
            ({ txHash }) => scene.l1.signedCbors.has(txHash),
          );
          expect(landed).toHaveLength(1);
        }
      } finally {
        await dispute.close();
      }
    },
    TIMEOUT,
  );
});
