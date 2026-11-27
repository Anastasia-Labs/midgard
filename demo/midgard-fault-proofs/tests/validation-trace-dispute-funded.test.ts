/** The validation-trace dispute's VerifySource under the watcher's prover
 * funding, on the emulator: it locks exactly the deployment-bound dispute
 * bond and carries the case at no less than its floor, funding every stage
 * from plain coins while a larger token-bearing coin sits at the prover's
 * address. A process stopped after broadcasting
 * VerifySource lands those same bytes, and one stopped after they landed
 * sends nothing again; after one stopped before broadcasting, the next
 * process builds VerifySource again from the chain and its own lands, so
 * one case and one bond open and the stopped bytes cannot land beside it. A wallet that cannot pay VerifySource pauses the objective on the
 * watcher's prover-funding hold until a top-up, then lands.
 *
 * The category is not installed until its defender exists; this file
 * stands in for that install so the production classifier mints the
 * objective's actuation permit. */
import { parseOutRefLabel } from "@al-ft/midgard-core/out-ref";
import { CML } from "@lucid-evolution/lucid";
import {
  holdWatcherFaultProofOnFunding,
  WATCHER_PROVER_FUNDING_UNAVAILABLE,
} from "midgard-watcher/tests/support/prover-funding-hold";
import { beforeEach, describe, expect, it } from "vitest";

import { VALIDATION_TRACE_CASE_OPEN } from "../src/validation-trace-dispute/adapter.js";
import { isWorkflowFundingPauseReason } from "../src/workflow/funding-pause-reason.js";
import { WORKFLOW_FUNDING_SHORTFALL } from "../src/workflow/workflow-funding-shortfall.js";
import {
  drainProver,
  drainProverToOneCoin,
  topUpProver,
} from "./support/installed-prover-wallet.js";
import {
  plantTokenCoin,
  watchLedger,
} from "./support/own-outputs-installed.js";
import { ourOpener } from "./support/validation-trace-dispute-funded-moves.js";
import {
  openFundedDispute,
  turnUntil,
} from "./support/validation-trace-dispute-funded-runner.js";
import {
  ADA,
  caseAndBond,
  confirmedStages,
  expectOneCaseAndBond,
  FLOOR,
  FUNDED,
  stopAtVerifySourceSubmit,
  TIMEOUT,
  verifySourceSends,
} from "./support/validation-trace-dispute-funded-verify-source.js";
import {
  BOND_LOVELACE,
  type Scene,
  setUpScene,
} from "./support/validation-trace-dispute-open-scene.js";

let scene: Scene;

describe("validation-trace dispute VerifySource under the prover funding", () => {
  // Each case disputes the scene's failing block from a fresh deployment,
  // its prover wallet split into a collateral output and plain funding.
  beforeEach(async () => {
    scene = await setUpScene();
  }, TIMEOUT);

  it(
    "locks exactly the deployment-bound bond and carries the case at the larger of its opener and its floor",
    async () => {
      await drainProver(scene.h as never, FUNDED);
      const dispute = await openFundedDispute(scene);
      const { result, history } = await turnUntil(
        dispute,
        VALIDATION_TRACE_CASE_OPEN,
      );
      expect(result.kind, history.join("\n")).toBe("pending");
      const { caseOutput, bond, caseClaim, bondClaim } = await caseAndBond(
        scene,
        dispute,
      );
      // The opener VerifySource spends: the pending claim our Open left.
      const opener = await ourOpener(scene, dispute);
      const [verifySource] = verifySourceSends(dispute);
      const inputs = CML.Transaction.from_cbor_hex(
        scene.l1.signedCbors.get(verifySource!)!,
      )
        .body()
        .inputs();
      expect(
        Array.from({ length: inputs.len() }, (_, index) => {
          const input = inputs.get(index);
          return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
        }),
      ).toContain(`${opener.txHash}#${opener.outputIndex.toString()}`);
      const openerLovelace = opener.assets.lovelace;
      expect(bond.address).toBe(scene.chain.disputeBond.spendingScriptAddress);
      expect(bond.assets).toStrictEqual({
        lovelace: BOND_LOVELACE,
        [bondClaim.unit]: 1n,
      });
      expect(caseOutput.address).toBe(scene.chain.case.spendingScriptAddress);
      expect(caseOutput.assets[caseClaim.unit]).toBe(1n);
      // The pending claim's datum alone needs more than the floor in
      // min-Ada, so a production-built opener always clears it and the
      // case carries the opener's lovelace.
      expect(openerLovelace).toBeGreaterThan(FLOOR);
      expect(caseOutput.assets.lovelace).toBe(openerLovelace);
    },
    TIMEOUT,
  );

  it(
    "lands the same signed VerifySource after a process stop between its broadcast and its landing",
    async () => {
      await drainProver(scene.h as never, FUNDED);
      const dispute = await openFundedDispute(scene);
      const stop = stopAtVerifySourceSubmit({ broadcast: true });
      await stop.stopped(dispute);
      dispute.restart();
      const { result, history } = await turnUntil(
        dispute,
        VALIDATION_TRACE_CASE_OPEN,
      );
      expect(result.kind, history.join("\n")).toBe("pending");
      // The bytes already reached the ledger's mempool: nothing is sent again.
      expect(dispute.rebroadcasts).toStrictEqual([]);
      const { verify } = await caseAndBond(scene, dispute);
      expect(
        verify.outputs.map(({ outRef }) => outRef.split("#")[0]),
      ).toStrictEqual([stop.signedHash(), stop.signedHash()]);
      expect(verifySourceSends(dispute)).toStrictEqual([stop.signedHash()]);
      await expectOneCaseAndBond(scene);
    },
    TIMEOUT,
  );

  it(
    "sends nothing again after a process stop once its broadcast VerifySource landed",
    async () => {
      await drainProver(scene.h as never, FUNDED);
      const dispute = await openFundedDispute(scene);
      const stop = stopAtVerifySourceSubmit({ broadcast: true });
      await stop.stopped(dispute);
      scene.h.emulator.awaitBlock(1);
      expect(
        (await scene.h.emulator.getTransactionStatus(stop.signedHash())).status,
      ).toBe("confirmed");
      dispute.restart();
      const { result, history } = await turnUntil(
        dispute,
        VALIDATION_TRACE_CASE_OPEN,
      );
      expect(result.kind, history.join("\n")).toBe("pending");
      expect(dispute.rebroadcasts).toStrictEqual([]);
      expect(verifySourceSends(dispute)).toStrictEqual([stop.signedHash()]);
      const { verify } = await caseAndBond(scene, dispute);
      expect(
        verify.outputs.map(({ outRef }) => outRef.split("#")[0]),
      ).toStrictEqual([stop.signedHash(), stop.signedHash()]);
      await expectOneCaseAndBond(scene);
    },
    TIMEOUT,
  );

  it(
    "builds VerifySource again after a stop before broadcast: the next process's own lands, opening one case and one bond, and the stopped bytes cannot land beside it",
    async () => {
      await drainProver(scene.h as never, FUNDED);
      const dispute = await openFundedDispute(scene);
      const stop = stopAtVerifySourceSubmit({ broadcast: false });
      await stop.stopped(dispute);
      dispute.restart();
      const { result, history } = await turnUntil(
        dispute,
        VALIDATION_TRACE_CASE_OPEN,
      );
      expect(result.kind, history.join("\n")).toBe("pending");
      // The next process knows nothing of the stopped bytes: it rebroadcasts
      // nothing and sends the VerifySource it builds from the chain, which
      // may be the very same transaction.
      expect(dispute.rebroadcasts).not.toContain(stop.signedHex());
      const sends = verifySourceSends(dispute);
      expect(sends).toHaveLength(2);
      expect(sends[0]).toBe(stop.signedHash());
      const { verify } = await caseAndBond(scene, dispute);
      expect(
        verify.outputs.map(({ outRef }) => outRef.split("#")[0]),
      ).toStrictEqual([sends[1], sends[1]]);
      await expectOneCaseAndBond(scene);
      // The ledger refuses the stopped bytes now.
      await expect(
        scene.h.emulator.submitTx(stop.signedHex()),
      ).rejects.toThrow();
    },
    TIMEOUT,
  );

  it(
    "funds Init, Open and VerifySource from plain coins, leaving a larger token-bearing coin at the prover address unspent",
    async () => {
      const { h } = scene;
      await drainProver(h as never, FUNDED);
      const watch = watchLedger(h.emulator);
      try {
        await plantTokenCoin(watch, h.funderLucid, h.proverSigner.address);
        h.emulator.awaitBlock(1);
        const [tokenCoin, ...others] = watch.accepted
          .filter(({ planted }) => planted)
          .flatMap(({ txHash, outputs }) =>
            outputs.flatMap((output, index) =>
              output.address === h.proverSigner.address &&
              Object.keys(output.assets).length > 1
                ? [`${txHash}#${index.toString()}`]
                : [],
            ),
          );
        expect(others).toStrictEqual([]);
        const dispute = await openFundedDispute(scene);
        const { result, history } = await turnUntil(
          dispute,
          VALIDATION_TRACE_CASE_OPEN,
        );
        expect(result.kind, history.join("\n")).toBe("pending");
        expect(await confirmedStages(dispute)).toStrictEqual([
          "init",
          "open",
          "verify_source",
        ]);
        expect(watch.ours()).toHaveLength(3);
        for (const transaction of watch.ours())
          expect(transaction.inputs).not.toContain(tokenCoin);
        expect(
          await h.emulator.getUtxosByOutRef([parseOutRefLabel(tokenCoin!)]),
        ).toHaveLength(1);
      } finally {
        watch.restore();
      }
    },
    TIMEOUT,
  );

  it(
    "pauses VerifySource on the funding-shortfall hold when the wallet cannot pay it, and lands after a top-up",
    async () => {
      const { h } = scene;
      const dispute = await openFundedDispute(scene);
      // One coin enough for the bond, the case floor and the wallet-move
      // reserve, not to pay for Init, Open and VerifySource besides.
      await drainProverToOneCoin(
        h as never,
        BOND_LOVELACE + FLOOR + dispute.walletMoveReserve(),
      );
      const paused = await turnUntil(dispute, WORKFLOW_FUNDING_SHORTFALL);
      expect(paused.result.kind, paused.history.join("\n")).toBe("pending");
      if (paused.result.kind !== "pending") return;
      expect(paused.result.reason).toContain(
        "cannot fund validation-trace-verify-source:",
      );
      expect(isWorkflowFundingPauseReason(paused.result.reason)).toBe(true);
      const holds = new Map<string, { reason: string; detail: string }>();
      holdWatcherFaultProofOnFunding(
        holds,
        {
          category: "validationTraceDispute",
          headerHash: dispute.decision.headerHash,
        } as never,
        paused.result.reason,
      );
      expect([...holds.values()].map(({ reason }) => reason)).toStrictEqual([
        WATCHER_PROVER_FUNDING_UNAVAILABLE,
      ]);
      expect(await confirmedStages(dispute)).toStrictEqual(["init", "open"]);

      await topUpProver(h as never, 100n * ADA);
      const { result, history } = await turnUntil(
        dispute,
        VALIDATION_TRACE_CASE_OPEN,
      );
      expect(result.kind, history.join("\n")).toBe("pending");
      expect((await caseAndBond(scene, dispute)).bond.assets.lovelace).toBe(
        BOND_LOVELACE,
      );
    },
    TIMEOUT,
  );
});
