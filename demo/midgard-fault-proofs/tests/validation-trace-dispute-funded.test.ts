/** The validation-trace dispute's VerifySource under the watcher's prover
 * funding, on the emulator: it locks exactly the deployment-bound dispute
 * bond and carries the case at no less than its floor, funding every stage
 * from plain coins while a larger token-bearing coin sits at the prover's
 * address. A process stopped after broadcasting
 * VerifySource lands those same bytes, and one stopped after they landed
 * sends nothing again; one stopped before broadcasting supersedes its
 * unsent bytes once they expire with a single re-signed attempt, so one
 * case and one bond open and the stopped bytes can no longer land. A
 * wallet that cannot hold the bond, or cannot pay VerifySource, pauses the
 * objective on the watcher's prover-funding hold until a top-up, then
 * lands.
 *
 * The category is not installed until its defender exists; this file
 * stands in for that install so the production classifier mints the
 * objective's actuation permit. */
import {
  holdWatcherFaultProofOnFunding,
  WATCHER_PROVER_FUNDING_UNAVAILABLE,
} from "midgard-watcher/tests/support/prover-funding-hold";
import { beforeEach, describe, expect, it, vi } from "vitest";

import { VALIDATION_TRACE_CASE_OPEN } from "../src/validation-trace-dispute/adapter.js";
import { readValidationDisputeFundingAuthority } from "../src/workflow/funding-reservation-permit.assert-validation-dispute-funding.js";
import {
  isWorkflowFundingPauseReason,
  WORKFLOW_FUNDING_SHORTFALL,
} from "../src/workflow/funding-reservation-permit.js";
import { parseOwnOutRef } from "../src/workflow/own-outputs.js";
import { workflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import { drainProver, topUpProver } from "./support/installed-prover-wallet.js";
import {
  plantTokenCoin,
  watchLedger,
} from "./support/own-outputs-installed.js";
import {
  type AdapterWrap,
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
  isVerifySource,
  stopAtVerifySourceSubmit,
  TIMEOUT,
  verifySourceIntents,
} from "./support/validation-trace-dispute-funded-verify-source.js";
import {
  BOND_LOVELACE,
  type Scene,
  setUpScene,
} from "./support/validation-trace-dispute-open-scene.js";

// Stands in for the family's install, which waits on its defender.
vi.mock("../src/workflow/installed-categories.js", async (importOriginal) => {
  const actual =
    await importOriginal<
      typeof import("../src/workflow/installed-categories.js")
    >();
  return {
    ...actual,
    isInstalledFraudProofCategory: (
      category: Parameters<typeof actual.isInstalledFraudProofCategory>[0],
    ) =>
      category === "validationTraceDispute" ||
      actual.isInstalledFraudProofCategory(category),
  };
});

let scene: Scene;

describe("validation-trace dispute VerifySource under the prover funding permit", () => {
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
      try {
        // The opener VerifySource spends, read from the ledger before it does.
        let openerLovelace: bigint | undefined;
        const observeOpener: AdapterWrap = (adapter) => ({
          ...adapter,
          preflight: async (context) => {
            const preflight = await adapter.preflight(context);
            if (isVerifySource(context.action)) {
              const signed = workflowPreflightTransaction(preflight)!;
              const [opener] = await scene.h.emulator.getUtxosByOutRef([
                parseOwnOutRef(
                  readValidationDisputeFundingAuthority(signed)!.openerOutRef,
                ),
              ]);
              openerLovelace = opener!.assets.lovelace;
            }
            return preflight;
          },
        });
        const { result, history } = await turnUntil(
          dispute,
          VALIDATION_TRACE_CASE_OPEN,
          { wrap: observeOpener },
        );
        expect(result.kind, history.join("\n")).toBe("pending");
        const { caseOutput, bond, caseClaim, bondClaim } = await caseAndBond(
          scene,
          result,
        );
        expect(bond.address).toBe(
          scene.chain.disputeBond.spendingScriptAddress,
        );
        expect(bond.assets).toStrictEqual({
          lovelace: BOND_LOVELACE,
          [bondClaim.unit]: 1n,
        });
        expect(caseOutput.address).toBe(scene.chain.case.spendingScriptAddress);
        expect(caseOutput.assets[caseClaim.unit]).toBe(1n);
        // The pending claim's datum alone needs more than the floor in
        // min-Ada, so a production-built opener always clears it and the
        // case carries the opener's lovelace.
        expect(openerLovelace).toBeDefined();
        expect(openerLovelace!).toBeGreaterThan(FLOOR);
        expect(caseOutput.assets.lovelace).toBe(openerLovelace);
      } finally {
        await dispute.close();
      }
    },
    TIMEOUT,
  );

  it(
    "lands the same signed VerifySource after a process stop between its broadcast and its landing",
    async () => {
      await drainProver(scene.h as never, FUNDED);
      const dispute = await openFundedDispute(scene);
      try {
        const stop = stopAtVerifySourceSubmit({ broadcast: true });
        await stop.stopped(dispute);
        await dispute.restart();
        const { result, history } = await turnUntil(
          dispute,
          VALIDATION_TRACE_CASE_OPEN,
        );
        expect(result.kind, history.join("\n")).toBe("pending");
        // The bytes already reached the ledger's mempool: nothing is sent again.
        expect(dispute.rebroadcasts).toStrictEqual([]);
        const { verify } = await caseAndBond(scene, result);
        expect(
          verify.outputs.map(({ outRef }) => outRef.split("#")[0]),
        ).toStrictEqual([stop.signedHash(), stop.signedHash()]);
        expect(
          verifySourceIntents(result).map(({ txHash }) => txHash),
        ).toStrictEqual([stop.signedHash()]);
        await expectOneCaseAndBond(scene);
      } finally {
        await dispute.close();
      }
    },
    TIMEOUT,
  );

  it(
    "sends nothing again after a process stop once its broadcast VerifySource landed",
    async () => {
      await drainProver(scene.h as never, FUNDED);
      const dispute = await openFundedDispute(scene);
      try {
        const stop = stopAtVerifySourceSubmit({ broadcast: true });
        await stop.stopped(dispute);
        scene.h.emulator.awaitBlock(1);
        expect(
          (await scene.h.emulator.getTransactionStatus(stop.signedHash()))
            .status,
        ).toBe("confirmed");
        await dispute.restart();
        const { result, history } = await turnUntil(
          dispute,
          VALIDATION_TRACE_CASE_OPEN,
        );
        expect(result.kind, history.join("\n")).toBe("pending");
        expect(dispute.rebroadcasts).toStrictEqual([]);
        expect(
          verifySourceIntents(result).map(({ txHash }) => txHash),
        ).toStrictEqual([stop.signedHash()]);
        const { verify } = await caseAndBond(scene, result);
        expect(
          verify.outputs.map(({ outRef }) => outRef.split("#")[0]),
        ).toStrictEqual([stop.signedHash(), stop.signedHash()]);
        await expectOneCaseAndBond(scene);
      } finally {
        await dispute.close();
      }
    },
    TIMEOUT,
  );

  it(
    "supersedes VerifySource's unsent signed bytes once they expire after a stop before broadcast: one re-signed attempt, one case and bond, and the stopped bytes can no longer land",
    async () => {
      await drainProver(scene.h as never, FUNDED);
      const dispute = await openFundedDispute(scene);
      try {
        const stop = stopAtVerifySourceSubmit({ broadcast: false });
        await stop.stopped(dispute);
        await dispute.restart();
        const { result, history } = await turnUntil(
          dispute,
          VALIDATION_TRACE_CASE_OPEN,
        );
        expect(result.kind, history.join("\n")).toBe("pending");
        // Only the stopped process's exact bytes are ever offered again; the
        // journal's 30 s retry interval outlasts their validity range, so they
        // expire unsent and the attempt is re-signed.
        expect(dispute.rebroadcasts.length).toBeGreaterThan(0);
        expect(new Set(dispute.rebroadcasts)).toStrictEqual(
          new Set([stop.signedHex()]),
        );
        const intents = verifySourceIntents(result);
        expect(intents.map(({ attempt }) => attempt)).toStrictEqual([1, 2]);
        expect(intents[0]!.txHash).toBe(stop.signedHash());
        expect(intents[1]!.txHash).not.toBe(stop.signedHash());
        expect(
          (await scene.h.emulator.getTransactionStatus(stop.signedHash()))
            .status,
        ).not.toBe("confirmed");
        const { verify } = await caseAndBond(scene, result);
        expect(
          verify.outputs.map(({ outRef }) => outRef.split("#")[0]),
        ).toStrictEqual([intents[1]!.txHash, intents[1]!.txHash]);
        await expectOneCaseAndBond(scene);
        // The superseded bytes are dead: the ledger refuses them now.
        await expect(
          scene.h.emulator.submitTx(stop.signedHex()),
        ).rejects.toThrow();
        expect(
          (await scene.h.emulator.getTransactionStatus(stop.signedHash()))
            .status,
        ).not.toBe("confirmed");
      } finally {
        await dispute.close();
      }
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
        try {
          const { result, history } = await turnUntil(
            dispute,
            VALIDATION_TRACE_CASE_OPEN,
          );
          expect(result.kind, history.join("\n")).toBe("pending");
          expect(confirmedStages(result)).toStrictEqual([
            "init",
            "open",
            "verify_source",
          ]);
          expect(watch.ours()).toHaveLength(3);
          for (const transaction of watch.ours())
            expect(transaction.inputs).not.toContain(tokenCoin);
          expect(
            await h.emulator.getUtxosByOutRef([parseOwnOutRef(tokenCoin!)]),
          ).toHaveLength(1);
        } finally {
          await dispute.close();
        }
      } finally {
        watch.restore();
      }
    },
    TIMEOUT,
  );

  it(
    "holds an objective whose wallet cannot reserve the bond on the prover-funding pause, then lands after a top-up",
    async () => {
      const { h } = scene;
      await drainProver(h as never, 5_000n * ADA);
      const dispute = await openFundedDispute(scene);
      try {
        const unavailable = await dispute.turn().then(
          () => undefined,
          (cause: unknown) => cause,
        );
        expect(String(unavailable)).toContain(
          `prover wallet cannot reserve the validation dispute bond and case floor: required=${(BOND_LOVELACE + FLOOR).toString()} available=${(5_000n * ADA).toString()}`,
        );
        const holds = new Map<string, { reason: string; detail: string }>();
        expect(
          holdWatcherFaultProofOnFunding(
            holds,
            {
              category: "validationTraceDispute",
              headerHash: dispute.decision.headerHash,
            } as never,
            unavailable,
          ),
        ).toMatchObject({ kind: "pending" });
        expect([...holds.values()].map(({ reason }) => reason)).toStrictEqual([
          WATCHER_PROVER_FUNDING_UNAVAILABLE,
        ]);
        expect(await dispute.readReservations()).toHaveLength(0);

        await topUpProver(h as never, BOND_LOVELACE);
        const { result, history } = await turnUntil(
          dispute,
          VALIDATION_TRACE_CASE_OPEN,
        );
        expect(result.kind, history.join("\n")).toBe("pending");
        expect((await caseAndBond(scene, result)).bond.assets.lovelace).toBe(
          BOND_LOVELACE,
        );
      } finally {
        await dispute.close();
      }
    },
    TIMEOUT,
  );

  it(
    "pauses VerifySource on the funding-shortfall hold when the wallet cannot pay it, releases the reservation, and lands after a top-up",
    async () => {
      const { h } = scene;
      // Enough to reserve the bond and the case floor, not to pay for Init,
      // Open and VerifySource besides.
      await drainProver(h as never, BOND_LOVELACE + FLOOR + 2n * ADA);
      const dispute = await openFundedDispute(scene);
      try {
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
        expect(confirmedStages(paused.result)).toStrictEqual(["init", "open"]);

        // The paused attempt released every input it held.
        const reservations = await dispute.readReservations();
        expect(reservations.length).toBeGreaterThan(0);
        for (const reservation of reservations)
          expect(reservation).toMatchObject({ activeInputs: [] });
        await topUpProver(h as never, 100n * ADA);
        const { result, history } = await turnUntil(
          dispute,
          VALIDATION_TRACE_CASE_OPEN,
        );
        expect(result.kind, history.join("\n")).toBe("pending");
        expect((await caseAndBond(scene, result)).bond.assets.lovelace).toBe(
          BOND_LOVELACE,
        );
      } finally {
        await dispute.close();
      }
    },
    TIMEOUT,
  );
});
