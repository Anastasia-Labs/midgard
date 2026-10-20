/**
 * The validation-trace dispute's funding under the watcher's prover funding,
 * on the emulator. A wallet the operator empties once Init has landed pauses
 * the objective at Open on the funding-shortfall hold, with the
 * prover-funding alarm, on every observation until a top-up, releases what
 * the paused attempt held, and then Opens and VerifySources. Coins at the prover address that carry an inline datum, a
 * datum hash or a reference script are never spent and never offered as
 * collateral, even when each is larger than every plain coin.
 *
 * The category is not installed until its defender exists; this file
 * stands in for that install so the production classifier mints the
 * objective's actuation permit.
 */
import {
  CML,
  Data,
  type Emulator,
  getAddressDetails,
  scriptFromNative,
} from "@lucid-evolution/lucid";
import {
  holdWatcherFaultProofOnFunding,
  WATCHER_PROVER_FUNDING_UNAVAILABLE,
} from "midgard-watcher/tests/support/prover-funding-hold";
import { beforeEach, describe, expect, it, vi } from "vitest";

import { VALIDATION_TRACE_CASE_OPEN } from "../src/validation-trace-dispute/adapter.js";
import {
  isWorkflowFundingPauseReason,
  WORKFLOW_FUNDING_SHORTFALL,
} from "../src/workflow/funding-reservation-permit.js";
import { parseOwnOutRef } from "../src/workflow/own-outputs.js";
import { drainProver, topUpProver } from "./support/installed-prover-wallet.js";
import {
  openFundedDispute,
  turnUntil,
} from "./support/validation-trace-dispute-funded-runner.js";
import {
  ADA,
  caseAndBond,
  confirmedStages,
  FLOOR,
  FUNDED,
  TIMEOUT,
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

type Submitted = Readonly<{
  inputs: readonly string[];
  collateral: readonly string[];
}>;

const outRefs = (list: CML.TransactionInputList | undefined) => {
  const refs: string[] = [];
  for (let index = 0; list !== undefined && index < list.len(); index += 1) {
    const input = list.get(index);
    refs.push(`${input.transaction_id().to_hex()}#${input.index().toString()}`);
  }
  return refs;
};

/** Every transaction the application submits to `emulator` from now on:
 * its inputs and its collateral inputs. What `asOtherParty` submits is not
 * recorded. */
const recordSubmitted = (emulator: Emulator) => {
  const submitted: Submitted[] = [];
  let others = 0;
  const submit = emulator.submitTx.bind(emulator);
  const spy = vi
    .spyOn(emulator, "submitTx")
    .mockImplementation(async (cbor: string) => {
      const body = CML.Transaction.from_cbor_hex(cbor).body();
      if (others === 0)
        submitted.push({
          inputs: outRefs(body.inputs()),
          collateral: outRefs(body.collateral_inputs()),
        });
      return await submit(cbor);
    });
  return {
    submitted,
    asOtherParty: async (act: () => Promise<void>) => {
      others += 1;
      try {
        await act();
      } finally {
        others -= 1;
      }
    },
    restore: () => spy.mockRestore(),
  };
};

let scene: Scene;

describe("validation-trace dispute funding under the prover funding permit", () => {
  beforeEach(async () => {
    scene = await setUpScene();
  }, TIMEOUT);

  it(
    "pauses Open on the funding-shortfall hold when the wallet empties after Init, releases the reservation, and opens after a top-up",
    async () => {
      const { h } = scene;
      await drainProver(h as never, FUNDED);
      const dispute = await openFundedDispute(scene);
      try {
        // Init lands; then the operator moves all but 3 ada of plain funding
        // out of the wallet, which Open cannot be paid from.
        const init = await dispute.turn();
        expect("reason" in init && init.reason).toContain(
          "validation-trace-init:first",
        );
        await drainProver(h as never, 3n * ADA);
        const paused = await turnUntil(dispute, WORKFLOW_FUNDING_SHORTFALL);
        expect(paused.result.kind, paused.history.join("\n")).toBe("pending");
        if (paused.result.kind !== "pending") return;
        expect(paused.result.reason).toContain(
          "cannot fund validation-trace-open:",
        );
        expect(isWorkflowFundingPauseReason(paused.result.reason)).toBe(true);
        expect(confirmedStages(paused.result)).toStrictEqual(["init"]);
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
        const reservations = await dispute.readReservations();
        expect(reservations.length).toBeGreaterThan(0);
        for (const reservation of reservations)
          expect(reservation).toMatchObject({ activeInputs: [] });

        // Paused, not abandoned: the next observation pauses again until the
        // wallet is funded.
        const again = await dispute.turn();
        expect(again).toMatchObject({ kind: "pending" });
        expect("reason" in again && again.reason).toContain(
          WORKFLOW_FUNDING_SHORTFALL,
        );

        await topUpProver(h as never, BOND_LOVELACE + FLOOR + 100n * ADA);
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
    "pauses VerifySource on a wallet whose ada sits in coins carrying an inline datum, a datum hash or a reference script, and lands after a top-up without spending them or offering them as collateral",
    async () => {
      const { h } = scene;
      // Plain funding for Init and Open, not VerifySource besides.
      await drainProver(h as never, BOND_LOVELACE + FLOOR + 2n * ADA);
      const address = h.proverSigner.address;
      // Each shaped coin alone could pay VerifySource's bond.
      const lovelace = BOND_LOVELACE + 1_000n * ADA;
      const scriptRef = scriptFromNative({
        type: "sig",
        keyHash: getAddressDetails(address).paymentCredential!.hash,
      });
      h.funderLucid.clearUTxOOverride();
      const planting = await h.funderLucid
        .newTx()
        .pay.ToAddressWithData(
          address,
          { kind: "inline", value: Data.void() },
          { lovelace },
        )
        .pay.ToAddressWithData(
          address,
          { kind: "asHash", value: Data.void() },
          { lovelace },
        )
        .pay.ToAddressWithData(address, undefined, { lovelace }, scriptRef)
        .complete();
      const planted = await (
        await planting.sign.withWallet().complete()
      ).submit();
      h.emulator.awaitBlock(1);
      const shaped = [0, 1, 2].map((index) => `${planted}#${index.toString()}`);
      expect(
        await h.emulator.getUtxosByOutRef(shaped.map(parseOwnOutRef)),
      ).toHaveLength(3);

      const recorded = recordSubmitted(h.emulator);
      const dispute = await openFundedDispute(scene);
      try {
        const paused = await turnUntil(dispute, WORKFLOW_FUNDING_SHORTFALL);
        expect(paused.result.kind, paused.history.join("\n")).toBe("pending");
        if (paused.result.kind !== "pending") return;
        expect(paused.result.reason).toContain(
          "cannot fund validation-trace-verify-source:",
        );
        expect(confirmedStages(paused.result)).toStrictEqual(["init", "open"]);

        await recorded.asOtherParty(
          async () => await topUpProver(h as never, BOND_LOVELACE + 100n * ADA),
        );
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
        expect(recorded.submitted.length).toBeGreaterThanOrEqual(3);
        for (const { inputs, collateral } of recorded.submitted) {
          expect(collateral.length).toBeGreaterThan(0);
          for (const outRef of shaped) {
            expect(inputs).not.toContain(outRef);
            expect(collateral).not.toContain(outRef);
          }
        }
        expect(
          await h.emulator.getUtxosByOutRef(shaped.map(parseOwnOutRef)),
        ).toHaveLength(3);
      } finally {
        recorded.restore();
        await dispute.close();
      }
    },
    TIMEOUT,
  );
});
