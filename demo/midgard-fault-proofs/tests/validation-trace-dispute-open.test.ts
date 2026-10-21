/** Opening a validation case on the emulator. The complete DA replay of the
 * committed blocks detects the normal transaction whose script fails, and
 * only it; Open and VerifySource over that detection land, with the case datum
 * the off-chain claim material derives, each within the transaction limits,
 * also when the validity bounds are not on a slot boundary and at the
 * largest step count the trace descriptor admits (2^32 - 1). source-v1
 * refuses a terminal the challenger's own committed root does not prove, a
 * forced transaction (R-DESC), a state 0 the header does not derive (N5), a
 * bond one lovelace short, and a VerifySource after `block_end + M/2`. */
import {
  hashMidgardValidationMachineState,
  MIDGARD_VALIDATION_CASE_OPEN_WINDOW_MS,
} from "@al-ft/midgard-core";
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import { buildValidationClaimWitness } from "../src/validation-trace-dispute/claim-material.js";
import { expectOnchainRefusal } from "./support/submit-init-emulator-shared.js";
import { sparseTrace } from "./support/validation-game-deep-traces.js";
import {
  BOND_LOVELACE,
  detectOn,
  failingClaim,
  failingMaterial,
  honestPresentation,
  initAndOpen,
  passingTrace,
  type Scene,
  setUpScene,
  verifySource,
} from "./support/validation-trace-dispute-open-scene.js";
import { measureWithinTransactionLimits } from "./support/validation-trace-dispute-transaction-limits.js";

let scene: Scene;

describe("validation-trace dispute: Open and VerifySource", () => {
  beforeAll(async () => {
    scene = await setUpScene();
  }, 1_800_000);

  it("detects and opens a case on a committed transaction whose script fails", async () => {
    const { h, chain } = scene;
    expect(await detectOn(scene.failing)).toHaveLength(1);
    expect(await detectOn(scene.passing)).toStrictEqual([]);
    expect(await detectOn(scene.forced)).toStrictEqual([]);
    const material = await failingMaterial(scene);
    const { opener } = await initAndOpen(
      scene,
      scene.failing,
      failingClaim(material),
    );
    const verification = verifySource(
      scene,
      opener,
      honestPresentation(material),
    );
    const signed = await (await verification.build()).sign
      .withWallet()
      .complete();
    measureWithinTransactionLimits(signed.toCBOR());
    const txHash = await signed.submit();
    await h.proverLucid.awaitTx(txHash);

    const { opening } = verification;
    const [caseOutput] = await h.proverLucid.utxosAtWithUnit(
      chain.case.spendingScriptAddress,
      opening.caseUnit,
    );
    expect(caseOutput?.txHash).toBe(txHash);
    const threadUnit = Object.keys(opener.assets).find(
      (unit) => unit !== "lovelace",
    )!;
    expect(caseOutput!.assets[threadUnit]).toBe(1n);
    const datum = Data.from(caseOutput!.datum!, SDK.Case);
    expect(datum).toStrictEqual(opening.case);
    expect(datum).toMatchObject({
      block_header_hash: scene.failing.evidence.headerHash,
      event_key: scene.failing.eventKey,
      challenger_root: material.challengerRoot,
      step_count: material.stepCount,
      started_at: BigInt(verification.validTo - 1),
      agreed_initial_hash: hashMidgardValidationMachineState(
        material.initialState,
      ).toString("hex"),
      challenger_terminal_hash: hashMidgardValidationMachineState(
        material.terminal,
      ).toString("hex"),
      bounty_claimant: h.proverSigner.paymentKeyHash,
    });

    const [bond] = await h.proverLucid.utxosAtWithUnit(
      chain.disputeBond.spendingScriptAddress,
      opening.bondMarkerUnit,
    );
    expect(bond?.txHash).toBe(txHash);
    expect(bond!.assets.lovelace).toBe(BOND_LOVELACE);
    expect(Data.from(bond!.datum!, SDK.DisputeBond)).toStrictEqual(
      opening.bond,
    );
  });

  it("lands VerifySource whose wall-clock bounds are not slot-aligned", async () => {
    const { h, chain } = scene;
    const offsetMs = 500;
    expect(
      SELECTED_DEPLOYMENT_PROFILE.timing.dispute_max_validity_range_ms % 1_000,
    ).toBe(0);
    const material = await failingMaterial(scene);
    const { opener } = await initAndOpen(
      scene,
      scene.failing,
      failingClaim(material),
    );
    const verification = verifySource(scene, opener, {
      ...honestPresentation(material),
      offsetMs,
    });
    const signed = await (await verification.build()).sign
      .withWallet()
      .complete();
    const txHash = await signed.submit();
    await h.proverLucid.awaitTx(txHash);
    const [caseOutput] = await h.proverLucid.utxosAtWithUnit(
      chain.case.spendingScriptAddress,
      verification.opening.caseUnit,
    );
    expect(caseOutput?.txHash).toBe(txHash);
    // The upper bound the transaction carries is the slot that holds
    // `validTo`, exclusive: the case starts one millisecond before it.
    expect(Data.from(caseOutput!.datum!, SDK.Case).started_at).toBe(
      BigInt(verification.validTo - offsetMs - 1),
    );
  });

  it("refuses a terminal the challenger's own committed root does not prove", async () => {
    const honest = await passingTrace(scene);
    const { opener } = await initAndOpen(scene, scene.passing, honest.claim);
    const pin = {
      refusedBy: "fraud_proofs/validation_trace/source_v1",
      check:
        /expect and \{ challenger_terminal\.phase == validation_trace_v1\.Terminal/u,
    };
    // The only terminal the committed root proves is Accepted.
    await expectOnchainRefusal(
      () =>
        verifySource(scene, opener, {
          terminal: honest.terminal,
          terminalProof: honest.terminalProof,
          initialState: honest.initialState,
        }).build(),
      pin,
    );
    // A Rejected terminal at S is not the leaf the root holds.
    const rejected = (await failingMaterial(scene)).terminal;
    await expectOnchainRefusal(
      () =>
        verifySource(scene, opener, {
          terminal: {
            ...honest.terminal,
            verdict: "rejected",
            rejectionCodeHash: rejected.rejectionCodeHash,
          },
          terminalProof: honest.terminalProof,
          initialState: honest.initialState,
        }).build(),
      pin,
    );
  });

  it("refuses a case on a forced transaction (R-DESC)", async () => {
    const material = await failingMaterial(scene);
    const claimWitness = await buildValidationClaimWitness({
      reconstruction: scene.forced.evidence.reconstruction,
      eventKey: scene.forced.eventKey,
    });
    expect(claimWitness.source_membership).toHaveProperty(
      "ForcedValidationSource",
    );
    const { opener } = await initAndOpen(scene, scene.forced, {
      ...failingClaim(material),
      claimWitness,
    });
    await expectOnchainRefusal(
      () => verifySource(scene, opener, honestPresentation(material)).build(),
      {
        refusedBy: "fraud_proofs/validation_trace/source_v1",
        check: /expect validation_claim_v1\.source_requires_trace/u,
      },
    );
  });

  it("lands Open and VerifySource at the maximum step count within the transaction limits", async () => {
    const { h, chain } = scene;
    const material = await failingMaterial(scene);
    // 2^32 - 1 steps: a 32-level trace tree, the deepest the case admits.
    const stepCount = 2 ** 32 - 1;
    const trace = sparseTrace(
      [
        { from: 0, state: material.initialState },
        { from: stepCount, state: material.terminal },
      ],
      stepCount,
    );
    const terminalProof = trace.proofAt(stepCount);
    expect(terminalProof.siblings).toHaveLength(32);
    const { opener } = await initAndOpen(scene, scene.failing, {
      claimWitness: material.claimWitness,
      challengerRoot: trace.descriptor.traceRoot.toString("hex"),
      stepCount: BigInt(stepCount),
    });
    const verification = verifySource(scene, opener, {
      terminal: material.terminal,
      terminalProof,
      initialState: material.initialState,
    });
    const signed = await (await verification.build()).sign
      .withWallet()
      .complete();
    measureWithinTransactionLimits(signed.toCBOR());
    const txHash = await signed.submit();
    await h.proverLucid.awaitTx(txHash);
    const [caseOutput] = await h.proverLucid.utxosAtWithUnit(
      chain.case.spendingScriptAddress,
      verification.opening.caseUnit,
    );
    expect(caseOutput?.txHash).toBe(txHash);
    expect(Data.from(caseOutput!.datum!, SDK.Case).step_count).toBe(
      BigInt(stepCount),
    );
  });

  // Last: it moves the emulator past the case open window of every block.
  it("refuses a wrong state 0, a short bond and a late VerifySource", async () => {
    const { h } = scene;
    const material = await failingMaterial(scene);
    const { opener } = await initAndOpen(
      scene,
      scene.failing,
      failingClaim(material),
    );
    const honest = honestPresentation(material);
    const finalCheck = {
      refusedBy: "fraud_proofs/validation_trace/source_v1",
      check: /^Validator returned false$/u,
    };
    // State 0 over the block's post-state root instead of the event's
    // pre-state root (N5).
    await expectOnchainRefusal(
      () =>
        verifySource(scene, opener, {
          ...honest,
          initialState: {
            ...material.initialState,
            priorLedgerRoot: Buffer.from(
              scene.failing.evidence.header.utxosRoot,
              "hex",
            ),
          },
        }).build(),
      finalCheck,
    );
    await expectOnchainRefusal(
      () =>
        verifySource(scene, opener, {
          ...honest,
          bondLovelace: BOND_LOVELACE - 1n,
        }).build(),
      finalCheck,
    );

    const caseOpenDeadline =
      Number(scene.failing.evidence.header.endTime) +
      MIDGARD_VALIDATION_CASE_OPEN_WINDOW_MS;
    h.emulator.awaitSlot(
      Math.ceil((caseOpenDeadline - h.emulator.now()) / 1_000) + 1,
    );
    await expectOnchainRefusal(
      () => verifySource(scene, opener, honest).build(),
      {
        refusedBy: "fraud_proofs/validation_trace/source_v1",
        check: /expect validation_dispute_v1\.case_can_open/u,
      },
    );
  });
});
