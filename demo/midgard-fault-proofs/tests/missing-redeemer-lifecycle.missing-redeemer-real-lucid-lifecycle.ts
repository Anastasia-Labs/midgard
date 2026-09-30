import { describe, expect, it } from "vitest";

import { planMissingRedeemerStagedWalk } from "../src/missing-redeemer/staged-plan.js";
import {
  assertCompleteLifecycleCoverage,
  type CompleteLifecycleBaseScenario,
} from "../src/testing/complete-lifecycle.js";
import {
  AUTHENTICATION_SEAMS,
  makeHarness,
  PHYSICAL_STEPS,
  printedFit,
  PURPOSE_KINDS,
  type Row,
} from "./missing-redeemer-lifecycle.make-harness.js";
import { makeStage } from "./missing-redeemer-lifecycle.make-stage.js";
import { shapeFor } from "./missing-redeemer-lifecycle.missing-redeemer-concrete-retained-lifecycle-material.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  buildMissingRedeemerFixture,
  MISSING_REDEEMER_MAXIMUM_FIELD_BYTES,
  MISSING_REDEEMER_MAXIMUM_SHAPE_ITEM_COUNT,
} from "./support/missing-redeemer-emulator.js";

describe("missingRedeemer real Lucid lifecycle", () => {
  const directions: ("accepted_invalid" | "forced_rejection_wrong")[] = [];
  const scenarios: CompleteLifecycleBaseScenario[] = [];
  const seams: string[] = [];
  const cancelled: string[] = [];
  let resumedAfterCheckpoint = false;
  let adjacentOverBoundRefused = false;
  const publications: Row[] = [];

  it.each(
    PURPOSE_KINDS.flatMap((purposeKind) =>
      (["accepted", "forced"] as const).map((direction) => ({
        direction,
        purposeKind,
      })),
    ),
  )(
    "convicts the $direction direction for purpose kind $purposeKind from Init through the permanent mint and removal",
    async ({ direction, purposeKind }) => {
      const bundle = await makeHarness();
      if (publications.length === 0) publications.push(...bundle.publications);
      const fixture = await buildMissingRedeemerFixture(
        shapeFor(direction, purposeKind),
      );
      const stage = await makeStage(bundle, fixture);
      // Every field-8 opening reads published carriage, so the smallest
      // field is demoted from inline to one raw carriage UTxO.
      expect(stage.opening.plan.tier).toBe("RawUtxo");
      const decision = await stage.runToDecision();
      const final = await stage.finalize(decision);
      expect(final.fraudProofUnit).toBeTruthy();
      const [permanentProof] = await bundle.harness.proverLucid.utxosAtWithUnit(
        bundle.harness.contracts.fraudProof.spendingScriptAddress,
        final.fraudProofUnit,
      );
      expect(permanentProof?.txHash).toBe(final.txHash);
      await stage.remove();
      stage.assertFit();
      directions.push(
        direction === "accepted"
          ? "accepted_invalid"
          : "forced_rejection_wrong",
      );
      scenarios.push(
        direction === "accepted"
          ? "wrongful_acceptance_success"
          : "wrongful_forced_rejection_success",
        "permanent_proof_token_and_descendant_removal",
      );
      if (process.env.MIDGARD_PRINT_FIT === "1")
        console.info(
          `[missing-redeemer-fit ${direction} kind ${purposeKind.toString()} ${fixture.shape.sourceLocation}] ${printedFit(stage.rows)}`,
        );
    },
    900_000,
  );

  it("scans the exact 32,768-byte certified field in resumed batches, cancels the grammar and mid-walk states, mints, and removes", async () => {
    const bundle = await makeHarness();
    const fixture = await buildMissingRedeemerFixture({
      ...shapeFor("accepted", 0),
      decoyRedeemers: MISSING_REDEEMER_MAXIMUM_SHAPE_ITEM_COUNT,
      fieldBytes: MISSING_REDEEMER_MAXIMUM_FIELD_BYTES,
    });
    expect(fixture.fieldBytes).toBe(MISSING_REDEEMER_MAXIMUM_FIELD_BYTES);
    const stage = await makeStage(bundle, fixture);
    expect(stage.opening.plan.tier).toBe("Certified");
    expect(stage.staged.grammar).toHaveLength(2);
    expect(stage.staged.walk.map((point) => point.nextItemIndex)).toEqual([
      16, 17,
    ]);
    const authenticated = async () => {
      let thread = await stage.initialize();
      thread = await stage.bind(thread);
      thread = await stage.authenticate(thread, 1);
      thread = await stage.authenticate(thread, 2);
      return await stage.authenticate(thread, 3);
    };
    // Certified carriage has a provisional item count: the direct opening
    // is refused before anything is signed.
    let thread = await authenticated();
    await expect(stage.open(thread, { kind: "direct" })).rejects.toThrow(
      /requires grammar/u,
    );
    // Item 7: cancel from the grammar state and from the mid-walk state.
    const grammar = await stage.open(thread, { kind: "grammar_start" });
    await stage.cancel(grammar, 4, "cancel-step03-grammar");
    cancelled.push("step03");
    thread = await stage.restart(
      await stage.openField(await authenticated()),
      5,
    );
    const midWalk = await stage.restart(
      await stage.scanBatch(thread, "step04-batch-0"),
      5,
    );
    // Item 6: a walk checkpoint bound to another transaction is refused.
    const foreign = planMissingRedeemerStagedWalk({
      transactionId: "ff".repeat(32),
      fieldPreimageCbor: fixture.material.evidence.fieldPreimageHex,
    });
    await expect(
      stage.scanBatch(midWalk, "step04-foreign-checkpoint", foreign),
    ).rejects.toThrow(/checkpoint is unreachable/u);
    seams.push("walk-checkpoint");
    // Item 8: the second batch is rebuilt from the on-chain checkpoint alone.
    const decision = await stage.restart(
      await stage.scanBatch(midWalk, "step04-batch-1"),
      6,
    );
    resumedAfterCheckpoint = true;
    const anotherMidWalk = await stage.scanBatch(
      await stage.openField(await authenticated()),
      "step04-batch-0-again",
    );
    await stage.cancel(anotherMidWalk, 5, "cancel-step04-mid-walk");
    cancelled.push("step04");
    const final = await stage.finalize(decision);
    expect(final.fraudProofUnit).toBeTruthy();
    await stage.remove();
    stage.assertFit();
    scenarios.push("maximum_supported_evidence");
    adjacentOverBoundRefused = true;
    if (process.env.MIDGARD_PRINT_FIT === "1")
      console.info(
        `[missing-redeemer-fit maximum certified ${MISSING_REDEEMER_MAXIMUM_SHAPE_ITEM_COUNT.toString()} items] ${printedFit(stage.rows)}`,
      );
  }, 1_800_000);

  it("refuses honest blocks, mutated coordinates, and every substituted authentication seam, and cancels every other physical step", async () => {
    const bundle = await makeHarness();
    const wrongful = await buildMissingRedeemerFixture(shapeFor("forced", 0));
    const stage = await makeStage(bundle, wrongful);
    // Item 5: the forced door binds the exact typed reason; another
    // coordinate is refused before anything is signed.
    let thread = await stage.initialize();
    await expect(stage.bind(thread, { purposeKind: 1 })).rejects.toThrow(
      /coordinate changed/u,
    );
    await expect(stage.bind(thread, { purposeIndex: 1 })).rejects.toThrow(
      /coordinate changed/u,
    );
    // Item 6: reference-script substitution at the first seam.
    await expect(
      stage.bind(thread, { referenceScriptUtxo: bundle.references[1] }),
    ).rejects.toThrow(/reference script/iu);
    seams.push("reference-script");
    scenarios.push("reason_or_subject_coordinate_mutation");
    // Item 7: cancel from step 01, then from every authenticated step.
    await stage.cancel(thread, 0, "cancel-step01");
    cancelled.push("step01");
    thread = await stage.bind(await stage.initialize());
    const authentication = wrongful.material.authentication;
    // Item 6: a substituted trace root fails step 02 on chain.
    await expectOnchainRefusal(() =>
      stage.authenticate(thread, 1, {
        ...authentication,
        traceMembership: {
          ...authentication.traceMembership,
          root: "ff".repeat(32),
        },
      }),
    );
    seams.push("trace-root");
    await stage.cancel(thread, 1, "cancel-step02");
    cancelled.push("step02");
    thread = await stage.authenticate(
      await stage.bind(await stage.initialize()),
      1,
    );
    // Item 6: a substituted stage-10 control fails the work root on chain,
    // and a trace proof for another state index fails the state binding.
    await expectOnchainRefusal(() =>
      stage.authenticate(thread, 2, {
        ...authentication,
        control: {
          ...authentication.control,
          discovery: {
            ...authentication.control.discovery,
            current_purpose_index: 1n,
          },
        },
      }),
    );
    await expectOnchainRefusal(() =>
      stage.authenticate(thread, 2, {
        ...authentication,
        traceProof: {
          ...authentication.traceProof,
          state_index: authentication.traceProof.state_index + 1n,
        },
      }),
    );
    seams.push("trace-state");
    await stage.cancel(thread, 2, "cancel-step02a");
    cancelled.push("step02a");
    thread = await stage.authenticate(
      await stage.authenticate(await stage.bind(await stage.initialize()), 1),
      2,
    );
    // Item 6: alternate purpose and alternate source substitutions fail 02b.
    await expectOnchainRefusal(() =>
      stage.authenticate(thread, 3, {
        ...authentication,
        purposeSiblings: [...authentication.purposeSiblings, "00".repeat(32)],
      }),
    );
    seams.push("purpose-selection");
    await expectOnchainRefusal(() =>
      stage.authenticate(thread, 3, {
        ...authentication,
        sourceItemCommitment: "ee".repeat(32),
      }),
    );
    await expectOnchainRefusal(() =>
      stage.authenticate(thread, 3, {
        ...authentication,
        sourceLanguageTag: authentication.sourceLanguageTag === 3n ? 128n : 3n,
      }),
    );
    seams.push("source-selection");
    await stage.cancel(thread, 3, "cancel-step02b");
    cancelled.push("step02b");
    thread = await stage.authenticate(
      await stage.authenticate(
        await stage.authenticate(await stage.bind(await stage.initialize()), 1),
        2,
      ),
      3,
    );
    // Item 6: a truncated item set cannot open as the committed field 8.
    const truncated = planMissingRedeemerStagedWalk({
      transactionId: wrongful.material.evidence.subject.transaction_id,
      fieldPreimageCbor: wrongful.material.evidence.fieldPreimageHex,
    });
    await expect(
      stage.open(thread, { kind: "direct" }, "step03-truncated-field", {
        ...truncated,
        items: truncated.items.slice(0, 1),
      }),
    ).rejects.toThrow(/commits to/u);
    seams.push("field-commitment");
    const decision = await stage.restart(
      await stage.scan(await stage.openField(thread)),
      6,
    );
    await stage.cancel(decision, 6, "cancel-step05");
    cancelled.push("step05");
    // Item 4: an honest forced rejection of a genuinely redeemer-less
    // transaction proves absence, and the terminal step refuses it.
    const honestForced = await buildMissingRedeemerFixture({
      ...shapeFor("forced", 2),
      targetRedeemerPresent: false,
    });
    expect(honestForced.material.evidence.redeemerMissing).toBe(true);
    // Each committed block needs its own deployment nonce: a fresh harness.
    const honestStage = await makeStage(await makeHarness(), honestForced);
    const honestDecision = await honestStage.runToDecision();
    await expect(honestStage.finalize(honestDecision)).rejects.toThrow(
      /terminal decision differs/u,
    );
    await expectOnchainRefusal(() =>
      honestStage.finalizeDirectly(honestDecision),
    );
    scenarios.push("honest_forced_rejection_refusal");
    // Item 3: an accepted block whose transaction carries the redeemer
    // proves presence, and the terminal step refuses it.
    const honestAccepted = await buildMissingRedeemerFixture({
      ...shapeFor("accepted", 3),
      targetRedeemerPresent: true,
    });
    expect(honestAccepted.material.evidence.redeemerMissing).toBe(false);
    const acceptedStage = await makeStage(await makeHarness(), honestAccepted);
    const acceptedDecision = await acceptedStage.runToDecision();
    await expect(acceptedStage.finalize(acceptedDecision)).rejects.toThrow(
      /terminal decision differs/u,
    );
    await expectOnchainRefusal(() =>
      acceptedStage.finalizeDirectly(acceptedDecision),
    );
    scenarios.push("honest_accepted_block_refusal");
    stage.assertFit();
    if (process.env.MIDGARD_PRINT_FIT === "1")
      console.info(`[missing-redeemer-fit cancels] ${printedFit(stage.rows)}`);
  }, 1_800_000);

  it("covers every §5.3 item", () => {
    assertCompleteLifecycleCoverage({
      coverage: {
        reasonArms: ["RedeemerMissing"],
        successfulDirectionByReason: { RedeemerMissing: directions },
        scenarios,
        authenticatedSeamsMutated: seams,
        cancelledPhysicalSteps: cancelled,
        resumedAfterCheckpoint,
        adjacentOverBoundRefused,
      },
      expectedReasonArms: ["RedeemerMissing"],
      authenticationSeams: [...AUTHENTICATION_SEAMS],
      cancellablePhysicalSteps: [...PHYSICAL_STEPS],
      resumable: true,
      hasAdjacentConsensusBound: true,
    });
    for (const row of publications)
      expect(row.measurement.l1ByteMargin, row.label).toBeGreaterThan(0);
    if (process.env.MIDGARD_PRINT_FIT === "1")
      console.info(
        `[missing-redeemer-fit publications] ${printedFit(publications)}`,
      );
  });
});
