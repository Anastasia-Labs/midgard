import "node:crypto";
import "node:fs";
import "node:url";
import "@al-ft/midgard-core";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "vitest";
import "../src/committed-field-shape/submit-committed-field-shape-init.js";
import "../src/field-item-width-illegal/index.js";
import "../src/proof-fit/van-rossem-fit-ledger.js";
import "../src/remove-fraudulent-block.js";
import "../src/testing/complete-lifecycle.js";
import "../src/transition-trace/witnesses.js";
import "./support/emulator/blueprints.js";
import "./support/emulator/emulator-context.js";
import "./support/emulator/expect-onchain-refusal.js";
import "./support/emulator/harness.js";
import "./support/emulator/measurement.js";
import "./support/emulator/reference-scripts.js";
import "./support/emulator/registered-chain.js";
import "./support/emulator/removal-deployment.js";
import "./support/emulator/setup-tx.js";
import "./support/field-item-width-illegal-shapes.js";
import "./support/lifecycle-coverage.js";
import "./support/submit-init-emulator-fixtures.js";
import "./support/submit-init-emulator-shared.js";
import "./field-item-width-illegal-lifecycle.record.js";
import "./field-item-width-illegal-lifecycle.make-harness.js";
import "./field-item-width-illegal-lifecycle.forced-block.js";

import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";

import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { type FieldItemWidthFinding } from "../src/field-item-width-illegal/index.js";
import {
  buildVanRossemFitLedger,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { assertCompleteLifecycleCoverage } from "../src/testing/complete-lifecycle.js";
import {
  acceptedBlock,
  acceptedEvidence,
  forcedHonestRefusal,
  forcedSuccess,
  mutateCertifiedCarriage,
  recordCarriage,
  widthReason,
} from "./field-item-width-illegal-lifecycle.forced-block.js";
import { makeHarness } from "./field-item-width-illegal-lifecycle.make-harness.js";
import {
  AUTHENTICATION_SEAMS,
  CANCELLABLE_STEPS,
  coverage,
  ledgerPath,
  measurements,
  REASON_ARM,
  record,
} from "./field-item-width-illegal-lifecycle.record.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  boundaryLegalOutputShape,
  compactCborHex,
  emptyMintItemShape,
  forcedIllegalOutputShape,
  forcedMaximumLegalOutputShape,
  MAXIMUM_FIELD_BYTES,
  maximumIllegalOutputShape,
  mintFieldTx,
  nonEmptyMintItemShape,
} from "./support/field-item-width-illegal-shapes.js";

describe("fieldItemWidthIllegal registered-chain lifecycle", () => {
  it("convicts the widest accepted output a maximum field can carry: cancels every step, refuses every step-02 seam, the adjacent item coordinate and the honest bound, then mints and removes", async () => {
    const h = await makeHarness();
    const maximum = maximumIllegalOutputShape();
    const legal = boundaryLegalOutputShape();
    const { setup, inclusions } = await acceptedBlock(h, [maximum, legal]);
    const maximumInclusion = inclusions[0]!;
    const legalInclusion = inclusions[1]!;
    const evidence = acceptedEvidence(maximum);
    expect(evidence.fieldPreimageHex).toHaveLength(MAXIMUM_FIELD_BYTES * 2);
    expect(evidence.carriage).toBe("Certified");
    expect(evidence.decisiveFaultHolds).toBe(true);
    const legalEvidence = acceptedEvidence(legal);
    expect(legalEvidence.itemWidth).toBe(16_384);
    expect(legalEvidence.decisiveFaultHolds).toBe(false);

    const initialized = await h.init(setup.fraudulentBlockOutRef);
    record("accepted-init", maximum.label, initialized.measurement);
    const bound = await h.step01Accepted(
      initialized.result,
      evidence,
      maximumInclusion,
      setup.fraudulentBlockOutRef,
    );
    record("accepted-step01", maximum.label, bound.measurement);
    const authenticated = await h.step02(
      bound.result.nextThreadOutRef,
      evidence,
      maximum,
    );
    expect(authenticated.result.carriageTier).toBe("Certified");
    recordCarriage("accepted", maximum.label, authenticated, 3);
    const proven = await h.step03(
      authenticated.result.nextThreadOutRef,
      evidence,
    );
    expect(proven.result.fraudProofUnit).toBeTruthy();
    record("accepted-step03-proof-mint", maximum.label, proven.measurement);
    coverage.reason(REASON_ARM, "accepted_invalid");
    coverage.scenario("wrongful_acceptance_success");
    coverage.scenario("maximum_supported_evidence");
    // Cancel from every nonterminal physical step.
    const cancelAt01 = await h.init(setup.fraudulentBlockOutRef);
    record(
      "accepted-cancel-step01",
      maximum.label,
      (await h.cancel(h.threadOf(cancelAt01.result), 0)).measurement,
    );
    const cancelAt02 = await h.step01Accepted(
      (await h.init(setup.fraudulentBlockOutRef)).result,
      evidence,
      maximumInclusion,
      setup.fraudulentBlockOutRef,
    );
    record(
      "accepted-cancel-step02",
      maximum.label,
      (await h.cancel(cancelAt02.result.nextThreadOutRef, 1)).measurement,
    );
    const cancelAt03 = await h.step02(
      (
        await h.step01Accepted(
          (await h.init(setup.fraudulentBlockOutRef)).result,
          evidence,
          maximumInclusion,
          setup.fraudulentBlockOutRef,
        )
      ).result.nextThreadOutRef,
      evidence,
      maximum,
    );
    record(
      "accepted-cancel-step03",
      maximum.label,
      (await h.cancel(cancelAt03.result.nextThreadOutRef, 2)).measurement,
    );

    // Step-01 seam: the transaction's membership in the committed block.
    const membershipThread = await h.init(setup.fraudulentBlockOutRef);
    await expectOnchainRefusal(() =>
      h.step01Accepted(
        membershipThread.result,
        evidence,
        { ...maximumInclusion, transactionsPhasRoot: "ff".repeat(32) },
        setup.fraudulentBlockOutRef,
      ),
    );
    coverage.seamMutated("tx_membership");
    await h.cancel(h.threadOf(membershipThread.result), 0);

    // Step-02 seams against one bound thread; a refused spend leaves it bound.
    const seamThread = (
      await h.step01Accepted(
        (await h.init(setup.fraudulentBlockOutRef)).result,
        evidence,
        maximumInclusion,
        setup.fraudulentBlockOutRef,
      )
    ).result.nextThreadOutRef;
    const otherCompact = compactCborHex(legal.nativeTx, 0n);
    await expectOnchainRefusal(() =>
      h.step02Raw(seamThread, evidence, maximum, {
        mutateOpening: (opening) => {
          if (!("BodyFieldOpening" in opening))
            throw new Error("body opening expected");
          return {
            BodyFieldOpening: {
              ...opening.BodyFieldOpening,
              native_tx_compact_cbor: otherCompact,
            },
          };
        },
      }),
    );
    coverage.seamMutated("native_tx_source");
    await expectOnchainRefusal(() =>
      h.step02Raw(seamThread, evidence, maximum, {
        mutateOpening: (opening) =>
          mutateCertifiedCarriage(opening, (carriage) => ({
            ...carriage,
            cert_ref_input_index: carriage.chunk_ref_input_indices[0]!,
          })),
      }),
    );
    coverage.seamMutated("field_certificate");
    await expectOnchainRefusal(() =>
      h.step02Raw(seamThread, evidence, maximum, {
        mutateOpening: (opening) =>
          mutateCertifiedCarriage(opening, (carriage) => ({
            ...carriage,
            chunk_ref_input_indices: [
              ...carriage.chunk_ref_input_indices,
            ].reverse(),
          })),
      }),
    );
    coverage.seamMutated("field_chunks");
    await expectOnchainRefusal(() =>
      h.step02Raw(seamThread, evidence, maximum, { nextStepIndex: 1 }),
    );
    coverage.seamMutated("successor_script");
    await h.cancel(seamThread, 1);

    // Adjacent-over-bound coordinate: the field has one item, the thread binds
    // index 1, and the door refuses the read at the item-count bound.
    const overBound = await h.step01Accepted(
      (await h.init(setup.fraudulentBlockOutRef)).result,
      { subject: evidence.subject, fieldIndex: 2, itemIndex: 1 },
      maximumInclusion,
      setup.fraudulentBlockOutRef,
    );
    await expectOnchainRefusal(() =>
      h.step02Raw(overBound.result.nextThreadOutRef, evidence, maximum, {}),
    );
    coverage.adjacentOverBoundRefused();
    await h.cancel(overBound.result.nextThreadOutRef, 1);

    // Honest accepted block: the adjacent legal width, exactly at the bound,
    // authenticates but the terminal step refuses to convict it.
    const honest = await h.step02(
      (
        await h.step01Accepted(
          (await h.init(setup.fraudulentBlockOutRef)).result,
          legalEvidence,
          legalInclusion,
          setup.fraudulentBlockOutRef,
        )
      ).result.nextThreadOutRef,
      legalEvidence,
      legal,
    );
    await expectOnchainRefusal(() =>
      h.step03Raw(honest.result.nextThreadOutRef),
    );
    coverage.scenario("honest_accepted_block_refusal");
    await h.cancel(honest.result.nextThreadOutRef, 2);

    // Removal last: it consumes the fraudulent block every thread above bound.
    record(
      "accepted-remove",
      maximum.label,
      (await h.removal(setup.headerHash)).measurement,
    );
  }, 600_000);

  it("convicts an accepted empty mint-policy item through inline carriage, refuses substituted field bytes and the honest non-empty item, then mints and removes", async () => {
    const h = await makeHarness();
    const nativeTx = mintFieldTx();
    const empty = emptyMintItemShape(nativeTx);
    const nonEmpty = nonEmptyMintItemShape(nativeTx);
    const { setup, inclusions } = await acceptedBlock(h, [empty]);
    const inclusion = inclusions[0]!;
    const evidence = acceptedEvidence(empty);
    expect(evidence.carriage).toBe("Inline");
    expect(evidence.itemWidth).toBe(0);
    expect(evidence.decisiveFaultHolds).toBe(true);

    const initialized = await h.init(setup.fraudulentBlockOutRef);
    record("accepted-mint-init", empty.label, initialized.measurement);
    const bound = await h.step01Accepted(
      initialized.result,
      evidence,
      inclusion,
      setup.fraudulentBlockOutRef,
    );
    record("accepted-mint-step01", empty.label, bound.measurement);
    const authenticated = await h.step02(
      bound.result.nextThreadOutRef,
      evidence,
      empty,
    );
    expect(authenticated.result.carriageTier).toBe("Inline");
    recordCarriage("accepted-mint", empty.label, authenticated, 0);
    const proven = await h.step03(
      authenticated.result.nextThreadOutRef,
      evidence,
    );
    expect(proven.result.fraudProofUnit).toBeTruthy();
    record("accepted-mint-step03-proof-mint", empty.label, proven.measurement);
    coverage.reason(REASON_ARM, "accepted_invalid");
    coverage.scenario("wrongful_acceptance_success");
    // Inline carriage exposes the bytes themselves: one flipped byte breaks
    // the field commitment and the door refuses the opening.
    const seamThread = (
      await h.step01Accepted(
        (await h.init(setup.fraudulentBlockOutRef)).result,
        evidence,
        inclusion,
        setup.fraudulentBlockOutRef,
      )
    ).result.nextThreadOutRef;
    await expectOnchainRefusal(() =>
      h.step02Raw(seamThread, evidence, empty, {
        mutateOpening: (opening) => {
          if (!("BodyFieldOpening" in opening))
            throw new Error("body opening expected");
          const carriage = opening.BodyFieldOpening.carriage;
          if (!("Inline" in carriage)) throw new Error("inline expected");
          const bytes = Buffer.from(carriage.Inline.preimage, "hex");
          bytes[bytes.length - 1] = bytes[bytes.length - 1]! ^ 0x01;
          return {
            BodyFieldOpening: {
              ...opening.BodyFieldOpening,
              carriage: { Inline: { preimage: bytes.toString("hex") } },
            },
          };
        },
      }),
    );
    coverage.seamMutated("field_preimage_bytes");
    await h.cancel(seamThread, 1);

    // Honest accepted block on field 5: the 29-byte item is legal.
    const honestEvidence = acceptedEvidence(nonEmpty);
    expect(honestEvidence.decisiveFaultHolds).toBe(false);
    const honest = await h.step02(
      (
        await h.step01Accepted(
          (await h.init(setup.fraudulentBlockOutRef)).result,
          honestEvidence,
          inclusion,
          setup.fraudulentBlockOutRef,
        )
      ).result.nextThreadOutRef,
      honestEvidence,
      nonEmpty,
    );
    await expectOnchainRefusal(() =>
      h.step03Raw(honest.result.nextThreadOutRef),
    );
    coverage.scenario("honest_accepted_block_refusal");
    await h.cancel(honest.result.nextThreadOutRef, 2);

    record(
      "accepted-mint-remove",
      empty.label,
      (await h.removal(setup.headerHash)).measurement,
    );
  }, 600_000);

  it("contradicts a wrongful forced rejection of the maximum legal output field, refusing every forced-door seam first", async () => {
    const shape = forcedMaximumLegalOutputShape();
    await forcedSuccess("forced", shape, 3, async (h) => {
      const { forced, setup, membership, evidence, source } = h;
      const thread = h.threadOf(
        (await h.init(setup.fraudulentBlockOutRef)).result,
      );
      const door = async (
        seam: (typeof AUTHENTICATION_SEAMS)[number],
        finding: FieldItemWidthFinding,
        patch: Partial<typeof source>,
      ) => {
        await expectOnchainRefusal(() =>
          h.step01Forced(thread, finding, { ...source, ...patch }),
        );
        coverage.seamMutated(seam);
      };
      // The leaf rejects item 1; the thread binds item 0 of the same field.
      await door(
        "forced_leaf_reason_coordinate",
        {
          subject: forcedVerdictSubject({
            transactionId: forced.transaction.tx_id,
            sourceKey: membership.key,
            rejectionReason: widthReason(2, 0),
          }),
          fieldIndex: 2,
          itemIndex: 0,
        },
        {},
      );
      coverage.scenario("reason_or_subject_coordinate_mutation");
      await door("forced_leaf_header", evidence, {
        header: { ...forced.header, validationTracesRoot: "ff".repeat(32) },
      });
      await door("forced_leaf_membership", evidence, {
        membership: { ...membership, root: "ee".repeat(32) },
      });
      await door(
        "forced_direction",
        {
          subject: acceptedVerdictSubject(forced.transaction.tx_id),
          fieldIndex: 2,
          itemIndex: 1,
        },
        { direction: 0n },
      );
      await h.cancel(thread, 0);
    });
  }, 600_000);

  it("contradicts a wrongful forced rejection of a non-empty mint-policy item", async () => {
    await forcedSuccess("forced-mint", nonEmptyMintItemShape(), 0);
  }, 600_000);

  it("refuses to contradict an honest forced rejection of an output one byte over the bound", async () => {
    await forcedHonestRefusal(forcedIllegalOutputShape());
  }, 600_000);

  it("refuses to contradict an honest forced rejection of an empty mint-policy item", async () => {
    await forcedHonestRefusal(emptyMintItemShape());
  }, 600_000);

  it("closes the coverage gate and the Van Rossem fit ledger", async () => {
    assertCompleteLifecycleCoverage({
      coverage: coverage.snapshot(),
      expectedReasonArms: [REASON_ARM],
      authenticationSeams: [...AUTHENTICATION_SEAMS],
      cancellablePhysicalSteps: [...CANCELLABLE_STEPS],
      resumable: false,
      hasAdjacentConsensusBound: true,
    });
    const blueprintBytes = readFileSync(realBlueprintPath);
    const preamble = JSON.parse(blueprintBytes.toString("utf8")) as {
      readonly preamble?: { readonly compiler?: { readonly version?: string } };
    };
    const ledger = buildVanRossemFitLedger({
      category: "fieldItemWidthIllegal:00000021:testnet",
      blueprintSha256: createHash("sha256")
        .update(blueprintBytes)
        .digest("hex"),
      compilerVersion: `aiken ${preamble.preamble?.compiler?.version ?? "unknown"}`,
      measurements,
    });
    for (const entry of ledger.entries) {
      expect(entry.signedByteMargin, entry.name).toBeGreaterThan(0);
      expect(BigInt(entry.memoryUnitMargin), entry.name).toBeGreaterThan(0n);
      expect(BigInt(entry.cpuUnitMargin), entry.name).toBeGreaterThan(0n);
      if (entry.kind === "publication")
        expect(
          entry.publicationReserveMargin,
          entry.name,
        ).toBeGreaterThanOrEqual(0);
    }
    if (process.env.MIDGARD_WRITE_FIT_LEDGER === "1") {
      await writeVanRossemFitLedger(ledgerPath, ledger);
      console.info(`[field-item-width-illegal-fit-ledger] wrote ${ledgerPath}`);
    }
    console.info(
      `[field-item-width-illegal-fit-ledger] ${JSON.stringify(ledger.entries)}`,
    );
  });
});
