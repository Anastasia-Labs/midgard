import "./missing-redeemer-lifecycle.make-stage.js";

import { selectMidgardFieldCarriageTier } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { type MissingRedeemerPurposeKind } from "../src/missing-redeemer/family.js";
import { buildMissingRedeemerMaterialFromRetainedDa } from "../src/missing-redeemer/replay.js";
import { PURPOSE_KINDS } from "./missing-redeemer-lifecycle.make-harness.js";
import {
  buildMissingRedeemerFixture,
  MISSING_REDEEMER_MAXIMUM_FIELD_BYTES,
  MISSING_REDEEMER_MAXIMUM_SHAPE_ITEM_COUNT,
  type MissingRedeemerFixtureShape,
} from "./support/missing-redeemer-emulator.js";

export const shapeFor = (
  direction: "accepted" | "forced",
  purposeKind: MissingRedeemerPurposeKind,
): MissingRedeemerFixtureShape => ({
  direction,
  purposeKind,
  // Both source locations in both directions: half the kinds keep their
  // script inline and the other half resolve it from a reference input,
  // swapped between directions; the MidgardV1 receive script is inline by
  // construction.
  sourceLocation:
    purposeKind === 3
      ? "inline"
      : (direction === "accepted") === (purposeKind % 2 === 0)
        ? "reference"
        : "inline",
});

describe("missingRedeemer concrete retained lifecycle material", () => {
  it.each(PURPOSE_KINDS)(
    "derives the accepted absence and forced presence for purpose kind %d",
    async (purposeKind) => {
      const accepted = await buildMissingRedeemerFixture(
        shapeFor("accepted", purposeKind),
      );
      expect(accepted.material.evidence.redeemerMissing).toBe(true);
      expect(accepted.material.evidence.purposeKind).toBe(purposeKind);
      expect(accepted.material.evidence.purpose.sourceLanguageTag).toBe(
        purposeKind === 3 ? 128 : 3,
      );
      expect(accepted.material.evidence.purpose.source).toBe(
        accepted.shape.sourceLocation === "inline"
          ? "witness"
          : "resolved-reference",
      );
      expect(accepted.material.authentication.control.stage).toBe(10n);
      const forced = await buildMissingRedeemerFixture(
        shapeFor("forced", purposeKind),
      );
      expect(forced.material.evidence.redeemerMissing).toBe(false);
      expect(forced.material.evidence.purpose.source).toBe(
        forced.shape.sourceLocation === "inline"
          ? "witness"
          : "resolved-reference",
      );
      expect(forced.material.authentication.control.stage).toBe(10n);
      expect(
        forced.material.authentication.control.discovery.current_purpose_kind,
      ).toBe(BigInt(purposeKind));
    },
    120_000,
  );

  it("builds the exact maximum certified field and refuses the adjacent over-bound field", async () => {
    const maximum = await buildMissingRedeemerFixture({
      ...shapeFor("accepted", 0),
      decoyRedeemers: MISSING_REDEEMER_MAXIMUM_SHAPE_ITEM_COUNT,
      fieldBytes: MISSING_REDEEMER_MAXIMUM_FIELD_BYTES,
    });
    expect(maximum.fieldBytes).toBe(MISSING_REDEEMER_MAXIMUM_FIELD_BYTES);
    expect(maximum.material.evidence.carriage).toBe("Certified");
    expect(maximum.material.evidence.itemCount).toBe(
      MISSING_REDEEMER_MAXIMUM_SHAPE_ITEM_COUNT,
    );
    expect(
      maximum.material.evidence.checkpoints.map(({ cursor }) => cursor),
    ).toEqual([16, 17]);
    expect(() =>
      selectMidgardFieldCarriageTier(MISSING_REDEEMER_MAXIMUM_FIELD_BYTES + 1),
    ).toThrow(/aggregate bound/u);
    await expect(
      buildMissingRedeemerFixture({
        ...shapeFor("accepted", 0),
        decoyRedeemers: MISSING_REDEEMER_MAXIMUM_SHAPE_ITEM_COUNT,
        fieldBytes: MISSING_REDEEMER_MAXIMUM_FIELD_BYTES + 1,
      }),
    ).rejects.toThrow();
  }, 180_000);

  it("refuses a substituted trace root and an omitted purpose witness", async () => {
    const fixture = await buildMissingRedeemerFixture(shapeFor("accepted", 1));
    const rebuild = (
      overrides: Partial<
        Parameters<typeof buildMissingRedeemerMaterialFromRetainedDa>[0]
      >,
    ) =>
      buildMissingRedeemerMaterialFromRetainedDa({
        eventKey: fixture.eventKey,
        subject: fixture.subject,
        purposeKind: 1,
        purposeIndex: 0,
        txCbor: fixture.transaction.txCbor,
        authenticatedValidationTraceEntries: fixture.descriptorEntries,
        retainedValidationWitnessEntries: fixture.retainedEntries,
        expectedValidationTracesRoot: fixture.validationTracesRoot,
        ...overrides,
      });
    await expect(
      rebuild({ expectedValidationTracesRoot: "ff".repeat(32) }),
    ).rejects.toThrow(/validation root changed/u);
    const withoutPurpose = fixture.retainedEntries.filter(({ value }) => {
      const auxiliary = SDK.decodeRetainedValidationWitness(value).auxiliary;
      return !(
        typeof auxiliary === "object" && "ScriptPurposeScanWitness" in auxiliary
      );
    });
    await expect(
      rebuild({ retainedValidationWitnessEntries: withoutPurpose }),
    ).rejects.toThrow(/purpose membership witness/u);
    await expect(rebuild({ purposeIndex: 1 })).rejects.toThrow(
      /purpose selection is absent/u,
    );
  }, 120_000);
});
