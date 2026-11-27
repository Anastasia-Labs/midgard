/** Which cancel the family application registry offers a thread of ours
 * with no header in hand: every registered family names its
 * computation-thread minting script, so a thread at any of its cancellable
 * roles is cancellable; a validation-trace thread past the opener is not,
 * and is named so; a thread no registered family or script names is
 * unavailable. */
import { FRAUD_PROOF_CATALOGUE_CATEGORY_IDS } from "@al-ft/midgard-sdk/core";
import { describe, expect, it } from "vitest";

import { PAST_THE_OPENER } from "../src/validation-trace-dispute/own-thread-reclaim.js";
import { VALIDATION_TRACE_DISPUTE_REFERENCE_SCRIPTS } from "../src/validation-trace-dispute/workflow.js";
import { FAMILY_APPLICATION_REGISTRY } from "../src/workflow/family-application-registry.family-application-registry.js";
import {
  FAMILY_OWN_THREAD_CANCELS,
  ownThreadCategory,
  planOwnThreadCancel,
} from "../src/workflow/family-application-registry.own-thread-cancel.js";
import type { InstalledFraudProofCategory } from "../src/workflow/installed-categories.js";

const CATEGORIES = Object.keys(
  FAMILY_APPLICATION_REGISTRY,
) as InstalledFraudProofCategory[];

/** Distinct script hashes for every contract a roster names. */
const hashesOf = (
  roster: Readonly<Record<string, string>>,
): Record<string, string> =>
  Object.fromEntries(
    Object.values(roster).map((contractName, index) => [
      contractName,
      index.toString(16).padStart(56, "0"),
    ]),
  );

describe("own-thread cancel plans from the family application registry", () => {
  it("offers one cancel per registered family, keyed by its catalogue id", () => {
    expect(Object.keys(FAMILY_OWN_THREAD_CANCELS).sort()).toEqual(
      [...CATEGORIES].sort(),
    );
    for (const category of CATEGORIES) {
      expect(
        ownThreadCategory(FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[category]),
      ).toBe(category);
      expect(FAMILY_APPLICATION_REGISTRY[category].roster).toHaveProperty(
        "computationThreadMint",
      );
    }
  });

  it("plans a cancel at every role of every family but validation-trace's past the opener", () => {
    for (const category of CATEGORIES) {
      const roster: Readonly<Record<string, string>> =
        FAMILY_APPLICATION_REGISTRY[category].roster;
      const hashes = hashesOf(roster);
      const categoryId = FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[category];
      const stepRoles = Object.keys(roster).filter((role) =>
        /^step\d+$/.test(role),
      );
      for (const role of stepRoles)
        expect(
          planOwnThreadCancel(
            { categoryId, paymentScriptHash: hashes[roster[role]!]! },
            hashes,
          ),
          `${category} ${role}`,
        ).toEqual({
          kind: "cancellable",
          category,
          role,
          contractName: roster[role],
        });
    }
  });

  it("names a validation-trace thread past the opener uncancellable", () => {
    const category = "validationTraceDispute";
    const roster: Readonly<Record<string, string>> =
      FAMILY_APPLICATION_REGISTRY[category].roster;
    const hashes = hashesOf(roster);
    const categoryId = FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[category];
    const at = (
      role: keyof typeof VALIDATION_TRACE_DISPUTE_REFERENCE_SCRIPTS,
    ) =>
      planOwnThreadCancel(
        {
          categoryId,
          paymentScriptHash:
            hashes[VALIDATION_TRACE_DISPUTE_REFERENCE_SCRIPTS[role]]!,
        },
        hashes,
      );
    expect(at("opener")).toMatchObject({ kind: "cancellable", role: "opener" });
    for (const role of [
      "source",
      "case",
      "bond",
      "game",
      "timeout",
      "award",
    ] as const)
      expect(at(role), role).toEqual({
        kind: "uncancellable",
        category,
        reason: PAST_THE_OPENER,
      });
  });

  it("is unavailable for a catalogue id or script no registered family names", () => {
    const roster: Readonly<Record<string, string>> =
      FAMILY_APPLICATION_REGISTRY.doubleSpend.roster;
    const hashes = hashesOf(roster);
    expect(
      planOwnThreadCancel(
        { categoryId: "ffffffff", paymentScriptHash: "00".repeat(28) },
        hashes,
      ),
    ).toMatchObject({ kind: "unavailable" });
    expect(
      planOwnThreadCancel(
        {
          categoryId: FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.doubleSpend,
          paymentScriptHash: "ee".repeat(28),
        },
        hashes,
      ),
    ).toMatchObject({ kind: "unavailable" });
    // The minting policy is no step a thread stands at.
    expect(
      planOwnThreadCancel(
        {
          categoryId: FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.doubleSpend,
          paymentScriptHash: hashes[roster.computationThreadMint!]!,
        },
        hashes,
      ),
    ).toMatchObject({ kind: "unavailable" });
  });

  it("refuses a thread whose catalogue id names another family", async () => {
    const roster: Readonly<Record<string, string>> =
      FAMILY_APPLICATION_REGISTRY.doubleSpend.roster;
    const hashes = hashesOf(roster);
    const build = await FAMILY_OWN_THREAD_CANCELS.minAda({
      subject: {
        threadOutRef: `${"aa".repeat(32)}#0`,
        categoryId: FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.doubleSpend,
        paymentScriptHash: hashes[roster.step01!]!,
      },
      deployment: {
        contractScriptHashes: hashes,
        resolveReferenceScript: async () => {
          throw new Error("not reached");
        },
      },
      validTo: 0,
    } as unknown as Parameters<
      (typeof FAMILY_OWN_THREAD_CANCELS)["doubleSpend"]
    >[0]);
    expect(build).toMatchObject({
      kind: "unavailable",
      reason: expect.stringContaining("names doubleSpend"),
    });
  });
});
