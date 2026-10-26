/** Real-contract emulator fixtures for the `receivePurposeLanguage` family:
 * the harness, the 21 published role scripts, and a driver from a genuine
 * Init through step-01 and the planned custody to the terminal. Subjects are
 * the direct purpose subjects of `missing-redeemer-emulator.subject.ts`. */
import * as SDK from "@al-ft/midgard-sdk";

import type { DirectPurposeRole } from "../../src/direct-purpose-family/index.js";
import {
  prepareReceivePurposeLanguageProof,
  RECEIVE_PURPOSE_LANGUAGE_FAMILY,
  type ReceivePurposeLanguageProofPlan,
} from "../../src/receive-purpose-language/index.js";
import {
  type DirectPurposeHarness,
  driveDirectPurposeThread,
  publishDirectPurposeRoles,
  type RoleReference,
} from "./direct-purpose-emulator.js";
import {
  honestForcedObserverShape,
  missingRedeemerLedgerEntries,
  type MissingRedeemerScenario,
  type MissingRedeemerSubjectShape,
} from "./missing-redeemer-emulator.subject.js";
import {
  makeFaultProofEmulatorHarness,
  registerChunkedVerifyRewardAccount,
} from "./submit-init-emulator-shared.js";

export {
  DIRECT_PURPOSE_STEP_BOUNDS as RECEIVE_PURPOSE_LANGUAGE_STEP_BOUNDS,
  type RoleReference,
  type StepMeasurement,
  stepTable,
} from "./direct-purpose-emulator.js";

/** The emulator harness with the family's real contracts. */
export const makeReceivePurposeLanguageHarness = async () => {
  const harness = await makeFaultProofEmulatorHarness({
    contractOptions: {
      realReceivePurposeLanguage: true,
      alwaysFraudProofCatalogue: true,
    },
    registerAdditionalRewardAccounts: registerChunkedVerifyRewardAccount,
  });
  const direct = harness.contracts.receivePurposeLanguage;
  const category = harness.catalogue.categories.receivePurposeLanguage;
  if (direct === undefined || category === undefined)
    throw new Error(
      "receivePurposeLanguage harness contracts/category omitted",
    );
  if (
    category.categoryId !==
    SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.receivePurposeLanguage
  )
    throw new Error("unexpected receivePurposeLanguage category id");
  return { ...harness, direct, category, receivePurposeLanguage: direct };
};

export type ReceivePurposeLanguageHarness = DirectPurposeHarness & {
  readonly receivePurposeLanguage: DirectPurposeHarness["direct"];
};

/** Publishes every role script once through the signed publication path,
 * which refuses any publication above 15,872 bytes. */
export const publishReceivePurposeLanguageRoles = (
  harness: DirectPurposeHarness,
) => publishDirectPurposeRoles(harness, RECEIVE_PURPOSE_LANGUAGE_FAMILY);

/** Genuine Init, the source-appropriate step-01, and the planned custody up
 * to the terminal's input. Every thread transaction is measured. */
export const driveReceivePurposeLanguageThread = ({
  harness,
  scenario,
  references,
  observed,
  transitions,
}: {
  readonly harness: DirectPurposeHarness;
  readonly scenario: MissingRedeemerScenario;
  readonly references: ReadonlyMap<DirectPurposeRole, RoleReference>;
  /** The receive purpose to observe; the first convicting one otherwise. */
  readonly observed?: SDK.DirectPurposeCoordinate;
  /** Replaces the planned custody (e.g. a prefix for refusal rows). */
  readonly transitions?: (
    prepared: ReceivePurposeLanguageProofPlan,
  ) => ReceivePurposeLanguageProofPlan["transitions"];
}) =>
  driveDirectPurposeThread({
    harness,
    family: RECEIVE_PURPOSE_LANGUAGE_FAMILY,
    scenario,
    references,
    prepare: (args) =>
      prepareReceivePurposeLanguageProof({
        ...args,
        ...(observed === undefined ? {} : { observed }),
      }),
    ...(transitions === undefined ? {} : { transitions }),
  });

/** A forced rejection recording `ReceivePurposePlutusV3Forbidden`. */
export const receivePurposeLanguageVerdict = (
  purposeKind: bigint,
  purposeIndex: bigint,
): SDK.OperatorVerdict => ({
  ForcedTxInvalid: {
    reason: {
      ReceivePurposePlutusV3Forbidden: {
        purpose_kind: purposeKind,
        purpose_index: purposeIndex,
      },
    },
  },
});

/** Receive purpose `index`. */
export const receiveCoordinate = (
  index: bigint,
): SDK.DirectPurposeCoordinate => ({ kind: 3n, index });

/** A forced transaction the admission screen accepts: the honest signed
 * spend beside one receive purpose paying `script`, with no redeemer. A
 * Plutus `script` is never carried as a witness: `sourced` puts it in a
 * reference input, and without it the purpose has no source. A native
 * `script` is carried as its witness. */
export const forcedReceiving = (
  script: MissingRedeemerSubjectShape["observers"][number]["script"],
  { sourced = true }: { readonly sourced?: boolean } = {},
): MissingRedeemerSubjectShape => {
  const native = script.language === "NativeCardano";
  return {
    ...honestForcedObserverShape(5),
    observers: [],
    receives: [{ script, witness: native, redeemer: false }],
    reference:
      sourced && !native ? missingRedeemerLedgerEntries(0x31, [script]) : [],
  };
};
