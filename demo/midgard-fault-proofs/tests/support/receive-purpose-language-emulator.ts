/** The `receivePurposeLanguage` family's emulator driver: the walk-keyed
 * driver with the receive purpose to observe, the verdict its forced rows
 * record, and its receiving subjects. Subjects are the direct purpose
 * subjects of `missing-redeemer-emulator.subject.ts`. */
import type * as SDK from "@al-ft/midgard-sdk";

import type { DirectPurposeRole } from "../../src/direct-purpose-family/index.js";
import {
  prepareReceivePurposeLanguageProof,
  RECEIVE_PURPOSE_LANGUAGE_DIRECT_WALK,
  type ReceivePurposeLanguageProofPlan,
} from "../../src/receive-purpose-language/index.js";
import {
  type DirectWalkHarness,
  driveDirectWalkThread,
  type RoleReference,
} from "./direct-walk-emulator.js";
import {
  honestForcedObserverShape,
  missingRedeemerLedgerEntries,
  type MissingRedeemerScenario,
  type MissingRedeemerSubjectShape,
} from "./missing-redeemer-emulator.subject.js";

/** Genuine Init, the source-appropriate step-01, and the planned custody up
 * to the terminal's input. Every thread transaction is measured. */
export const driveReceivePurposeLanguageThread = ({
  harness,
  scenario,
  references,
  observed,
  transitions,
}: {
  readonly harness: DirectWalkHarness;
  readonly scenario: MissingRedeemerScenario;
  readonly references: ReadonlyMap<DirectPurposeRole, RoleReference>;
  /** The receive purpose to observe; the first convicting one otherwise. */
  readonly observed?: SDK.DirectPurposeCoordinate;
  /** Replaces the planned custody (e.g. a prefix for refusal rows). */
  readonly transitions?: (
    prepared: ReceivePurposeLanguageProofPlan,
  ) => ReceivePurposeLanguageProofPlan["transitions"];
}) =>
  driveDirectWalkThread({
    harness,
    walk: RECEIVE_PURPOSE_LANGUAGE_DIRECT_WALK,
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
