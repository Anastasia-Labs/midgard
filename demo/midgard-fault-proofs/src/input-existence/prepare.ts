/** The four-stage proof's single event-relative preparation for spend and
 * reference inputs, normal and forced sources, acceptance and rejection. */
import { computeHash32 } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk/core";
import { Data } from "@lucid-evolution/lucid";

import { ledgerBeforeStep, readingLedger } from "../event-ledger.ledger.js";
import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { ledgerEntryLeaf } from "../ledger-at.js";
import { type Ledger, ledgerProof } from "../ledger-store/index.js";
import { eventKeyPhase } from "../transition-trace/reconstruct.transition-trace-reconstruction.js";
import {
  buildEventToStepMembershipProof,
  buildIndexedTraceProof,
} from "../transition-trace/witnesses.build-source-membership-proof.js";
import { deriveInputExistenceSelection } from "./selection.js";
import {
  type OpenedProofThreadSource,
  openProofThreadSubjectFromCanonicalEvidence,
} from "./source.js";

/** Proof material only. The submitter constructs NonMembershipCarriage with
 * actual resolved withdrawal/reference indices; preparation invents none. */
export type PreparedInputExistenceLedgerEvidence =
  | "OutOfDomain"
  | Readonly<{
      Present: Readonly<{
        value: string;
        value_hash: string;
        proof: SDK.Proof;
      }>;
    }>
  | Readonly<{ Absent: Readonly<{ proof: SDK.Proof }> }>;

export type PreparedInputExistence = Readonly<{
  field: 0 | 1;
  source: OpenedProofThreadSource;
  eventMembership: SDK.EventToStepMembershipProof;
  transitionMembership: SDK.IndexedTraceProof;
  ledgerEvidence: PreparedInputExistenceLedgerEvidence;
}> &
  ReturnType<typeof deriveInputExistenceSelection>;

export const prepareInputExistence = async ({
  block,
  eventKey,
  field,
  inputIndex,
  predecessor,
  owner,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly eventKey: SDK.EventKey;
  readonly field: 0 | 1;
  readonly inputIndex?: bigint;
  /** The ledger the block starts from; none when it starts from genesis. */
  readonly predecessor?: Ledger;
  readonly owner: string;
}): Promise<PreparedInputExistence> => {
  const source = await openProofThreadSubjectFromCanonicalEvidence({
    block,
    eventKey,
  });
  const selection = deriveInputExistenceSelection({
    source,
    field,
    inputIndex,
    owner,
  });
  const { inputIndex: selectedIndex, inputItems, selectedInput } = selection;
  const selectedItem =
    selectedInput === null ? undefined : inputItems[Number(selectedIndex)];
  const eventMembership = await buildEventToStepMembershipProof({
    reconstruction: block.reconstruction,
    eventKey,
  });
  const transitionMembership = await buildIndexedTraceProof({
    reconstruction: block.reconstruction,
    stepIndex: eventMembership.value.step_index,
  });
  const step = transitionMembership.value;
  if (
    eventMembership.value.phase !== eventKeyPhase(eventKey) ||
    step.phase !== eventKeyPhase(eventKey) ||
    step.schema_version !== 1n ||
    step.step_index !== transitionMembership.key ||
    step.step_index !== eventMembership.value.step_index ||
    Data.to(step.event_key, SDK.EventKey) !== Data.to(eventKey, SDK.EventKey)
  ) {
    throw new Error("input-existence: event/step identity changed");
  }
  // Replay even for an impossible WR index: absent ledger evidence cannot
  // waive the event/root linkage or the checked predecessor authority.
  const ledger = await ledgerBeforeStep({
    block,
    eventKey,
    stepIndex: step.step_index,
    predecessor,
  });
  const ledgerEvidence: PreparedInputExistenceLedgerEvidence =
    selectedItem === undefined
      ? "OutOfDomain"
      : await readingLedger("input-existence ledger evidence", async () => {
          const output = (await ledger.read([selectedItem])).get(
            selectedItem.toString("hex"),
          )?.output;
          // Membership when the ledger holds the input, non-membership when
          // it does not: the proof `output` calls for.
          const proof = await ledgerProof(ledger, selectedItem);
          if (output === undefined) return { Absent: { proof } };
          const { leaf } = ledgerEntryLeaf(
            { outRef: selectedItem, output },
            "input-existence ledger evidence",
          );
          return {
            Present: {
              value: leaf.toString("hex"),
              value_hash: computeHash32(leaf).toString("hex"),
              proof,
            },
          };
        });
  return {
    field,
    source,
    ...selection,
    eventMembership,
    transitionMembership,
    ledgerEvidence,
  };
};
