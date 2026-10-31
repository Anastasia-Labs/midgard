/** The four-stage proof's single event-relative preparation for spend and
 * reference inputs, normal and forced sources, acceptance and rejection. */
import { computeHash32 } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { CheckedLedger } from "../ledger-at.js";
import {
  keyValuePhasNonMembershipProof,
  keyValuePhasProof,
} from "../transition-trace/phas.js";
import { eventKeyPhase } from "../transition-trace/reconstruct.js";
import {
  buildEventToStepMembershipProof,
  buildIndexedTraceProof,
} from "../transition-trace/witnesses.js";
import { ledgerBeforeTransactionEvent } from "./event-ledger.js";
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
  readonly predecessor?: CheckedLedger;
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
  const ledger = await ledgerBeforeTransactionEvent({
    block,
    eventKey,
    stepIndex: step.step_index,
    predecessor,
  });
  let ledgerEvidence: PreparedInputExistenceLedgerEvidence;
  if (selectedItem === undefined) {
    ledgerEvidence = "OutOfDomain";
  } else {
    const existing = ledger.trie.entries.find(({ key }) =>
      key.equals(selectedItem),
    );
    if (existing !== undefined) {
      ledgerEvidence = {
        Present: {
          value: existing.value.toString("hex"),
          value_hash: computeHash32(existing.value).toString("hex"),
          proof: await keyValuePhasProof(
            ledger.trie,
            selectedItem,
            existing.value,
          ),
        },
      };
    } else {
      ledgerEvidence = {
        Absent: {
          proof: await keyValuePhasNonMembershipProof(
            ledger.trie,
            selectedItem,
          ),
        },
      };
    }
  }
  return {
    field,
    source,
    ...selection,
    eventMembership,
    transitionMembership,
    ledgerEvidence,
  };
};
