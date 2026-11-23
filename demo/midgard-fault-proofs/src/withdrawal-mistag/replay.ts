import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk/core";

import {
  ledgerBeforeTraceStep,
  readingLedger,
} from "../event-ledger.ledger.js";
import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { type Ledger, ledgerProof } from "../ledger-store/index.js";
import {
  type CountedRoot,
  keyValuePhasProof,
} from "../transition-trace/phas.js";
import {
  type DecodedRootEntry,
  eventKeyFingerprint,
  type TransitionTraceReconstruction,
} from "../transition-trace/reconstruct.transition-trace-reconstruction.js";
import { withdrawalSubject } from "../workflow/detection-subject.js";
import { collectReplayFindings } from "../workflow/replay-prerequisite.js";
import { prepareWithdrawalMistag } from "./prepare-withdrawal-mistag.js";

const membership = async <K, V>(
  root: CountedRoot,
  entry: DecodedRootEntry<K, V>,
  domain: SDK.RootDomain,
): Promise<SDK.RootMembershipProof<K, V>> => ({
  root: root.root,
  phas_root: root.phasRoot,
  count: root.count,
  domain,
  key: entry.key,
  value: entry.value,
  proof: await keyValuePhasProof(
    { ...root, root: root.phasRoot },
    entry.keyBytes,
    entry.valueBytes,
  ),
});

export const withdrawalMistagDetectionId = (index: number) =>
  `withdrawal-mistag:${index}`;

export const prepareWithdrawalMistagReplay = async ({
  current,
  predecessor,
  index,
}: {
  current: TransitionTraceReconstruction;
  /** The ledger the block starts from; none when it starts from genesis. */
  predecessor?: Ledger;
  index: number;
}) => {
  if (!Number.isSafeInteger(index) || index < 0)
    throw new Error("withdrawalMistag invalid index");
  const entry = current.withdrawals[index];
  if (entry === undefined)
    throw new Error("withdrawalMistag source index is absent");
  const fingerprint = eventKeyFingerprint({
    WithdrawalEventKey: { withdrawal_id: entry.key },
  });
  const mapping = current.eventToStepByFingerprint.get(fingerprint);
  const step =
    mapping === undefined
      ? undefined
      : current.traceByStepIndex.get(mapping.value.step_index);
  if (mapping === undefined || step === undefined)
    throw new Error("withdrawalMistag authenticated event mapping is absent");
  const ledger = await ledgerBeforeTraceStep(
    current,
    mapping.value.step_index,
    predecessor,
  );
  const key = encodeMidgardSpendInputItem({
    txId: Buffer.from(entry.value.body.l2_outref.transactionId, "hex"),
    outputIndex: Number(entry.value.body.l2_outref.outputIndex),
  });
  const ledgerEvidence = await readingLedger(
    "withdrawalMistag ledger evidence",
    async () => {
      const output = (await ledger.read([key])).get(
        key.toString("hex"),
      )?.output;
      // Membership when the ledger holds the out-ref, non-membership when
      // it does not: the proof `output` calls for.
      const proof = await ledgerProof(ledger, key);
      return output === undefined
        ? { AbsentLedgerOutput: { non_membership_proof: proof } }
        : {
            PresentLedgerOutput: {
              output_cbor: output.toString("hex"),
              membership_proof: proof,
            },
          };
    },
  );
  try {
    return await prepareWithdrawalMistag({
      challengedHeaderHash: current.headerHash,
      committedWithdrawal: await membership(
        current.rootData.withdrawals,
        entry,
        "WithdrawalsRootDomain",
      ),
      committedWithdrawalInfoCbor: entry.valueBytes.toString("hex"),
      eventToStep: await membership(
        current.rootData.eventToStep,
        mapping,
        "EventToStepRootDomain",
      ),
      transitionStep: await membership(
        current.rootData.transitionTrace,
        step,
        "TransitionTraceRootDomain",
      ),
      ledgerEvidence,
    });
  } catch (error) {
    if (
      error instanceof Error &&
      error.message === "withdrawal-mistag evidence is honestly tagged"
    )
      return null;
    throw error;
  }
};

export const detectWithdrawalMistagReplay = async ({
  block,
  predecessor,
}: {
  block: CanonicalBlockEvidence;
  predecessor?: Ledger;
}) =>
  await collectReplayFindings(
    block.reconstruction.withdrawals,
    async (_, index) => {
      const prepared = await prepareWithdrawalMistagReplay({
        current: block.reconstruction,
        predecessor,
        index,
      });
      return prepared === null
        ? null
        : {
            ...withdrawalSubject(block.reconstruction.withdrawals[index]!.key),
            violationId: SDK.WITHDRAWAL_MISTAG_VIOLATION_ID,
            headerHash: block.headerHash,
            detectionId: withdrawalMistagDetectionId(index),
            position: BigInt(index),
            index,
            prepared,
          };
    },
  );
