import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { nativeScriptDecodingPriorLedger } from "../native-script-decoding/replay.js";
import {
  type CountedRoot,
  keyValuePhasNonMembershipProof,
  keyValuePhasProof,
} from "../transition-trace/phas.js";
import {
  type DecodedRootEntry,
  eventKeyFingerprint,
  type TransitionTraceReconstruction,
} from "../transition-trace/reconstruct.js";
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
  predecessor?: TransitionTraceReconstruction;
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
  const ledger = await nativeScriptDecodingPriorLedger(
    current,
    mapping.value.step_index,
    predecessor,
  );
  if (ledger.root.root !== step.value.pre_utxos_root)
    throw new Error("withdrawalMistag prior ledger differs from event root");
  const key = encodeMidgardSpendInputItem({
    txId: Buffer.from(entry.value.body.l2_outref.transactionId, "hex"),
    outputIndex: Number(entry.value.body.l2_outref.outputIndex),
  });
  const output = ledger.outputs.get(key.toString("hex"));
  const descriptor = ledger.root.entries.find((e) => e.key.equals(key));
  const ledgerEvidence =
    output === undefined
      ? {
          AbsentLedgerOutput: {
            non_membership_proof: await keyValuePhasNonMembershipProof(
              ledger.root,
              key,
            ),
          },
        }
      : {
          PresentLedgerOutput: {
            output_cbor: output.value.toString("hex"),
            membership_proof: await keyValuePhasProof(
              ledger.root,
              key,
              descriptor!.value,
            ),
          },
        };
  try {
    return await prepareWithdrawalMistag({
      challengedHeaderHash: current.headerHash,
      committedWithdrawal: await membership(
        current.rootData.withdrawals,
        entry,
        "WithdrawalsRootDomain",
      ),
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
  predecessor?: CanonicalBlockEvidence;
}) => {
  const findings = [];
  for (const index of block.reconstruction.withdrawals.keys()) {
    const prepared = await prepareWithdrawalMistagReplay({
      current: block.reconstruction,
      predecessor: predecessor?.reconstruction,
      index,
    });
    if (prepared !== null)
      findings.push({
        violationId: SDK.WITHDRAWAL_MISTAG_VIOLATION_ID,
        headerHash: block.headerHash,
        detectionId: withdrawalMistagDetectionId(index),
        position: BigInt(index),
        index,
        prepared,
      });
  }
  return findings;
};
