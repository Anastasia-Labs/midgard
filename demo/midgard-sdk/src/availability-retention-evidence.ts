import {
  CML,
  coreToUtxo,
  Data,
  type UTxO,
  utxoToCore,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { hashBlockHeader, StateQueueNode } from "./ledger-state.js";
import {
  getLinkedListNodeViewFromUTxO,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
} from "./linked-list.js";
import { StateQueueRedeemer } from "./state-queue.js";
import {
  parseStateQueueAuthenticatedTransition,
  type StateQueueAuthenticatedTransition,
} from "./state-queue-correction-transition.js";

export type DaAvailabilityRetentionEvidence = Readonly<{
  version: 1;
  deploymentIdentityDigest: string;
  stateQueuePolicyId: string;
  availabilityPolicyId: string;
  headerHash: string;
  blockEndTimeMs: string;
  terminalTransitionDigest: string;
  kind: "published" | "timed_out";
  removedQueueInputCbor: string;
}>;
export type DaAvailabilityRetentionAuthority = Readonly<{
  deploymentIdentityDigest: string;
  stateQueuePolicyId: string;
  stateQueueAddress: string;
  availabilityPolicyId: string;
  minimumFinalityDepth: bigint;
}>;

/**
 * Called only after the local chain source authenticates a finalized transition
 * and its consumed output. Published is irreversible: the deployed close arm
 * burns the bond/challenge, and opening accepts only Attested. A final unavailable
 * removal binds the same challenge through the authenticated correction lock;
 * its Idle entry burns the challenge and Locked continuations preserve identity.
 * Queue absence and generic merge/removal are never sufficient on their own.
 */
export const deriveDaAvailabilityRetentionEvidence = (
  transitionInput: unknown,
  removedQueueInput: UTxO,
  authority: DaAvailabilityRetentionAuthority,
): DaAvailabilityRetentionEvidence | null => {
  try {
    const transition = parseStateQueueAuthenticatedTransition(transitionInput);
    if (
      !transition ||
      !/^[0-9a-f]{56}$/u.test(authority.availabilityPolicyId) ||
      authority.minimumFinalityDepth <= 0n ||
      transition.deploymentIdentityDigest !==
        authority.deploymentIdentityDigest ||
      transition.stateQueuePolicyId !== authority.stateQueuePolicyId ||
      BigInt(transition.finalityDepth) < authority.minimumFinalityDepth ||
      transition.removedHeaderHashes.length !== 1
    )
      return null;
    const headerHash = transition.removedHeaderHashes[0]!;
    const reference = `${removedQueueInput.txHash}#${removedQueueInput.outputIndex}`;
    const unit =
      authority.stateQueuePolicyId +
      STATE_QUEUE_NODE_ASSET_NAME_PREFIX +
      headerHash;
    if (
      removedQueueInput.address !== authority.stateQueueAddress ||
      removedQueueInput.scriptRef != null ||
      removedQueueInput.assets[unit] !== 1n ||
      Object.entries(removedQueueInput.assets).some(
        ([asset, amount]) =>
          asset !== "lovelace" && (asset !== unit || amount !== 1n),
      ) ||
      transition.previousQueue.find((node) => node.headerHash === headerHash)
        ?.outRef !== reference ||
      !transition.consumedQueueOutRefs.includes(reference)
    )
      return null;
    const linked = Effect.runSync(
      getLinkedListNodeViewFromUTxO(removedQueueInput),
    );
    const node = Data.castFrom(linked.data, StateQueueNode);
    if (
      linked.key === "Empty" ||
      linked.key.Key.key !== headerHash ||
      Effect.runSync(hashBlockHeader(node.header)) !== headerHash
    )
      return null;
    const status = node.da_attestation;
    let kind: DaAvailabilityRetentionEvidence["kind"];
    if (typeof status === "object" && "Published" in status) {
      if (!/^[0-9a-f]{64}$/u.test(status.Published.terminal_commitment))
        return null;
      kind = "published";
    } else if (typeof status === "object" && "Challenged" in status) {
      const lock = transition.correctionLockWitness;
      const decoded = Data.from(
        transition.stateQueueMintRedeemer.cborHex,
        StateQueueRedeemer,
      );
      if (
        typeof decoded !== "object" ||
        !("RemoveUnavailableBlockAfterTimeout" in decoded) ||
        !(
          "RemoveTimedOutHead" in
          decoded.RemoveUnavailableBlockAfterTimeout.removal_approach
        ) ||
        decoded.RemoveUnavailableBlockAfterTimeout.unavailable_header_hash !==
          headerHash ||
        decoded.RemoveUnavailableBlockAfterTimeout.challenge_asset_name !==
          status.Challenged.challenge_asset_name ||
        lock.kind !== "correction_transition" ||
        lock.targetHeaderHash !== headerHash ||
        lock.nextDatum !== "Idle" ||
        typeof lock.correctionIdentity !== "object" ||
        !("AvailabilityChallenge" in lock.correctionIdentity) ||
        lock.correctionIdentity.AvailabilityChallenge.challenge_asset_name !==
          status.Challenged.challenge_asset_name
      )
        return null;
      kind = "timed_out";
    } else return null;
    return Object.freeze({
      version: 1,
      deploymentIdentityDigest: authority.deploymentIdentityDigest,
      stateQueuePolicyId: authority.stateQueuePolicyId,
      availabilityPolicyId: authority.availabilityPolicyId,
      headerHash,
      blockEndTimeMs: node.header.endTime.toString(),
      terminalTransitionDigest: transition.transitionDigest,
      kind,
      removedQueueInputCbor: utxoToCore(removedQueueInput).to_cbor_hex(),
    });
  } catch {
    return null;
  }
};

/** Revalidates durable evidence against its exact authenticated terminal record. */
export const parseDaAvailabilityRetentionEvidence = (
  input: unknown,
  transition: StateQueueAuthenticatedTransition,
  authority: DaAvailabilityRetentionAuthority,
): DaAvailabilityRetentionEvidence | null => {
  try {
    if (typeof input !== "object" || input === null || Array.isArray(input))
      return null;
    const keys = [
      "version",
      "deploymentIdentityDigest",
      "stateQueuePolicyId",
      "availabilityPolicyId",
      "headerHash",
      "blockEndTimeMs",
      "terminalTransitionDigest",
      "kind",
      "removedQueueInputCbor",
    ];
    if (
      Reflect.ownKeys(input).length !== keys.length ||
      Reflect.ownKeys(input).some(
        (key) => typeof key !== "string" || !keys.includes(key),
      )
    )
      return null;
    const record = input as Record<string, unknown>;
    if (
      record.version !== 1 ||
      typeof record.removedQueueInputCbor !== "string" ||
      !/^(?:[0-9a-f]{2})+$/u.test(record.removedQueueInputCbor)
    )
      return null;
    const parsed = deriveDaAvailabilityRetentionEvidence(
      transition,
      coreToUtxo(
        CML.TransactionUnspentOutput.from_cbor_hex(
          record.removedQueueInputCbor,
        ),
      ),
      authority,
    );
    return parsed &&
      keys.every((key) => Reflect.get(parsed, key) === record[key])
      ? parsed
      : null;
  } catch {
    return null;
  }
};
