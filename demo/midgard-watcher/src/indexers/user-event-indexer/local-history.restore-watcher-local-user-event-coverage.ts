import {
  admitFraudProofRawL1Point,
  computeFraudProofRawL1PointId,
} from "@al-ft/midgard-fault-proofs";

import {
  readWatcherLocalBackfillFinalityObservation,
  readWatcherLocalBackfillFinalityOriginalWitness,
} from "../../l1/finality-engine.js";
import { readWatcherUserEventScriptBinding } from "../../runtime/deployment-identity.js";
import { type WatcherUserEventOriginFacts } from ".././user-event-origin.js";
import {
  admitWatcherLocalBackfillUserEventReferenceEvidence,
  readWatcherUserEventReferenceEvidence,
} from ".././user-event-reference-authority.js";
import {
  localCoverageHead,
  localCoverageOnHead,
  type LocalHistoryOwner,
  localOwner,
  type LocalPair,
  localRefuse,
  type WatcherLocalUserEventCoverage,
  type WatcherLocalUserEventHistory,
} from "./local-history.local-history-owner.js";
import { isHex32, isNatural, same } from "./policy.js";

/**
 * Admits one quiet native block above the covered head. The link is checked
 * locally from the header the native stream delivered: parent hash, block
 * number and slot. No request, no observation, no digest-chain change.
 */
export const advanceWatcherLocalUserEventCoverage = (
  input: Readonly<{
    history: WatcherLocalUserEventHistory;
    header: Readonly<{
      blockHash: string;
      parentBlockHash: string;
      blockNo: string;
      slot: string;
    }>;
  }>,
): WatcherLocalUserEventCoverage => {
  const owner = localOwner(input.history);
  const head = owner.entries.at(-1);
  if (
    head === undefined ||
    owner.checkpoint === null ||
    owner.candidate !== null ||
    owner.anchorCandidate !== null ||
    owner.semanticReplay
  )
    return localRefuse("coverage requires a settled publication");
  const covered = localCoverageHead(owner);
  const { header } = input;
  if (
    !isHex32(header.blockHash) ||
    !isHex32(header.parentBlockHash) ||
    !isNatural(header.blockNo) ||
    !isNatural(header.slot) ||
    header.parentBlockHash !== covered.blockHash ||
    BigInt(header.blockNo) !== BigInt(covered.blockNo) + 1n ||
    BigInt(header.slot) <= BigInt(covered.slot)
  )
    return localRefuse(
      "quiet block is not the direct child of the covered head",
    );
  const point = admitFraudProofRawL1Point({
    blockHash: header.blockHash,
    blockNo: header.blockNo,
    slot: header.slot,
    pointId: computeFraudProofRawL1PointId({
      blockHash: header.blockHash,
      blockNo: header.blockNo,
      slot: header.slot,
    }),
  });
  owner.coverage = Object.freeze({
    point: Object.freeze({ ...point }),
    headEntryDigest: head.entryDigest,
  });
  return owner.coverage;
};

/**
 * Moves coverage back to a point at or above the head entry after a native
 * rollback whose fork lies inside the quiet stretch. The caller resolved the
 * point's block number and hash from the headers it admitted; a fork below
 * the head entry is not a coverage matter and goes through rollback recovery.
 */
export const rewindWatcherLocalUserEventCoverage = (
  input: Readonly<{
    history: WatcherLocalUserEventHistory;
    point: WatcherUserEventOriginFacts["parentPoint"];
  }>,
): WatcherLocalUserEventCoverage => {
  const owner = localOwner(input.history);
  const head = owner.entries.at(-1);
  if (
    head === undefined ||
    owner.checkpoint === null ||
    owner.candidate !== null ||
    owner.anchorCandidate !== null ||
    owner.semanticReplay
  )
    return localRefuse("coverage requires a settled publication");
  const covered = localCoverageHead(owner);
  const point = admitFraudProofRawL1Point(input.point);
  if (
    BigInt(point.blockNo) > BigInt(covered.blockNo) ||
    BigInt(point.blockNo) < BigInt(head.cursor.blockNo) ||
    (point.blockNo === head.cursor.blockNo && !same(point, head.cursor))
  )
    return localRefuse("coverage rewind target is outside the covered stretch");
  owner.coverage = Object.freeze({
    point: Object.freeze({ ...point }),
    headEntryDigest: head.entryDigest,
  });
  return owner.coverage;
};

/**
 * Restores saved coverage over a restored head. A record that references an
 * older entry is a torn write between the head publication and the coverage
 * update; it is discarded and coverage restarts at the head. A record on the
 * current head that lies below it is a bug and fails loudly.
 */
export const restoreWatcherLocalUserEventCoverage = (
  input: Readonly<{
    history: WatcherLocalUserEventHistory;
    saved: Readonly<{
      blockHash: string;
      blockNo: string;
      slot: string;
      headEntryDigest: string;
      checkpointDigest: string;
    }> | null;
  }>,
): Readonly<{
  coverage: WatcherLocalUserEventCoverage;
  disposition: "restored" | "head" | "discarded_stale";
}> => {
  const owner = localOwner(input.history);
  const head = owner.entries.at(-1);
  if (head === undefined || owner.checkpoint === null)
    return localRefuse("coverage requires a settled publication");
  const onHead = localCoverageOnHead(head);
  const saved = input.saved;
  if (saved === null) {
    owner.coverage = onHead;
    return Object.freeze({ coverage: onHead, disposition: "head" });
  }
  if (
    saved.headEntryDigest !== head.entryDigest ||
    saved.checkpointDigest !== owner.checkpoint.checkpointDigest
  ) {
    owner.coverage = onHead;
    return Object.freeze({ coverage: onHead, disposition: "discarded_stale" });
  }
  if (
    !isHex32(saved.blockHash) ||
    !isNatural(saved.blockNo) ||
    !isNatural(saved.slot) ||
    BigInt(saved.blockNo) < BigInt(head.cursor.blockNo) ||
    BigInt(saved.slot) < BigInt(head.cursor.slot) ||
    (saved.blockNo === head.cursor.blockNo &&
      saved.blockHash !== head.cursor.blockHash)
  )
    return localRefuse(
      "saved coverage lies below the head entry it references",
    );
  const point = admitFraudProofRawL1Point({
    blockHash: saved.blockHash,
    blockNo: saved.blockNo,
    slot: saved.slot,
    pointId: computeFraudProofRawL1PointId({
      blockHash: saved.blockHash,
      blockNo: saved.blockNo,
      slot: saved.slot,
    }),
  });
  owner.coverage = Object.freeze({
    point: Object.freeze({ ...point }),
    headEntryDigest: head.entryDigest,
  });
  return Object.freeze({ coverage: owner.coverage, disposition: "restored" });
};

export const localLivePair = (owner: LocalHistoryOwner, pair: LocalPair) => {
  const scripts = readWatcherUserEventScriptBinding({
    binding: owner.scriptBinding,
    deploymentIdentity: owner.deploymentIdentity,
  });
  const current = readWatcherLocalBackfillFinalityObservation(pair);
  const witness = readWatcherLocalBackfillFinalityOriginalWitness(pair);
  const evidence = readWatcherUserEventReferenceEvidence(
    pair.referenceAuthority,
  );
  const referenceEvidence = admitWatcherLocalBackfillUserEventReferenceEvidence(
    {
      ...pair,
      evidence,
      deploymentIdentity: owner.deploymentIdentity,
    },
  );
  if (
    scripts !== owner.origin.scripts ||
    referenceEvidence !== evidence ||
    witness.current.finality !== current.finality ||
    witness.current.observation !== current.observation ||
    !same(current.finality.policy, owner.finalityPolicy) ||
    !same(
      current.observation.capture.sourceBinding,
      owner.origin.originalWitness.current.observation.capture.sourceBinding,
    ) ||
    current.observation.sourceIdentityDigest !==
      owner.origin.originalWitness.current.observation.sourceIdentityDigest ||
    witness.first.observation.capture.nativeBlock.rawBlockCbor !==
      current.observation.capture.nativeBlock.rawBlockCbor ||
    !same(
      witness.first.observation.capture.predecessorPoint,
      current.observation.capture.predecessorPoint,
    )
  ) {
    return localRefuse("live finality/reference/source binding differs");
  }
  return { witness, referenceEvidence };
};
