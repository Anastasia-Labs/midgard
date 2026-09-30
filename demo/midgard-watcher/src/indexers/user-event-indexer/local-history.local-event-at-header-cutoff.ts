import { CML } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import {
  readWatcherProtectedUserEventCheckpoint,
  readWatcherProtectedUserEventCheckpointReceipt,
} from "../../storage/durable-runtime.js";
import { type WatcherUserEventArchive } from "../../storage/user-event-checkpoint.js";
import { type WatcherStateQueueHeaderObservation } from ".././authenticated-state-queue-observation.js";
import { outputReference } from "./decode.js";
import {
  localAuthorityUnavailable,
  localHeaderFields,
} from "./local-history.assert-watcher-local-user-event-head-current.js";
import { localEntryAtBlock } from "./local-history.local-entry-at-block.js";
import {
  localEventAuthorities,
  type LocalEventAuthorityOwner,
  type LocalHistoryOwner,
  localOwner,
  localRefuse,
  type LocalRetainedEvidence,
  type WatcherLocalUserEventAuthority,
  type WatcherLocalUserEventAuthorityRead,
  type WatcherLocalUserEventEntry,
} from "./local-history.local-history-owner.js";
import { localLivePair } from "./local-history.restore-watcher-local-user-event-coverage.js";
import { isHexBytes, isNatural, same, sha256Bytes } from "./policy.js";
import {
  type WatcherIndexedUserEvent,
  type WatcherTerminalUserEvent,
} from "./types.js";

export const localHeaderBlock = async (
  owner: LocalHistoryOwner,
  header: Pick<
    ReturnType<typeof localHeaderFields>,
    "observedBlockHash" | "observedBlockNo" | "observedSlot"
  >,
  archive: WatcherUserEventArchive,
): Promise<
  Readonly<{ entry: WatcherLocalUserEventEntry; rawBlockCbor: string }>
> => {
  const originDigest = owner.originDigest;
  const policyDigest = owner.policy.policyDigest;
  const found = await localEntryAtBlock(
    owner,
    BigInt(header.observedBlockNo),
    archive,
  );
  if (found === null)
    return localRefuse("header cutoff block carried no observed event");
  const { entry, rawBlockCbor } = found;
  if (
    entry.originDigest !== originDigest ||
    entry.policyDigest !== policyDigest ||
    entry.cursor.blockHash !== header.observedBlockHash ||
    entry.cursor.slot !== header.observedSlot ||
    entry.cursor.blockNo !== header.observedBlockNo ||
    typeof rawBlockCbor !== "string" ||
    !isHexBytes(rawBlockCbor)
  )
    return localRefuse("header cutoff is not the exact accepted block");
  return Object.freeze({ entry, rawBlockCbor });
};

/** Pure decoding of the already admitted lineage's original bytes. This creates
 * neither a native acquisition receipt nor fresh W12 finality authority.
 */
export const localCutoffTransactionOrder = (
  block: Readonly<{ entry: WatcherLocalUserEventEntry; rawBlockCbor: string }>,
) => {
  const decoded = CML.Block.from_cbor_hex(block.rawBlockCbor);
  const header = decoded.header();
  const body = header.header_body();
  if (
    decoded.to_cbor_hex() !== block.rawBlockCbor ||
    Buffer.from(
      blake2b(Buffer.from(header.to_cbor_hex(), "hex"), { dkLen: 32 }),
    ).toString("hex") !== block.entry.cursor.blockHash ||
    body.slot().toString() !== block.entry.cursor.slot ||
    body.block_number().toString() !== block.entry.cursor.blockNo ||
    body.prev_hash()?.to_hex() !== block.entry.parent.blockHash
  )
    return localRefuse("historical cutoff raw header differs");
  const bodies = decoded.transaction_bodies();
  const transactionIds = Array.from({ length: bodies.len() }, (_, index) =>
    CML.hash_transaction(bodies.get(index)).to_hex(),
  );
  if (new Set(transactionIds).size !== transactionIds.length)
    return localRefuse("historical cutoff transaction order is ambiguous");
  return {
    bodies,
    transactionIds,
    invalidTransactions: new Set(decoded.invalid_transactions()),
  };
};

export const localEventAtHeaderCutoff = async (
  owner: LocalHistoryOwner,
  event: WatcherIndexedUserEvent,
  terminal: WatcherTerminalUserEvent | null,
  originEvidence: LocalRetainedEvidence,
  terminalEvidence: LocalRetainedEvidence,
  header: WatcherStateQueueHeaderObservation,
  archive: WatcherUserEventArchive,
) => {
  const fields = localHeaderFields(header);
  const block = await localHeaderBlock(owner, fields, archive);
  const ordered = localCutoffTransactionOrder(block);
  const transactionIndex = ordered.transactionIds.indexOf(
    fields.observedTransactionHash,
  );
  if (transactionIndex < 0 || ordered.invalidTransactions.has(transactionIndex))
    return localRefuse("header cutoff transaction is not validly included");
  const outRef = fields.queueOutRef.split("#");
  if (
    outRef.length !== 2 ||
    outRef[0] !== fields.observedTransactionHash ||
    !isNatural(outRef[1]) ||
    BigInt(outRef[1]) >=
      BigInt(ordered.bodies.get(transactionIndex).outputs().len())
  )
    return localRefuse("header cutoff output reference differs");
  const output = ordered.bodies
    .get(transactionIndex)
    .outputs()
    .get(Number(outRef[1]));
  if (
    output.datum()?.as_datum()?.to_canonical_cbor_hex() !==
      header.linkedListDatumCborHex ||
    Buffer.from(
      blake2b(Buffer.from(fields.headerCborHex, "hex"), { dkLen: 28 }),
    ).toString("hex") !== fields.headerHash
  )
    return localRefuse("header cutoff output or header bytes differ");
  const occursThroughHeader = (
    evidence: LocalRetainedEvidence,
    transactionHash: string,
  ): boolean => {
    const entrySequence = BigInt(evidence.entry.sequence);
    if (entrySequence !== BigInt(block.entry.sequence))
      return entrySequence < BigInt(block.entry.sequence);
    if (!same(evidence.entry, block.entry))
      return localRefuse("event cutoff entry membership differs");
    const index = ordered.transactionIds.indexOf(transactionHash);
    if (index < 0 || ordered.invalidTransactions.has(index))
      return localRefuse("event cutoff transaction is not validly included");
    return index <= transactionIndex;
  };
  // Pointer continuations replace transactionHash/outRef, but admission remains
  // the unique valid transaction that consumed the immutable event nonce in
  // the authenticated origin block. Archived bytes describe this live owner's
  // accepted lineage; they do not establish a new origin or fresh authority.
  const originOrder = localCutoffTransactionOrder(originEvidence);
  const admissions = originOrder.transactionIds.filter((_, index) => {
    if (originOrder.invalidTransactions.has(index)) return false;
    const inputs = originOrder.bodies.get(index).inputs();
    for (let inputIndex = 0; inputIndex < inputs.len(); inputIndex += 1) {
      if (outputReference(inputs.get(inputIndex)) === event.nonceOutRef)
        return true;
    }
    return false;
  });
  if (admissions.length !== 1)
    return localRefuse("event origin nonce is not uniquely consumed");
  if (!occursThroughHeader(originEvidence, admissions[0]!))
    return localAuthorityUnavailable(
      "event origin occurs after the challenged header",
    );
  let selected: WatcherIndexedUserEvent | WatcherTerminalUserEvent = event;
  let includeTerminal = false;
  if (terminal !== null) {
    includeTerminal = occursThroughHeader(
      terminalEvidence,
      terminal.terminalTransactionHash,
    );
    if (includeTerminal) selected = terminal;
    else {
      const {
        terminalStatus: _status,
        terminalTransactionHash: _tx,
        terminalPointDigest: _point,
        terminalBlockHash: _hash,
        terminalSlot: _slot,
        terminalBlockNo: _number,
        terminalFinalityStatus: _finality,
        terminalClassification: _classification,
        ...origin
      } = terminal;
      selected = Object.freeze(origin);
    }
  }
  if (!same(localHeaderFields(header), fields))
    return localRefuse("header cutoff changed during its archive read");
  return Object.freeze({
    event: selected,
    includeTerminal,
    throughHeader: Object.freeze({
      ...fields,
      transactionIndex: transactionIndex.toString(),
      historyEntryDigest: block.entry.entryDigest,
    }),
  });
};

const localEventAuthorityCurrent = (
  authority: LocalEventAuthorityOwner,
): LocalHistoryOwner => {
  const owner = localOwner(authority.history);
  if (authority.header !== null) {
    const cutoff = authority.value.throughHeader;
    if (cutoff === null) return localRefuse("event header cutoff is absent");
    const {
      transactionIndex: _index,
      historyEntryDigest: _entry,
      ...fields
    } = cutoff;
    if (!same(localHeaderFields(authority.header), fields))
      return localRefuse("event header cutoff is no longer identical");
  }
  const head = owner.entries.at(-1);
  const accepted = owner.acceptedEvidence.at(-1);
  if (
    owner.semanticReplay ||
    owner.generation !== authority.generation ||
    owner.candidate !== null ||
    owner.anchorCandidate !== null ||
    owner.checkpoint === null ||
    owner.acceptedAtMonotonicMs === null ||
    owner.snapshot.quarantined ||
    head === undefined ||
    accepted === undefined ||
    owner.checkpoint.checkpointDigest !== authority.value.checkpointDigest ||
    head.entryDigest !== authority.value.headEntryDigest
  )
    return localRefuse(
      "event authority no longer matches the published semantic head",
    );
  // This is a new corroboration, acquired after publication. Original archived
  // W12 observations and depths remain unchanged and are never revived from JSON.
  const { witness } = localLivePair(owner, authority.pair);
  if (
    witness.first.observation.capture.startedAtMonotonicMs <
      owner.acceptedAtMonotonicMs ||
    !same(witness.current.observation.capture.point, head.cursor) ||
    witness.current.observation.capture.nativeBlock.rawBlockCbor !==
      accepted.rawBlockCbor
  )
    return localRefuse(
      "event authority requires a fresh post-publication capture of the exact head",
    );
  return owner;
};

/** Final synchronous fence after all asynchronous authority reads. This checks
 * same-runtime protected-head changes as well as closure, source liveness and
 * private owner generation. The async reader remains necessary for disk freshness.
 */
export const assertWatcherLocalUserEventAuthorityCurrent = (
  receipt: WatcherLocalUserEventAuthority,
): void => {
  const authority =
    localEventAuthorities.get(receipt) ??
    localRefuse("event authority is not privately admitted");
  const owner = localEventAuthorityCurrent(authority);
  const publication = authority.protectedRead.receipt;
  const finality = authority.runtime.read().currentFinalityState;
  if (
    publication === null ||
    finality.phase === "quarantined" ||
    finality.incident !== null ||
    !same(
      readWatcherProtectedUserEventCheckpointReceipt(publication).checkpoint,
      owner.checkpoint,
    )
  )
    return localRefuse(
      "event authority protected checkpoint is no longer current",
    );
};

/** Descriptive output is never accepted as authority. Each read refreshes the
 * protected head and checks the still-live post-publication capture after await.
 * The runtime owner must close this history on a source rollback or shutdown.
 */
export const readWatcherLocalUserEventAuthority = async (
  receipt: WatcherLocalUserEventAuthority,
): Promise<WatcherLocalUserEventAuthorityRead> => {
  const authority =
    localEventAuthorities.get(receipt) ??
    localRefuse("event authority is not privately admitted");
  localEventAuthorityCurrent(authority);
  const publication = await readWatcherProtectedUserEventCheckpoint(
    authority.runtime,
  );
  const owner = localEventAuthorityCurrent(authority);
  const protectedHead =
    readWatcherProtectedUserEventCheckpointReceipt(publication);
  const finalityState = authority.runtime.read().currentFinalityState;
  if (
    finalityState.phase === "quarantined" ||
    finalityState.incident !== null ||
    !same(protectedHead.checkpoint, owner.checkpoint) ||
    protectedHead.payload === null ||
    sha256Bytes(protectedHead.payload) !==
      authority.value.checkpointPayloadDigest
  )
    return localRefuse(
      "event authority protected checkpoint is no longer current",
    );
  authority.protectedRead.receipt = publication;
  return authority.value;
};

/**
 * How a point at or below the head entry is covered: "event" when the exact
 * accepted block was observed there, "quiet" when the block lies inside the
 * linked stretch between observations. A quiet point above the release-final
 * boundary still needs its hash confirmed against the canonical chain, which
 * the runtime resolves with one node lookup on demand.
 */
export type WatcherLocalUserEventPointCoverage = "event" | "quiet";
