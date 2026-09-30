import { createHash } from "node:crypto";

import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core/codec";
import { keyValuePhasRootWithCount } from "@al-ft/midgard-fault-proofs";
import type { EventKey } from "@al-ft/midgard-sdk";
import {
  DepositEvent,
  Header,
  OutputReference,
  TxOrderEvent,
  WithdrawalEvent,
} from "@al-ft/midgard-sdk";
import {
  buildCanonicalMidgardLedgerEntryOutputMaterial,
  buildCanonicalTransitionEffect,
  type CanonicalTransitionEffect,
  type ValidationMachineLedgerEntry,
} from "@al-ft/midgard-validation";
import type { PhaseBConfig } from "@al-ft/midgard-validation/types";
import { Data as LucidData } from "@lucid-evolution/lucid";

import {
  type WatcherForcedOperatorVerdict,
  type WatcherIndexedUserEvent,
  type WatcherLocalUserEventHeaderCutoff,
  type WatcherTerminalUserEvent,
} from "../indexers/user-event-indexer.js";
import {
  HEX_BYTES,
  normalizeRootHex,
  type WatcherBlockReplayPriorUtxo,
} from "./block-replay.watcher-block-replay-rejection-projection.js";
import {
  assertWatcherFullBlockReplayResult,
  fail,
  type WatcherBlockReplayCommittedStep,
  type WatcherBlockReplayEventAuthority,
  type WatcherBlockReplayResult,
} from "./block-replay.watcher-block-replay-result.js";
import { type WatcherCommittedEventClaim } from "./event-claims.js";
import { type WatcherRuleBundle } from "./rule-bundle.js";

/**
 * Turns the supplied prior-state entries into canonical ledger entries and
 * recomputes their PHAS root with the canonical helper.
 *
 * Every value is canonicalised by `buildCanonicalMidgardLedgerEntryOutputMaterial`,
 * the exact descriptor builder the canonical reconstruction uses for
 * `block_body.utxos` (reconstruct.ts:816-826), so the root computed here is
 * comparable to the header's committed roots by construction rather than by
 * agreement.
 */
export const watcherBlockReplayPriorState = async (
  entries: readonly WatcherBlockReplayPriorUtxo[],
): Promise<{
  readonly ledgerEntries: readonly ValidationMachineLedgerEntry[];
  readonly root: string;
}> => {
  const ledgerEntries: ValidationMachineLedgerEntry[] = [];
  const descriptors: { readonly key: Buffer; readonly value: Buffer }[] = [];
  const seen = new Set<string>();
  for (const [index, entry] of entries.entries()) {
    const path = `$.priorState[${index.toString()}]`;
    if (!HEX_BYTES.test(entry.outRef) || !HEX_BYTES.test(entry.outputCbor)) {
      fail("malformed_prior_state", path);
    }
    if (seen.has(entry.outRef)) {
      fail("malformed_prior_state", `${path}.outRef`);
    }
    seen.add(entry.outRef);
    const outRef = Buffer.from(entry.outRef, "hex");
    const output = Buffer.from(entry.outputCbor, "hex");
    let descriptorCbor: Buffer;
    try {
      descriptorCbor = buildCanonicalMidgardLedgerEntryOutputMaterial({
        outRef,
        outputCbor: output,
      }).descriptorCbor;
    } catch {
      return fail("malformed_prior_state", `${path}.outputCbor`);
    }
    ledgerEntries.push({ outRef, output });
    descriptors.push({ key: outRef, value: descriptorCbor });
  }
  const phas = await keyValuePhasRootWithCount(descriptors);
  return {
    ledgerEntries: Object.freeze(
      [...ledgerEntries].sort((left, right) =>
        Buffer.compare(left.outRef, right.outRef),
      ),
    ),
    root: normalizeRootHex(phas.root),
  };
};

// ---------------------------------------------------------------------------
// Canonical replay core
// ---------------------------------------------------------------------------

/**
 * Phase B is deterministic in the candidates, the prior state, and the
 * header-committed block slot. Concurrency is pinned to 1 because a verifier
 * has no throughput requirement and the value must not influence a digest, and
 * the script budget is enforced because the canonical default for a validating
 * node is enforcement.
 */
export const WATCHER_BLOCK_REPLAY_BUCKET_CONCURRENCY = 1 as const;

/**
 * Builds the canonical `PhaseBConfig` from the L1-committed header. Nothing
 * here is a policy choice by the watcher: the slot is a header field and the
 * other two values are pinned constants that cannot change a verdict's
 * direction.
 */
export const makeWatcherPhaseBConfig = (header: Header): PhaseBConfig =>
  Object.freeze({
    nowCardanoSlotNo: header.blockSlot,
    bucketConcurrency: WATCHER_BLOCK_REPLAY_BUCKET_CONCURRENCY,
    enforceScriptBudget: true,
  });

export const eventKeyTxId = (eventKey: EventKey): string | null =>
  "L2TransactionEventKey" in eventKey
    ? eventKey.L2TransactionEventKey.tx_id
    : null;

export const phaseForEventKey = (
  eventKey: EventKey,
): WatcherBlockReplayCommittedStep["phase"] =>
  "DepositEventKey" in eventKey
    ? "Deposit"
    : "WithdrawalEventKey" in eventKey
      ? "Withdrawal"
      : "ForcedTransactionEventKey" in eventKey
        ? "ForcedTransaction"
        : "L2Transaction";

export const eventKeyFingerprint = (eventKey: EventKey): string => {
  if ("L2TransactionEventKey" in eventKey) {
    return `L2Transaction:${eventKey.L2TransactionEventKey.tx_id}`;
  }
  if ("DepositEventKey" in eventKey) {
    const id = eventKey.DepositEventKey.deposit_id;
    return `Deposit:${id.transactionId}:${id.outputIndex.toString()}`;
  }
  if ("WithdrawalEventKey" in eventKey) {
    const id = eventKey.WithdrawalEventKey.withdrawal_id;
    return `Withdrawal:${id.transactionId}:${id.outputIndex.toString()}`;
  }
  const id = eventKey.ForcedTransactionEventKey.tx_order_id;
  return `ForcedTransaction:${id.transactionId}:${id.outputIndex.toString()}`;
};

export type WatcherBlockReplayEventOriginRecord = Readonly<{
  source: "local_publication";
  snapshotDigest: string;
  historyEntryDigests: readonly string[];
  deploymentManifestId: string;
  blueprintHash: string;
  checkpointDigest: string;
  checkpointPayloadDigest: string;
  headEntryDigest: string;
  throughHeader: WatcherLocalUserEventHeaderCutoff | null;
}>;

export type WatcherBlockReplayEffectRecord = Readonly<{
  canonicalCborHex: string;
  digest: string;
  operations: readonly Readonly<
    | { type: "delete"; outRefCborHex: string }
    | { type: "insert"; outRefCborHex: string; outputCborHex: string }
  >[];
}>;

/** Descriptive canonical replay input/output material. This is never authority. */
export type WatcherBlockReplayEventAuthorityRecord = Readonly<{
  phase: WatcherBlockReplayEventAuthority["phase"];
  eventKey: EventKey;
  event: WatcherIndexedUserEvent | WatcherTerminalUserEvent;
  network: WatcherRuleBundle["network"];
  origin: WatcherBlockReplayEventOriginRecord;
  committedClaim: WatcherCommittedEventClaim;
  canonicalNativeTxCborHex: string | null;
  programMaterialSidecarCborHex: string | null;
  transitionEffect: WatcherBlockReplayEffectRecord;
}>;

export const fullReplayEventRecords = new WeakMap<
  WatcherBlockReplayResult,
  readonly WatcherBlockReplayEventAuthorityRecord[]
>();

/** Copies only descriptive material from an actually admitted full W25 replay. */
export const readWatcherBlockReplayEventAuthorityRecords = (
  result: WatcherBlockReplayResult,
): readonly WatcherBlockReplayEventAuthorityRecord[] => {
  assertWatcherFullBlockReplayResult(result);
  const records = fullReplayEventRecords.get(result);
  if (records === undefined)
    return fail("canonical_replay_threw", "$.eventAuthorityRecords");
  return structuredClone(records);
};

export const watcherBlockReplayEventAuthorityManifest = (
  record: Pick<
    WatcherBlockReplayEventAuthorityRecord,
    "phase" | "eventKey" | "event" | "origin" | "committedClaim"
  >,
): Readonly<Record<string, unknown>> => {
  const { event, origin } = record;
  return Object.freeze({
    phase: record.phase,
    eventKeyFingerprint: eventKeyFingerprint(record.eventKey),
    authoritySource: origin.source,
    deploymentManifestId: origin.deploymentManifestId,
    blueprintHash: origin.blueprintHash,
    checkpointDigest: origin.checkpointDigest,
    checkpointPayloadDigest: origin.checkpointPayloadDigest,
    userEventSnapshotDigest: origin.snapshotDigest,
    headEntryDigest: origin.headEntryDigest,
    throughHeader: origin.throughHeader,
    historyEntryDigests: origin.historyEntryDigests,
    eventId: event.eventId,
    eventOutRef: event.outRef,
    transactionHash: event.transactionHash,
    eventContentDigest: event.eventContentDigest,
    datumDigest: event.datumDigest,
    outputDigest: event.outputDigest,
    originPointDigest: event.originPointDigest,
    originChainPointId: event.originChainPointId,
    originBlockHash: event.originBlockHash,
    originSlot: event.originSlot,
    originBlockNo: event.originBlockNo,
    finalityStatus: event.finalityStatus,
    committedSource: record.committedClaim,
  });
};

export type ValidatedEventAuthority = Readonly<{
  phase: WatcherBlockReplayEventAuthority["phase"];
  eventKeyFingerprint: string;
  effect: CanonicalTransitionEffect | null;
  canonicalNativeTxCbor: Buffer | null;
  programMaterialSidecarCbor: Buffer | null;
  committedForcedValidity: WatcherForcedOperatorVerdict | null;
  userEvent: WatcherIndexedUserEvent | WatcherTerminalUserEvent;
  authorityManifest: Readonly<Record<string, unknown>>;
  effectManifest: Readonly<Record<string, unknown>> | null;
  recordSource: Omit<
    WatcherBlockReplayEventAuthorityRecord,
    "transitionEffect"
  >;
}>;

export const sha256Hex = (bytes: Uint8Array): string =>
  createHash("sha256").update(bytes).digest("hex");

export const userEventKindForPhase = (
  phase: WatcherBlockReplayEventAuthority["phase"],
): WatcherIndexedUserEvent["kind"] =>
  phase === "Deposit"
    ? "deposit"
    : phase === "Withdrawal"
      ? "withdrawal"
      : "forced_order";

export const eventIdForKey = (
  eventKey: EventKey,
): Readonly<{ transactionId: string; outputIndex: bigint }> =>
  "DepositEventKey" in eventKey
    ? eventKey.DepositEventKey.deposit_id
    : "WithdrawalEventKey" in eventKey
      ? eventKey.WithdrawalEventKey.withdrawal_id
      : "ForcedTransactionEventKey" in eventKey
        ? eventKey.ForcedTransactionEventKey.tx_order_id
        : fail("user_event_authority_identity_mismatch", "$.eventKey");

export const eventIdForKeyCborHex = (eventKey: EventKey): string =>
  LucidData.to(eventIdForKey(eventKey) as never, OutputReference as never);

/**
 * The ledger trie key / transition-effect out-ref: the §5.3 field-0/1 item form
 * `82 ‖ 58 20 tx_id(32) ‖ 19 index_be16`, a fixed 38 bytes, matching on-chain
 * `ledger_outref_key` / `encode_midgard_tx_input`. CML's minimal-index
 * `TransactionInput` CBOR would not compare equal to the effect out-refs
 * `canonicalOutRefCbor` admits.
 */
export const ledgerOutRefCborHex = (value: {
  readonly transactionId: string;
  readonly outputIndex: bigint;
}): string =>
  encodeMidgardSpendInputItem({
    txId: Buffer.from(value.transactionId, "hex"),
    outputIndex: Number(value.outputIndex),
  }).toString("hex");

export const decodeUserEventIdCborHex = (
  event: WatcherIndexedUserEvent,
): string | null => {
  try {
    const schema =
      event.kind === "deposit"
        ? DepositEvent
        : event.kind === "withdrawal"
          ? WithdrawalEvent
          : TxOrderEvent;
    const decoded = LucidData.from(event.eventCborHex, schema as never) as {
      readonly id: unknown;
    };
    return LucidData.to(decoded.id as never, OutputReference as never);
  } catch {
    return null;
  }
};

export const canonicalEffectFromAuthority = (
  authority: Extract<
    WatcherBlockReplayEventAuthority,
    { phase: "Withdrawal" | "Deposit" }
  >,
): CanonicalTransitionEffect => {
  const rebuilt = buildCanonicalTransitionEffect(
    authority.transitionEffect.operations,
  );
  if (
    rebuilt.schemaVersion !== authority.transitionEffect.schemaVersion ||
    rebuilt.digest !== authority.transitionEffect.digest ||
    !rebuilt.canonicalCbor.equals(authority.transitionEffect.canonicalCbor)
  ) {
    return fail(
      "transition_effect_digest_mismatch",
      "$.transitionEffect.digest",
    );
  }
  return rebuilt;
};
