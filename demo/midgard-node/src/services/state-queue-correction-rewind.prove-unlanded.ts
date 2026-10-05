import { SqlClient } from "@effect/sql";
import { CML } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";

import * as Pending from "../database/pendingBlockFinalizations.js";
import { DatabaseError } from "../database/utils/common.js";
import { type StateQueueCorrectionObserverState } from "./state-queue-correction-observer.js";
import {
  type AdmittedRemovals,
  admittedRemovals,
  C,
  type ChainMember,
  journal,
  nonAbandonedChildren,
  type Obligation,
  removedStatusBlocked,
  ROOT_TAIL_HEADER_HASH,
  type StateQueueCorrectionRewindAuthority,
  UNLANDED_STATUSES,
  unresolvedRemovedHeaders,
} from "./state-queue-correction-rewind.admitted-removals.js";

/** Validates one linear removed chain, earliest first, and the retained
 * parent aggregate of its replay base. Any other shape is not a rewind this
 * node can prove, and stays blocked with its reason. */
export const validateChain = (
  chain: readonly ChainMember[],
  manifestId: string,
): Effect.Effect<Obligation, DatabaseError, SqlClient.SqlClient> =>
  Effect.gen(function* () {
    for (let index = 0; index < chain.length; index += 1) {
      const { record, kind } = chain[index]!;
      const header = record[C.HEADER_HASH].toString("hex");
      if (kind === "removed") {
        const blocked = removedStatusBlocked(record);
        if (blocked !== undefined) return { kind: "blocked", reason: blocked };
      } else if (!UNLANDED_STATUSES.includes(record[C.STATUS]))
        return {
          kind: "blocked",
          reason: `unlanded descendant ${header} has journal status ${record[C.STATUS]}, which records an L1 observation`,
        };
      if (record[C.DEPLOYMENT_MANIFEST_ID] !== manifestId)
        return {
          kind: "blocked",
          reason: `removed block ${header} belongs to another deployment`,
        };
      const replay = record.nativeMpfReplay;
      if (
        replay === undefined ||
        replay.baseRoot.toString("hex") !== record[C.BASE_UTXOS_ROOT] ||
        replay.candidateRoot.toString("hex") !== record[C.EXPECTED_UTXOS_ROOT]
      )
        return {
          kind: "blocked",
          reason: `removed block ${header} has no native replay matching its journal roots`,
        };
      if (index > 0) {
        const parent = chain[index - 1]!.record;
        if (
          !record[C.BASE_TAIL_HEADER_HASH].equals(parent[C.HEADER_HASH]) ||
          record[C.BASE_UTXOS_ROOT] !== parent[C.EXPECTED_UTXOS_ROOT]
        )
          return {
            kind: "blocked",
            reason: `removed block ${header} does not extend ${parent[C.HEADER_HASH].toString("hex")}`,
          };
      }
    }
    const earliest = chain[0]!.record;
    const baseTail = earliest[C.BASE_TAIL_HEADER_HASH];
    let parentAggregate: Pending.UtxoPayloadSizeAggregate | undefined;
    if (!baseTail.equals(ROOT_TAIL_HEADER_HASH)) {
      const parent = yield* Pending.retrieveByHeaderHash(baseTail);
      if (Option.isSome(parent)) {
        if (
          parent.value[C.STATUS] === Pending.Status.Abandoned ||
          parent.value[C.EXPECTED_UTXOS_ROOT] !== earliest[C.BASE_UTXOS_ROOT]
        )
          return {
            kind: "blocked",
            reason: `the replay base of removed block ${earliest[C.HEADER_HASH].toString("hex")} is not its retained parent's root`,
          };
        parentAggregate = parent.value.utxoPayloadAggregate;
      }
    }
    return { kind: "ready", chain, parentAggregate };
  });

export const OUT_REF = /^([0-9a-f]{64})#(0|[1-9][0-9]*)$/u;

/** True only when the signed transaction's body hashes to `txHash` and spends
 * `outRef`. Undecodable bytes are not evidence. */
export const signedTxSpends = (
  cbor: Buffer,
  txHash: Buffer,
  outRef: string,
): boolean => {
  const parsed = OUT_REF.exec(outRef);
  if (parsed === null) return false;
  let tx: CML.Transaction | undefined;
  try {
    tx = CML.Transaction.from_cbor_bytes(cbor);
    const body = tx.body();
    const hash = CML.hash_transaction(body);
    const inputs = body.inputs();
    try {
      if (hash.to_hex() !== txHash.toString("hex")) return false;
      for (let index = 0; index < inputs.len(); index += 1) {
        const input = inputs.get(index);
        const id = input.transaction_id();
        try {
          if (
            id.to_hex() === parsed[1] &&
            input.index().toString() === parsed[2]
          )
            return true;
        } finally {
          id.free();
          input.free();
        }
      }
      return false;
    } finally {
      inputs.free();
      hash.free();
      body.free();
    }
  } catch {
    return false;
  } finally {
    tx?.free();
  }
};

/** A header the authenticated state-queue view still knows: in the current
 * cursor queue, or in any pending or admitted transition's queues. Such a
 * block landed (or may yet be admitted as landed) and is never unlanded. */
const onAuthenticatedQueue = (
  state: StateQueueCorrectionObserverState,
  header: string,
) =>
  state.cursorQueue.some((node) => node.headerHash === header) ||
  [...state.pending, ...state.admitted].some(
    (transition) =>
      transition.removedHeaderHashes.includes(header) ||
      transition.previousQueue.some((node) => node.headerHash === header) ||
      transition.nextQueue.some((node) => node.headerHash === header),
  );

/**
 * Proves that `childHeader`, the only non-abandoned journal extending the
 * removed block `parent`, never reached L1 and never can: its commit spends
 * exactly the queue node of `parent` that the admitted correction consumed.
 * That correction is at the release depth, so the input is gone for as long
 * as the removal itself stands. Anything short of that proof is blocked with
 * its reason; a journal whose header the authenticated view knows is never
 * abandoned.
 */
export const proveUnlanded = (
  parent: ChainMember,
  childHeader: string,
  admitted: AdmittedRemovals,
): Effect.Effect<
  | Readonly<{ kind: "blocked"; reason: string }>
  | Readonly<{ kind: "unlanded"; member: ChainMember }>,
  DatabaseError,
  SqlClient.SqlClient
> =>
  Effect.gen(function* () {
    const parentHeader = parent.record[C.HEADER_HASH].toString("hex");
    const blocked = (reason: string) => ({ kind: "blocked" as const, reason });
    const notYet = `descendant ${childHeader} of removed block ${parentHeader} is not removed by an admitted correction yet`;
    if (parent.kind !== "removed")
      return blocked(
        `descendant ${childHeader} extends unlanded block ${parentHeader}; only a direct descendant of a removed block can be proven unlanded`,
      );
    const found = yield* journal(childHeader);
    if (Option.isNone(found))
      return blocked(`descendant ${childHeader} lost its journal`);
    const record = found.value;
    const status = record[C.STATUS];
    if (!UNLANDED_STATUSES.includes(status))
      return blocked(`${notYet} (journal status ${status})`);
    if (onAuthenticatedQueue(admitted.state, childHeader))
      return blocked(`${notYet}; it is on the authenticated state queue`);
    const removal = admitted.transitions.get(parentHeader);
    const baseTail = record[C.BASE_TAIL_OUT_REF];
    const parentNode = removal?.previousQueue.find(
      (node) => node.headerHash === parentHeader,
    );
    if (
      removal === undefined ||
      removal.transitionDigest !== parent.transitionDigest ||
      parentNode === undefined ||
      parentNode.outRef !== baseTail ||
      !removal.consumedQueueOutRefs.includes(baseTail)
    )
      return blocked(
        `${notYet}; its commit spends ${baseTail}, which the admitted correction of ${parentHeader} did not consume`,
      );
    const intended = record[C.INTENDED_TX_HASH] ?? null;
    const signed = record[C.SIGNED_TX_CBOR] ?? null;
    const submitted = record[C.SUBMITTED_TX_HASH];
    if (intended !== null && signed !== null) {
      if (submitted !== null && !submitted.equals(intended))
        return blocked(
          `${notYet}; it was submitted as ${submitted.toString("hex")}, not its retained signed intent`,
        );
      if (!signedTxSpends(signed, intended, baseTail))
        return blocked(
          `${notYet}; its retained signed commit does not spend ${baseTail}`,
        );
    } else if (
      submitted !== null ||
      status !== Pending.Status.PendingSubmission
    )
      return blocked(
        `${notYet}; it was handed to L1 without retained signed bytes, so it cannot be proven never to land`,
      );
    return {
      kind: "unlanded" as const,
      member: {
        record,
        transitionDigest: removal.transitionDigest,
        kind: "unlanded" as const,
      },
    };
  });

/** The removed chain owed a rewind: an admitted removal whose journal is not
 * abandoned, anchored at the unique owed block whose parent is not owed, with
 * every non-abandoned descendant also removed, or proven never to land (see
 * proveUnlanded). A suffix whose later removals are not admitted yet stays
 * blocked; each admission re-evaluates, so any order of suffix finality
 * reaches the same end state. */
export const loadObligation = (
  authority: StateQueueCorrectionRewindAuthority,
) =>
  Effect.gen(function* () {
    const unresolved = yield* unresolvedRemovedHeaders(authority);
    if (unresolved.length === 0) return { kind: "none" } satisfies Obligation;
    const admitted = yield* admittedRemovals(authority);
    if (admitted.kind === "blocked") return admitted satisfies Obligation;
    const owed = new Map<string, ChainMember>();
    for (const header of unresolved) {
      const digest = admitted.removals.get(header);
      const record = yield* journal(header);
      if (digest === undefined || Option.isNone(record))
        return {
          kind: "blocked",
          reason: `removed block ${header} lost its admitted correction or journal`,
        } satisfies Obligation;
      owed.set(header, {
        record: record.value,
        transitionDigest: digest,
        kind: "removed",
      });
    }
    const anchors = [...owed.values()].filter(
      ({ record }) =>
        !owed.has(record[C.BASE_TAIL_HEADER_HASH].toString("hex")),
    );
    if (anchors.length !== 1)
      return {
        kind: "blocked",
        reason: `removed blocks ${[...owed.keys()].join(",")} do not form one chain`,
      } satisfies Obligation;
    const chain = [anchors[0]!];
    for (;;) {
      const children = yield* nonAbandonedChildren(
        chain.at(-1)!.record[C.HEADER_HASH].toString("hex"),
      );
      if (children.length === 0) break;
      if (children.length !== 1)
        return {
          kind: "blocked",
          reason: `removed block ${chain.at(-1)!.record[C.HEADER_HASH].toString("hex")} has competing descendants ${children.join(",")}`,
        } satisfies Obligation;
      const child = owed.get(children[0]!);
      if (child === undefined) {
        const unlanded = yield* proveUnlanded(
          chain.at(-1)!,
          children[0]!,
          admitted,
        );
        if (unlanded.kind === "blocked") return unlanded satisfies Obligation;
        chain.push(unlanded.member);
        continue;
      }
      if (chain.at(-1)!.kind === "unlanded")
        return {
          kind: "blocked",
          reason: `removed block ${children[0]!} extends unlanded block ${chain.at(-1)!.record[C.HEADER_HASH].toString("hex")}`,
        } satisfies Obligation;
      if (chain.includes(child))
        return {
          kind: "blocked",
          reason: "removed block ancestry is cyclic",
        } satisfies Obligation;
      chain.push(child);
    }
    if (chain.filter(({ kind }) => kind === "removed").length !== owed.size)
      return {
        kind: "blocked",
        reason: `removed blocks ${[...owed.keys()].join(",")} do not form one chain`,
      } satisfies Obligation;
    return yield* validateChain(chain, authority.manifestId);
  });
