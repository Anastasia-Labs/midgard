import { SqlClient } from "@effect/sql";
import { Effect, Option, Ref } from "effect";

import {
  type DisplacementCompensationIntent,
  prepareDisplacementCompensation,
} from "../database/eventHistoryRecoveryPlans.displacement-compensation.js";
import * as Pending from "../database/pendingBlockFinalizations.js";
import {
  eventHistoryCanonicalJson,
  readBoundRecoveryLedgerSnapshot,
} from "../l1-event-history-source.js";
import { LEDGER_SCAN_TIMEOUT_MS } from "../l1-ledger-snapshot.js";
import {
  signedIntentReplacementDigest,
  SignedIntentReplacementIntegrityError,
} from "./canonical-journal-recovery.js";
import { HistoryRecoverySuperseded } from "./event-history-recovery.js";
import { Globals } from "./globals.js";
import { executeHistoryDependentRecovery } from "./history-dependent-recovery.js";
import {
  type DisplacementCompensationSource,
  loadCompensationSource,
} from "./history-expired-intent-release.compensation-source.js";
import {
  displacement,
  observedRemoval,
} from "./history-expired-intent-release.displacement.js";
import { openRetainedNativeOwner } from "./history-expired-intent-release.open-retained-native-owner.js";
import { ownedBy } from "./history-expired-intent-release.owned.js";
import type { ReplacedBlockRevivalInput } from "./history-expired-intent-release.prepare-replaced-block-revival.js";
import { displacementIdentity } from "./history-expired-intent-release.recover-displacement.js";
import { canonicalEvidence } from "./history-expired-intent-release.replaced-block-landing.js";
import { authenticateQueue } from "./history-expired-intent-release.signed-commit-node.js";
import {
  C,
  failure,
  sha,
  signedTtl,
} from "./history-expired-intent-release.table.js";
import { reincludeStateQueueCorrectedBlocks } from "./state-queue-correction-recovery.js";
import {
  admittedRemovals,
  loadStateQueueCorrectionObserverState,
  stateQueueCorrectionRewindDisposition,
} from "./state-queue-correction-rewind.admitted-removals.js";
import {
  nativeOwnerOpenWait,
  prepareStateQueueCorrectionRewind,
} from "./state-queue-correction-rewind.prepare-state-queue-correction-rewind.js";
import { signedTxSpends } from "./state-queue-correction-rewind.prove-unlanded.js";

/** The committed compensation intent owns retries and any branch retarget.
 * No native root outside the full original journal closure is a valid source.
 * A live exact signed suffix input + expired TTL + canonical absence proves
 * that suffix cannot land; a missing fact retains the signed ambiguity. */
export const compensateDisplacement = (
  input: ReplacedBlockRevivalInput,
  source: DisplacementCompensationSource,
) =>
  Effect.gen(function* () {
    const owned = ownedBy(input.preparation);
    const original = source.originalIntent;
    const integrity = (message: string) =>
      Effect.fail(
        new SignedIntentReplacementIntegrityError(original.headerHash, message),
      );
    const load = loadCompensationSource(input, () => source);
    const bound = yield* owned(load);
    const capture = yield* Effect.tryPromise({
      try: (signal) =>
        readBoundRecoveryLedgerSnapshot({
          ...input.transport,
          binding: input.binding,
          timeoutMs: LEDGER_SCAN_TIMEOUT_MS,
          addresses: [input.contracts.stateQueue.spendingScriptAddress],
          at: input.checkpoint.head,
          signal,
        }),
      catch: (cause) =>
        failure("Compensation exact-point capture failed", cause),
    });
    yield* input.preparation.assertCurrent;
    const queue = yield* authenticateQueue(
      capture.ledger.outputs,
      input.contracts,
    );
    const evidence = yield* owned(
      canonicalEvidence(
        input.binding,
        input.checkpoint,
        [bound.winner, ...bound.displaced].flatMap((record) =>
          record[C.INTENDED_TX_HASH] === null
            ? []
            : [record[C.INTENDED_TX_HASH]!.toString("hex")],
        ),
      ),
    );
    const prove = Effect.gen(function* () {
      const fresh = yield* load;
      const observer = yield* loadStateQueueCorrectionObserverState(
        input.rewindAuthority,
        true,
      );
      const depth = evidence.canonicalDepth;
      if (observer.kind !== "observed" || depth === undefined) return undefined;
      const removals = yield* admittedRemovals(input.rewindAuthority);
      if (removals.kind !== "admitted") return undefined;
      const correction = yield* stateQueueCorrectionRewindDisposition(
        input.rewindAuthority,
      );
      const originalNode = queue.nodes.find(
        (node) =>
          node !== queue.root && node.headerHash === original.headerHash,
      );
      if (originalNode !== undefined) {
        const base = fresh.winner[C.BASE_TAIL_HEADER_HASH].toString("hex");
        if (correction !== undefined) {
          const removal = removals.transitions.get(base);
          const index =
            removal?.previousQueue.findIndex(
              (member) => member.headerHash === original.headerHash,
            ) ?? -1;
          if (
            index <= 0 ||
            removal?.previousQueue[index - 1]?.headerHash !== base ||
            !removal.nextQueue.some(
              (member) =>
                member.headerHash === original.headerHash &&
                member.outRef ===
                  `${originalNode.node.utxo.txHash}#${originalNode.node.utxo.outputIndex}`,
            )
          )
            return undefined;
        }
        const proved = yield* displacement({
          blocking: fresh.displaced.map((record) =>
            record[C.HEADER_HASH].toString("hex"),
          ),
          node: originalNode,
          queued: true,
          winner: fresh.winner,
          base,
          baseRoot: original.targetRoot,
          queue,
          observer,
          depth,
          required: input.rewindAuthority.requiredFinalityDepth,
        });
        if (
          typeof proved === "string" ||
          displacementIdentity(fresh.winner, proved) !== original.journalDigest
        )
          return undefined;
        const members = fresh.displaced.map((record) => ({
          headerHash: record[C.HEADER_HASH].toString("hex"),
          transitionDigest: signedIntentReplacementDigest(record)!,
          kind: "displaced" as const,
        }));
        return {
          ...fresh,
          prefix: [] as Pending.Record[],
          suffix: fresh.displaced,
          members,
          targetRoot: original.targetRoot,
          head: fresh.winner,
          correction: correction !== undefined,
        };
      }
      const baseHeader = fresh.winner[C.BASE_TAIL_HEADER_HASH];
      const base = yield* Pending.retrieveByHeaderHash(baseHeader);
      if (
        observedRemoval(observer, baseHeader.toString("hex"), false) !==
          undefined ||
        (Option.isSome(base) &&
          base.value[C.STATUS] === Pending.Status.Abandoned)
      )
        return undefined;
      const prefix: Pending.Record[] = [];
      for (const record of fresh.displaced) {
        const header = record[C.HEADER_HASH].toString("hex");
        if (
          !queue.nodes.some((node) => node.headerHash === header) ||
          observedRemoval(observer, header, false) !== undefined
        )
          break;
        prefix.push(record);
      }
      const head = prefix.at(-1);
      if (head === undefined) return undefined;
      const headNode = queue.nodes.find(
        (node) => node.headerHash === head[C.HEADER_HASH].toString("hex"),
      )!;
      const held = depth.of(headNode.node.utxo.txHash) ?? depth.retained + 1n;
      if (held < input.rewindAuthority.requiredFinalityDepth) return undefined;
      const suffix = fresh.displaced.slice(prefix.length);
      const members: DisplacementCompensationIntent["suffixMembers"][number][] =
        [];
      const firstRemoval =
        suffix[0] === undefined
          ? undefined
          : removals.transitions.get(suffix[0][C.HEADER_HASH].toString("hex"));
      // If X's exact signed input remains on the checkpoint queue, X cannot
      // already have landed on this branch. Expiry then excludes a late landing.
      // An admitted removal instead proves that a landed X was corrected.
      if (
        suffix.length > 0 &&
        firstRemoval === undefined &&
        suffix[0]![C.BASE_TAIL_OUT_REF] !==
          `${headNode.node.utxo.txHash}#${headNode.node.utxo.outputIndex}`
      )
        return undefined;
      for (const record of suffix) {
        const header = record[C.HEADER_HASH].toString("hex");
        if (
          queue.nodes.some((node) => node.headerHash === header) ||
          queue.root.headerHash === header ||
          observedRemoval(observer, header, true) !== undefined
        )
          return undefined;
        const removal = removals.transitions.get(header);
        if (removal !== undefined) {
          members.push({
            headerHash: header,
            transitionDigest: removal.transitionDigest,
            kind: "removed",
          });
          continue;
        }
        if (observedRemoval(observer, header, false) !== undefined)
          return undefined;
        const intended = record[C.INTENDED_TX_HASH] ?? null;
        const signed = record[C.SIGNED_TX_CBOR] ?? null;
        const submitted = record[C.SUBMITTED_TX_HASH];
        const ttl = signed === null ? undefined : signedTtl(signed);
        if (
          intended === null ||
          signed === null ||
          (submitted !== null && !submitted.equals(intended)) ||
          !signedTxSpends(signed, intended, record[C.BASE_TAIL_OUT_REF]) ||
          ttl === undefined ||
          BigInt(input.checkpoint.head.slot) < ttl ||
          evidence.canonicalHistory.has(intended.toString("hex"))
        )
          return undefined;
        const replacement = signedIntentReplacementDigest(record);
        if (replacement === undefined) return undefined;
        members.push({
          headerHash: header,
          transitionDigest: replacement,
          kind: "displaced",
        });
      }
      // Unrelated admitted corrections retain their own priority. A correction
      // removing this suffix is resolved by this same native compensation.
      if (
        correction !== undefined &&
        !members.some((member) => member.kind === "removed")
      )
        return undefined;
      return {
        ...fresh,
        prefix,
        suffix,
        members,
        targetRoot: head[C.EXPECTED_UTXOS_ROOT],
        head,
        correction: correction !== undefined,
      };
    });
    const proof = yield* owned(prove);
    if (proof === undefined) return false;
    const globals = yield* Globals;
    const owner = yield* openRetainedNativeOwner(globals, input.config).pipe(
      Effect.catchIf(
        (error) => nativeOwnerOpenWait(error) !== undefined,
        () => Effect.succeed(undefined),
      ),
    );
    if (owner === undefined) return false;
    const diagnostics = yield* Effect.tryPromise({
      try: () => owner.diagnostics(),
      catch: (cause) =>
        failure("Compensation native diagnostics failed", cause),
    });
    const roots = new Set([
      original.expectedRoot,
      original.targetRoot,
      ...proof.displaced.map((record) => record[C.EXPECTED_UTXOS_ROOT]),
    ]);
    if (!roots.has(diagnostics.durableRoot))
      return yield* integrity(
        "Compensation native root is outside the original retained closure",
      );
    const sameDisposition = (current: NonNullable<typeof proof>) =>
      eventHistoryCanonicalJson({
        prefix: current.prefix.map((record) =>
          record[C.HEADER_HASH].toString("hex"),
        ),
        members: current.members,
        targetRoot: current.targetRoot,
        correction: current.correction,
      });
    const recheck = Effect.gen(function* () {
      const current = yield* prove;
      if (
        current === undefined ||
        sameDisposition(current) !== sameDisposition(proof)
      )
        return yield* Effect.fail(
          new HistoryRecoverySuperseded({
            message: "Compensation canonical disposition changed",
          }),
        );
      return current;
    });
    const prefixHeaderHashes = proof.prefix.map((record) =>
      record[C.HEADER_HASH].toString("hex"),
    );
    const suffixHeaderHashes = proof.suffix.map((record) =>
      record[C.HEADER_HASH].toString("hex"),
    );
    const sameRetained =
      proof.retained.kind === "displacement_compensation" &&
      eventHistoryCanonicalJson({
        prefixHeaderHashes,
        suffixHeaderHashes,
        suffixMembers: proof.members,
        targetRoot: proof.targetRoot,
      }) ===
        eventHistoryCanonicalJson({
          prefixHeaderHashes: proof.retained.intent.prefixHeaderHashes,
          suffixHeaderHashes: proof.retained.intent.suffixHeaderHashes,
          suffixMembers: proof.retained.intent.suffixMembers,
          targetRoot: proof.retained.intent.targetRoot,
        });
    const intent: DisplacementCompensationIntent =
      sameRetained && proof.retained.kind === "displacement_compensation"
        ? proof.retained.intent
        : {
            bindingDigest: input.binding.digest,
            manifestId: input.checkpoint.manifestId,
            headerHash: proof.head[C.HEADER_HASH].toString("hex"),
            originalRecoveryId: source.originalRecoveryId,
            originalIntent: original,
            prefixHeaderHashes,
            suffixHeaderHashes,
            suffixMembers: proof.members,
            expectedRoot: diagnostics.durableRoot,
            targetRoot: proof.targetRoot,
            journalDigest: original.journalDigest,
          };
    if (
      sameRetained &&
      diagnostics.durableRoot !== intent.expectedRoot &&
      diagnostics.durableRoot !== intent.targetRoot
    )
      return yield* integrity(
        "Native root no longer matches the exact retained compensation",
      );
    const plan = yield* owned(
      recheck.pipe(
        Effect.zipRight(
          prepareDisplacementCompensation(
            input.checkpoint,
            source.recoveryId,
            intent,
            sha(
              eventHistoryCanonicalJson({
                point: input.checkpoint.head,
                snapshot: input.checkpoint.capture.snapshotDigest,
                disposition: sameDisposition(proof),
              }),
            ),
          ),
        ),
      ),
    );
    // From here the prepared row is a compensation row, so rechecks must read
    // it by its exact NEW identity. Original journal/source proof stays bound.
    source = { ...source, recoveryId: plan.recoveryId };
    yield* executeHistoryDependentRecovery({
      checkpoint: input.checkpoint,
      preparation: input.preparation,
      plan,
      owner,
      repair: Effect.gen(function* () {
        yield* recheck;
        const sql = yield* SqlClient.SqlClient;
        const current = yield* sql<{
          root_hex: string;
        }>`SELECT root_hex FROM mpf_engine_state WHERE store_name = 'ledger' FOR UPDATE`;
        if (
          current.length !== 1 ||
          current[0]!.root_hex !== original.expectedRoot
        )
          return yield* integrity(
            "Compensation SQL marker differs from the original finalized snapshot",
          );
        const aggregate =
          proof.prefix.length > 0
            ? proof.head.utxoPayloadAggregate
            : Option.getOrUndefined(
                yield* Pending.retrieveByHeaderHash(
                  proof.winner[C.BASE_TAIL_HEADER_HASH],
                ),
              )?.utxoPayloadAggregate;
        yield* sql`UPDATE mpf_engine_state SET root_hex = ${intent.targetRoot}, utxo_payload_entry_count = ${aggregate?.entryCount ?? null}, utxo_payload_encoded_tuple_bytes = ${aggregate?.encodedTupleBytes ?? null}, updated_at = NOW() WHERE store_name = 'ledger'`;
        const reopened = yield* reincludeStateQueueCorrectedBlocks(
          intent.suffixMembers,
        );
        if (
          reopened.length !== intent.suffixMembers.length ||
          reopened.some(
            (member) =>
              !member.journalFound || member.abandonedFromStatus === undefined,
          )
        )
          return yield* integrity(
            "Compensation did not reopen its exact absent suffix",
          );
      }),
      afterSqlCommit: Effect.gen(function* () {
        yield* Ref.set(globals.LOCAL_FINALIZATION_PENDING, false);
        yield* Ref.set(globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK, "");
      }),
    });
    if (proof.correction)
      yield* prepareStateQueueCorrectionRewind({
        bindingDigest: input.binding.digest,
        checkpoint: input.checkpoint,
        preparation: input.preparation,
        config: input.config,
        authority: input.rewindAuthority,
      });
    return true;
  });
