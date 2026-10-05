import { randomUUID } from "node:crypto";

import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import * as Adoptions from "../database/foreignNativeAdoptions.js";
import * as MpfEngineState from "../database/mpfEngineState.js";
import {
  revalidateForeignCommitBase,
  type VerifiedForeignCommitBase,
  verifyForeignCommitBase,
} from "../workers/commit-block-header.verify-foreign-base.js";
import type { HistoryOwnerCoverage } from "./event-history-owner.js";
import { withHistoryWrite } from "./event-history-producer.js";
import {
  HistoryPreparation,
  type HistoryRecoveryPreparation,
} from "./event-history-recovery.js";
import { reconcileForeignConfirmedLedger } from "./foreign-confirmed-ledger.js";
import {
  foreignAdoptionProjection,
  foreignAdoptionRowsForKeys,
} from "./foreign-native-adoption-projection.js";
import { prepareForeignNativeReplay } from "./foreign-native-replay.js";
import { reconcileLandedMergeConfirmedLedger } from "./history-landed-merge-ledger.js";
import { retainedLocalNativeRoot } from "./history-local-native-root.js";
import { Lucid } from "./lucid.js";
import { MidgardContracts } from "./midgard-contracts.js";
import type {
  NativeMpfOwnerService,
  PersistedNativeMpfReplay,
} from "./mpf-native-owner/protocol.js";
import { encodeNativeMpfEventLog } from "./mpf-native-owner/service.js";
import {
  digest,
  EVENT_LOG_DIGEST_DOMAIN,
} from "./mpf-native-owner/service.normalize-owner-options.js";
import { fetchCanonicalStateQueueNodesProgram } from "./state-queue-topology.js";

const promise = <A>(work: () => Promise<A>) =>
  Effect.tryPromise({ try: work, catch: (cause) => cause });

const projectionMaterial = (
  base: VerifiedForeignCommitBase,
  replay: PersistedNativeMpfReplay,
  nativeNoop = false,
) => {
  const start = nativeNoop
    ? 0
    : base.importedBlocks.findIndex(
        (block) => block.parentUtxosRoot === replay.baseRoot,
      );
  if (start < 0)
    throw new Error(
      "Foreign adoption projection lacks its exact verified prefix",
    );
  const noopKeys = [
    ...new Set(
      base.importedBlocks.flatMap((block) =>
        block.kind === "foreign"
          ? block.events.flatMap((event) =>
              event.map((mutation) => mutation.key),
            )
          : [],
      ),
    ),
  ].map((key) => Buffer.from(key, "hex"));
  const projection = nativeNoop
    ? {
        keys: noopKeys,
        rows: foreignAdoptionRowsForKeys(noopKeys, base.entries),
      }
    : foreignAdoptionProjection(replay, base.entries);
  const deposits: Adoptions.AdoptionEvents["deposits"][number][] = [];
  const forced: Adoptions.AdoptionEvents["forced"][number][] = [];
  const withdrawals: Adoptions.AdoptionEvents["withdrawals"][number][] = [];
  const origins = new Map<string, string>();
  const unspent = new Set(
    base.entries.map((entry) => entry.outref.toString("hex")),
  );
  // Native replay may begin after an already durable prefix. All verified
  // foreign memberships still require exact canonical assignments.
  for (const block of base.importedBlocks) {
    if (block.kind === "local") continue;
    const memberships = block.memberships;
    if (memberships === undefined)
      throw new Error("Verified foreign adoption lacks event memberships");
    for (const member of memberships.deposits) {
      const key = member.entry.outref.toString("hex");
      const id = member.id.toString("hex");
      origins.set(key, id);
      deposits.push({
        id,
        header: block.headerHash,
        status: unspent.has(key) ? "projected" : "consumed",
      });
    }
    for (const id of memberships.forcedTransactions)
      forced.push({ id: id.toString("hex"), header: block.headerHash });
    for (const member of memberships.withdrawals)
      withdrawals.push({
        id: member.id.toString("hex"),
        header: block.headerHash,
        validity: member.classification.validity,
        detail: member.classification.validityDetail,
        settlement: member.classification.settlementEventInfo.toString("hex"),
      });
  }
  return {
    ...projection,
    rows: projection.rows.map((row) => ({
      ...row,
      ...(origins.has(row.outref)
        ? { source_event_id: origins.get(row.outref)! }
        : {}),
    })),
    events: {
      deposits,
      forced,
      withdrawals,
    } satisfies Adoptions.AdoptionEvents,
  };
};

/** Only the source owner calls this after producers/deferred persistence drain.
 * Every attempt observes and verifies the current complete canonical queue.
 * Retained replay supplies bytes, never a new Ready permit or source authority.
 */
export const recoverForeignNativeAdoptions = (input: {
  readonly owner: NativeMpfOwnerService;
  readonly ownerBinarySha256: string;
  readonly coverage: HistoryOwnerCoverage;
  readonly preparation: HistoryRecoveryPreparation;
}) =>
  Effect.gen(function* () {
    yield* input.preparation.assertCurrent;
    const sql = yield* SqlClient.SqlClient;
    const existing = yield* withHistoryWrite(
      Adoptions.unresolved(input.coverage.bindingDigest),
    );
    if (
      !existing.some(
        (plan) => plan.state !== "applied" || !plan.canonical || plan.removed,
      )
    ) {
      const pending = yield* sql`SELECT 1 FROM pending_block_finalizations
      WHERE status NOT IN ('finalized','abandoned') LIMIT 1`;
      if (pending.length !== 0) return;
    }
    const lucid = yield* Lucid;
    const contracts = yield* MidgardContracts;
    const observe = Effect.gen(function* () {
      yield* input.preparation.assertCurrent;
      const nodes = yield* fetchCanonicalStateQueueNodesProgram(
        lucid.api,
        contracts.stateQueue,
      );
      const tail = nodes.at(-1);
      if (tail === undefined)
        return yield* Effect.fail(
          new Error("Foreign adoption requires the canonical queue root"),
        );
      const base = yield* verifyForeignCommitBase(tail, input.coverage);
      if (base.authority !== "recovery")
        return yield* Effect.fail(
          new Error("Foreign adoption requires current source recovery"),
        );
      return base;
    });
    const result = yield* MpfEngineState.tryWithLedgerStoreLease(
      `foreign-adoption:${randomUUID()}`,
      (leaseOwner) =>
        Effect.gen(function* () {
          yield* reconcileLandedMergeConfirmedLedger({
            coverage: input.coverage,
            preparation: input.preparation,
            leaseOwner,
          });
          yield* reconcileForeignConfirmedLedger({
            coverage: input.coverage,
            preparation: input.preparation,
            leaseOwner,
          });
          let base = yield* observe;
          let plans = yield* withHistoryWrite(
            Adoptions.unresolved(input.coverage.bindingDigest),
          );
          for (const plan of plans) {
            if (plan.state === "requested") continue;
            if (plan.state === "applied" && plan.canonical && !plan.removed)
              continue;
            const replay = yield* Effect.try(() =>
              Adoptions.retainedReplay(plan),
            );
            if (replay.ownerBinarySha256 !== input.ownerBinarySha256)
              return yield* Effect.fail(
                new Error(
                  "Foreign adoption replay belongs to another owner binary",
                ),
              );
            const retainedPrefix =
              base.root === plan.target_root ||
              base.importedBlocks.some(
                (block) =>
                  block.headerHash === plan.header_hash.toString("hex") &&
                  block.root === plan.target_root,
              );
            if (
              plan.state !== "rewinding" &&
              plan.canonical &&
              !plan.removed &&
              retainedPrefix
            ) {
              yield* revalidateForeignCommitBase(base);
              if (!Adoptions.isNativeNoop(plan))
                yield* promise(() => input.owner.recover(replay));
              yield* input.preparation.assertCurrent;
              yield* revalidateForeignCommitBase(base);
              yield* withHistoryWrite(Adoptions.apply(plan, leaseOwner));
            } else {
              // Persist direction before the CAS: a crash resumes the same inverse.
              yield* withHistoryWrite(
                Adoptions.markRewinding(plan, leaseOwner),
              );
              yield* input.preparation.assertCurrent;
              if (!Adoptions.isNativeNoop(plan))
                yield* promise(() =>
                  input.owner.restoreCanonicalRoot({
                    recoveryId: `foreign-adoption-rewind:${plan.adoption_id.toString("hex")}`,
                    expectedRoot: replay.candidateRoot,
                    targetRoot: replay.baseRoot,
                  }),
                );
              yield* input.preparation.assertCurrent;
              yield* withHistoryWrite(Adoptions.rewind(plan, leaseOwner));
            }
          }
          base = yield* observe;
          plans = yield* withHistoryWrite(
            Adoptions.unresolved(input.coverage.bindingDigest),
          );
          const requests = plans.filter((plan) => plan.state === "requested");
          const { durableRoot } = yield* promise(() =>
            input.owner.diagnostics(),
          );
          // An all-local prefix has no foreign projection to adopt. A removed
          // local native suffix belongs to the existing authenticated correction
          // or signed-header disposition, including while its proof is pending.
          if (
            !base.importedBlocks.some((block) => block.kind === "foreign") &&
            (durableRoot === base.root ||
              (yield* retainedLocalNativeRoot({
                base,
                durableRoot,
                ownerBinarySha256: input.ownerBinarySha256,
              })))
          ) {
            yield* revalidateForeignCommitBase(base);
            for (const request of requests)
              yield* withHistoryWrite(Adoptions.discardRequest(request));
            return;
          }
          if (
            durableRoot === base.root &&
            (yield* withHistoryWrite(Adoptions.hasAppliedForeignBase(base)))
          ) {
            for (const request of requests)
              yield* withHistoryWrite(Adoptions.discardRequest(request));
            return;
          }
          // Startup/source rollback can discover a newly changed base before a
          // parent producer had a chance to request it. The fresh recovery permit
          // authorizes creating that request directly, without a fake local journal.
          if (requests.length === 0) {
            yield* withHistoryWrite(Adoptions.request(base));
            plans = yield* withHistoryWrite(
              Adoptions.unresolved(input.coverage.bindingDigest),
            );
          }
          const request = plans.find((plan) => plan.state === "requested");
          if (request === undefined)
            return yield* Effect.fail(
              new Error("Foreign adoption lacks its durable request"),
            );
          const prepared = yield* prepareForeignNativeReplay({
            owner: input.owner,
            ownerBinarySha256: input.ownerBinarySha256,
            base,
          });
          const nativeNoop = prepared === undefined;
          const eventLog = encodeNativeMpfEventLog(base.root, []);
          const replay: PersistedNativeMpfReplay = prepared?.replay ?? {
            schema: 1,
            ownerBinarySha256: input.ownerBinarySha256,
            baseRoot: base.root,
            candidateRoot: base.root,
            eventLog,
            eventLogDigest: digest(EVENT_LOG_DIGEST_DOMAIN, eventLog).toString(
              "hex",
            ),
            eventRoots: Buffer.alloc(0),
            eventCount: 0,
          };
          // Once preparation commits the generation is recoverable by its retained
          // log. Before that point discard it on any source/SQL failure.
          let retained = false;
          yield* Effect.gen(function* () {
            const material = yield* Effect.try(() =>
              projectionMaterial(base, replay, nativeNoop),
            );
            yield* revalidateForeignCommitBase(base);
            yield* withHistoryWrite(
              Adoptions.prepare({
                request,
                base,
                replay,
                ...material,
                leaseOwner,
                ...(nativeNoop ? { nativeNoop: true as const } : {}),
              }),
            );
            retained = true;
            yield* input.preparation.assertCurrent;
            yield* revalidateForeignCommitBase(base);
            if (prepared !== undefined)
              yield* promise(() => input.owner.promote(prepared.handle));
            yield* input.preparation.assertCurrent;
            yield* revalidateForeignCommitBase(base);
            const persisted = (yield* withHistoryWrite(
              Adoptions.unresolved(input.coverage.bindingDigest),
            )).find((plan) => plan.adoption_id.equals(request.adoption_id));
            if (persisted === undefined)
              return yield* Effect.fail(
                new Error("Prepared foreign adoption disappeared"),
              );
            yield* withHistoryWrite(Adoptions.apply(persisted, leaseOwner));
          }).pipe(
            Effect.ensuring(
              Effect.suspend(() =>
                retained || prepared === undefined
                  ? Effect.void
                  : promise(() => input.owner.discard(prepared.handle)).pipe(
                      Effect.catchAll((cause) =>
                        Effect.logWarning(
                          "Failed to discard unretained foreign native generation",
                          cause,
                        ),
                      ),
                    ),
              ),
            ),
          );
        }),
    );
    if (result._tag === "Busy")
      return yield* Effect.fail(
        new Error("Foreign adoption awaits the ledger store lease"),
      );
    yield* input.preparation.assertCurrent;
  }).pipe(Effect.provideService(HistoryPreparation, input.preparation));
