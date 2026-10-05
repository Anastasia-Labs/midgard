import { assertDeploymentMarkerMatches } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Context, Effect, Option } from "effect";

import {
  fetchForeignRetainedDa,
  foreignRetainedDaInsert,
} from "../da/foreign-retained-da.js";
import {
  ConfirmedLedgerDB,
  DaPayloadsDB,
  PendingBlockFinalizationsDB,
} from "../database/index.js";
import * as Ledger from "../database/utils/ledger.js";
import {
  canonicalSlotConfigForLucid,
  unixTimeToSlotForConfig,
} from "../lucid-time.js";
import {
  computeLedgerMpfRootFromLedgerEntries,
  utxoToLedgerInsertMaterial,
} from "../mpf/index.js";
import {
  ForeignBlockVerificationError,
  verifyAndImportBlock,
} from "../mpf/verified-block-import.js";
import type { HistoryOwnerCoverage } from "../services/event-history-owner.js";
import {
  type HistoryProducerPermit,
  withHistoryWrite,
} from "../services/event-history-producer.js";
import type { ForeignBaseVerificationOutcome } from "../services/foreign-base-verification.js";
import {
  assertForeignVerificationSource,
  currentForeignVerificationSource,
  ForeignVerificationSource,
} from "../services/foreign-verification-source.js";
import {
  ContractDeploymentIdentity,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../services/index.js";
import { fetchCanonicalStateQueueNodesProgram } from "../services/state-queue-topology.js";
import { sha256 } from "../sha256.js";
import {
  materializeLedgerDeltaSuffix,
  pendingUtxoMemberToConfirmedLedgerEntry,
} from "../transactions/state-queue/confirmed-ledger-snapshot.js";
import {
  canonicalObservation,
  ledgerSegmentBefore,
} from "./commit-block-header.foreign-base-material.js";
import {
  foreignEventMaterial,
  type ForeignEventMemberships,
} from "./commit-block-header.foreign-event-material.js";
import { decodeHeader } from "./commit-block-header/da-payload.verify-payload-commitments.js";
import { decodeStoredPayload } from "./t2-foreign-event-reconciliation.resolve-t2-foreign-event-evidence.js";

export type ForeignImportedBlock = Readonly<{
  kind: "foreign" | "local";
  nativeMpfReplay?: PendingBlockFinalizationsDB.NativeMpfReplayInput;
  memberships?: ForeignEventMemberships;
  headerCbor: string;
  ledgerKeys: readonly Buffer[];
  ledgerBefore: readonly Ledger.MinimalEntry[];
  headerHash: string;
  parentHeaderHash: string;
  parentUtxosRoot: string;
  root: string;
  events: readonly (readonly {
    readonly key: string;
    readonly output: Buffer | null;
  }[])[];
  eventRoots: readonly string[];
}>;
export type VerifiedForeignCommitBase = Readonly<{
  authority: "ready" | "recovery";
  headerHash: string;
  root: string;
  entries: readonly Ledger.MinimalEntry[];
  history: HistoryProducerPermit;
  observation: readonly {
    readonly txHash: string;
    readonly outputIndex: number;
    readonly datumCbor: string;
  }[];
  importedBlocks: readonly ForeignImportedBlock[];
  verification: ForeignBaseVerificationOutcome;
}>;
export const VerifiedForeignBase = Context.GenericTag<{
  readonly base: VerifiedForeignCommitBase;
  readonly assertCurrent: Effect.Effect<
    void,
    unknown,
    import("../services/database.js").Database
  >;
}>("midgard/VerifiedForeignCommitBase");
/** Revalidate current owner/generation, exact source prefix and the full queue.
 * A cached root or prior invocation never authorizes reuse after restart/rewind. */
export const revalidateForeignCommitBase = (base: VerifiedForeignCommitBase) =>
  Effect.gen(function* () {
    const source = { kind: base.authority, binding: base.history };
    yield* assertForeignVerificationSource(source);
    const lucid = yield* Lucid;
    const contracts = yield* MidgardContracts;
    const nodes = yield* fetchCanonicalStateQueueNodesProgram(
      lucid.api,
      contracts.stateQueue,
    );
    if (
      JSON.stringify(canonicalObservation(nodes)) !==
      JSON.stringify(base.observation)
    )
      return yield* Effect.fail(
        new ForeignBlockVerificationError({
          foreignHeaderHash: base.headerHash,
          reason: "missing",
          detail: "Canonical state queue changed during foreign verification",
        }),
      );
    yield* assertForeignVerificationSource(source);
  });

/** Verify the exact observed tip by importing its ancestors from a verified
 * local or confirmed parent. Every invocation rebinds against current topology,
 * including after restart or rollback; no root-only durable cache is authority. */
export const verifyForeignCommitBase = (
  latestBlock: SDK.StateQueueUTxO,
  recoveryCoverage?: HistoryOwnerCoverage,
) =>
  Effect.gen(function* () {
    const lucid = yield* Lucid;
    const contracts = yield* MidgardContracts;
    const identity = yield* ContractDeploymentIdentity;
    const config = yield* NodeConfig;
    const headerHash =
      latestBlock.datum.key === "Empty"
        ? (yield* SDK.getConfirmedStateFromStateQueueDatum(latestBlock.datum))
            .data.headerHash
        : yield* SDK.hashBlockHeader(
            yield* SDK.getHeaderFromStateQueueDatum(latestBlock.datum),
          );
    const source = yield* currentForeignVerificationSource(recoveryCoverage);
    const binding = source.binding;
    if (
      identity.kind !== "manifest" ||
      identity.manifestId !== binding.token.deploymentIdentity
    )
      return yield* Effect.fail(
        new ForeignBlockVerificationError({
          foreignHeaderHash: headerHash,
          reason: "invalid",
          detail:
            "Foreign verification deployment differs from authenticated source owner",
        }),
      );
    const importedBlocks: ForeignImportedBlock[] = [];
    const verifiedHeaderHashes: string[] = [];
    const acquiredRows: DaPayloadsDB.InsertInput[] = [];
    const fail = (reason: "missing" | "invalid", detail: string) =>
      Effect.fail(
        new ForeignBlockVerificationError({
          foreignHeaderHash: headerHash,
          reason,
          detail,
        }),
      );
    if (identity.deploymentMarker === undefined)
      return yield* fail(
        "missing",
        "foreign import requires the active deployment marker",
      );
    const deploymentMarker = identity.deploymentMarker;
    const observe = fetchCanonicalStateQueueNodesProgram(
      lucid.api,
      contracts.stateQueue,
    );
    const nodes = yield* observe;
    const observed = nodes.find(
      (node) =>
        node.utxo.txHash === latestBlock.utxo.txHash &&
        node.utxo.outputIndex === latestBlock.utxo.outputIndex,
    );
    if (observed === undefined)
      return yield* fail(
        "missing",
        "foreign tip observation is no longer canonical",
      );
    const observedHash =
      observed.datum.key === "Empty"
        ? (yield* SDK.getConfirmedStateFromStateQueueDatum(observed.datum)).data
            .headerHash
        : yield* SDK.hashBlockHeader(
            yield* SDK.getHeaderFromStateQueueDatum(observed.datum),
          );
    if (observedHash !== headerHash)
      return yield* fail(
        "invalid",
        "foreign tip header substituted at the canonical out-ref",
      );
    const confirmedNode = nodes.find((node) => node.datum.key === "Empty");
    if (confirmedNode === undefined)
      return yield* fail(
        "missing",
        "canonical confirmed parent is unavailable",
      );
    const { data: confirmed } = yield* SDK.getConfirmedStateFromStateQueueDatum(
      confirmedNode.datum,
    );
    let entries: readonly Ledger.MinimalEntry[] =
      yield* ConfirmedLedgerDB.retrieve;
    let root = yield* computeLedgerMpfRootFromLedgerEntries(entries);
    if (
      root !== confirmed.utxoRoot &&
      confirmed.headerHash === SDK.GENESIS_HEADER_HASH
    ) {
      entries = yield* Effect.forEach(config.GENESIS_UTXOS, (utxo) =>
        utxoToLedgerInsertMaterial(utxo).pipe(
          Effect.map(({ ledgerOp, outputCbor }) => ({
            [Ledger.Columns.OUTREF]: ledgerOp.key,
            [Ledger.Columns.OUTPUT]: outputCbor,
          })),
        ),
      );
      root = yield* computeLedgerMpfRootFromLedgerEntries(entries);
    }
    if (root !== confirmed.utxoRoot)
      return yield* fail(
        "missing",
        "confirmed ledger snapshot does not reproduce the canonical parent",
      );
    const complete = (root: string, entries: readonly Ledger.MinimalEntry[]) =>
      Effect.gen(function* () {
        const result: VerifiedForeignCommitBase = {
          headerHash,
          root,
          entries,
          authority: source.kind,
          history: binding,
          observation: canonicalObservation(nodes),
          importedBlocks: Object.freeze(importedBlocks),
          verification:
            verifiedHeaderHashes.length > 0
              ? {
                  status: "verified",
                  foreignHeaderHash: headerHash,
                  verifiedHeaderHashes: Object.freeze(verifiedHeaderHashes),
                }
              : { status: "not_required", baseHeaderHash: headerHash },
        };
        yield* revalidateForeignCommitBase(result);
        // Retention is fenced; acquiring bytes never itself grants root authority.
        yield* withHistoryWrite(
          Effect.forEach(acquiredRows, DaPayloadsDB.upsertAvailable, {
            discard: true,
          }),
        );
        yield* assertForeignVerificationSource(source);
        return result;
      });
    if (latestBlock.datum.key === "Empty")
      return yield* complete(root, entries);
    let parentHash = confirmed.headerHash;
    let parentEndTime = confirmed.endTime;
    const headers: { header: SDK.Header; hash: string }[] = [];
    for (const node of nodes) {
      if (node.datum.key === "Empty") continue;
      const next = yield* SDK.getHeaderFromStateQueueDatum(node.datum);
      headers.push({ header: next, hash: yield* SDK.hashBlockHeader(next) });
    }
    // Linked-list topology orders oldest to newest. Only import the prefix ending
    // at the observed base; later queue nodes confer no authority on it.
    for (const next of headers) {
      if (
        next.header.prevHeaderHash !== parentHash ||
        next.header.startTime !== parentEndTime
      )
        return yield* fail(
          "invalid",
          "foreign import canonical parent hash/time mismatch",
        );
      const journal = yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
        Buffer.from(next.hash, "hex"),
      );
      if (Option.isSome(journal)) {
        yield* Effect.try({
          try: () =>
            assertDeploymentMarkerMatches(
              deploymentMarker,
              {
                schemaVersion:
                  journal.value[
                    PendingBlockFinalizationsDB.Columns
                      .DEPLOYMENT_MARKER_SCHEMA_VERSION
                  ],
                manifestId:
                  journal.value[
                    PendingBlockFinalizationsDB.Columns.DEPLOYMENT_MANIFEST_ID
                  ],
              },
              "foreign import local ancestry",
            ),
          catch: (cause) =>
            new ForeignBlockVerificationError({
              foreignHeaderHash: next.hash,
              reason: "invalid",
              detail: `Local ancestry deployment binding is invalid: ${String(cause)}`,
            }),
        });
        if (
          journal.value[PendingBlockFinalizationsDB.Columns.STATUS] ===
          PendingBlockFinalizationsDB.Status.Abandoned
        )
          return yield* fail(
            "missing",
            "foreign import parent journal is abandoned",
          );
        const journalHeader = yield* decodeHeader(journal.value);
        if (
          (yield* SDK.hashBlockHeader(journalHeader)) !== next.hash ||
          journal.value[PendingBlockFinalizationsDB.Columns.BASE_UTXOS_ROOT] !==
            root ||
          journal.value[
            PendingBlockFinalizationsDB.Columns.CONSENSUS_PROFILE_ID
          ] !== identity.consensusProfile.profileId
        )
          return yield* fail(
            "invalid",
            "local verified parent journal identity differs from canonical header/base",
          );
        const segmentBefore = ledgerSegmentBefore(
          [
            ...journal.value.ledgerDelta.spent,
            ...journal.value.ledgerDelta.produced.map(
              (member) => member.outref,
            ),
          ].map((key) => key.toString("hex")),
          entries,
        );
        const baseEntries = yield* Effect.forEach(
          entries,
          pendingUtxoMemberToConfirmedLedgerEntry,
        );
        const snapshot = yield* materializeLedgerDeltaSuffix({
          baseEntries,
          baseRoot: root,
          records: [journal.value],
        });
        if (snapshot.root !== next.header.utxosRoot)
          return yield* fail(
            "invalid",
            "local verified parent root differs from canonical header",
          );
        const replay = journal.value.nativeMpfReplay;
        if (
          replay !== undefined &&
          (replay.baseRoot.toString("hex") !== root ||
            replay.candidateRoot.toString("hex") !== snapshot.root)
        )
          return yield* fail(
            "invalid",
            "Local journal native replay is not the exact verified ancestry segment",
          );
        importedBlocks.push({
          kind: "local",
          headerCbor: Data.to(next.header, SDK.Header),
          ...segmentBefore,
          headerHash: next.hash,
          parentHeaderHash: parentHash,
          parentUtxosRoot: root,
          root: snapshot.root,
          events: [],
          eventRoots:
            replay === undefined
              ? []
              : Array.from({ length: replay.eventCount }, (_, index) =>
                  replay.eventRoots
                    .subarray(index * 32, (index + 1) * 32)
                    .toString("hex"),
                ),
          ...(replay === undefined ? {} : { nativeMpfReplay: replay }),
        });
        entries = snapshot.entries;
        root = snapshot.root;
      } else {
        const stored = yield* DaPayloadsDB.retrieveByHeaderHash(
          Buffer.from(next.hash, "hex"),
        );
        let payload: SDK.DaPayload;
        if (Option.isNone(stored)) {
          const acquired = yield* fetchForeignRetainedDa(
            next.hash,
            next.header,
          );
          payload = acquired.payload;
          acquiredRows.push(
            foreignRetainedDaInsert(
              next.hash,
              next.header,
              acquired.payloadBytes,
            ),
          );
        } else {
          const row = stored.value;
          if (
            !row[DaPayloadsDB.Columns.HEADER_HASH].equals(
              Buffer.from(next.hash, "hex"),
            ) ||
            row[DaPayloadsDB.Columns.CONSENSUS_PROFILE_ID] !==
              identity.consensusProfile.profileId ||
            !sha256(row[DaPayloadsDB.Columns.PAYLOAD_CBOR]).equals(
              row[DaPayloadsDB.Columns.PAYLOAD_SHA256],
            )
          )
            return yield* fail(
              "invalid",
              "Foreign retained DA identity mismatch",
            );
          payload = yield* Effect.tryPromise({
            try: () =>
              decodeStoredPayload({
                payloadCbor: row[DaPayloadsDB.Columns.PAYLOAD_CBOR],
                schemaVersion: row[DaPayloadsDB.Columns.VERSION],
              }),
            catch: (cause) =>
              new ForeignBlockVerificationError({
                foreignHeaderHash: next.hash,
                reason: "invalid",
                detail: `Foreign retained DA codec failure: ${String(cause)}`,
              }),
          });
        }
        const material = yield* foreignEventMaterial(
          next.hash,
          next.header,
          payload,
        ).pipe(Effect.provideService(ForeignVerificationSource, source));
        const events: {
          readonly key: string;
          readonly output: Buffer | null;
        }[][] = [];
        const eventRoots: string[] = [];
        const imported = yield* verifyAndImportBlock({
          onReplayedEvent: ({ mutations, root }) => {
            events.push([...mutations]);
            eventRoots.push(root);
          },
          ...material,
          header: next.header,
          headerHash: next.hash,
          parentHeaderHash: parentHash,
          parentUtxosRoot: root,
          parentEntries: entries,
          payload,
          expectedNetworkId: config.NETWORK === "Mainnet" ? 1n : 0n,
          minFeeA: config.MIN_FEE_A,
          minFeeB: config.MIN_FEE_B,
          blockSlot: BigInt(
            unixTimeToSlotForConfig(
              Number(next.header.endTime),
              canonicalSlotConfigForLucid(lucid.api),
            ),
          ),
        });
        importedBlocks.push({
          kind: "foreign",
          headerCbor: Data.to(next.header, SDK.Header),
          ...ledgerSegmentBefore(
            events.flatMap((event) => event.map((mutation) => mutation.key)),
            entries,
          ),
          memberships: material.memberships,
          headerHash: next.hash,
          parentHeaderHash: parentHash,
          parentUtxosRoot: root,
          root: imported.root,
          events: Object.freeze(events),
          eventRoots: Object.freeze(eventRoots),
        });
        verifiedHeaderHashes.push(next.hash);
        entries = imported.entries;
        root = imported.root;
      }
      parentHash = next.hash;
      parentEndTime = next.header.endTime;
      if (next.hash === headerHash) return yield* complete(root, entries);
    }
    return yield* fail(
      "missing",
      "foreign tip is absent from the current canonical chain",
    );
  });
