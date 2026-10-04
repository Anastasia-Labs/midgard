import {
  decodeMidgardSpendInputItem,
  decodeMidgardTxOutput,
  encodeMidgardAddressText,
} from "@al-ft/midgard-core/codec";
import { assertDeploymentMarkerMatches } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Data } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";

import * as Confirmed from "../database/confirmedLedger.js";
import * as Segments from "../database/foreignVerifiedSegments.js";
import { revalidateLedgerStoreLease } from "../database/mpfEngineState.js";
import * as Pending from "../database/pendingBlockFinalizations.js";
import type * as Ledger from "../database/utils/ledger.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../mpf/index.js";
import { materializeLedgerDeltaSuffix } from "../transactions/state-queue/confirmed-ledger-snapshot.js";
import type { HistoryOwnerCoverage } from "./event-history-owner.js";
import { withHistoryWrite } from "./event-history-producer.js";
import type { HistoryRecoveryPreparation } from "./event-history-recovery.js";
import { assertForeignVerificationSource } from "./foreign-verification-source.js";
import { Lucid } from "./lucid.js";
import {
  ContractDeploymentIdentity,
  MidgardContracts,
} from "./midgard-contracts.js";
import { fetchCanonicalStateQueueNodesProgram } from "./state-queue-topology.js";

const row = (key: string, output: string): Ledger.EntryNoTimeStamp => ({
  outref: Buffer.from(key, "hex"),
  output: Buffer.from(output, "hex"),
  tx_id: Buffer.from(decodeMidgardSpendInputItem(Buffer.from(key, "hex")).txId),
  address: encodeMidgardAddressText(
    decodeMidgardTxOutput(Buffer.from(output, "hex")).address,
  ),
});
const queueIdentity = (nodes: readonly SDK.StateQueueUTxO[]) =>
  nodes.map((node) => [
    node.utxo.txHash,
    node.utxo.outputIndex,
    SDK.encodeLinkedListNodeView(node.datum),
  ]);
const fail = (message: string) => Effect.fail(new Error(message));

/** Bridge a newly confirmed header using previously fully verified source-bound
 * ancestry. This runs before foreign verification chooses ConfirmedLedger as
 * its parent. An exact current source checkpoint and complete queue are checked
 * again before SQL; neither a cached marker nor an old Ready permit is authority.
 */
export const reconcileForeignConfirmedLedger = (input: {
  coverage: HistoryOwnerCoverage;
  preparation: HistoryRecoveryPreparation;
  leaseOwner: string;
}) =>
  Effect.gen(function* () {
    const binding = input.coverage.bindingDigest;
    const source = {
      kind: "recovery" as const,
      binding: { token: input.preparation.token, coverage: input.coverage },
    };
    yield* assertForeignVerificationSource(source);
    const retained = yield* Segments.retrieveVerifiedForeignSegments(binding);
    if (retained.length === 0) return;
    const lucid = yield* Lucid;
    const contracts = yield* MidgardContracts;
    const identity = yield* ContractDeploymentIdentity;
    if (identity.deploymentMarker === undefined)
      return yield* fail(
        "Confirmed foreign ancestry requires the active deployment marker",
      );
    const nodes = yield* fetchCanonicalStateQueueNodesProgram(
      lucid.api,
      contracts.stateQueue,
    );
    const roots = nodes.filter((node) => node.datum.key === "Empty");
    if (roots.length !== 1)
      return yield* fail(
        "Confirmed foreign ancestry requires exactly one current queue root",
      );
    const target = (yield* SDK.getConfirmedStateFromStateQueueDatum(
      roots[0]!.datum,
    )).data;
    const sql = yield* SqlClient.SqlClient;
    const [frontier] = yield* sql<{
      header_hash: Buffer;
      utxos_root: string;
      manifest_id: Buffer;
    }>`
      SELECT header_hash,utxos_root,manifest_id FROM foreign_confirmed_frontier WHERE binding_digest=${Buffer.from(binding, "hex")}`;
    if (
      frontier === undefined ||
      frontier.manifest_id.toString("hex") !==
        input.preparation.token.deploymentIdentity
    )
      return yield* fail(
        "Confirmed foreign ancestry lacks its exact retained frontier",
      );
    const entries = yield* Confirmed.retrieve;
    const actualRoot = yield* computeLedgerMpfRootFromLedgerEntries(entries);
    const currentHeader = frontier.header_hash.toString("hex");
    if (
      currentHeader === target.headerHash &&
      frontier.utxos_root === target.utxoRoot &&
      actualRoot === target.utxoRoot
    )
      return;
    const byHeader = new Map(
      retained.map((record) => [record.header_hash.toString("hex"), record]),
    );
    const decoded = new Map<string, Segments.RetainedForeignSegment>();
    const segment = (headerHash: string, inverseOnly = false) =>
      Effect.gen(function* () {
        const record = byHeader.get(headerHash);
        if (
          record === undefined ||
          record.manifest_id.toString("hex") !==
            input.preparation.token.deploymentIdentity ||
          (!inverseOnly && !record.canonical && !record.source_sealed)
        )
          return yield* fail(
            "Confirmed foreign ancestry is missing current canonical segment evidence",
          );
        const cached = decoded.get(headerHash);
        if (cached !== undefined) return cached;
        const value = yield* Effect.try(() =>
          Segments.decodeRetainedForeignSegment(record),
        );
        const header = yield* Effect.try(() =>
          Data.from(value.headerCbor, SDK.Header),
        );
        if (
          (yield* SDK.hashBlockHeader(header)) !== headerHash ||
          value.headerHash !== headerHash ||
          header.prevHeaderHash !== value.parentHeaderHash ||
          header.prevUtxosRoot !== value.parentUtxosRoot ||
          header.utxosRoot !== value.root ||
          record.parent_header_hash.toString("hex") !==
            value.parentHeaderHash ||
          record.parent_root !== value.parentUtxosRoot ||
          record.utxos_root !== value.root
        )
          return yield* fail(
            "Confirmed foreign ancestry changed its exact header/parent/root binding",
          );
        decoded.set(headerHash, value);
        return value;
      });
    // Find the common retained ancestor by exact header identity. Repeated
    // roots (rejected forced events) never select an ancestor by root equality.
    const currentAncestors = new Map<string, number>([[currentHeader, 0]]);
    const currentPath: Segments.RetainedForeignSegment[] = [];
    let cursor = currentHeader;
    for (
      let count = 0;
      byHeader.has(cursor) && count < retained.length;
      count++
    ) {
      const value = yield* segment(cursor, true);
      currentPath.push(value);
      cursor = value.parentHeaderHash;
      if (currentAncestors.has(cursor))
        return yield* fail("Confirmed foreign ancestry contains a cycle");
      currentAncestors.set(cursor, currentPath.length);
    }
    const targetPath: Segments.RetainedForeignSegment[] = [];
    cursor = target.headerHash;
    const seen = new Set<string>();
    while (!currentAncestors.has(cursor)) {
      if (seen.has(cursor))
        return yield* fail(
          "Confirmed foreign target ancestry contains a cycle",
        );
      seen.add(cursor);
      const value = yield* segment(cursor);
      targetPath.push(value);
      cursor = value.parentHeaderHash;
    }
    const inverses = currentPath.slice(0, currentAncestors.get(cursor)!);
    const forwards = targetPath.reverse();
    let state = new Map<string, Ledger.Entry>(
      entries.map((entry) => [entry.outref.toString("hex"), entry]),
    );
    let running = actualRoot;
    const changed = new Set<string>();
    const inverse = (value: Segments.RetainedForeignSegment) =>
      Effect.gen(function* () {
        if (running !== value.root)
          return yield* fail(
            "Confirmed inverse is not based on its exact post root",
          );
        for (const key of value.ledgerKeys) {
          state.delete(key);
          changed.add(key);
        }
        for (const old of value.ledgerBefore)
          state.set(old.outref, row(old.outref, old.output));
        running = yield* computeLedgerMpfRootFromLedgerEntries([
          ...state.values(),
        ]);
        if (running !== value.parentUtxosRoot)
          return yield* fail(
            "Confirmed inverse does not reproduce its retained parent root",
          );
      });
    // Ordinary local merging may already have projected the observed endpoint.
    // Reconstruct its exact parent in memory, then prove the same continuous
    // chain forward; the SQL rows themselves remain untouched in this case.
    const alreadyApplied =
      actualRoot !== frontier.utxos_root &&
      actualRoot === target.utxoRoot &&
      inverses.length === 0;
    if (alreadyApplied)
      for (const value of [...forwards].reverse()) yield* inverse(value);
    if (running !== frontier.utxos_root)
      return yield* fail(
        "Confirmed SQL ledger differs from its retained header frontier",
      );
    for (const value of inverses) yield* inverse(value);
    for (const value of forwards) {
      if (running !== value.parentUtxosRoot)
        return yield* fail("Confirmed forward ancestry has a root gap");
      for (const key of value.ledgerKeys) changed.add(key);
      const prior = new Map(
        value.ledgerBefore.map((entry) => [entry.outref, entry.output]),
      );
      if (
        value.ledgerKeys.some(
          (key) => state.get(key)?.output.toString("hex") !== prior.get(key),
        )
      )
        return yield* fail(
          "Confirmed segment beforeimages differ from its authenticated parent",
        );
      if (value.kind === "local") {
        const journal = yield* Pending.retrieveByHeaderHash(
          Buffer.from(value.headerHash, "hex"),
        );
        if (Option.isNone(journal))
          return yield* fail(
            "Confirmed local segment lacks its retained journal material",
          );
        yield* Effect.try(() =>
          assertDeploymentMarkerMatches(
            identity.deploymentMarker!,
            {
              schemaVersion:
                journal.value[Pending.Columns.DEPLOYMENT_MARKER_SCHEMA_VERSION],
              manifestId: journal.value[Pending.Columns.DEPLOYMENT_MANIFEST_ID],
            },
            "confirmed local ancestry",
          ),
        );
        if (
          journal.value[Pending.Columns.HEADER_CBOR].toString("hex") !==
          value.headerCbor
        )
          return yield* fail(
            "Confirmed local segment differs from its exact retained header",
          );
        const materialized = yield* materializeLedgerDeltaSuffix({
          baseEntries: [...state.values()],
          baseRoot: running,
          records: [journal.value],
        });
        state = new Map(
          materialized.entries.map((entry) => [
            entry.outref.toString("hex"),
            entry,
          ]),
        );
        running = materialized.root;
      } else {
        if (value.events.length !== value.eventRoots.length)
          return yield* fail("Confirmed foreign event roots are incomplete");
        for (const [index, event] of value.events.entries()) {
          for (const mutation of event) {
            if (mutation.output === null) {
              if (!state.delete(mutation.key))
                return yield* fail(
                  "Confirmed foreign event spends an absent output",
                );
            } else {
              if (state.has(mutation.key))
                return yield* fail(
                  "Confirmed foreign event replaces an existing output",
                );
              state.set(mutation.key, row(mutation.key, mutation.output));
            }
          }
          running = yield* computeLedgerMpfRootFromLedgerEntries([
            ...state.values(),
          ]);
          if (running !== value.eventRoots[index])
            return yield* fail(
              "Confirmed foreign event replay differs from its verified intermediate root",
            );
        }
      }
      if (running !== value.root)
        return yield* fail(
          "Confirmed segment replay differs from its exact header root",
        );
    }
    if (running !== target.utxoRoot)
      return yield* fail(
        "Confirmed ancestry does not end at the freshly observed header/root",
      );
    yield* input.preparation.assertCurrent;
    const latest = yield* fetchCanonicalStateQueueNodesProgram(
      lucid.api,
      contracts.stateQueue,
    );
    if (
      JSON.stringify(queueIdentity(latest)) !==
      JSON.stringify(queueIdentity(nodes))
    )
      return yield* fail(
        "Canonical queue changed during confirmed ancestry recovery",
      );
    yield* assertForeignVerificationSource(source);
    yield* withHistoryWrite(
      Effect.gen(function* () {
        yield* assertForeignVerificationSource(source);
        yield* revalidateLedgerStoreLease(input.leaseOwner);
        const [same] =
          yield* sql`SELECT 1 FROM foreign_confirmed_frontier WHERE binding_digest=${Buffer.from(binding, "hex")}
        AND header_hash=${frontier.header_hash} AND utxos_root=${frontier.utxos_root} FOR UPDATE`;
        if (same === undefined)
          return yield* fail(
            "Confirmed ancestry frontier changed before projection",
          );
        if (!alreadyApplied) {
          yield* Confirmed.clearUTxOs(
            [...changed].map((key) => Buffer.from(key, "hex")),
          );
          yield* Confirmed.insertMultiple(
            [...changed].flatMap((key) => {
              const value = state.get(key);
              return value === undefined ? [] : [value];
            }),
          );
        }
        yield* sql`UPDATE foreign_confirmed_frontier SET header_hash=${Buffer.from(target.headerHash, "hex")},utxos_root=${target.utxoRoot}
        WHERE binding_digest=${Buffer.from(binding, "hex")}`;
        for (const value of forwards)
          yield* sql`UPDATE foreign_verified_segments SET confirmed=true
        WHERE binding_digest=${Buffer.from(binding, "hex")} AND header_hash=${Buffer.from(value.headerHash, "hex")}`;
      }),
    );
    yield* input.preparation.assertCurrent;
  });
