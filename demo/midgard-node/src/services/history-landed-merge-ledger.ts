import { assertDeploymentMarkerMatches } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Option } from "effect";

import * as Confirmed from "../database/confirmedLedger.js";
import { revalidateLedgerStoreLease } from "../database/mpfEngineState.js";
import * as Pending from "../database/pendingBlockFinalizations.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../mpf/index.js";
import {
  applyConfirmedLedgerDeltaChainTransaction,
  materializeConfirmedLedgerDeltaChain,
} from "../transactions/state-queue/confirmed-ledger-snapshot.js";
import { MAX_LANDED_MERGE_CATCH_UP } from "../transactions/state-queue/merge-to-confirmed-state.finalize-confirmed-merge-program.js";
import { canonicalObservation } from "../workers/commit-block-header.foreign-base-material.js";
import { decodeHeader } from "../workers/commit-block-header/da-payload.verify-payload-commitments.js";
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

const fail = (message: string) => Effect.fail(new Error(message));

/** Establish the confirmed parent before foreign verification during recovery.
 * A merge can land while its local finalization is blocked by the closed gate.
 * Only its checked ledger fold runs here; the runtime still owns its mutation
 * job, event consumption and native-owner observation after Ready.
 */
export const reconcileLandedMergeConfirmedLedger = (input: {
  readonly coverage: HistoryOwnerCoverage;
  readonly preparation: HistoryRecoveryPreparation;
  readonly leaseOwner: string;
}) =>
  Effect.gen(function* () {
    const source = {
      kind: "recovery" as const,
      binding: { token: input.preparation.token, coverage: input.coverage },
    };
    yield* assertForeignVerificationSource(source);
    const lucid = yield* Lucid;
    const contracts = yield* MidgardContracts;
    const identity = yield* ContractDeploymentIdentity;
    const nodes = yield* fetchCanonicalStateQueueNodesProgram(
      lucid.api,
      contracts.stateQueue,
    );
    const confirmedNode = nodes.find((node) => node.datum.key === "Empty");
    if (confirmedNode === undefined)
      return yield* fail("Landed merge recovery requires the canonical root");
    const target = (yield* SDK.getConfirmedStateFromStateQueueDatum(
      confirmedNode.datum,
    )).data;
    const entries = yield* Confirmed.retrieve;
    const actualRoot = yield* computeLedgerMpfRootFromLedgerEntries(entries);
    if (actualRoot === target.utxoRoot) return;
    const targetJournal = yield* Pending.retrieveByHeaderHash(
      Buffer.from(target.headerHash, "hex"),
    );
    // A foreign confirmed parent retains its separate verified-segment owner.
    if (Option.isNone(targetJournal)) return;
    if (identity.deploymentMarker === undefined)
      return yield* fail(
        "Landed merge recovery requires the deployment marker",
      );
    const records = new Map<string, Pending.Record>();
    let hash = target.headerHash;
    let root = target.utxoRoot;
    for (;;) {
      if (records.size >= MAX_LANDED_MERGE_CATCH_UP || records.has(hash))
        return yield* fail(
          "Landed merge recovery ancestry is cyclic or exceeds its bound",
        );
      const journal = yield* Pending.retrieveByHeaderHash(
        Buffer.from(hash, "hex"),
      );
      if (Option.isNone(journal))
        return yield* fail(
          "Landed merge recovery lacks its local ancestor journal",
        );
      const record = journal.value;
      yield* Effect.try(() =>
        assertDeploymentMarkerMatches(
          identity.deploymentMarker!,
          {
            schemaVersion:
              record[Pending.Columns.DEPLOYMENT_MARKER_SCHEMA_VERSION],
            manifestId: record[Pending.Columns.DEPLOYMENT_MANIFEST_ID],
          },
          "landed merge recovery ancestry",
        ),
      );
      const header = yield* decodeHeader(record);
      if (
        record[Pending.Columns.STATUS] !== Pending.Status.Finalized ||
        record[Pending.Columns.CONSENSUS_PROFILE_ID] !==
          identity.consensusProfile.profileId ||
        !record[Pending.Columns.HEADER_HASH].equals(Buffer.from(hash, "hex")) ||
        (yield* SDK.hashBlockHeader(header)) !== hash ||
        header.utxosRoot !== root ||
        record[Pending.Columns.EXPECTED_UTXOS_ROOT] !== root ||
        header.prevHeaderHash !==
          record[Pending.Columns.BASE_TAIL_HEADER_HASH].toString("hex") ||
        header.prevUtxosRoot !== record[Pending.Columns.BASE_UTXOS_ROOT]
      )
        return yield* fail(
          "Landed merge recovery journal differs from its canonical header/parent/root",
        );
      records.set(hash, record);
      root = header.prevUtxosRoot;
      hash = header.prevHeaderHash;
      if (root === actualRoot) break;
    }
    const snapshot = yield* materializeConfirmedLedgerDeltaChain({
      record: targetJournal.value,
      confirmedEntries: entries,
      retrieveParent: (headerHash) =>
        Effect.succeed(
          Option.fromNullable(records.get(headerHash.toString("hex"))),
        ),
    });
    if (snapshot.root !== target.utxoRoot)
      return yield* fail(
        "Landed merge recovery delta chain differs from the canonical root",
      );
    yield* input.preparation.assertCurrent;
    const latest = yield* fetchCanonicalStateQueueNodesProgram(
      lucid.api,
      contracts.stateQueue,
    );
    if (
      JSON.stringify(canonicalObservation(latest)) !==
      JSON.stringify(canonicalObservation(nodes))
    )
      return yield* fail(
        "Canonical queue changed during landed merge recovery",
      );
    yield* withHistoryWrite(
      Effect.gen(function* () {
        yield* assertForeignVerificationSource(source);
        yield* revalidateLedgerStoreLease(input.leaseOwner);
        yield* applyConfirmedLedgerDeltaChainTransaction(snapshot);
        yield* input.preparation.assertCurrent;
      }),
    );
    yield* input.preparation.assertCurrent;
  });
