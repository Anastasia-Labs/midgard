import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";
import { expect, vi } from "vitest";

import { foreignRetainedDaInsert } from "../src/da/foreign-retained-da.js";
import {
  DaPayloadsDB,
  DepositsDB,
  PendingBlockFinalizationsDB,
} from "../src/database/index.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../src/mpf/index.js";
import {
  encodeEventToStepValueCbor,
  encodeTransitionIntegerCbor,
  encodeTransitionStepCbor,
} from "../src/mpf/transition-cbor.js";
import type { HistoryOwnerCoverage } from "../src/services/event-history-owner.js";
import { HistoryProducer } from "../src/services/event-history-producer.js";
import {
  HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS,
  historyCommitTimingBudget,
  historyEligibilityHorizon,
} from "../src/services/history-commit-window.js";
import {
  Lucid,
  MidgardContracts,
  NodeConfig,
  type NodeConfigDep,
} from "../src/services/index.js";
import { fetchCanonicalStateQueueNodesProgram } from "../src/services/state-queue-topology.js";
import { materializeConfirmedLedgerSnapshot } from "../src/transactions/state-queue/confirmed-ledger-snapshot.js";
import { verifyForeignCommitBase } from "../src/workers/commit-block-header.verify-foreign-base.js";
import { buildUnsignedCommitTx } from "../src/workers/commit-block-header/build-unsigned-tx.js";
import { computeDaPayloadRoots } from "../src/workers/commit-block-header/da-payload.js";
import { makeEventCommitments } from "../src/workers/commit-block-header/transition-commitments.js";
import { resolveHistoryCommitEndTime } from "../src/workers/utils/commit-end-time.js";
import {
  advanceEmulatorPastUnixTime,
  alignCommitSchedulerBeforeTestWorker,
  type EmulatorFixture,
  fetchLatestCommittedBlock,
  type makeLucidRuntimeService,
  submitDepositWithDiagnostics,
} from "./deposit-flow-emulator-shared.js";
import type { openHistoryProductionOwnerLifecycle } from "./helpers/history-production-owner-lifecycle.js";

export const submitOwnedDeposit = async (
  owner: Awaited<ReturnType<typeof openHistoryProductionOwnerLifecycle>>,
  lovelace: bigint,
) => {
  const { fixture } = owner;
  const submittedTxHash = await submitDepositWithDiagnostics(fixture, {
    l2Address: await fixture.depositorLucid.wallet().address(),
    l2Datum: null,
    lovelace,
    additionalAssets: {},
  });
  const visible = await Effect.runPromise(
    SDK.fetchDepositUTxOsProgram(
      fixture.depositorLucid,
      SDK.eventHistoryDeploymentFromContracts(
        SDK.requireEventHistoryContracts(fixture.contracts).deposit,
      ),
    ),
  );
  await advanceEmulatorPastUnixTime(
    fixture,
    Math.max(...visible.map((entry) => Number(entry.facts.inclusion_time))),
  );
  await owner.synchronize();
  const { coverage } = await owner.command(HistoryProducer);
  return { submittedTxHash, coverage };
};

/** A genuinely foreign header has no local finalization journal. Build its
 * deposit transitions from ordinary node material and retain its exact DA;
 * the production verifier still authenticates the independent source census. */
export const submitIndependentDepositBlock = (
  fixture: EmulatorFixture,
  lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>,
  parentHeaderHash: string,
  endTimeMs: number,
  config: NodeConfigDep,
) =>
  Effect.gen(function* () {
    const latest = yield* Effect.promise(() =>
      fetchLatestCommittedBlock(lucidService.api, fixture.contracts),
    );
    const parent = yield* SDK.getHeaderFromStateQueueDatum(latest.datum);
    const journal = yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
      Buffer.from(parentHeaderHash, "hex"),
    );
    if (Option.isNone(journal))
      return yield* Effect.fail(
        new Error("Missing independently verified local parent journal"),
      );
    const snapshot = yield* materializeConfirmedLedgerSnapshot(journal.value);
    const ledger = new Map(
      snapshot.entries.map((entry) => [
        entry.outref.toString("hex"),
        entry.output,
      ]),
    );
    const deposits = yield* DepositsDB.retrievePendingHeaderEntriesUpTo(
      new Date(endTimeMs),
    );
    const counts: SDK.DaPayloadCounts = {
      depositCount: BigInt(deposits.length),
      withdrawalCount: 0n,
      forcedTransactionCount: 0n,
      l2TransactionCount: 0n,
      totalEventCount: BigInt(deposits.length),
      transitionStepCount: BigInt(deposits.length),
      validationTraceCount: 0n,
    };
    const steps: SDK.DaPayloadEntry[] = [];
    const mapping: SDK.DaPayloadEntry[] = [];
    for (const [index, deposit] of deposits.entries()) {
      const preRoot = yield* computeLedgerMpfRootFromLedgerEntries(
        [...ledger].map(([key, output]) => ({
          outref: Buffer.from(key, "hex"),
          output,
        })),
      );
      const entry = yield* DepositsDB.toLedgerEntry(deposit);
      ledger.set(entry.outref.toString("hex"), entry.output);
      const postRoot = yield* computeLedgerMpfRootFromLedgerEntries(
        [...ledger].map(([key, output]) => ({
          outref: Buffer.from(key, "hex"),
          output,
        })),
      );
      const event: SDK.EventKey = {
        DepositEventKey: {
          deposit_id: Data.from(
            deposit[DepositsDB.Columns.ID].toString("hex"),
            SDK.OutputReference,
          ),
        },
      };
      steps.push([
        encodeTransitionIntegerCbor(BigInt(index)).toString("hex"),
        encodeTransitionStepCbor({
          schema_version: 1n,
          step_index: BigInt(index),
          event_key: event,
          phase: "Deposit",
          pre_utxos_root: preRoot,
          post_utxos_root: postRoot,
        }).toString("hex"),
      ]);
      mapping.push([
        Data.to(event, SDK.EventKey),
        encodeEventToStepValueCbor({
          step_index: BigInt(index),
          phase: "Deposit",
        }).toString("hex"),
      ]);
    }
    const payload: SDK.DaPayload = {
      version: SDK.DA_PAYLOAD_VERSION,
      block_body: {
        header_hash: "00".repeat(28),
        header: parent,
        utxos: [...ledger].map(([key, output]) => [
          key,
          output.toString("hex"),
        ]),
        deposits: deposits.map((entry) => [
          entry[DepositsDB.Columns.ID].toString("hex"),
          entry[DepositsDB.Columns.INFO].toString("hex"),
        ]),
        withdrawals: [],
        forced_transactions: [],
        transactions: [],
        transaction_preimages: [],
        forced_transaction_preimages: [],
        cek_program_material: [],
        transition_trace: steps,
        event_to_step: mapping,
        validation_traces: [],
        validation_trace_witnesses: [],
        counts,
      },
    };
    const roots = yield* computeDaPayloadRoots(payload);
    const commitments = yield* makeEventCommitments(roots, counts, {
      validationTracesRoot: roots.validationTracesRoot,
      validationTraceCount: 0n,
    });
    const built = yield* buildUnsignedCommitTx(
      fixture.contracts,
      latest,
      roots.utxosRoot,
      roots.transactionsRoot,
      roots.depositsRoot,
      roots.withdrawalsRoot,
      commitments,
      MIDGARD_CONSENSUS_PROFILE,
      new Date(endTimeMs),
    );
    if ("dueWork" in built)
      return yield* Effect.fail(
        new Error("Independent commit still has scheduler due work"),
      );
    payload.block_body.header = built.newHeader;
    payload.block_body.header_hash = built.newHeaderHash;
    const bytes = yield* Effect.tryPromise(() =>
      wrapDaPayload(Buffer.from(Data.to(payload, SDK.DaPayload), "hex"), {
        mode: "identity",
      }),
    );
    yield* DaPayloadsDB.upsertAvailable(
      foreignRetainedDaInsert(built.newHeaderHash, built.newHeader, bytes),
    );
    const submittedTxHash = yield* built.signAndSubmitProgram;
    yield* Effect.promise(() => lucidService.api.awaitTx(submittedTxHash));
    return {
      headerHash: built.newHeaderHash,
      blockEndTimeMs: built.blockEndTimeMs,
      payload,
      bytes,
    };
  }).pipe(
    Effect.provideService(Lucid, lucidService as never),
    Effect.provideService(MidgardContracts, fixture.contracts as never),
    Effect.provideService(NodeConfig, config),
  );

/** An append spends and replaces its parent UTxO. Reverify the foreign header
 * using its current canonical out-ref instead of its pre-append observation. */
export const verifyCurrentForeignParent = (
  lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>,
  contracts: SDK.MidgardValidators,
  headerHash: string,
) =>
  Effect.gen(function* () {
    const nodes = yield* fetchCanonicalStateQueueNodesProgram(
      lucidService.api,
      contracts.stateQueue,
    );
    for (const node of nodes) {
      if (node.datum.key === "Empty") continue;
      const hash = yield* SDK.hashBlockHeader(
        yield* SDK.getHeaderFromStateQueueDatum(node.datum),
      );
      if (hash === headerHash) return yield* verifyForeignCommitBase(node);
    }
    return yield* Effect.fail(
      new Error("Authenticated foreign parent is absent from the queue"),
    );
  });

export const assertRetainedForeignDepositEvidence = (
  headerHash: string,
  utxosRoot: string,
) =>
  Effect.gen(function* () {
    const retained = yield* DaPayloadsDB.retrieveByHeaderHash(
      Buffer.from(headerHash, "hex"),
    );
    expect(Option.isSome(retained)).toBe(true);
    if (Option.isSome(retained)) {
      expect(retained.value[DaPayloadsDB.Columns.UTXOS_ROOT]).toBe(utxosRoot);
      expect(retained.value[DaPayloadsDB.Columns.DEPOSIT_COUNT]).toBe(1n);
      expect(
        retained.value[DaPayloadsDB.Columns.PAYLOAD_CBOR].length,
      ).toBeGreaterThan(0);
    }
  });

/** Select an inclusive end using the same source horizon and slot floors as
 * production, leaving a witness reserve after scheduler appointment. */
export const alignIndependentDepositCommit = (
  fixture: EmulatorFixture,
  lucidService: Awaited<ReturnType<typeof makeLucidRuntimeService>>,
  latestEndTime: number,
  coverage: HistoryOwnerCoverage,
) =>
  Effect.gen(function* () {
    for (let attempt = 1; attempt <= 3; attempt += 1) {
      const before = yield* lucidService.submitSlotSnapshot();
      const fit = resolveHistoryCommitEndTime({
        lucid: lucidService.api,
        currentSlot: before.currentSlot,
        latestEndTime,
        nowMs: before.observedAtMs,
        minimumFutureBufferMs: HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS,
        eventEndTimeMs: Math.min(
          historyEligibilityHorizon(coverage),
          before.observedAtMs +
            HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS +
            60_000,
        ),
      });
      if (fit.status !== "fits")
        return yield* Effect.fail(new Error(fit.reason));
      const endTimeMs = fit.resolvedEndTime - 1;
      yield* Effect.promise(() =>
        alignCommitSchedulerBeforeTestWorker({
          fixture,
          lucidService,
          targetEndTimeMs: endTimeMs,
        }),
      );
      const after = yield* lucidService.submitSlotSnapshot();
      vi.setSystemTime(new Date(after.observedAtMs));
      if (
        historyCommitTimingBudget({
          checkpoint: "pre_witness",
          resolvedEndTimeMs: endTimeMs,
          nowMs: after.observedAtMs,
        }).satisfied
      ) {
        expect(endTimeMs).toBeLessThanOrEqual(
          historyEligibilityHorizon(coverage),
        );
        return endTimeMs;
      }
    }
    return yield* Effect.fail(
      new Error(
        "T2 independent scheduler alignment eroded the witness reserve",
      ),
    );
  });
