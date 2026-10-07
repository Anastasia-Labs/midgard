import {
  decodeMidgardSpendInputItem,
  decodeMidgardTxOutput,
  encodeMidgardAddressText,
} from "@al-ft/midgard-core/codec";
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Data, Emulator, Lucid as makeLucid } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect, vi } from "vitest";

import * as Adoptions from "../../src/database/foreignNativeAdoptions.js";
import type { DepositEntry } from "../../src/database/mempoolLedger.js";
import type { MinimalEntry } from "../../src/database/utils/ledger.js";
import type { HistoryRecoveryPreparation } from "../../src/services/event-history-recovery.js";
import * as ConfirmedRecovery from "../../src/services/foreign-confirmed-ledger.js";
import { recoverForeignNativeAdoptions } from "../../src/services/foreign-native-adoption.js";
import { foreignAdoptionProjection } from "../../src/services/foreign-native-adoption-projection.js";
import { assertForeignVerificationSource } from "../../src/services/foreign-verification-source.js";
import { Lucid } from "../../src/services/lucid.js";
import {
  ContractDeploymentIdentity,
  MidgardContracts,
} from "../../src/services/midgard-contracts.js";
import type {
  NativeMpfOwnerService,
  PersistedNativeMpfReplay,
} from "../../src/services/mpf-native-owner/protocol.js";
import { encodeNativeMpfEventLog } from "../../src/services/mpf-native-owner/service.js";
import {
  digest,
  EVENT_LOG_DIGEST_DOMAIN,
} from "../../src/services/mpf-native-owner/service.normalize-owner-options.js";
import * as Topology from "../../src/services/state-queue-topology.js";
import * as Verification from "../../src/workers/commit-block-header.verify-foreign-base.js";
import { countsFromLengths, headerFor } from "../da-payload.record.js";
import { hash, run } from "../event-history-recovery-plans.registration.js";
import { loadRealMidgardContractsForTest } from "../helpers/real-midgard-contracts.js";

/** Recovery component: actual source SQL authority and coordinator; complete
 * L1 semantic verification and native RPC are explicit controlled boundaries.
 */
export const adoptionRecoveryFixture = async (
  initial: Verification.VerifiedForeignCommitBase,
  replay: PersistedNativeMpfReplay,
) => {
  let base = initial;
  let durableRoot = replay.baseRoot;
  let crashAfterPromotion = false;
  const phases: string[] = [];
  const handle = {
    ownerEpoch: Buffer.alloc(16, 1),
    generationId: Buffer.alloc(16, 2),
    baseRoot: replay.baseRoot,
  };
  const owner: NativeMpfOwnerService = {
    createWorkerPort: () => {
      throw new Error("Recovery cannot start a worker");
    },
    terminalFailure: () => undefined,
    close: async () => undefined,
    fork: vi.fn(async () => {
      phases.push("fork");
      return handle;
    }),
    applyEvents: vi.fn(async (_handle, log) => ({
      handle,
      candidateRoot: base.root,
      eventRoots: Array.from({ length: replay.eventCount }, () => base.root),
      eventLogDigest: digest(EVENT_LOG_DIGEST_DOMAIN, log).toString("hex"),
      proofArenaDurationNs: 0,
      mutationDurationNs: 0,
    })),
    discard: vi.fn(async () => {
      phases.push("discard");
    }),
    promote: vi.fn(async () => {
      phases.push(
        `promote:${(await run(Adoptions.unresolved(base.history.coverage.bindingDigest)))[0]!.state}`,
      );
      durableRoot = base.root;
      if (crashAfterPromotion) {
        crashAfterPromotion = false;
        throw new Error("Modeled process crash after native promotion");
      }
    }),
    recover: vi.fn(async (log) => {
      phases.push(
        `recover:${(await run(Adoptions.unresolved(base.history.coverage.bindingDigest)))[0]!.state}`,
      );
      durableRoot = log.candidateRoot;
    }),
    restoreCanonicalRoot: vi.fn(async (plan) => {
      phases.push(
        `restore:${(await run(Adoptions.unresolved(base.history.coverage.bindingDigest)))[0]!.state}`,
      );
      if (durableRoot !== plan.expectedRoot && durableRoot !== plan.targetRoot)
        throw new Error("Native recovery CAS mismatch");
      durableRoot = plan.targetRoot;
    }),
    diagnostics: vi.fn(async () => ({
      ownerEpoch: handle.ownerEpoch,
      durableRoot,
      residentNodes: 0,
      residentEdges: 0,
      residentBytes: 0,
      activeGenerations: 0,
      generatedNodes: 0,
      generatedBytes: 0,
      rssBytes: 0,
      peakRssBytes: 0,
      childRestarts: 0,
    })),
  };
  const contracts = await loadRealMidgardContractsForTest({
    txHash: hash(900),
    outputIndex: 0,
  });
  const api = await makeLucid(new Emulator([]), "Custom");
  const service = Lucid.make({
    api,
    referenceScriptsApi: api,
    operatorMainAddress: contracts.stateQueue.spendingScriptAddress,
    operatorMergeAddress: contracts.stateQueue.spendingScriptAddress,
    referenceScriptsWalletAddress: contracts.stateQueue.spendingScriptAddress,
    referenceScriptsAddress: contracts.stateQueue.spendingScriptAddress,
    submitSlotSnapshot: () =>
      Effect.fail(new Error("Recovery fixture cannot submit")),
    switchToOperatorsMainWallet: Effect.void,
    switchToOperatorsMergingWallet: Effect.void,
    switchToReferenceScriptWallet: Effect.void,
  });
  const header = base.importedBlocks.at(-1)
    ? Data.from(base.importedBlocks.at(-1)!.headerCbor, SDK.Header)
    : headerFor(
        {
          utxosRoot: base.root,
          transactionsRoot: hash(0),
          depositsRoot: hash(0),
          withdrawalsRoot: hash(0),
          forcedTransactionsRoot: hash(0),
          transitionTraceRoot: hash(0),
          eventToStepRoot: hash(0),
          validationTracesRoot: hash(0),
        },
        countsFromLengths({}),
      );
  const node: SDK.StateQueueUTxO = {
    utxo: {
      txHash: hash(99),
      outputIndex: 1,
      address: contracts.stateQueue.spendingScriptAddress,
      assets: {
        lovelace: 2_000_000n,
        [contracts.stateQueue.policyId +
        SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX +
        base.headerHash]: 1n,
      },
    },
    datum: {
      key: { Key: { key: base.headerHash } },
      next: "Empty",
      data: Data.castTo(
        { header, da_attestation: SDK.NO_DA_ATTESTATION, proven_fraud: null },
        SDK.StateQueueNode,
      ),
    },
    assetName: SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + base.headerHash,
  };
  node.utxo.datum = SDK.encodeLinkedListNodeView(node.datum);
  // This component models a foreign confirmed parent, with no local journal
  // for landed-merge repair. Keep its real canonical-root boundary present;
  // complete foreign semantic verification remains the controlled seam below.
  const root: SDK.StateQueueUTxO = {
    utxo: {
      txHash: hash(99),
      outputIndex: 0,
      address: contracts.stateQueue.spendingScriptAddress,
      assets: {
        lovelace: 2_000_000n,
        [contracts.stateQueue.policyId + SDK.STATE_QUEUE_ROOT_ASSET_NAME]: 1n,
      },
    },
    datum: {
      key: "Empty",
      next: { Key: { key: base.headerHash } },
      data: SDK.castConfirmedStateToData({
        headerHash: header.prevHeaderHash,
        prevHeaderHash: SDK.GENESIS_HEADER_HASH,
        utxoRoot: header.prevUtxosRoot,
        startTime: 0n,
        endTime: header.startTime,
        protocolVersion: header.protocolVersion,
      }) as never,
    },
    assetName: SDK.STATE_QUEUE_ROOT_ASSET_NAME,
  };
  root.utxo.datum = SDK.encodeLinkedListNodeView(root.datum);
  vi.spyOn(Topology, "fetchCanonicalStateQueueNodesProgram").mockReturnValue(
    Effect.succeed([root, node]),
  );
  const verify = vi
    .spyOn(Verification, "verifyForeignCommitBase")
    .mockImplementation(() => Effect.succeed(base));
  vi.spyOn(
    ConfirmedRecovery,
    "reconcileForeignConfirmedLedger",
  ).mockReturnValue(Effect.succeed(undefined));
  vi.spyOn(Verification, "revalidateForeignCommitBase").mockImplementation(
    (accepted) =>
      assertForeignVerificationSource({
        kind: "recovery",
        binding: accepted.history,
      }),
  );
  const recover = (preparation: HistoryRecoveryPreparation) =>
    run(
      recoverForeignNativeAdoptions({
        owner,
        ownerBinarySha256: replay.ownerBinarySha256,
        coverage: base.history.coverage,
        preparation,
      }).pipe(
        Effect.provideService(Lucid, service),
        Effect.provideService(
          MidgardContracts,
          MidgardContracts.make({
            ...contracts,
            consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          }),
        ),
        Effect.provideService(
          ContractDeploymentIdentity,
          ContractDeploymentIdentity.make({
            kind: "manifest",
            manifestId: base.history.token.deploymentIdentity,
            deploymentMarker: makeDeploymentMarker(
              base.history.token.deploymentIdentity,
            ),
            consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          }),
        ),
      ),
    );
  return {
    owner,
    phases,
    verify,
    recover,
    root: () => durableRoot,
    crashAfterPromotion: () => {
      crashAfterPromotion = true;
    },
    rebind: (current: Verification.VerifiedForeignCommitBase) => {
      base = current;
    },
  };
};

export const assertRawOutputReplayRefused = (
  replay: PersistedNativeMpfReplay,
  entry: MinimalEntry,
) => {
  const rawLog = encodeNativeMpfEventLog(replay.baseRoot, [
    [{ type: "insert", key: entry.outref, value: entry.output }],
  ]);
  expect(() =>
    foreignAdoptionProjection(
      {
        ...replay,
        eventLog: rawLog,
        eventLogDigest: digest(EVENT_LOG_DIGEST_DOMAIN, rawLog).toString("hex"),
      },
      [entry],
    ),
  ).toThrow("Foreign adoption projection differs from the verified ledger");
};

/** Modeled authenticated deposit membership for the SQL coordinator boundary. */
export const withDepositMembership = async (
  base: Verification.VerifiedForeignCommitBase,
) => {
  const ledger = base.entries[0]!;
  const input = decodeMidgardSpendInputItem(ledger.outref);
  const id = Buffer.from(
    Data.to(
      {
        transactionId: Buffer.from(input.txId).toString("hex"),
        outputIndex: BigInt(input.outputIndex),
      },
      SDK.OutputReference,
    ),
    "hex",
  );
  const entry: DepositEntry = {
    tx_id: computeHash32(id),
    outref: ledger.outref,
    output: ledger.output,
    address: encodeMidgardAddressText(
      decodeMidgardTxOutput(ledger.output).address,
    ),
    source_event_id: id,
  };
  expect(entry.tx_id.equals(Buffer.from(input.txId))).toBe(false);
  await run(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      // Explicit modeled follower admission of the deposit source.
      const key = Buffer.alloc(32, 72);
      const origin = Buffer.concat([Buffer.from(input.txId), Buffer.alloc(2)]);
      yield* sql`INSERT INTO l1_event_keys(kind,key,origin_outref,first_canonical_slot)
      VALUES ('deposit',${key},${origin},0)`;
      yield* sql`INSERT INTO deposits_utxos(event_id,event_info,inclusion_time,deposit_l1_tx_hash,ledger_tx_id,
      ledger_output,ledger_address,status,l1_event_key,l1_origin_outref)
      VALUES (${id},${Buffer.from("80", "hex")},NOW(),${Buffer.from(input.txId)},${entry.tx_id},${entry.output},${entry.address},'awaiting',${key},${origin})`;
    }),
  );
  const block = base.importedBlocks[0]!;
  return {
    base: {
      ...base,
      importedBlocks: [
        {
          ...block,
          memberships: {
            deposits: [{ id, entry }],
            forcedTransactions: [],
            withdrawals: [],
          },
        },
      ],
    },
    assertProjected: async () => {
      const rows = await run(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          return yield* sql`SELECT tx_id,outref,output,address,source_event_id FROM mempool_ledger WHERE outref=${entry.outref}`;
        }),
      );
      expect(rows).toEqual([entry]);
    },
  };
};
