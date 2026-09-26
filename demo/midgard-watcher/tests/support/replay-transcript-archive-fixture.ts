import { computeHash28 } from "@al-ft/midgard-core/codec/hash";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { buildCountedRoot } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { unsafeAdmitWatcherStateQueueObservationForReplayTest } from "../../src/indexers/authenticated-state-queue-observation.js";
import { watcherSha256CanonicalJson } from "../../src/storage/durable-store.js";
import { watcherBlockReplayPriorState } from "../../src/verification/block-replay.js";
import {
  computeWatcherRuleBundleCommitment,
  makeWatcherCanonicalRuleBundle,
} from "../../src/verification/rule-bundle.js";
import { makeWatcherDeploymentAuthorityFixture } from "./deployment-authority-fixture.js";

const h28 = (byte: string): string => byte.repeat(28);
const h32 = (byte: string): string => byte.repeat(32);

export const makeWatcherTranscriptArchiveFixture = async () => {
  const baseAuthority = makeWatcherDeploymentAuthorityFixture();
  const bundle = makeWatcherCanonicalRuleBundle({
    constructionIdentity: {
      manifestId: baseAuthority.result.manifestId,
      network: baseAuthority.result.network,
      blueprintHash: baseAuthority.result.blueprintHash,
      programCommitments: baseAuthority.result.programCommitments,
    },
    targetParameterSnapshot: { finalityDepth: 30 },
  });
  const ruleBundleCommitment = computeWatcherRuleBundleCommitment(bundle);
  const authority = makeWatcherDeploymentAuthorityFixture({
    ruleBundleCommitment,
    programCommitments: baseAuthority.result.programCommitments,
    blueprintHash: baseAuthority.result.blueprintHash,
  });
  expect(authority.result.manifestId).toBe(baseAuthority.result.manifestId);

  const emptyRoot = async (domain: SDK.RootDomain): Promise<string> =>
    (await buildCountedRoot(domain, [])).root;
  const ledger = await watcherBlockReplayPriorState([]);
  const counts = Object.freeze({
    withdrawalCount: 0n,
    forcedTransactionCount: 0n,
    l2TransactionCount: 0n,
    depositCount: 0n,
    totalEventCount: 0n,
    transitionStepCount: 0n,
    validationTraceCount: 0n,
  });
  const header: SDK.Header = Object.freeze({
    prevUtxosRoot: ledger.root,
    utxosRoot: ledger.root,
    withdrawalsRoot: await emptyRoot(SDK.ROOT_DOMAINS.withdrawals),
    forcedTransactionsRoot: await emptyRoot(
      SDK.ROOT_DOMAINS.forcedTransactionsV1,
    ),
    transactionsRoot: await emptyRoot(SDK.ROOT_DOMAINS.transactionsV1),
    depositsRoot: await emptyRoot(SDK.ROOT_DOMAINS.deposits),
    transitionTraceRoot: await emptyRoot(SDK.ROOT_DOMAINS.transitionTrace),
    eventToStepRoot: await emptyRoot(SDK.ROOT_DOMAINS.eventToStep),
    validationTracesRoot: await emptyRoot(SDK.ROOT_DOMAINS.validationTraces),
    ...counts,
    startTime: 1n,
    endTime: 2n,
    blockSlot: 0n,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: 0n,
    prevHeaderHash: h28("31"),
    operatorVkey: h28("32"),
    protocolVersion: BigInt(bundle.protocolVersion),
  });
  const headerCborHex = Data.to(header, SDK.Header);
  const headerHash = computeHash28(Buffer.from(headerCborHex, "hex")).toString(
    "hex",
  );
  const payload: SDK.DaPayload = {
    version: SDK.DA_PAYLOAD_VERSION,
    block_body: {
      header_hash: headerHash,
      header,
      utxos: [],
      withdrawals: [],
      forced_transactions: [],
      transactions: [],
      deposits: [],
      transition_trace: [],
      event_to_step: [],
      transaction_preimages: [],
      forced_transaction_preimages: [],
      cek_program_material: [],
      validation_traces: [],
      validation_trace_witnesses: [],
      counts,
    },
  };
  const payloadEnvelopeCbor = await wrapDaPayload(
    SDK.encodeDaPayload(payload),
    { mode: "identity" },
  );
  const headerObservation = {
    headerHash,
    headerCborHex,
    stateQueueNodeCborHex: Data.to(
      { proven_fraud: null, header, da_attestation: "Unattested" },
      SDK.StateQueueNode,
    ),
    linkedListDatumCborHex: "80",
    daAvailability: "Unattested" as const,
    queueOutRef: `${h32("41")}#1`,
    nextHeaderHash: null,
    observedTransactionHash: h32("42"),
    observedBlockHash: h32("43"),
    observedSlot: "4242",
    observedBlockNo: "9000",
    observedChainPointId: h32("44"),
    finalityDepth: "30",
  };
  const canonicalObservation = {
    schemaVersion: "midgard-watcher-production-state-queue-observation-v1",
    deploymentIdentityDigest: authority.result.manifestId,
    protocolScriptAuthorityDigest: h32("45"),
    stateQueuePolicyId: h28("46"),
    hubOraclePolicyId: h28("47"),
    nativePoint: {
      blockHash: h32("43"),
      parentBlockHash: h32("40"),
      slot: "4242",
      blockNo: "9000",
      chainPointId: h32("44"),
      finalityDepth: "30",
    },
    sourceId: "local-kupmios:test",
    previousObservationDigest: null,
    checkpoints: [],
    finalizedQueue: [
      { headerHash: null, outRef: `${h32("41")}#0` },
      { headerHash, outRef: headerObservation.queueOutRef },
    ],
    finalizedHeaders: [headerObservation],
    finalizedCorrectionLock: null,
    correctionLockWitnesses: [],
  };
  const stateQueueObservation =
    unsafeAdmitWatcherStateQueueObservationForReplayTest({
      ...canonicalObservation,
      observationDigest: watcherSha256CanonicalJson(canonicalObservation),
    });
  const admittedHeader = stateQueueObservation.finalizedHeaders[0]!;
  const daProvenance: SDK.EvidenceProvenance = Object.freeze({
    trustClass: "public_or_permissionless_da",
    sourceId: "watcher-da-peer-1",
    grade: "security",
  });
  const createInput = Object.freeze({
    coordinate: Object.freeze({ domain: "block" as const, index: "0" }),
    deploymentIdentity: authority.result,
    stateQueueObservation,
    header: admittedHeader,
    payloadEnvelopeCbor,
    daProvenance,
    priorState: Object.freeze([]),
    ruleBundle: bundle,
    ruleBundleCommitment,
    eventAuthorities: Object.freeze([]),
  });
  return Object.freeze({ createInput });
};
