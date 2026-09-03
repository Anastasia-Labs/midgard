import { computeHash28 } from "@al-ft/midgard-core/codec/hash";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { buildCountedRoot } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { unsafeAdmitWatcherStateQueueObservationForReplayTest } from "../../src/indexers/authenticated-state-queue-observation.js";
import { watcherSha256CanonicalJson } from "../../src/storage/durable-store.js";
import {
  assertWatcherAuthenticatedReplayTranscript,
  createWatcherAuthenticatedReplayTranscript,
  replayWatcherAuthenticatedReplayTranscript,
  type WatcherAuthenticatedReplayTranscript,
  watcherAuthenticatedReplayTranscriptCborHex,
  watcherReplayRawRecordCborHex,
} from "../../src/verification/authenticated-replay-transcript.js";
import { watcherBlockReplayPriorState } from "../../src/verification/block-replay.js";
import {
  computeWatcherRuleBundleCommitment,
  makeWatcherCanonicalRuleBundle,
} from "../../src/verification/rule-bundle.js";
import { makeWatcherDeploymentAuthorityFixture } from "../support/deployment-authority-fixture.js";

const h28 = (byte: string): string => byte.repeat(28);
const h32 = (byte: string): string => byte.repeat(32);

const genuineFixture = async () => {
  const baseAuthority = makeWatcherDeploymentAuthorityFixture();
  const bundle = makeWatcherCanonicalRuleBundle({
    constructionIdentity: {
      manifestId: baseAuthority.result.manifestId,
      network: baseAuthority.result.network,
      releaseEvidenceDigest: baseAuthority.result.releaseEvidenceDigest,
      programCommitments: baseAuthority.result.programCommitments,
    },
    targetParameterSnapshot: { finalityDepth: 30 },
  });
  const ruleBundleCommitment = computeWatcherRuleBundleCommitment(bundle);
  const authority = makeWatcherDeploymentAuthorityFixture({
    ruleBundleCommitment,
    programCommitments: baseAuthority.result.programCommitments,
    releaseDigest: baseAuthority.result.releaseEvidenceDigest,
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
      { header, da_attestation: "Unattested" },
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

describe("production authenticated replay transcript V1", () => {
  it("canonically encodes exact raw replay records", () => {
    const left = watcherReplayRawRecordCborHex({
      z: [1n, new Uint8Array([2, 3])],
      a: { accepted: true, label: "W22" },
    });
    const right = watcherReplayRawRecordCborHex({
      a: { label: "W22", accepted: true },
      z: [1n, new Uint8Array([2, 3])],
    });

    expect(left).toBe(right);
  });

  it("rejects mutable code-bearing and noncanonical raw records", () => {
    const cycle: Record<string, unknown> = {};
    cycle.self = cycle;
    const accessor = Object.defineProperty({}, "finding", {
      enumerable: true,
      get: () => "caller-authored",
    });

    expect(() => watcherReplayRawRecordCborHex(cycle)).toThrow("cycle");
    expect(() => watcherReplayRawRecordCborHex(accessor)).toThrow(
      "exact data property",
    );
    expect(() => watcherReplayRawRecordCborHex({ value: -0 })).toThrow(
      "noncanonical number",
    );
    expect(() => watcherReplayRawRecordCborHex({ value: undefined })).toThrow(
      "exact data property",
    );
  });

  it("does not admit a structural transcript or mutated digest", () => {
    const structural = Object.freeze({
      schemaVersion:
        "midgard-watcher-production-authenticated-replay-transcript-v1",
      transcriptDigest: "00".repeat(32),
      decisionDigest: "11".repeat(32),
      finding: "caller-authored",
    }) as unknown as WatcherAuthenticatedReplayTranscript;

    expect(() =>
      assertWatcherAuthenticatedReplayTranscript(structural),
    ).toThrow("not admitted");
    expect(() =>
      assertWatcherAuthenticatedReplayTranscript({
        ...structural,
        transcriptDigest: "22".repeat(32),
      }),
    ).toThrow("not admitted");
  });

  it("constructs and offline re-admits only a byte-exact fresh W22/W24/W25 replay", async () => {
    const fixture = await genuineFixture();
    const transcript = await createWatcherAuthenticatedReplayTranscript({
      ...fixture.createInput,
      coordinate: { domain: "block", index: "0" },
    });
    assertWatcherAuthenticatedReplayTranscript(transcript);
    const persistedTranscriptCborHex =
      watcherAuthenticatedReplayTranscriptCborHex(transcript);
    const replayed = await replayWatcherAuthenticatedReplayTranscript({
      ...fixture.createInput,
      persistedTranscriptCborHex,
    });
    expect(replayed).toEqual(transcript);
    expect(watcherAuthenticatedReplayTranscriptCborHex(replayed)).toBe(
      persistedTranscriptCborHex,
    );

    await expect(
      replayWatcherAuthenticatedReplayTranscript({
        ...fixture.createInput,
        payloadEnvelopeCbor: Uint8Array.from([
          ...fixture.createInput.payloadEnvelopeCbor.slice(0, -1),
          fixture.createInput.payloadEnvelopeCbor.at(-1)! ^ 1,
        ]),
        persistedTranscriptCborHex,
      }),
    ).rejects.toThrow();
    await expect(
      replayWatcherAuthenticatedReplayTranscript({
        ...fixture.createInput,
        persistedTranscriptCborHex: watcherReplayRawRecordCborHex({
          ...transcript,
          coordinate: { domain: "transaction", index: "0" },
        }),
      }),
    ).rejects.toThrow("coordinate is outside exact replay");
    await expect(
      replayWatcherAuthenticatedReplayTranscript({
        ...fixture.createInput,
        persistedTranscriptCborHex: watcherReplayRawRecordCborHex({
          ...transcript,
          payloadSha256: "ff".repeat(32),
        }),
      }),
    ).rejects.toThrow("differs from fresh authenticated replay");
    await expect(
      replayWatcherAuthenticatedReplayTranscript({
        ...fixture.createInput,
        persistedTranscriptCborHex: watcherReplayRawRecordCborHex({
          ...transcript,
          decisionDigest: "aa".repeat(32),
          finding: "caller-authored",
        }),
      }),
    ).rejects.toThrow("differs from fresh authenticated replay");
    await expect(
      createWatcherAuthenticatedReplayTranscript({
        ...fixture.createInput,
        deploymentIdentity: {
          ...fixture.createInput.deploymentIdentity,
        },
        coordinate: { domain: "block", index: "0" },
      }),
    ).rejects.toThrow("invalid_field");
  });
});
