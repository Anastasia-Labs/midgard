import { computeHash28 } from "@al-ft/midgard-core/codec/hash";
import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { reconstructDaPayload } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";

import { watcherSha256CanonicalJson } from "../storage/durable-store.js";
import { WATCHER_AUTHENTICATED_REPLAY_TRANSCRIPT } from "./authenticated-replay-transcript.assert-raw-cbor-value.js";
import type { WatcherAuthenticatedReplayTranscript } from "./authenticated-replay-transcript.js";
import {
  watcherBlockReplayCommittedSteps,
  watcherBlockReplayEventAuthorityManifest,
  watcherBlockReplayPriorState,
} from "./block-replay.js";
import { type WatcherCommittedEventClaim } from "./event-claims.js";
import {
  HEADER_OBSERVATION_KEYS,
  INCLUSION_KEYS,
  parseAuthority,
  provenance,
  TRANSCRIPT_KEYS,
  type WatcherReplayTranscriptRecords,
} from "./replay-transcript-records.parse-authority.js";
import {
  parseW22,
  parseW24,
  parseW25,
} from "./replay-transcript-records.parse-w25.js";
import {
  count,
  decodeWatcherReplayRawRecord,
  equal,
  HEX,
  HEX28,
  HEX32,
  list,
  NATURAL,
  nullableText,
  OUT_REF,
  record,
  requireCondition,
  sha256,
  stringFields,
  text,
} from "./replay-transcript-records.w25-keys.js";
import {
  computeWatcherRuleBundleCommitment,
  parseWatcherRuleBundle,
} from "./rule-bundle.js";

/** Checks historical bytes for integrity and semantic consistency only. Original
 * source provenance is descriptive; current authority must be acquired separately.
 */
export const readWatcherReplayTranscriptRecords = async (
  cborHex: string,
  minimumConfirmationDepth: number,
): Promise<WatcherReplayTranscriptRecords> => {
  const t = record(
    decodeWatcherReplayRawRecord(cborHex),
    TRANSCRIPT_KEYS,
    "transcript",
  );
  equal(
    t.schemaVersion,
    WATCHER_AUTHENTICATED_REPLAY_TRANSCRIPT,
    "transcript schema",
  );
  stringFields(
    t,
    [
      "deploymentFingerprint",
      "stateQueueObservationDigest",
      "payloadEnvelopeSha256",
      "payloadSha256",
      "ruleBundleCommitment",
      "blockReplayResultDigest",
      "transcriptDigest",
    ],
    "transcript",
    HEX32,
  );
  text(t.headerHash, "transcript headerHash", HEX28);
  const { transcriptDigest, ...transcriptMaterial } = t;
  equal(
    transcriptDigest,
    computeDeploymentManifestJsonDigest(transcriptMaterial),
    "transcript digest",
  );
  const inclusion = record(t.inclusionPoint, INCLUSION_KEYS, "inclusionPoint");
  stringFields(
    inclusion,
    ["transactionHash", "blockHash", "chainPointId"],
    "inclusionPoint",
    HEX32,
  );
  stringFields(
    inclusion,
    ["blockNo", "slot", "finalityDepth"],
    "inclusionPoint",
    NATURAL,
  );
  const coordinate = record(t.coordinate, ["domain", "index"], "coordinate");
  requireCondition(
    ["block", "transaction", "mutation", "event", "transition_step"].includes(
      text(coordinate.domain, "coordinate domain"),
    ),
    "coordinate domain",
  );
  text(coordinate.index, "coordinate index", NATURAL);
  const priorState = list(t.priorState, "priorState").map((value) => {
    const r = record(value, ["outRef", "outputCbor"], "priorState entry");
    return {
      outRef: text(r.outRef, "priorState outRef", HEX),
      outputCbor: text(r.outputCbor, "priorState output", HEX),
    };
  });
  const prior = await watcherBlockReplayPriorState(priorState);
  equal(
    priorState,
    [...priorState].sort((a, b) => a.outRef.localeCompare(b.outRef)),
    "priorState order",
  );
  const raw = (key: string): unknown =>
    decodeWatcherReplayRawRecord(text(t[key], `transcript.${key}`, HEX));
  provenance(raw("daProvenanceCborHex"), "public_or_permissionless_da");
  const headerRecord = record(
    raw("stateQueueHeaderObservationCborHex"),
    HEADER_OBSERVATION_KEYS,
    "header observation",
  );
  equal(headerRecord.headerHash, t.headerHash, "header hash copies");
  const headerCborHex = text(headerRecord.headerCborHex, "HeaderV1 CBOR", HEX);
  const header = Data.from(headerCborHex, SDK.Header);
  equal(Data.to(header, SDK.Header), headerCborHex, "canonical HeaderV1");
  equal(
    computeHash28(Buffer.from(headerCborHex, "hex")).toString("hex"),
    t.headerHash,
    "HeaderV1 hash",
  );
  const nodeCbor = text(
    headerRecord.stateQueueNodeCborHex,
    "StateQueueNode CBOR",
    HEX,
  );
  const node = Data.from(nodeCbor, SDK.StateQueueNode);
  equal(
    Data.to(node, SDK.StateQueueNode),
    nodeCbor,
    "canonical StateQueueNode",
  );
  equal(
    Data.to(node.header, SDK.Header),
    headerCborHex,
    "StateQueueNode header",
  );
  equal(
    node.da_attestation,
    headerRecord.daAvailability,
    "StateQueueNode availability",
  );
  const linkedCbor = text(
    headerRecord.linkedListDatumCborHex,
    "linked list CBOR",
    HEX,
  );
  const linked = Data.from(linkedCbor, SDK.LinkedListDatum);
  equal(
    CML.PlutusData.from_cbor_hex(linkedCbor).to_canonical_cbor_hex(),
    linkedCbor,
    "canonical linked list",
  );
  const view = SDK.linkedListDatumToNodeView(
    linked,
    `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${text(t.headerHash, "headerHash")}`,
  );
  equal(
    Data.to(
      Data.castFrom(view.data, SDK.StateQueueNode) as SDK.StateQueueNode,
      SDK.StateQueueNode,
    ),
    nodeCbor,
    "linked list node",
  );
  equal(view.key, { Key: { key: t.headerHash } }, "linked list key");
  equal(
    view.next === "Empty" ? null : view.next.Key.key,
    headerRecord.nextHeaderHash,
    "linked list next",
  );
  text(headerRecord.queueOutRef, "queue outRef", OUT_REF);
  nullableText(headerRecord.nextHeaderHash, "next header", HEX28);
  for (const [a, b] of [
    ["observedTransactionHash", "transactionHash"],
    ["observedBlockHash", "blockHash"],
    ["observedBlockNo", "blockNo"],
    ["observedSlot", "slot"],
    ["observedChainPointId", "chainPointId"],
    ["finalityDepth", "finalityDepth"],
  ])
    equal(headerRecord[a!], inclusion[b!], `inclusion ${a}`);
  const observed = record(
    raw("authenticatedHeaderObservationCborHex"),
    [
      "schemaVersion",
      "sourceMode",
      "provenance",
      "chainPoint",
      "confirmationDepth",
      "headerHash",
      "header",
    ],
    "authenticated header description",
  );
  equal(
    observed.schemaVersion,
    SDK.CANONICAL_EVIDENCE_SOURCE_SCHEMA_VERSION,
    "SDK observation schema",
  );
  equal(observed.sourceMode, "local_node", "SDK observation source mode");
  provenance(observed.provenance, "authenticated_cardano_l1");
  equal(observed.header, header, "SDK observation HeaderV1");
  equal(observed.headerHash, t.headerHash, "SDK observation header hash");
  const chainPoint = record(
    observed.chainPoint,
    ["slot", "blockHash"],
    "SDK observation point",
  );
  equal(chainPoint.blockHash, inclusion.blockHash, "SDK block hash");
  equal(
    chainPoint.slot,
    BigInt(text(inclusion.slot, "inclusion slot")),
    "SDK slot",
  );
  equal(
    count(observed.confirmationDepth, "SDK confirmation depth").toString(),
    inclusion.finalityDepth,
    "SDK depth",
  );
  const ruleBundle = parseWatcherRuleBundle(raw("ruleBundleCborHex"));
  equal(
    computeWatcherRuleBundleCommitment(ruleBundle),
    t.ruleBundleCommitment,
    "rule bundle commitment",
  );
  equal(
    ruleBundle.deploymentManifestId,
    t.deploymentFingerprint,
    "rule deployment",
  );
  requireCondition(
    BigInt(text(inclusion.finalityDepth, "inclusion depth")) >=
      BigInt(
        count(minimumConfirmationDepth, "current release confirmation depth"),
      ),
    "persisted release depth",
  );
  const envelopeHex = text(t.payloadEnvelopeCborHex, "payload envelope", HEX);
  const envelope = Buffer.from(envelopeHex, "hex");
  equal(sha256(envelope), t.payloadEnvelopeSha256, "payload envelope hash");
  const reconstructed = await reconstructDaPayload({
    payloadEnvelopeCbor: envelope,
    expectedHeaderHash: text(t.headerHash, "headerHash"),
    committedHeader: header,
  });
  equal(sha256(reconstructed.payloadCbor), t.payloadSha256, "payload hash");
  equal(prior.root, header.prevUtxosRoot, "prior state root");
  const w22 = parseW22(raw("reconstructionRecordCborHex"));
  const w24 = parseW24(raw("phaseARecordCborHex"));
  const w25 = parseW25(raw("blockReplayRecordCborHex"));
  for (const r of [w22, w24, w25]) {
    equal(r.headerHash, t.headerHash, "result header");
    equal(r.payloadEnvelopeSha256, t.payloadEnvelopeSha256, "result envelope");
    equal(r.payloadSha256, t.payloadSha256, "result payload");
  }
  for (const r of [w24, w25]) {
    equal(
      r.reconstructionDigest,
      w22.resultDigest,
      "result reconstruction digest",
    );
    equal(r.ruleBundleCommitment, t.ruleBundleCommitment, "result rule bundle");
  }
  equal(w25.phaseAResultDigest, w24.resultDigest, "W25 Phase A digest");
  equal(w25.resultDigest, t.blockReplayResultDigest, "W25 result digest");
  equal(w25.priorStateRoot, prior.root, "W25 prior root");
  equal(
    w25.expectedPriorStateRoot,
    header.prevUtxosRoot,
    "W25 expected prior root",
  );
  equal(w25.expectedPostStateRoot, header.utxosRoot, "W25 expected post root");
  const events = list(t.eventAuthorityRecordsCborHex, "event records").map(
    (value) =>
      parseAuthority(
        decodeWatcherReplayRawRecord(text(value, "event record CBOR", HEX)),
      ),
  );
  const claims: WatcherCommittedEventClaim[] = [
    ...reconstructed.deposits.map((entry) => ({
      phase: "Deposit" as const,
      eventIdCborHex: entry.keyBytes.toString("hex"),
      valueCborHex: entry.valueBytes.toString("hex"),
      canonicalNativeTxCborHex: null,
    })),
    ...reconstructed.withdrawals.map((entry) => ({
      phase: "Withdrawal" as const,
      eventIdCborHex: entry.keyBytes.toString("hex"),
      valueCborHex: entry.valueBytes.toString("hex"),
      canonicalNativeTxCborHex: null,
    })),
    ...reconstructed.forcedTransactions.map((entry) => ({
      phase: "ForcedTransaction" as const,
      eventIdCborHex: entry.keyBytes.toString("hex"),
      valueCborHex: entry.valueBytes.toString("hex"),
      canonicalNativeTxCborHex: entry.fullTransactionCbor.toString("hex"),
    })),
  ];
  equal(events.length, claims.length, "event authority count");
  const steps = watcherBlockReplayCommittedSteps({
    transitionTrace: reconstructed.transitionTrace,
    eventToStep: reconstructed.eventToStep,
  });
  equal(
    events.map(
      (event) =>
        watcherBlockReplayEventAuthorityManifest(event).eventKeyFingerprint,
    ),
    steps
      .filter((step) => step.phase !== "L2Transaction")
      .map((step) => step.eventKeyFingerprint),
    "event authority order",
  );
  for (const event of events) {
    const matching = claims.filter(
      (claim) =>
        claim.phase === event.phase &&
        claim.eventIdCborHex === event.event.eventId,
    );
    equal(matching, [event.committedClaim], "event DA claim");
    equal(event.network, ruleBundle.network, "event network");
    equal(
      event.origin.deploymentManifestId,
      t.deploymentFingerprint,
      "event deployment",
    );
    equal(
      event.origin.blueprintHash,
      ruleBundle.blueprintHash,
      "event release",
    );
    const cutoff = event.origin.throughHeader;
    for (const key of [
      "headerHash",
      "headerCborHex",
      "queueOutRef",
      "observedTransactionHash",
      "observedBlockHash",
      "observedSlot",
      "observedBlockNo",
    ] as const)
      equal(cutoff[key], headerRecord[key], `event cutoff ${key}`);
  }
  equal(
    w25.authorityManifestDigest,
    watcherSha256CanonicalJson(
      events.map(watcherBlockReplayEventAuthorityManifest),
    ),
    "W25 authority manifest",
  );
  equal(
    w25.sourceManifestDigest,
    watcherSha256CanonicalJson(
      steps.map(
        ({
          stepIndex,
          phase,
          eventKeyFingerprint,
          preRoot,
          postRoot,
          eventToStepIndex,
          eventToStepPhase,
        }) => ({
          stepIndex,
          phase,
          eventKeyFingerprint,
          preRoot,
          postRoot,
          eventToStepIndex,
          eventToStepPhase,
        }),
      ),
    ),
    "W25 source manifest",
  );
  equal(
    w25.effectManifestDigest,
    watcherSha256CanonicalJson(
      events.map((event) => ({
        phase: event.phase,
        eventKeyFingerprint:
          watcherBlockReplayEventAuthorityManifest(event).eventKeyFingerprint,
        effectDigest: event.transitionEffect.digest,
        effectCborSha256: sha256(
          Buffer.from(event.transitionEffect.canonicalCborHex, "hex"),
        ),
        operations: event.transitionEffect.operations.map((op) => ({
          type: op.type,
          outRefCbor: op.outRefCborHex,
          ...(op.type === "insert"
            ? { outputCborSha256: sha256(Buffer.from(op.outputCborHex, "hex")) }
            : {}),
        })),
      })),
    ),
    "W25 effect manifest",
  );
  // This cast follows exact structural/content validation. The returned record is
  // descriptive and is deliberately never inserted into an authority WeakSet.
  return {
    transcript: t as unknown as WatcherAuthenticatedReplayTranscript,
    headerCborHex,
    reconstruction: w22,
    phaseA: w24,
    blockReplay: w25,
    events,
  };
};
