import {
  encodeCborArrayRaw,
  encodeCborBytes,
} from "@al-ft/midgard-core/codec/cbor";
import { canonicalBlockEvidenceFromVerifiedPayload } from "@al-ft/midgard-fault-proofs";
import type { AuthenticatedStateQueueHeaderObservation } from "@al-ft/midgard-sdk";
import { Header } from "@al-ft/midgard-sdk";
import type {
  PhaseAConfig,
  PhaseAValidatedTx,
  PhaseBConfig,
  QueuedTx,
} from "@al-ft/midgard-validation/types";
import { Data as LucidData } from "@lucid-evolution/lucid";

import { finalizeResult } from "./block-replay.bind-committed-steps.js";
import { replayCommittedBlock } from "./block-replay.replay-committed-block.js";
import {
  readUserEventAuthority,
  type ReplayDeploymentBinding,
} from "./block-replay.validate-event-authority.js";
import {
  bindPhaseAV1,
  bindReconstruction,
  deriveCandidates,
  type EvaluateWatcherBlockReplayInput,
  snapshotWatcherBlockReplayEventAuthorities,
  watcherBlockReplayCommittedSteps,
} from "./block-replay.watcher-block-replay-committed-steps.js";
import {
  fullReplayEventRecords,
  makeWatcherPhaseBConfig,
} from "./block-replay.watcher-block-replay-prior-state.js";
import {
  errorResult,
  type WatcherBlockReplayContext,
  type WatcherBlockReplayPriorUtxo,
} from "./block-replay.watcher-block-replay-rejection-projection.js";
import {
  admitFullBlockReplayResult,
  fail,
  reasonCodeOf,
  type WatcherBlockReplayCommittedStep,
  type WatcherBlockReplayEventAuthority,
  type WatcherBlockReplayResult,
} from "./block-replay.watcher-block-replay-result.js";
import { type WatcherCommittedEventClaim } from "./event-claims.js";
import {
  makeWatcherPhaseAConfig,
  watcherPhaseAQueuedTxs,
} from "./phase-a-verifier.js";
import {
  computeWatcherRuleBundleCommitment,
  type WatcherRuleBundle,
} from "./rule-bundle.js";
import { assertWatcherUserEventAuthorityCurrent } from "./user-event.js";

/**
 * The W25 entry point: an accepted W22 reconstruction, an accepted W24 Phase A
 * record, the exact block bytes and the parent's prior-state material from the
 * header decision's canonical evidence, and the W23
 * rule bundle produce a frozen, digest-bound record of the canonical Phase B
 * replay of the block - prior state, dependencies, spends, references, scripts,
 * value, events, every intermediate root, and the exact post state.
 */
export const evaluateWatcherBlockReplay = async (
  input: EvaluateWatcherBlockReplayInput,
): Promise<WatcherBlockReplayResult> => {
  // Snapshot bytes before asynchronous authentication can yield to the caller.
  let payloadEnvelopeCbor: Buffer;
  let priorState: readonly WatcherBlockReplayPriorUtxo[];
  let eventAuthorities: readonly WatcherBlockReplayEventAuthority[];
  let ruleBundle: WatcherRuleBundle;
  let deployment: ReplayDeploymentBinding;
  let observation: AuthenticatedStateQueueHeaderObservation;
  const ruleBundleCommitment = input.ruleBundleCommitment;
  try {
    ruleBundle = structuredClone(input.ruleBundle);
    observation = structuredClone(input.observation);
    deployment = Object.freeze({
      deploymentManifestId: ruleBundle.deploymentManifestId,
      blueprintHash: ruleBundle.blueprintHash,
      network: ruleBundle.network,
    });
    eventAuthorities = snapshotWatcherBlockReplayEventAuthorities(
      input.eventAuthorities ?? [],
    );
    if (
      eventAuthorities.length > 0 &&
      computeWatcherRuleBundleCommitment(ruleBundle) !== ruleBundleCommitment
    ) {
      return fail(
        "user_event_authority_identity_mismatch",
        "$.userEvent.ruleBundleCommitment",
      );
    }
  } catch (error) {
    return admitFullBlockReplayResult(
      errorResult([reasonCodeOf(error)], null, 0),
    );
  }
  try {
    payloadEnvelopeCbor = Buffer.from(input.payloadEnvelopeCbor);
  } catch {
    return admitFullBlockReplayResult(
      errorResult(["canonical_reconstruction_failed"], null, 0),
    );
  }
  try {
    priorState = Object.freeze(
      input.priorState
        .map((entry) =>
          Object.freeze({
            outRef: entry.outRef,
            outputCbor: entry.outputCbor,
          }),
        )
        .sort((left, right) => left.outRef.localeCompare(right.outRef)),
    );
  } catch {
    return admitFullBlockReplayResult(
      errorResult(["malformed_prior_state"], null, 0),
    );
  }
  let evidence;
  try {
    evidence = await canonicalBlockEvidenceFromVerifiedPayload({
      observation,
      payloadEnvelopeCbor,
      daProvenance: input.daProvenance,
      ...(input.minimumConfirmationDepth === undefined
        ? {}
        : { minimumConfirmationDepth: input.minimumConfirmationDepth }),
    });
  } catch {
    return admitFullBlockReplayResult(
      errorResult(["canonical_reconstruction_failed"], null, 0),
    );
  }

  const context: WatcherBlockReplayContext = Object.freeze({
    headerHash: evidence.headerHash,
    payloadEnvelopeSha256: evidence.payloadEnvelopeSha256,
    payloadSha256: evidence.payloadSha256,
    reconstructionDigest: input.reconstruction.resultDigest,
    phaseAResultDigest: input.phaseA.resultDigest,
    ruleBundleCommitment,
  });
  const committedEventClaims: readonly WatcherCommittedEventClaim[] =
    Object.freeze([
      ...evidence.reconstruction.deposits.map((entry) =>
        Object.freeze({
          phase: "Deposit" as const,
          eventIdCborHex: entry.keyBytes.toString("hex"),
          valueCborHex: entry.valueBytes.toString("hex"),
          canonicalNativeTxCborHex: null,
        }),
      ),
      ...evidence.reconstruction.withdrawals.map((entry) =>
        Object.freeze({
          phase: "Withdrawal" as const,
          eventIdCborHex: entry.keyBytes.toString("hex"),
          valueCborHex: entry.valueBytes.toString("hex"),
          canonicalNativeTxCborHex: null,
        }),
      ),
      ...evidence.reconstruction.forcedTransactions.map((entry) =>
        Object.freeze({
          phase: "ForcedTransaction" as const,
          eventIdCborHex: entry.keyBytes.toString("hex"),
          valueCborHex: entry.valueBytes.toString("hex"),
          canonicalNativeTxCborHex: entry.fullTransactionCbor.toString("hex"),
        }),
      ),
    ]);
  const transactionCount = evidence.reconstruction.transactions.length;

  let candidates: readonly PhaseAValidatedTx[];
  let committedSteps: readonly WatcherBlockReplayCommittedStep[];
  let phaseAConfig: PhaseAConfig;
  let config: PhaseBConfig;
  try {
    bindReconstruction({
      reconstruction: input.reconstruction,
      headerHash: evidence.headerHash,
      payloadEnvelopeSha256: evidence.payloadEnvelopeSha256,
    });
    bindPhaseAV1({
      phaseA: input.phaseA,
      headerHash: evidence.headerHash,
      payloadEnvelopeSha256: evidence.payloadEnvelopeSha256,
      reconstructionDigest: input.reconstruction.resultDigest,
      ruleBundleCommitment,
    });
    // The Phase A configuration binding (rule bundle profile, header protocol
    // version) is W24's, reused unchanged rather than restated.
    phaseAConfig = makeWatcherPhaseAConfig({
      header: evidence.header,
      ruleBundle,
    });
    const queuedTxs: readonly QueuedTx[] = watcherPhaseAQueuedTxs({
      transactions: evidence.reconstruction.transactions.map((entry) =>
        Object.freeze({ txId: entry.txId, txCbor: entry.fullTransactionCbor }),
      ),
      programMaterial: evidence.reconstruction.payload.block_body
        .cek_program_material as readonly (readonly [string, string])[],
    });
    candidates = deriveCandidates(queuedTxs, phaseAConfig, input.phaseA);
    committedSteps = watcherBlockReplayCommittedSteps({
      transitionTrace: evidence.reconstruction.transitionTrace,
      eventToStep: evidence.reconstruction.eventToStep,
    });
    config = makeWatcherPhaseBConfig(evidence.header);
  } catch (error) {
    return admitFullBlockReplayResult(
      errorResult([reasonCodeOf(error)], context, transactionCount),
    );
  }

  try {
    const core = await replayCommittedBlock({
      candidates,
      priorState,
      expectedPriorStateRoot: evidence.header.prevUtxosRoot,
      config,
      phaseAConfig,
      committedSteps,
      eventAuthorities,
      committedEventClaims,
      deployment,
      header: {
        headerHash: evidence.headerHash,
        headerCborHex: LucidData.to(evidence.header, Header),
        observedBlockHash: observation.chainPoint.blockHash,
        observedSlot: observation.chainPoint.slot.toString(),
      },
    });
    // Replay yields during canonical ledger evaluation. Re-read the same private
    // handles after that work so a closed, rolled-back or replaced head cannot
    // authorize a newly admitted W25 record.
    for (const authority of eventAuthorities) {
      await readUserEventAuthority(authority.userEvent);
    }
    for (const authority of eventAuthorities) {
      try {
        assertWatcherUserEventAuthorityCurrent(authority.userEvent);
      } catch {
        return fail("user_event_authority_invalid", "$.userEvent");
      }
    }
    const replay = admitFullBlockReplayResult(
      finalizeResult({
        core,
        context,
        transactionCount,
        expectedPriorStateRoot: evidence.header.prevUtxosRoot,
        expectedPostStateRoot: evidence.header.utxosRoot,
        committedSteps,
      }),
    );
    fullReplayEventRecords.set(replay, core.eventAuthorityRecords ?? []);
    return replay;
  } catch (error) {
    return admitFullBlockReplayResult(
      errorResult([reasonCodeOf(error)], context, transactionCount),
    );
  }
};

// ---------------------------------------------------------------------------
// Durable record
// ---------------------------------------------------------------------------

/**
 * Canonical CBOR commitment persisted as the replayed state:
 * `[ header_hash, prior_state_root, post_state_root, [ per-transaction post
 * roots in canonical accepted order ] ]`. Unlike W22's record, which commits
 * the roots the header *claims*, this commits the roots the replay
 * *recomputed*, which is what makes the two records worth holding separately.
 */
export const replayedStateBytes = (result: WatcherBlockReplayResult): Buffer =>
  encodeCborArrayRaw([
    encodeCborBytes(Buffer.from(result.headerHash as string, "hex")),
    encodeCborBytes(Buffer.from(result.priorStateRoot as string, "hex")),
    encodeCborBytes(Buffer.from(result.postStateRoot as string, "hex")),
    encodeCborArrayRaw(
      result.transactionRoots.map((entry) =>
        encodeCborBytes(Buffer.from(entry.postRoot, "hex")),
      ),
    ),
  ]);

export type WatcherBlockReplayRecordErrorCode =
  | "invalid_input_ids"
  | "result_not_accepted"
  | "unsupported_schema";

export class WatcherBlockReplayRecordError extends Error {
  readonly code: WatcherBlockReplayRecordErrorCode;
  readonly path: string;

  constructor(code: WatcherBlockReplayRecordErrorCode, path: string) {
    super(`${code}: ${path}`);
    this.name = "WatcherBlockReplayRecordError";
    this.code = code;
    this.path = path;
  }
}

export const failRecord = (
  code: WatcherBlockReplayRecordErrorCode,
  path: string,
): never => {
  throw new WatcherBlockReplayRecordError(code, path);
};
