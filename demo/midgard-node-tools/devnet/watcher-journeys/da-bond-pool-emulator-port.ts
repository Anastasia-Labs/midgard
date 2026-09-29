/**
 * The emulator adapter for the pooled DA bond journey (ticket #692): a
 * `DaBondPoolJourneyPort` over the availability emulator harness, so the
 * journey driver runs its whole six-step chronology as a fast dry run.
 *
 * What runs for real, on the emulator ledger with the compiled validators:
 *
 * - block commits: the SDK's `CommitBlockHeader` builder (the node's), after
 *   the queue's tail, from an empty genesis queue;
 * - attestation: DA attestation init and threshold signatures, then the SDK
 *   Apply builder. A refused Apply is the builder's typed
 *   `DaAttestationBuildError` refusal (`pool-under-backed`,
 *   `pool-withdrawing`), not a check of this adapter's;
 * - pool reads, top-up and the withdrawal quorum steps: the operator
 *   `da-bond` commands (`status`, `top-up`, `withdraw begin|cancel|complete`
 *   with each owner's `witness` and `assemble`) over an emulator
 *   `DaBondContext`;
 * - alerts: the watcher's `deriveWatcherDaBondPoolObservation` over its
 *   authenticated pool read, and the committee's pool check, readiness
 *   reasons and one `createDaBondPoolMonitor`, fed only from `observeAlerts`;
 * - the challenge flow (Open, publications, settlements, Close, and the
 *   Timeout that slashes the pool and removes the head): the harness's
 *   hand-built mirrors of those transactions.
 *
 * Waiting advances the emulator clock; nothing sleeps.
 */
import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import {
  createDaBondPoolMonitor,
  daBondPoolCheckFromStatus,
  daBondPoolReadinessReasons,
} from "da-committee-node/coordinator/pool-monitor";
import { Effect, Either } from "effect";
import {
  daBondAssembleCommand,
  type DaBondContext,
  daBondStatusCommand,
  daBondTopUpCommand,
  daBondWithdrawBuildCommand,
  type DaBondWithdrawStep,
} from "midgard-node/commands/da-bond";
import { runDaBondWitnessCommand } from "midgard-node/commands/da-bond-files";
import { TEST_AVAILABILITY_PARAMETERS } from "midgard-node/tests/helpers/availability-challenge";
import {
  AVAILABILITY_ATTESTATION_OUTPUT_LOVELACE,
  AVAILABILITY_PROFILE,
  type AvailabilityCommitFixture,
  type AvailabilityCommittedBlock,
  type AvailabilityFixture,
  buildAvailabilityClose,
  buildAvailabilityPublication,
  buildAvailabilitySettlement,
  buildAvailabilityTimeout,
  commitAvailabilityBlock,
  createAvailabilityCommitFixture,
  liveAvailabilityTarget,
  type OpenAvailability,
  openAvailability,
  withLiveAvailabilityQueue,
} from "midgard-node/tests/helpers/availability-challenge-emulator";
import {
  authenticWatcherDaBondPool,
  deriveWatcherDaBondPoolObservation,
} from "midgard-watcher";

import type {
  DaBondPoolJourneyAlerts,
  DaBondPoolJourneyBlockStatus,
  DaBondPoolJourneyCommitIntent,
  DaBondPoolJourneyParams,
  DaBondPoolJourneyPort,
  DaBondPoolJourneySnapshot,
} from "./da-bond-pool-journey.js";

export type DaBondPoolEmulatorPortOptions = Readonly<{
  /**
   * The genesis pool's lovelace. Default `floor + da_bond + 20 ADA`: it backs
   * one bond and fewer than two, so the Timeout leaves it short (step 1).
   */
  poolLovelace?: bigint;
  /** Each committed block's DA payload size. Default 14,021 bytes. */
  payloadBytes?: number;
}>;

export type DaBondPoolEmulatorPort = DaBondPoolJourneyPort & {
  readonly fixture: AvailabilityCommitFixture;
  /** Removes the directory that holds the quorum steps' files. */
  dispose(): void;
};

type Challenge = {
  readonly open: OpenAvailability;
  readonly record: UTxO;
  readonly threads: UTxO[];
  readonly carriers: (UTxO | undefined)[];
  terminal: UTxO;
};

type Block = {
  readonly label: string;
  readonly responder: DaBondPoolJourneyCommitIntent["responder"];
  readonly fixture: AvailabilityCommittedBlock;
  /** Init and threshold signatures landed; a later attempt only applies. */
  signed: boolean;
  challenge?: Challenge;
  /** The Timeout burned the node. */
  removed: boolean;
};

const APPLY_VALIDITY_MS = 60_000n;

export const createDaBondPoolEmulatorPort = async (
  options: DaBondPoolEmulatorPortOptions = {},
): Promise<DaBondPoolEmulatorPort> => {
  const f = await createAvailabilityCommitFixture({
    seedPool: {
      lovelace:
        options.poolLovelace ??
        TEST_AVAILABILITY_PARAMETERS.da_bond_pool_floor_lovelace +
          TEST_AVAILABILITY_PARAMETERS.da_bond_lovelace +
          20_000_000n,
    },
  });
  const { lucid, contracts } = f;
  const parameters = f.parameters;
  const blocks = new Map<string, Block>();
  const dir = mkdtempSync(join(tmpdir(), "da-bond-pool-journey-"));
  let fileCount = 0;
  const file = (name: string): string => {
    fileCount += 1;
    return join(dir, `${fileCount.toString()}-${name}.json`);
  };
  const asResponder = () =>
    lucid.selectWallet.fromPrivateKey(f.responder.privateKey);
  const asChallenger = () =>
    lucid.selectWallet.fromPrivateKey(f.challenger.privateKey);

  const ctx: DaBondContext = {
    lucid,
    network: "Preprod",
    manifestId: "emulator-da-bond-pool-journey",
    poolValidator: contracts.daBondPool,
    poolSpendingReference: f.poolReferences.daBondPoolSpending,
    parameters,
    daParamsGovernor: {
      address: contracts.daParamsGovernor.spendingScriptAddress,
      unit: SDK.daParamsUnit(contracts.daParamsGovernor),
    },
    withdrawDelayMs: f.timing.daBondWithdrawDelayMs,
    now: () => f.emulator.now(),
    submit: async (txCbor) => {
      const txHash = await f.emulator.submitTx(txCbor);
      f.emulator.awaitBlock(1);
      return txHash;
    },
  };

  // One committee pool monitor, fed only from `observeAlerts` reads.
  const monitorEvents: string[] = [];
  const monitor = createDaBondPoolMonitor({
    writeEvent: (event) => {
      if (event.event !== undefined) monitorEvents.push(event.event);
    },
    now: () => new Date(f.emulator.now()),
  });

  const requireBlock = (headerHash: string): Block => {
    const block = blocks.get(headerHash);
    if (block === undefined)
      throw new Error(`Block ${headerHash} was not committed by this adapter`);
    return block;
  };
  const requireChallenge = (block: Block): Challenge => {
    if (block.challenge === undefined)
      throw new Error(`Block ${block.label} has no open challenge`);
    return block.challenge;
  };
  /** The block's fixture with its live node as the target. */
  const liveBlock = async (block: Block): Promise<AvailabilityFixture> => {
    const target = await liveAvailabilityTarget(block.fixture);
    if (target === undefined)
      throw new Error(`Block ${block.label} is no longer queued`);
    return { ...block.fixture, target };
  };
  const txIdOf = (outputs: readonly UTxO[], what: string): string => {
    const txHash = outputs[0]?.txHash;
    if (txHash === undefined) throw new Error(`${what} produced no output`);
    return txHash;
  };
  const attestationOf = async (
    headerHash: string,
  ): Promise<SDK.DaAttestationUtxo> => {
    const [utxo] = await lucid.utxosAtWithUnit(
      contracts.daAttestation.spendingScriptAddress,
      SDK.daAttestationUnit(contracts.daAttestation, headerHash),
    );
    if (!utxo?.datum) throw new Error(`No DA attestation for ${headerHash}`);
    return { utxo, datum: Data.from(utxo.datum, SDK.DaAttestationDatum) };
  };

  /** Init, then the committee's threshold signatures (as the harness does). */
  const collectSignatures = async (fb: AvailabilityFixture, label: string) => {
    asResponder();
    const init = await Effect.runPromise(
      SDK.incompleteInitDaAttestationTxProgram(lucid, contracts, {
        daParamsUtxo: f.daParamsUtxo,
        daParamsDatum: f.daParamsDatum,
        target: fb.target,
        referenceScripts: f.daReferences,
        attestationOutputLovelace: AVAILABILITY_ATTESTATION_OUTPUT_LOVELACE,
        rescueBeneficiary: await Effect.runPromise(
          SDK.addressDataFromBech32(f.responder.address),
        ),
        availabilityCommitment: fb.commitment,
      }),
    );
    await f.submit(`attestation init ${label}`, init, true);
    const message = SDK.daAvailabilityAttestationMessage(fb.commitment);
    const add = await Effect.runPromise(
      SDK.incompleteAddDaAttestationSignaturesTxProgram(lucid, contracts, {
        daParamsUtxo: f.daParamsUtxo,
        daParamsDatum: f.daParamsDatum,
        attestation: await attestationOf(fb.target.headerHash),
        witnesses: f.committeeKeys.map((key, signerIndex) => ({
          signerIndex,
          signatureHex: Buffer.from(key.sign(message).to_raw_bytes()).toString(
            "hex",
          ),
        })),
        referenceScripts: f.daReferences,
      }),
    );
    await f.submit(`attestation signatures ${label}`, add, true);
  };

  /** Build, witness with both owners, assemble: the operator quorum flow. */
  const quorumStep = async (
    step: DaBondWithdrawStep,
    extra: { amount?: string; to?: string } = {},
  ) => {
    const buildUnsigned = file(`unsigned-${step}`);
    try {
      const built = await daBondWithdrawBuildCommand(ctx, step, {
        feeAddress: f.responder.address,
        signers: f.daParamsDatum.owners.join(","),
        buildUnsigned,
        ...extra,
      });
      const witnesses: string[] = [];
      for (const key of [f.responder.privateKey, f.challenger.privateKey]) {
        const out = file(`witness-${step}`);
        await runDaBondWitnessCommand(
          buildUnsigned,
          { keyEnv: "OWNER_KEY", out },
          { OWNER_KEY: key },
        );
        witnesses.push(out);
      }
      const assembled = await daBondAssembleCommand(
        ctx,
        buildUnsigned,
        witnesses,
      );
      return { built, assembled };
    } finally {
      asResponder();
    }
  };

  const params = async (): Promise<DaBondPoolJourneyParams> => ({
    daBond: parameters.da_bond_lovelace,
    penalty: parameters.da_slash_penalty_lovelace,
    floor: parameters.da_bond_pool_floor_lovelace,
    minTopUp: parameters.da_bond_min_top_up_lovelace,
    maxTimeoutFee: parameters.max_timeout_fee_lovelace,
    challengeRecordLovelace: parameters.challenge_record_lovelace,
    withdrawDelayMs: Number(f.timing.daBondWithdrawDelayMs),
    attestationTimeoutMs: AVAILABILITY_PROFILE.timing.da_attestation_timeout_ms,
  });

  const poolSnapshot = async (): Promise<DaBondPoolJourneySnapshot> => {
    const status = await daBondStatusCommand(ctx);
    return {
      state: status.state,
      lovelace: BigInt(status.lovelace),
      backing: BigInt(status.backing),
      ...(status.unlockAt === undefined
        ? {}
        : { unlockAt: Number(status.unlockAt) }),
      utxoRef: status.poolOutRef,
    };
  };

  const observeAlerts = async (): Promise<DaBondPoolJourneyAlerts> => {
    const nowMs = f.emulator.now();
    const poolAddress = contracts.daBondPool.spendingScriptAddress;
    const policyId = contracts.daBondPool.policyId;
    // The watcher's authenticated read and its derived alerts.
    const watcher = deriveWatcherDaBondPoolObservation({
      pool: authenticWatcherDaBondPool({
        utxos: await lucid.utxosAt(poolAddress),
        policyId,
        address: poolAddress,
      }),
      policyId,
      parameters,
      nowMs: BigInt(nowMs),
    });
    // The committee's read: the SDK pool fetch and status, then its check,
    // readiness reasons and monitor transitions.
    const pool = await SDK.fetchDaBondPool(lucid, {
      policyId,
      address: poolAddress,
      parameters,
    });
    const check = daBondPoolCheckFromStatus(
      SDK.daBondPoolStatus({
        lovelace: pool.utxo.assets.lovelace ?? 0n,
        datum: pool.datum,
        parameters,
      }),
      new Date(nowMs).toISOString(),
    );
    monitorEvents.length = 0;
    monitor.record(check);
    const events = [...monitorEvents];
    monitorEvents.length = 0;
    return {
      watcher: {
        underBacked: watcher.alerts.underBacked,
        withdrawing: watcher.alerts.withdrawing,
      },
      committee: {
        readinessReasons: daBondPoolReadinessReasons(check),
        events,
      },
    };
  };

  const commitBlock: DaBondPoolJourneyPort["commitBlock"] = async (intent) => {
    const fixture = await commitAvailabilityBlock(f, {
      ...(options.payloadBytes === undefined
        ? {}
        : { payloadBytes: options.payloadBytes }),
    });
    asResponder();
    blocks.set(fixture.target.headerHash, {
      label: intent.label,
      responder: intent.responder,
      fixture,
      signed: false,
      removed: false,
    });
    return {
      headerHash: fixture.target.headerHash,
      txId: fixture.commitTxHash,
      headerEndTime: Number(fixture.headerEndTime),
    };
  };

  const attest: DaBondPoolJourneyPort["attest"] = async (headerHash) => {
    const block = requireBlock(headerHash);
    const fb = await liveBlock(block);
    if (!block.signed) {
      await collectSignatures(fb, block.label);
      block.signed = true;
    }
    asResponder();
    const now = BigInt(f.emulator.now());
    const built = await Effect.runPromise(
      Effect.either(
        SDK.incompleteApplyDaAttestationToStateQueueTxProgram(
          lucid,
          contracts,
          {
            daParamsUtxo: f.daParamsUtxo,
            daParamsDatum: f.daParamsDatum,
            attestation: await attestationOf(headerHash),
            target: fb.target,
            referenceScripts: f.daReferences,
            availabilityParameters: parameters,
            validityRange: { validFrom: now, validTo: now + APPLY_VALIDITY_MS },
          },
        ),
      ),
    );
    if (Either.isLeft(built))
      return {
        kind: "refused",
        reason: `${built.left.reason}: ${built.left.message}`,
      };
    const outputs = await f.submit(
      `attestation apply ${block.label}`,
      built.right,
      true,
    );
    return {
      kind: "applied",
      txId: txIdOf(outputs, "Apply"),
      appliedAt: f.emulator.now(),
    };
  };

  const open: DaBondPoolJourneyPort["open"] = async (headerHash) => {
    const block = requireBlock(headerHash);
    const fb = await liveBlock(block);
    try {
      const opened = await openAvailability(fb, {
        queue: fb.target.stateQueueUtxo.utxo,
        commitment: fb.commitment,
      });
      const state = await opened.submit();
      block.challenge = {
        open: opened,
        record: state.record,
        threads: [...state.threads],
        carriers: state.threads.map(() => undefined),
        terminal: state.terminal,
      };
      return {
        txId: state.record.txHash,
        responseDeadline: Number(opened.plan.responseDeadline),
      };
    } finally {
      asResponder();
    }
  };

  const respondAll: DaBondPoolJourneyPort["respondAll"] = async (
    headerHash,
  ) => {
    const block = requireBlock(headerHash);
    if (block.responder !== "serve")
      throw new Error(
        `Block ${block.label} is withheld: the committee is silent`,
      );
    const challenge = requireChallenge(block);
    const fb = await liveBlock(block);
    asResponder();
    const tranches = SDK.planDaAvailabilityPublications({
      commitment: fb.commitment,
      payload: fb.payload,
      challengeAssetName: challenge.open.plan.challengeAssetName,
    });
    const txIds: string[] = [];
    for (const [index, tranche] of tranches.entries()) {
      let thread = challenge.threads[index];
      let carrier = challenge.carriers[index];
      if (thread === undefined)
        throw new Error(`Challenge on ${block.label} has no thread ${index}`);
      for (const publication of tranche.publications) {
        const outputs = await f.submit(
          `publish ${block.label} tranche ${index} chunk ${publication.chunk_index}`,
          buildAvailabilityPublication(fb, thread, publication, carrier),
        );
        txIds.push(txIdOf(outputs, "Publication"));
        thread = outputs[0]!;
        carrier = outputs[1];
      }
      challenge.threads[index] = thread;
      challenge.carriers[index] = carrier;
    }
    return { txIds };
  };

  const settle: DaBondPoolJourneyPort["settle"] = async (headerHash) => {
    const block = requireBlock(headerHash);
    const challenge = requireChallenge(block);
    const fb = await liveBlock(block);
    const txIds: string[] = [];
    asChallenger();
    try {
      const next = Number(
        Data.from(
          challenge.terminal.datum!,
          SDK.DaAvailabilityTerminalAccumulatorDatum,
        ).next_tranche_index,
      );
      for (let index = next; index < challenge.threads.length; index += 1) {
        const outputs = await f.submit(
          `settle ${block.label} tranche ${index}`,
          buildAvailabilitySettlement(
            fb,
            challenge.open,
            challenge.record,
            challenge.terminal,
            challenge.threads[index]!,
            challenge.carriers[index],
          ),
        );
        challenge.terminal = outputs[0]!;
        txIds.push(txIdOf(outputs, "Settlement"));
      }
    } finally {
      asResponder();
    }
    return { txIds };
  };

  const close: DaBondPoolJourneyPort["close"] = async (headerHash) => {
    const block = requireBlock(headerHash);
    const challenge = requireChallenge(block);
    const fb = await liveBlock(block);
    asChallenger();
    try {
      const outputs = await f.submit(
        `close ${block.label}`,
        buildAvailabilityClose(
          fb,
          challenge.open,
          challenge.record,
          fb.target.stateQueueUtxo.utxo,
          challenge.terminal,
        ),
      );
      return { txId: txIdOf(outputs, "Close") };
    } finally {
      asResponder();
    }
  };

  const timeout: DaBondPoolJourneyPort["timeout"] = async (headerHash) => {
    const block = requireBlock(headerHash);
    const challenge = requireChallenge(block);
    // The commit and the Open moved the root and the node since the block's
    // fixture was taken; the Timeout spends the live ones.
    const fb = await withLiveAvailabilityQueue(block.fixture);
    const pool = await f.getPool();
    const terminal = Data.from(
      challenge.terminal.datum!,
      SDK.DaAvailabilityTerminalAccumulatorDatum,
    );
    asChallenger();
    const name = `timeout ${block.label}`;
    let outputs: UTxO[];
    try {
      const { tx } = await buildAvailabilityTimeout(
        fb,
        challenge.open,
        challenge.record,
        fb.target.stateQueueUtxo.utxo,
        challenge.terminal,
        { pool },
      );
      outputs = await f.submit(name, tx);
    } finally {
      asResponder();
    }
    block.removed = (await liveAvailabilityTarget(fb)) === undefined;
    const poolOutputs = outputs.filter((u) => u.assets[f.poolUnit] === 1n);
    const poolOutput = poolOutputs[0];
    if (poolOutputs.length !== 1 || poolOutput === undefined)
      throw new Error("The Timeout must produce exactly one pool output");
    const challengerOutputs = outputs.filter(
      (u) => u.address === f.challenger.address,
    );
    const measured = [...f.measurements].reverse().find((m) => m.name === name);
    if (measured === undefined) throw new Error(`Unmeasured ${name}`);
    return {
      txId: txIdOf(outputs, "Timeout"),
      fee: measured.fee,
      challengerOutputLovelace: challengerOutputs.reduce(
        (total, u) => total + (u.assets.lovelace ?? 0n),
        0n,
      ),
      poolBefore: pool.assets.lovelace ?? 0n,
      poolAfter: poolOutput.assets.lovelace ?? 0n,
      challengerRemainingLovelace: terminal.remaining_challenger_lovelace,
      challengerOutputCount: challengerOutputs.length,
      poolDatumAndNftKept:
        poolOutput.address === pool.address &&
        poolOutput.datum === pool.datum &&
        Object.keys(poolOutput.assets).length === 2,
    };
  };

  const removeOrPrune: DaBondPoolJourneyPort["removeOrPrune"] = async (
    headerHash,
  ) => {
    // The harness's Timeout removes the head in the same transaction
    // (`RemoveUnavailableBlockAfterTimeout` / `RemoveTimedOutHead`), so a
    // block the Timeout left queued is a failure, not a pending prune.
    throw new Error(
      `Block ${requireBlock(headerHash).label} survived its Timeout; the emulator adapter has no separate removal`,
    );
  };

  const blockStatus: DaBondPoolJourneyPort["blockStatus"] = async (
    headerHash,
  ): Promise<DaBondPoolJourneyBlockStatus> => {
    const block = requireBlock(headerHash);
    const target = await liveAvailabilityTarget(block.fixture);
    if (target === undefined) {
      if (block.removed) return "removed";
      // No merge runs in the emulator journey.
      throw new Error(`Block ${block.label} left the queue unexpectedly`);
    }
    const status = target.stateQueueNode.da_attestation;
    if (status === "Unattested") return "Unattested";
    if ("Attested" in status) return "Attested";
    if ("Challenged" in status) return "Challenged";
    return "Published";
  };

  return {
    fixture: f,
    dispose: () => rmSync(dir, { recursive: true, force: true }),
    params,
    now: async () => f.emulator.now(),
    poolSnapshot,
    observeAlerts,
    commitBlock,
    attest,
    open,
    respondAll,
    settle,
    close,
    awaitTime: async (posixMs) => f.advanceToMs(posixMs),
    timeout,
    removeOrPrune,
    topUp: async (amount) => {
      try {
        const result = await daBondTopUpCommand(ctx, {
          amount: amount.toString(),
          walletSecret: f.responder.privateKey,
        });
        return { txId: result.txHash };
      } finally {
        asResponder();
      }
    },
    beginWithdraw: async () => {
      const { built, assembled } = await quorumStep("begin");
      if (built.unlockAt === undefined)
        throw new Error("withdraw begin reported no unlock_at");
      return { txId: assembled.txHash, unlockAt: Number(built.unlockAt) };
    },
    cancelWithdraw: async () => ({
      txId: (await quorumStep("cancel")).assembled.txHash,
    }),
    completeWithdraw: async (amount) => ({
      txId: (
        await quorumStep("complete", {
          amount: amount.toString(),
          to: f.responder.address,
        })
      ).assembled.txHash,
    }),
    blockStatus,
  };
};
