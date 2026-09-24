import { existsSync } from "node:fs";
import { readdir } from "node:fs/promises";
import { join } from "node:path";

import {
  aikenSerialisedPlutusDataCborPreservingMapOrder as canonical,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import { buildCountedRoot } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import type { PublishedDepositTraceCheckpoint } from "midgard-watcher/tests/support/published-deposit-trace";

import { readJourneyArtifact, writeJourneyArtifact } from "./artifacts.js";
import type { JourneySuccessor } from "./fixture.js";
import {
  buildJourneyRepeatedDeposit,
  type CapturedEventHistoryWitness,
  captureStagedHistoryEvent,
} from "./history-events.js";
import { verifyJourneyResultEvidence } from "./readiness-evidence.js";
import {
  createPreparedJourneyFixture,
  decodeJourneyRetainedBlock,
  type JourneyFaultPreparationInput,
} from "./staging.js";

type SettlementCheckpoint = {
  deploymentFingerprint: string;
  headerHash: string;
  signedCbor: string;
  txHash: string;
  outcome: "prepared" | "submitted" | "unknown" | "confirmed";
  settlement?: UTxO;
};

export type DuplicateEventHistory = {
  deploymentFingerprint: string;
  source: JourneySuccessor;
  deposit: CapturedEventHistoryWitness;
  readyAt: bigint;
};

/** Content binding only; canonical commitment and healthy verdict are separate gates. */
export const verifyDuplicateEventSource = async (
  source: JourneySuccessor,
  deposit: CapturedEventHistoryWitness,
) => {
  if (
    (await Effect.runPromise(SDK.hashBlockHeader(source.header))) !==
    source.headerHash
  )
    throw new Error("Duplicate-event source header hash changed");
  const retained = await decodeJourneyRetainedBlock(source);
  const commitment = Data.from(
    deposit.commitmentCbor,
    SDK.EventHistoryCommitment,
  );
  const opening = Data.from(deposit.openingCbor, SDK.EventHistoryOpening);
  if (
    commitment.kind !== "Deposit" ||
    !("DepositPayload" in opening.payload) ||
    !SDK.opensEventHistoryCommitmentCbor(
      commitment,
      plutusConstrFieldCbor(deposit.openingCbor, [0]),
      plutusConstrFieldCbor(deposit.openingCbor, [1]),
    )
  )
    throw new Error(
      "Duplicate-event source does not contain the exact genuine deposit",
    );
  const payloadCbor = plutusConstrFieldCbor(deposit.openingCbor, [0]);
  const entries = retained.payload.block_body.deposits;
  if (
    entries.length !== 1 ||
    source.header.depositCount !== 1n ||
    entries[0]?.[0] !== canonical(plutusConstrFieldCbor(payloadCbor, [0, 0])) ||
    entries[0]?.[1] !== canonical(plutusConstrFieldCbor(payloadCbor, [0, 1])) ||
    commitment.inclusion_time <= source.header.startTime ||
    commitment.inclusion_time > source.header.endTime
  )
    throw new Error(
      "Duplicate-event source does not contain the exact genuine deposit",
    );
  const root = await buildCountedRoot(
    SDK.ROOT_DOMAINS.deposits,
    entries.map(([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: Buffer.from(value, "hex"),
    })),
  );
  if (root.root !== source.header.depositsRoot)
    throw new Error(
      "Duplicate-event source deposit root differs from its retained bytes",
    );
  return source.header.endTime + SDK.MATURITY_DURATION_MS;
};

/** Adopt the verified trace successor; its real deposit already started the clock. */
export const prepareDuplicateEventHistory = async (
  input: JourneyFaultPreparationInput,
): Promise<DuplicateEventHistory> => {
  const path = join(input.directory, "duplicate-event-history.json");
  if (existsSync(path)) {
    const existing = await readJourneyArtifact<DuplicateEventHistory>(path);
    if (
      existing.deploymentFingerprint !==
      input.context.deployment.manifest.manifestId
    )
      throw new Error("Duplicate-event history belongs to another deployment");
    if (
      existing.readyAt !==
      (await verifyDuplicateEventSource(existing.source, existing.deposit))
    )
      throw new Error(
        "Recorded duplicate-event maturity differs from the protocol",
      );
    await input.retain(existing.source, existing.source.commitTxHash);
    return existing;
  }
  const traceDirectory = join(
    input.context.runDirectory,
    "work/journeys/transition-trace",
  );
  const verified = await verifyJourneyResultEvidence(
    input.context.runDirectory,
    traceDirectory,
    "transitionTrace",
    input.context.deployment,
  );
  const source = await readJourneyArtifact<JourneySuccessor>(
    join(traceDirectory, "successor.json"),
  );
  if (source.headerHash !== verified.successor)
    throw new Error("Verified trace successor changed before history adoption");
  const staged = await readJourneyArtifact<PublishedDepositTraceCheckpoint>(
    join(traceDirectory, "staged.json"),
  );
  const contracts = input.context.deployment.contracts;
  const historyDeployment = SDK.eventHistoryDeploymentFromContracts(
    SDK.requireEventHistoryContracts(contracts).deposit,
  );
  const orders = await Effect.runPromise(
    SDK.fetchDepositUTxOsProgram(
      input.context.deployment.operatorLucid,
      historyDeployment,
    ),
  );
  const order = orders.find(
    (candidate) =>
      candidate.assetName === staged.depositMetadata.depositAssetName,
  );
  if (
    order === undefined ||
    staged.depositEvent.datum == null ||
    staged.depositMetadata.depositAuthUnit !==
      historyDeployment.policyId + staged.depositMetadata.depositAssetName
  )
    throw new Error("Trace source deposit lacks its exact live L1 event role");
  const prior = Data.from(staged.depositEvent.datum, SDK.EventHistoryNode);
  if (
    prior.payload === "RootContent" ||
    !("Order" in prior.payload) ||
    order.history.anchor.utxo.datum == null ||
    canonical(plutusConstrFieldCbor(staged.depositEvent.datum, [3, 0])) !==
      canonical(
        plutusConstrFieldCbor(order.history.anchor.utxo.datum, [3, 0]),
      ) ||
    !SDK.assetsEqual(
      SDK.eventHistoryOriginalAssets(
        prior,
        staged.depositEvent.assets,
        historyDeployment.policyId,
      ),
      order.originalAssets,
    )
  )
    throw new Error(
      "Trace source deposit immutable facts or original funds changed",
    );
  const deposit = captureStagedHistoryEvent({
    order,
    policyId: historyDeployment.policyId,
  });
  const readyAt = await verifyDuplicateEventSource(source, deposit);
  await input.retain(source, source.commitTxHash);
  const history: DuplicateEventHistory = {
    deploymentFingerprint: input.context.deployment.manifest.manifestId,
    source,
    deposit,
    readyAt,
  };
  await writeJourneyArtifact(path, history);
  return history;
};

/**
 * Merge the actual queue prefix through the selected source using the existing
 * SDK. Immature blocks return their due time; this never waits seven days or
 * changes block timestamps. Every signed merge survives submission ambiguity.
 */
export const settleDuplicateEventHistory = async (
  input: JourneyFaultPreparationInput,
  history: DuplicateEventHistory,
): Promise<
  { status: "ready"; settlement: UTxO } | { status: "waiting"; readyAt: bigint }
> => {
  const { deployment, provider } = input.context;
  if (history.deploymentFingerprint !== deployment.manifest.manifestId)
    throw new Error("Settlement history belongs to another deployment");
  if (input.predecessor.headerHash === history.source.headerHash)
    throw new Error(
      "Commit a later healthy journey head before settling the duplicate-event source",
    );
  const lucid = deployment.operatorLucid;
  const { contracts } = deployment;
  const targetUnit = contracts.settlement.policyId + history.source.headerHash;
  const settlementAt = (unit: string) =>
    provider.getUtxosWithUnit(contracts.settlement.spendingScriptAddress, unit);
  const reference = (name: string) => {
    const found = deployment.references.get(name);
    if (found === undefined)
      throw new Error(`Missing published ${name} reference`);
    return found;
  };
  // Reconcile records before advancing the queue. A previous merge can have
  // confirmed between submission and acknowledgement, moving its input out of
  // the queue before this process had recorded the outcome.
  for (const name of await readdir(input.directory)) {
    if (!/^settlement-[0-9a-f]{56}\.json$/u.test(name)) continue;
    const path = join(input.directory, name);
    const checkpoint = await readJourneyArtifact<SettlementCheckpoint>(path);
    if (checkpoint.deploymentFingerprint !== history.deploymentFingerprint)
      throw new Error("Settlement journal belongs to another deployment");
    if (
      CML.hash_transaction(
        CML.Transaction.from_cbor_hex(checkpoint.signedCbor).body(),
      ).to_hex() !== checkpoint.txHash
    )
      throw new Error(
        "Settlement journal signed bytes differ from the recorded transaction",
      );
    const found = await settlementAt(
      contracts.settlement.policyId + checkpoint.headerHash,
    );
    if (found.length > 1) throw new Error("Duplicate recorded settlement role");
    if (found[0] !== undefined) {
      if (found[0].txHash !== checkpoint.txHash)
        throw new Error(
          "Recorded settlement was created by a different transaction",
        );
      if (checkpoint.outcome !== "confirmed") {
        checkpoint.outcome = "confirmed";
        checkpoint.settlement = found[0];
        await writeJourneyArtifact(path, checkpoint);
      }
    } else if (checkpoint.outcome !== "confirmed") {
      const tx = CML.Transaction.from_cbor_hex(checkpoint.signedCbor);
      if (CML.hash_transaction(tx.body()).to_hex() !== checkpoint.txHash)
        throw new Error("Recorded merge hash differs from signed bytes");
      const inputs = tx.body().inputs();
      const references = Array.from({ length: inputs.len() }, (_, index) => ({
        txHash: inputs.get(index).transaction_id().to_hex(),
        outputIndex: Number(inputs.get(index).index()),
      }));
      const available = await provider.getUtxosByOutRef(references);
      if (available.length !== references.length)
        throw new Error(
          `Merge ${checkpoint.txHash} has spent inputs without its settlement; reconcile before extending history`,
        );
    } else if (checkpoint.headerHash === history.source.headerHash) {
      throw new Error(
        "Required settlement has already been consumed; a fresh duplicate-event fixture is necessary",
      );
    }
  }
  for (;;) {
    const target = await settlementAt(targetUnit);
    if (target.length > 1) throw new Error("Duplicate settlement role token");
    if (target[0] !== undefined) {
      const datum = Data.from(target[0].datum!, SDK.SettlementDatum);
      if (datum.deposits_root !== history.source.header.depositsRoot)
        throw new Error(
          "Settlement does not preserve the retained historical deposit root",
        );
      return { status: "ready", settlement: target[0] };
    }
    const fetchConfig = {
      stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
      stateQueuePolicyId: contracts.stateQueue.policyId,
    };
    const queue = await Effect.runPromise(
      SDK.fetchConfirmedStateAndItsLinkProgram(lucid, fetchConfig),
    );
    const block = await Effect.runPromise(
      SDK.getHeaderFromStateQueueDatum(queue.link.datum),
    );
    const headerHash = await Effect.runPromise(SDK.hashBlockHeader(block));
    const maturity = Number(block.endTime + SDK.MATURITY_DURATION_MS);
    const validFrom = lucid.slotToUnixTime(lucid.unixTimeToSlot(maturity) + 1);
    if (deployment.chain.now() < validFrom)
      return { status: "waiting", readyAt: BigInt(validFrom) };
    const path = join(input.directory, `settlement-${headerHash}.json`);
    let checkpoint: SettlementCheckpoint;
    if (existsSync(path)) {
      checkpoint = await readJourneyArtifact<SettlementCheckpoint>(path);
      if (
        checkpoint.deploymentFingerprint !== history.deploymentFingerprint ||
        checkpoint.headerHash !== headerHash ||
        CML.hash_transaction(
          CML.Transaction.from_cbor_hex(checkpoint.signedCbor).body(),
        ).to_hex() !== checkpoint.txHash
      )
        throw new Error(
          "Settlement checkpoint identity or signed bytes changed",
        );
    } else {
      const [hub, correctionLock] = await Promise.all([
        Effect.runPromise(
          SDK.fetchHubOracleUTxOProgram(lucid, {
            hubOracleAddress: contracts.hubOracle.spendingScriptAddress,
            hubOraclePolicyId: contracts.hubOracle.policyId,
          }),
        ),
        Effect.runPromise(
          SDK.fetchCorrectionLockUTxOProgram(lucid, {
            correctionLockAddress:
              contracts.correctionLock.spendingScriptAddress,
            hubOraclePolicyId: contracts.hubOracle.policyId,
          }),
        ),
      ]);
      const funding = (
        await lucid.utxosAt(await lucid.wallet().address())
      ).filter(
        (utxo) =>
          utxo.datum == null &&
          utxo.datumHash == null &&
          utxo.scriptRef == null &&
          Object.keys(utxo.assets).every((unit) => unit === "lovelace"),
      );
      if (funding.length === 0)
        throw new Error("Settlement has no ordinary funding input");
      lucid.overrideUTxOs(funding);
      const built = await Effect.runPromise(
        SDK.buildMergeToConfirmedStateTxProgram({
          lucid,
          contracts,
          fetchConfig,
          confirmedUTxO: queue.confirmed,
          firstBlockUTxO: queue.link,
          validFrom,
          presetWalletInputs: funding,
          hubOracleRefInput: hub.utxo,
          correctionLockRefInput: correctionLock,
          stateQueueMergeYieldRefInput: reference("stateQueueMergeWithdraw"),
          referenceScripts: {
            stateQueueSpending: reference("stateQueueSpend"),
            stateQueueMinting: reference("stateQueueMint"),
            settlementMinting: reference("settlementMint"),
          },
        }),
      );
      const signed = await built.tx.sign.withWallet().complete();
      checkpoint = {
        deploymentFingerprint: history.deploymentFingerprint,
        headerHash,
        signedCbor: signed.toCBOR(),
        txHash: signed.toHash(),
        outcome: "prepared",
      };
      await writeJourneyArtifact(path, checkpoint);
    }
    input.onStage(`settlement ${headerHash}`);
    checkpoint.outcome = "unknown";
    await writeJourneyArtifact(path, checkpoint);
    const unit = contracts.settlement.policyId + headerHash;
    try {
      const txHash = await provider.submitTx(checkpoint.signedCbor);
      if (txHash !== checkpoint.txHash)
        throw new Error("Provider changed the signed merge hash");
      checkpoint.outcome = "submitted";
      await writeJourneyArtifact(path, checkpoint);
    } catch (cause) {
      const found = await settlementAt(unit);
      if (found.length !== 1 || found[0]?.txHash !== checkpoint.txHash)
        throw new Error(
          `Settlement ${checkpoint.txHash} needs reconciliation; exact signed record retained`,
          { cause },
        );
    }
    await provider.awaitTx(checkpoint.txHash, 500);
    const settled = await settlementAt(unit);
    if (settled.length !== 1 || settled[0]?.txHash !== checkpoint.txHash)
      throw new Error("Confirmed merge lacks its exact settlement output");
    checkpoint.outcome = "confirmed";
    checkpoint.settlement = settled[0];
    await writeJourneyArtifact(path, checkpoint);
  }
};

export const crossBlockDuplicateEventJourneyFixture =
  createPreparedJourneyFixture("crossBlockDuplicateEvent", async (input) => {
    const history = await prepareDuplicateEventHistory(input);
    const settlement = await settleDuplicateEventHistory(input, history);
    if (settlement.status === "waiting")
      throw new Error(
        `Duplicate-event history is time-gated until ${new Date(Number(settlement.readyAt)).toISOString()}; continue other families from the staged healthy head`,
      );
    const settled = await decodeJourneyRetainedBlock(history.source);
    return {
      buildFault: (timed) => buildJourneyRepeatedDeposit({ ...timed, settled }),
      buildSuccessor: (timed) =>
        buildJourneyRepeatedDeposit({ ...timed, settled, honest: true }),
    };
  });
