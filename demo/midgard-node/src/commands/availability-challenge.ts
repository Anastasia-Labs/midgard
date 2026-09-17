import { readFile, stat } from "node:fs/promises";
import { isAbsolute, normalize } from "node:path";

import { openAvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import { verifyFinalizedDeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import {
  calculateMinLovelaceFromUTxO,
  Data,
  Kupmios,
  Lucid,
  type LucidEvolution,
  paymentCredentialOf,
  type UTxO,
} from "@lucid-evolution/lucid";
import { createScalusEvaluator } from "@lucid-evolution/scalus-uplc";
import { Effect } from "effect";

import { availabilityDeploymentFromManifest } from "./availability-challenge-deployment.js";
import { availabilityCommandCanonicalSource } from "./availability-challenge-source.js";
import { readDeploymentManifestFile } from "./contract-deployment-info.js";
import { resolveKupmiosConfig } from "./l1-utxos.js";

export type AvailabilityCommandAction =
  | "open"
  | "respond"
  | "settle"
  | "close"
  | "timeout"
  | "status"
  | "recover";
export type AvailabilityCommandOptions = Readonly<{
  manifest: string;
  journal: string;
  headerHash: string;
  walletSeedEnv: string;
  collateralOutRef?: string;
  fundingOutRef?: string;
  payloadFile?: string;
  trancheIndex?: number;
  kupoUrl?: string;
  ogmiosUrl?: string;
}>;

export const parseAvailabilityOutRef = (
  value: string,
): { txHash: string; outputIndex: number } => {
  const match = /^([0-9a-f]{64})#(0|[1-9][0-9]*)$/u.exec(value);
  const outputIndex = Number(match?.[2]);
  if (!match || !Number.isSafeInteger(outputIndex) || outputIndex > 65_535)
    throw new Error(
      "Availability output references must be canonical txHash#outputIndex",
    );
  return { txHash: match[1]!, outputIndex };
};

export const runAvailabilityChallengeCommand = async (
  action: AvailabilityCommandAction,
  options: AvailabilityCommandOptions,
  env: NodeJS.ProcessEnv = process.env,
): Promise<unknown> => {
  if (!/^[0-9a-f]{56}$/u.test(options.headerHash))
    throw new Error(
      "Availability --header-hash must be exactly 28 lowercase hex bytes",
    );
  if (
    !isAbsolute(options.journal) ||
    normalize(options.journal) !== options.journal
  )
    throw new Error(
      "Availability --journal requires a canonical absolute durable path",
    );
  if (
    options.trancheIndex !== undefined &&
    (!Number.isSafeInteger(options.trancheIndex) ||
      options.trancheIndex < 0 ||
      options.trancheIndex >= 16)
  )
    throw new Error("Availability --tranche-index must be between 0 and 15");
  if (!/^[A-Za-z_][A-Za-z0-9_]*$/u.test(options.walletSeedEnv))
    throw new Error(
      "Availability --wallet-seed-env must name one explicit environment variable",
    );
  const seed = env[options.walletSeedEnv]?.trim();
  if (!seed)
    throw new Error(
      `Availability actor seed is missing from ${options.walletSeedEnv}`,
    );
  const manifest = readDeploymentManifestFile(options.manifest);
  verifyFinalizedDeploymentManifest(manifest);
  const connection = resolveKupmiosConfig({
    kupoUrl: options.kupoUrl,
    ogmiosUrl: options.ogmiosUrl,
    network: manifest.network,
    env,
  });
  const provider = new Kupmios(connection.kupoUrl, connection.ogmiosUrl);
  const lucid = await Lucid(provider, connection.network, {
    evaluator: createScalusEvaluator(),
  });
  lucid.selectWallet.fromSeed(seed, { addressType: "Enterprise" });
  const actorAddress = await lucid.wallet().address();
  const actor = paymentCredentialOf(actorAddress);
  if (actor.type !== "Key")
    throw new Error("Availability actuation requires a payment-key wallet");
  const operationalSeeds = [
    "L1_OPERATOR_SEED_PHRASE",
    "L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX",
    "L1_REFERENCE_SCRIPT_SEED_PHRASE",
  ];
  if (operationalSeeds.includes(options.walletSeedEnv))
    throw new Error(
      "Availability requires a dedicated actor seed environment variable",
    );
  const operationalHashes = new Set<string>([
    manifest.availabilityChallenge.bondOwnerCredential,
    paymentCredentialOf(manifest.referenceScriptDeployAddress).hash,
  ]);
  for (const name of operationalSeeds) {
    if (env[name]?.trim()) {
      lucid.selectWallet.fromSeed(env[name]!.trim());
      operationalHashes.add(
        paymentCredentialOf(await lucid.wallet().address()).hash,
      );
    }
  }
  if (operationalHashes.has(actor.hash))
    throw new Error(
      "Availability actor payment credential overlaps an operational node wallet",
    );
  lucid.selectWallet.fromSeed(seed, { addressType: "Enterprise" });
  const deployment = await availabilityDeploymentFromManifest(lucid, manifest);
  const source = availabilityCommandCanonicalSource({ lucid, ...connection });
  const journal = openAvailabilityOperationJournal(options.journal);
  try {
    const canonicalAnchor = await source.readBoundary();
    const context: SDK.DaAvailabilityOperationContext = {
      deploymentIdentity: manifest.manifestId,
      actor: actor.hash,
      journal,
      stateQueuePolicyId: deployment.contracts.stateQueue.policyId,
      minimumConfirmationDepth: manifest.l1Finality.confirmationDepth,
      transactionLimits: SDK.daAvailabilityOperationLimits(
        lucid,
        deployment.parameters,
      ),
      assertActuationCurrent: async () => {
        await source.assertCanonicalAncestor(canonicalAnchor);
      },
      observe: source.observe,
      submit: (cbor) => provider.submitTx(cbor),
    };
    if (action === "recover") {
      return {
        action,
        headerHash: options.headerHash,
        operations: await SDK.reconcileDaAvailabilityOperations(context),
      };
    }
    if (action !== "status") {
      const operations = await SDK.reconcileDaAvailabilityOperations(context);
      if (
        operations.some(
          (operation) =>
            operation.status !== "included" &&
            operation.status !== "confirmed" &&
            operation.status !== "expired",
        )
      ) {
        return {
          action,
          headerHash: options.headerHash,
          status: "reconciling",
          operations,
        };
      }
    }
    const before = await source.readBoundary();
    const snapshot = await SDK.fetchDaAvailabilityChallengeSnapshot(
      lucid,
      deployment,
      options.headerHash,
    );
    const after = await source.readBoundary();
    if (before.pointId !== after.pointId)
      throw new Error(
        "Availability state changed during canonical discovery; rerun against the next stable point",
      );
    if (action === "status") {
      return {
        action,
        deploymentIdentity: manifest.manifestId,
        actor: actor.hash,
        headerHash: options.headerHash,
        canonicalPoint: after.pointId,
        bondState: snapshot.bondDatum
          ? "Available" in snapshot.bondDatum
            ? "available"
            : "challenged"
          : "absent",
        responseDeadline:
          snapshot.bondDatum && "ChallengedBond" in snapshot.bondDatum
            ? snapshot.bondDatum.ChallengedBond.response_deadline.toString()
            : null,
        nextTrancheIndex:
          snapshot.terminalDatum?.next_tranche_index.toString() ?? null,
        hasTimedOutTranche:
          snapshot.terminalDatum?.has_timed_out_tranche ?? false,
        queueAvailability: snapshot.queue
          ? Data.castFrom(snapshot.queue.datum.data, SDK.StateQueueNode)
              .da_attestation
          : null,
        tranches: snapshot.tranches.map(({ utxo, datum, carrier }) => {
          const fields = "Active" in datum ? datum.Active : datum.Receipt;
          return {
            index: fields.descriptor.tranche_index.toString(),
            state: "Active" in datum ? "active" : "published",
            nextOffset:
              "Active" in datum ? datum.Active.next_offset.toString() : null,
            outRef: `${utxo.txHash}#${utxo.outputIndex}`,
            carrierOutRef: carrier
              ? `${carrier.txHash}#${carrier.outputIndex}`
              : null,
          };
        }),
        unfinalized: journal
          .unfinalized(manifest.manifestId, actor.hash)
          .map(({ intent, state, inclusionPoint, detail }) => ({
            txHash: intent.txHash,
            action: intent.action,
            headerHash: intent.headerHash,
            state,
            inclusionPoint,
            detail,
          })),
      };
    }
    const operation = planAvailabilityCommandAction(
      action,
      snapshot,
      Date.now(),
    );
    const result = await SDK.runDaAvailabilityOperation(context, {
      headerHash: options.headerHash,
      action: operation,
      completesWorkflow:
        operation === "timeout" ? snapshot.descendant === undefined : undefined,
      build: async () =>
        (
          await buildAvailabilityCommandTransaction(
            lucid,
            deployment,
            snapshot,
            operation,
            options,
            actor.hash,
            new Set(journal.reservedOutRefs(actor.hash)),
          )
        ).tx,
    });
    return { action, operation, headerHash: options.headerHash, ...result };
  } finally {
    journal.close();
  }
};

export const planAvailabilityCommandAction = (
  action: Exclude<AvailabilityCommandAction, "status" | "recover">,
  snapshot: Pick<
    SDK.DaAvailabilityChallengeSnapshot,
    | "bondDatum"
    | "terminalDatum"
    | "correctionLock"
    | "headerHash"
    | "descendant"
  >,
  nowMs: number,
): SDK.DaAvailabilityTransactionAction => {
  if (action === "respond") return "publish";
  if (action !== "timeout") return action;
  if (snapshot.bondDatum && "ChallengedBond" in snapshot.bondDatum) {
    const bond = snapshot.bondDatum.ChallengedBond;
    if (BigInt(nowMs) <= bond.response_deadline)
      throw new Error(
        "Availability timeout requires the strict response deadline to have passed",
      );
    if (!snapshot.terminalDatum)
      throw new Error(
        "Availability timeout is missing its terminal accumulator",
      );
    if (
      snapshot.terminalDatum.next_tranche_index <
      BigInt(bond.commitment.tranche_descriptors.length)
    )
      return "settle";
    if (!snapshot.terminalDatum.has_timed_out_tranche)
      throw new Error(
        "Fully answered availability challenges must close instead of timing out",
      );
    return "timeout";
  }
  const lock = snapshot.correctionLock.datum
    ? Data.from(snapshot.correctionLock.datum, SDK.CorrectionLockDatum)
    : undefined;
  if (
    typeof lock !== "object" ||
    !lock ||
    !("Locked" in lock) ||
    lock.Locked.target_header_hash !== snapshot.headerHash ||
    typeof lock.Locked.correction_identity !== "object" ||
    !("AvailabilityChallenge" in lock.Locked.correction_identity)
  ) {
    throw new Error("Availability timeout has no matching active removal lock");
  }
  return snapshot.descendant ? "prune" : "remove";
};

const required = <T>(value: T | undefined, name: string): T => {
  if (value === undefined)
    throw new Error(`Availability action requires ${name}`);
  return value;
};

const liveOutRef = async (
  lucid: LucidEvolution,
  label: string | undefined,
  name: string,
): Promise<UTxO> => {
  const reference = parseAvailabilityOutRef(required(label, name));
  const utxos = await lucid.utxosByOutRef([reference]);
  if (utxos.length !== 1)
    throw new Error(`Availability ${name} is not currently unspent`);
  return utxos[0]!;
};

export const assertAvailabilityCommandRemovalCapital = (input: {
  readonly action: "open" | "timeout" | "prune" | "remove";
  readonly parameters: SDK.DaAvailabilityParameters;
  readonly remainingRemovalSteps: number;
  readonly minimumChangeLovelace: bigint;
  readonly walletAddress: string;
  readonly walletUtxos: readonly UTxO[];
  readonly collateral: UTxO;
  readonly reservedOutRefs: ReadonlySet<string>;
}): void => {
  if (
    !Number.isSafeInteger(input.remainingRemovalSteps) ||
    input.remainingRemovalSteps < 1
  )
    throw new Error(
      "Availability capital requires an authenticated remaining removal path",
    );
  const outRef = (utxo: UTxO) => `${utxo.txHash}#${utxo.outputIndex}`;
  const available = input.walletUtxos.filter(
    (utxo) =>
      utxo.address === input.walletAddress &&
      utxo.datum == null &&
      utxo.datumHash == null &&
      utxo.scriptRef == null &&
      Object.keys(utxo.assets).length === 1 &&
      (utxo.assets.lovelace ?? 0n) > 0n &&
      outRef(utxo) !== outRef(input.collateral) &&
      !input.reservedOutRefs.has(outRef(utxo)),
  );
  const requiredCapital =
    BigInt(input.remainingRemovalSteps) *
      input.parameters.max_timeout_fee_lovelace +
    input.minimumChangeLovelace +
    (input.action === "open"
      ? input.parameters.challenger_bond_lovelace +
        input.parameters.max_open_fee_lovelace
      : 0n);
  if (
    available.reduce((sum, utxo) => sum + utxo.assets.lovelace, 0n) <
    requiredCapital
  )
    throw new Error(
      "Availability actor cannot fund the challenger bond and remaining descendant removal path after excluding collateral and reserved inputs",
    );
};

const buildAvailabilityCommandTransaction = async (
  lucid: LucidEvolution,
  deployment: SDK.DaAvailabilityDeployment,
  snapshot: SDK.DaAvailabilityChallengeSnapshot,
  action: SDK.DaAvailabilityTransactionAction,
  options: AvailabilityCommandOptions,
  actor: string,
  reservedOutRefs: ReadonlySet<string>,
): Promise<SDK.BuiltDaAvailabilityTransaction> => {
  const p = deployment.parameters;
  const feeLovelace =
    action === "open"
      ? p.max_open_fee_lovelace
      : action === "publish"
        ? p.max_publication_fee_lovelace
        : action === "settle"
          ? p.max_settlement_fee_lovelace
          : action === "close"
            ? p.max_close_fee_lovelace
            : p.max_timeout_fee_lovelace;
  const collateral = await liveOutRef(
    lucid,
    options.collateralOutRef,
    "--collateral-out-ref",
  );
  const liveQueue =
    action === "open" ||
    action === "timeout" ||
    action === "prune" ||
    action === "remove"
      ? await SDK.fetchSortedStateQueueUTxOs(lucid, {
          stateQueueAddress:
            deployment.contracts.stateQueue.spendingScriptAddress,
          stateQueuePolicyId: deployment.contracts.stateQueue.policyId,
        })
      : undefined;
  if (
    liveQueue !== undefined &&
    (action === "open" ||
      action === "timeout" ||
      action === "prune" ||
      action === "remove")
  ) {
    const queue = required(snapshot.queue, "the authenticated queue header");
    const targetIndex = liveQueue.findIndex(
      ({ utxo }) =>
        utxo.txHash === queue.utxo.txHash &&
        utxo.outputIndex === queue.utxo.outputIndex,
    );
    if (targetIndex < 1)
      throw new Error(
        "Availability capital check cannot find the current challenged queue header",
      );
    const protocol = required(
      lucid.config().protocolParameters,
      "live protocol parameters",
    );
    const walletAddress = await lucid.wallet().address();
    assertAvailabilityCommandRemovalCapital({
      action,
      parameters: p,
      remainingRemovalSteps: liveQueue.length - targetIndex,
      minimumChangeLovelace: calculateMinLovelaceFromUTxO(
        protocol.coinsPerUtxoByte,
        {
          address: walletAddress,
          assets: { lovelace: 2_000_000n },
          txHash: "00".repeat(32),
          outputIndex: 0,
        },
      ),
      walletAddress,
      walletUtxos: await lucid.wallet().getUtxos(),
      collateral,
      reservedOutRefs,
    });
  }
  const bondDatum = snapshot.bondDatum;
  const challenged =
    bondDatum && "ChallengedBond" in bondDatum
      ? bondDatum.ChallengedBond
      : undefined;
  const now = Date.now();
  const expiredSettlement =
    action === "settle" &&
    snapshot.tranches.some(
      ({ datum }) =>
        "Active" in datum &&
        datum.Active.descriptor.tranche_index ===
          snapshot.terminalDatum?.next_tranche_index,
    );
  if (
    expiredSettlement &&
    challenged &&
    BigInt(now) <= challenged.response_deadline
  )
    throw new Error(
      "Active availability tranches cannot settle before the strict response deadline",
    );
  const protocolLower =
    (action === "timeout" || expiredSettlement) && challenged
      ? challenged.response_deadline + 1n
      : 0n;
  const backedOff = BigInt(Math.max(0, now - 60_000));
  const validFrom = protocolLower > backedOff ? protocolLower : backedOff;
  let validTo = validFrom + 120_000n;
  if (
    action === "publish" &&
    challenged &&
    validTo > challenged.response_deadline + 1n
  )
    validTo = challenged.response_deadline + 1n;
  const resources = {
    collateralInputs: [collateral],
    feeLovelace,
    validFrom,
    validTo,
  };
  const queue = () =>
    required(snapshot.queue, "the authenticated queue header").utxo;
  const bond = () => required(snapshot.bond, "the authenticated retained bond");
  const terminal = () =>
    required(snapshot.terminal, "the terminal accumulator");
  if (action === "open")
    return Effect.runPromise(
      SDK.buildOpenDaAvailabilityChallengeTxProgram(lucid, deployment, {
        ...resources,
        bond: bond(),
        queue: queue(),
        challengerFunding: await liveOutRef(
          lucid,
          options.fundingOutRef,
          "--funding-out-ref",
        ),
        challenger: actor,
      }),
    );
  if (action === "publish") {
    const activeBond = required(challenged, "an opened challenge");
    const path = required(
      options.payloadFile,
      "--payload-file with the exact retained envelope bytes",
    );
    if (
      BigInt((await stat(path)).size) !==
      activeBond.commitment.payload_byte_length
    )
      throw new Error(
        "Availability payload file length differs from the frozen commitment",
      );
    const payload = await readFile(path);
    const plans = SDK.planDaAvailabilityPublications({
      commitment: activeBond.commitment,
      payload,
      challengeAssetName: activeBond.challenge_asset_name,
    });
    const tranche = required(
      snapshot.tranches.find(
        ({ datum }) =>
          "Active" in datum &&
          (options.trancheIndex === undefined ||
            datum.Active.descriptor.tranche_index ===
              BigInt(options.trancheIndex)),
      ),
      "an active requested tranche",
    );
    if (!("Active" in tranche.datum))
      throw new Error("Availability response tranche is already terminal");
    const active = tranche.datum.Active;
    const publication = required(
      plans
        .find(
          (plan) =>
            plan.descriptor.tranche_index === active.descriptor.tranche_index,
        )
        ?.publications.find((item) => item.chunk_offset === active.next_offset),
      "the next committed chunk",
    );
    return Effect.runPromise(
      SDK.buildPublishDaAvailabilityChunkTxProgram(lucid, deployment, {
        ...resources,
        thread: tranche.utxo,
        previousCarrier: tranche.carrier,
        publication,
      }),
    );
  }
  if (action === "settle") {
    const tranche = required(
      snapshot.tranches.find(
        ({ datum }) =>
          ("Active" in datum ? datum.Active : datum.Receipt).descriptor
            .tranche_index === snapshot.terminalDatum?.next_tranche_index,
      ),
      "the next unsettled tranche",
    );
    return Effect.runPromise(
      SDK.buildSettleDaAvailabilityTrancheTxProgram(lucid, deployment, {
        ...resources,
        bond: bond(),
        terminal: terminal(),
        thread: tranche.utxo,
        carrier: tranche.carrier,
      }),
    );
  }
  if (action === "close")
    return Effect.runPromise(
      SDK.buildCloseDaAvailabilityChallengeTxProgram(lucid, deployment, {
        ...resources,
        bond: bond(),
        terminal: terminal(),
        queue: queue(),
      }),
    );
  const lock = snapshot.correctionLock.datum
    ? Data.from(snapshot.correctionLock.datum, SDK.CorrectionLockDatum)
    : undefined;
  const lockChallenge =
    typeof lock === "object" &&
    lock &&
    "Locked" in lock &&
    typeof lock.Locked.correction_identity === "object" &&
    "AvailabilityChallenge" in lock.Locked.correction_identity
      ? lock.Locked.correction_identity.AvailabilityChallenge
          .challenge_asset_name
      : undefined;
  const removal = {
    ...resources,
    queue: queue(),
    confirmedState: snapshot.confirmedState.utxo,
    descendant: snapshot.descendant?.utxo,
    correctionLock: snapshot.correctionLock,
    challengeAssetName: required(
      challenged?.challenge_asset_name ?? lockChallenge,
      "the authenticated removal challenge identity",
    ),
    headerHash: options.headerHash,
    rentRefundAddress: await lucid.wallet().address(),
    feeFunding: options.fundingOutRef
      ? await liveOutRef(lucid, options.fundingOutRef, "--funding-out-ref")
      : undefined,
    fundingQueueTailRefInput:
      action === "timeout" ? liveQueue?.at(-1)?.utxo : undefined,
  };
  if (action === "timeout")
    return Effect.runPromise(
      SDK.buildTimeoutDaAvailabilityChallengeTxProgram(lucid, deployment, {
        ...removal,
        bond: bond(),
        terminal: terminal(),
      }),
    );
  if (action === "prune")
    return Effect.runPromise(
      SDK.buildPruneDaUnavailableBlockDescendantTxProgram(
        lucid,
        deployment,
        removal,
      ),
    );
  return Effect.runPromise(
    SDK.buildRemoveDaUnavailableHeadTxProgram(lucid, deployment, removal),
  );
};
