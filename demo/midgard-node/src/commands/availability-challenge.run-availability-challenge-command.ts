import { isAbsolute, normalize } from "node:path";

import { openAvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import { verifyFinalizedDeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, Lucid, paymentCredentialOf } from "@lucid-evolution/lucid";
import { createScalusEvaluator } from "@lucid-evolution/scalus-uplc";

import {
  makeNodeKupmios,
  nativeLedgerSettingsFromEnv,
} from "../services/native-ledger.js";
import { buildAvailabilityCommandTransaction } from "./availability-challenge.build-availability-command-transaction.js";
import {
  type AvailabilityCommandAction,
  availabilityCommandBuildContext,
  type AvailabilityCommandOptions,
  planAvailabilityCommandAction,
} from "./availability-challenge.plan-availability-command-action.js";
import { availabilityDeploymentFromManifest } from "./availability-challenge-deployment.js";
import { availabilityCommandCanonicalSource } from "./availability-challenge-source.js";
import { readDeploymentManifestFile } from "./contract-deployment-info.js";
import { resolveKupmiosConfig } from "./l1-utxos.js";

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
  const provider = makeNodeKupmios({
    kupoUrl: connection.kupoUrl,
    ogmiosUrl: connection.ogmiosUrl,
    network: connection.network,
    nativeLedger: nativeLedgerSettingsFromEnv(env),
  });
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
      const record = snapshot.recordDatum;
      return {
        action,
        deploymentIdentity: manifest.manifestId,
        actor: actor.hash,
        headerHash: options.headerHash,
        canonicalPoint: after.pointId,
        challengeRecord:
          record && snapshot.record
            ? {
                outRef: `${snapshot.record.txHash}#${snapshot.record.outputIndex}`,
                challengeAssetName: record.challenge_asset_name,
                challenger: record.challenger,
                openedAt: record.opened_at.toString(),
              }
            : null,
        responseDeadline: record?.response_deadline.toString() ?? null,
        daBondPool:
          snapshot.pool && snapshot.poolDatum
            ? {
                outRef: `${snapshot.pool.txHash}#${snapshot.pool.outputIndex}`,
                state:
                  snapshot.poolDatum === "Bonded" ? "bonded" : "withdrawing",
                lovelace: (snapshot.pool.assets.lovelace ?? 0n).toString(),
                backing: SDK.daBondPoolBacking({
                  lovelace: snapshot.pool.assets.lovelace ?? 0n,
                  parameters: deployment.parameters,
                }).toString(),
              }
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
      // A built transaction carries the timeout's slashed fee part, which the
      // executor needs: its timeout fee ceiling caps only the challenger's share.
      build: () =>
        buildAvailabilityCommandTransaction(
          lucid,
          deployment,
          availabilityCommandBuildContext(manifest, connection.kupoUrl),
          snapshot,
          operation,
          options,
          actor.hash,
          new Set(journal.reservedOutRefs(actor.hash)),
        ),
    });
    return { action, operation, headerHash: options.headerHash, ...result };
  } finally {
    journal.close();
  }
};
