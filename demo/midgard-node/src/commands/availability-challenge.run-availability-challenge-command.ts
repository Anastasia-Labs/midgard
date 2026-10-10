import { isAbsolute, normalize } from "node:path";

import { openAvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import { verifyFinalizedDeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  type Network,
  paymentCredentialOf,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { createScalusEvaluator } from "@lucid-evolution/scalus-uplc";

import { resolveNetwork } from "./address-from-seed.js";
import { buildAvailabilityCommandTransaction } from "./availability-challenge.build-availability-command-transaction.js";
import {
  type AvailabilityCommandAction,
  availabilityCommandBuildContext,
  type AvailabilityCommandOptions,
  planAvailabilityCommandAction,
} from "./availability-challenge.plan-availability-command-action.js";
import { availabilityDeploymentFromManifest } from "./availability-challenge-deployment.js";
import {
  assertAvailabilityActionServed,
  availabilityCommandCanonicalSource,
} from "./availability-challenge-source.js";
import { readDeploymentManifestFile } from "./contract-deployment-info.js";
import {
  commandLucid,
  selectToolL1Access,
  type ToolL1Access,
  withCommandL1Access,
} from "./l1-command-access.js";
import { assertCommandPayerIsDedicated } from "./operational-wallet-refusal.js";

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
  // An action the selected access cannot observe is refused before anything
  // is read, opened, built or submitted.
  assertAvailabilityActionServed(action, { kind: selectToolL1Access(env) });
  const manifest = readDeploymentManifestFile(options.manifest);
  verifyFinalizedDeploymentManifest(manifest);
  const network = resolveNetwork({ network: manifest.network, env });
  // The node's operational wallets never pay for an availability action.
  assertCommandPayerIsDedicated({
    command: "availability",
    walletSeedEnv: options.walletSeedEnv,
    payerAddress: walletFromSeed(seed, { network, addressType: "Enterprise" })
      .address,
    referenceScriptDeployAddress: manifest.referenceScriptDeployAddress,
    network,
    env,
  });
  return withCommandL1Access({ network, env }, (access) =>
    runAvailabilityOnAccess(action, options, seed, manifest, network, access),
  );
};

/**
 * The command over the tool L1 access `--l1` selects: Lucid on the access's
 * provider and slot mapping, the canonical source over the same access
 * (`availability-challenge-source.ts`), submission through the provider.
 */
const runAvailabilityOnAccess = async (
  action: AvailabilityCommandAction,
  options: AvailabilityCommandOptions,
  seed: string,
  manifest: ReturnType<typeof readDeploymentManifestFile>,
  network: Network,
  access: ToolL1Access,
): Promise<unknown> => {
  const lucid = await commandLucid(access, network, {
    evaluator: createScalusEvaluator(),
  });
  lucid.selectWallet.fromSeed(seed, { addressType: "Enterprise" });
  const actorAddress = await lucid.wallet().address();
  const actor = paymentCredentialOf(actorAddress);
  if (actor.type !== "Key")
    throw new Error("Availability actuation requires a payment-key wallet");
  const deployment = await availabilityDeploymentFromManifest(lucid, manifest);
  const source = await availabilityCommandCanonicalSource({ lucid, access });
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
      submit: (cbor) => access.provider.submitTx(cbor),
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
          availabilityCommandBuildContext(manifest, source.unitHistory),
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
