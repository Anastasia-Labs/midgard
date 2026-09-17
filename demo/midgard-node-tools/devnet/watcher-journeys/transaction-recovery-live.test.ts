import { existsSync } from "node:fs";
import { readFile } from "node:fs/promises";
import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";

import {
  createLocalKupmiosHttpOgmiosRawSource,
  readAdmittedLocalKupmiosBoundary,
  readAdmittedLocalKupmiosSignedTransactionRecovery,
  type SignedWorkflowTransaction,
} from "@al-ft/midgard-fault-proofs";
import {
  loadWatcherVerifiedDeploymentAuthority,
  parseWatcherProcessConfig,
  parseWatcherProverFundingReservationRecord,
  watcherDeploymentReleaseFinalityAuthority,
} from "midgard-watcher";
import { expect, it } from "vitest";

import { readJourneyArtifact, writeJourneyArtifact } from "./artifacts.js";
import { loadJourneyContext } from "./live-context.js";

const runDirectory = process.env.MIDGARD_WATCHER_JOURNEY_RUN_DIR;

it.skipIf(runDirectory === undefined)(
  "derives expiry, rebroadcast eligibility and recorded inclusion from the actual node",
  async () => {
    const context = await loadJourneyContext(runDirectory!);
    const directory = join(runDirectory!, "work/journeys/transition-trace");
    const processConfig = parseWatcherProcessConfig(
      JSON.parse(
        await readFile(join(directory, "watcher-process.json"), "utf8"),
      ),
    );
    const config = processConfig.watcherConfig;
    const authority = await loadWatcherVerifiedDeploymentAuthority({
      path: processConfig.deploymentAuthorityPath,
      ruleBundlePath: processConfig.ruleBundlePath,
    });
    const releaseFinality = await watcherDeploymentReleaseFinalityAuthority(
      authority.deploymentIdentity,
    ).verifyForWorkflow({
      deploymentFingerprint: context.deployment.manifest.manifestId,
    });
    if (config.l1.source.sourceMode !== "local_node")
      throw new Error(
        "Transaction recovery probe requires the actual local node",
      );
    const services = config.l1.source.queryServices;
    const endpoint = (kind: "kupo" | "ogmios") => {
      const service = services.find((service) => service.kind === kind);
      if (service === undefined) throw new Error(`Missing ${kind} endpoint`);
      return service.endpoint;
    };
    const source = createLocalKupmiosHttpOgmiosRawSource({
      sourceId: "journey-signed-transaction-recovery",
      kupoHttpUrl: endpoint("kupo"),
      ogmiosUrl: endpoint("ogmios"),
      releaseFinality,
      timeoutMs: config.l1.requestTimeoutMs,
    });
    const boundary = await readAdmittedLocalKupmiosBoundary({ source });
    const lucid = context.deployment.publisherLucid;
    const address = await lucid.wallet().address();
    const funding = (await context.provider.getUtxos(address))
      .filter(
        (utxo) =>
          utxo.datum == null &&
          utxo.datumHash == null &&
          utxo.scriptRef == null &&
          Object.keys(utxo.assets).every((unit) => unit === "lovelace") &&
          utxo.assets.lovelace >= 5_000_000n,
      )
      .sort(
        (left, right) =>
          left.txHash.localeCompare(right.txHash) ||
          left.outputIndex - right.outputIndex,
      )[0];
    if (funding === undefined)
      throw new Error("Probe lacks an independent plain-Ada publisher input");
    const expirySlot = Number(boundary.kupoCheckpoint.slot) - 1;
    const built = await lucid
      .newTx()
      .collectFrom([funding])
      .pay.ToAddress(address, { lovelace: 2_000_000n })
      .validFrom(lucid.slotToUnixTime(expirySlot - 60))
      .validTo(lucid.slotToUnixTime(expirySlot))
      .complete({
        coinSelection: false,
        localUPLCEval: true,
        changeAddress: address,
      });
    const signed = await built.sign.withWallet().complete();
    const expiredIntent = {
      transactionHash: signed.toHash(),
      signedTransactionCborHex: signed.toCBOR(),
    };
    // This diagnostic signs only. It never submits or reserves the publisher input.
    await writeJourneyArtifact(
      join(runDirectory!, "work/never-submitted-expired-intent.json"),
      {
        deploymentFingerprint: context.deployment.manifest.manifestId,
        purpose: "Read-only production reconciliation probe; never submit",
        expirySlot,
        funding,
        ...expiredIntent,
      },
    );
    const started = performance.now();
    const expired = await readAdmittedLocalKupmiosSignedTransactionRecovery({
      source,
      ...expiredIntent,
    });
    expect(expired.status).toBe("expired");
    const tipSlot = Number(boundary.ogmiosTip.slot);
    const validBuilt = await lucid
      .newTx()
      .collectFrom([funding])
      .pay.ToAddress(address, { lovelace: 2_000_000n })
      .validFrom(lucid.slotToUnixTime(tipSlot - 60))
      .validTo(lucid.slotToUnixTime(tipSlot + 600))
      .complete({
        coinSelection: false,
        localUPLCEval: true,
        changeAddress: address,
      });
    const validSigned = await validBuilt.sign.withWallet().complete();
    const validIntent = {
      transactionHash: validSigned.toHash(),
      signedTransactionCborHex: validSigned.toCBOR(),
    };
    await writeJourneyArtifact(
      join(runDirectory!, "work/never-submitted-valid-intent.json"),
      {
        deploymentFingerprint: context.deployment.manifest.manifestId,
        purpose:
          "Read-only mempool/rebroadcast eligibility probe; never submit",
        expirySlot: tipSlot + 600,
        funding,
        ...validIntent,
      },
    );
    const rebroadcast = await readAdmittedLocalKupmiosSignedTransactionRecovery(
      {
        source,
        ...validIntent,
      },
    );
    expect(rebroadcast.status).toBe("rebroadcast");
    const noExpiryBuilt = await lucid
      .newTx()
      .collectFrom([funding])
      .pay.ToAddress(address, { lovelace: 2_000_000n })
      .complete({
        coinSelection: false,
        localUPLCEval: true,
        changeAddress: address,
      });
    const noExpirySigned = await noExpiryBuilt.sign.withWallet().complete();
    const noExpiryIntent = {
      transactionHash: noExpirySigned.toHash(),
      signedTransactionCborHex: noExpirySigned.toCBOR(),
    };
    await writeJourneyArtifact(
      join(runDirectory!, "work/never-submitted-no-expiry-intent.json"),
      {
        deploymentFingerprint: context.deployment.manifest.manifestId,
        purpose: "Read-only no-TTL recovery eligibility probe; never submit",
        ...noExpiryIntent,
      },
    );
    const noExpiry = await readAdmittedLocalKupmiosSignedTransactionRecovery({
      source,
      ...noExpiryIntent,
    });
    expect(noExpiry.status).toBe("rebroadcast");
    const evidencePath = join(
      runDirectory!,
      "work/transaction-recovery-source-evidence.json",
    );
    let recorded: SignedWorkflowTransaction | undefined;
    const database = new DatabaseSync(
      join(runDirectory!, "work/journeys/runtime/watcher.sqlite"),
      { readOnly: true },
    );
    try {
      const reservations = database
        .prepare(
          "SELECT canonical_json FROM watcher_prover_funding_reservation_v1",
        )
        .all()
        .map((row) => {
          if (typeof row.canonical_json !== "string")
            throw new Error("Funding reservation omitted its canonical record");
          return parseWatcherProverFundingReservationRecord(
            JSON.parse(row.canonical_json),
          );
        });
      recorded =
        reservations.find(
          (reservation) => reservation.pendingTransition !== null,
        )?.pendingTransition ?? undefined;
    } finally {
      database.close();
    }
    // Retain the same input case after the workflow has completed and cleared
    // its pending reservation. The node independently rechecks inclusion below.
    if (recorded === undefined && existsSync(evidencePath)) {
      const previous = await readJourneyArtifact<{
        deploymentFingerprint: string;
        recordedTransaction: SignedWorkflowTransaction;
      }>(evidencePath);
      expect(previous.deploymentFingerprint).toBe(
        context.deployment.manifest.manifestId,
      );
      recorded = previous.recordedTransaction;
    }
    if (recorded === undefined)
      throw new Error(
        "Probe requires a recorded chain-included signed transaction",
      );
    const included = await readAdmittedLocalKupmiosSignedTransactionRecovery({
      source,
      transactionHash: recorded.transactionHash,
      signedTransactionCborHex: recorded.signedTransactionCborHex,
    });
    expect(included.status).toBe("included");
    await writeJourneyArtifact(evidencePath, {
      deploymentFingerprint: context.deployment.manifest.manifestId,
      observedAt: new Date().toISOString(),
      durationMs: performance.now() - started,
      submittedTransactions: 0,
      recordedTransaction: {
        transactionHash: recorded.transactionHash,
        signedTransactionCborHex: recorded.signedTransactionCborHex,
      },
      expired,
      rebroadcast,
      noExpiry,
      included,
    });
  },
  180_000,
);
