import { createHash } from "node:crypto";
import { existsSync, readFileSync } from "node:fs";
import { join } from "node:path";

import { credentialToAddress } from "@lucid-evolution/lucid";

import {
  ACCEPTANCE_FIRST_DRILLS,
  ACCEPTANCE_LAST_DRILLS,
} from "./acceptance.js";
import {
  ACCEPTANCE_PHASE_DRILLS,
  requireInjectedDrills,
} from "./acceptance-journey.js";
import { captureAcceptanceNativeBoundary } from "./acceptance-native-boundary.js";
import { collectAcceptancePayoutLineages } from "./acceptance-payout-collect.js";
import { verifyAcceptanceCurrentPayouts } from "./acceptance-payout-current.js";
import {
  openAcceptanceKupoReads,
  readAcceptanceBoundedFile,
} from "./acceptance-payout-sources.js";
import { readAcceptanceSettlements } from "./acceptance-payout-sql.js";
import { requireAcceptance } from "./acceptance-payout-types.js";
import { drillCatalogue, type DrillRecord, selectDrills } from "./chaos.js";
import { owedRestorePath } from "./chaos-restore.js";
import { Journal } from "./journal.js";
import type { WithdrawalRecord } from "./journey-values.js";
import { type Layout, readRunEnv } from "./layout.js";
import { readFinalizedManifest } from "./watcher-release.js";

export type AcceptancePayoutReadLimits = Readonly<{
  timeoutMs: number;
  maxTransactionBytes: number;
  maxLineageTransactions: number;
  maxSettlementRows: number;
  maxKupoResponseBytes: number;
  maxUtxoResponseBytes: number;
  maxReferenceInputs: number;
  blockScanLimit: number;
  maxDrillEvidenceBytes: number;
}>;

/** Read-only final receipt. Submission/deployment/provider wallet services are unreachable here. */
export const sampleAcceptancePayouts = async (
  layout: Layout,
  limits: AcceptancePayoutReadLimits,
  signal?: AbortSignal,
) => {
  for (const [name, bound] of Object.entries(limits))
    requireAcceptance(
      Number.isSafeInteger(bound) && bound > 0 && bound <= 2_147_483_647,
      `invalid explicit ${name} read bound`,
    );
  const run = readRunEnv(layout);
  const manifest = readFinalizedManifest(layout);
  const digest = (path: string) =>
    createHash("sha256").update(readFileSync(path)).digest("hex");
  const journeyJournal = join(layout.journeyDir, "journey.json");
  const journalDigest = digest(journeyJournal);
  const runDigest = digest(layout.runEnv);
  const journal = new Journal(journeyJournal);
  requireAcceptance(
    journal.get("phase:holdings") === "done",
    "full journey has not finished",
  );
  const drillBytes = readAcceptanceBoundedFile(
    layout.drillsLog,
    limits.maxDrillEvidenceBytes,
  );
  const drillDigest = createHash("sha256").update(drillBytes).digest("hex");
  const drills = drillBytes
    .toString("utf8")
    .trim()
    .split("\n")
    .map((line) => JSON.parse(line) as DrillRecord);
  requireInjectedDrills(
    selectDrills(
      drillCatalogue([
        "node",
        "da-committee-0",
        "da-committee-1",
        "public-retained-da",
        "watcher",
      ]),
      [
        ...ACCEPTANCE_FIRST_DRILLS,
        ...Object.values(ACCEPTANCE_PHASE_DRILLS),
        ...ACCEPTANCE_LAST_DRILLS,
      ],
    ),
    drills,
  );
  requireAcceptance(
    drills.every(
      (record) =>
        Number.isFinite(Date.parse(record.injectedAt!)) &&
        Number.isFinite(Date.parse(record.recoveredAt!)) &&
        Date.parse(record.injectedAt!) <= Date.parse(record.recoveredAt!) &&
        Date.parse(record.recoveredAt!) <= Date.now(),
    ),
    "drill injection/recovery timestamps invalid",
  );
  requireAcceptance(
    !existsSync(owedRestorePath(layout.drillsLog)),
    "final payout capture has an owed restore",
  );
  const records = ["W1", "W2", "W3", "W4"].map((id) => {
    const record = journal.get<WithdrawalRecord>(`withdrawal:${id}`);
    requireAcceptance(
      record !== undefined && typeof record.withdrawalEventId === "string",
      `completed withdrawal ${id} is missing`,
    );
    return record;
  });
  const scriptHash = (name: string) => {
    const hash = manifest.contracts[name]?.scriptHash;
    requireAcceptance(
      typeof hash === "string" && /^[0-9a-f]{56}$/u.test(hash),
      "deployment payout contract missing",
    );
    return hash;
  };
  const config = {
    network: manifest.network,
    withdrawalPolicyId: scriptHash("withdrawalMint"),
    withdrawalAddress: credentialToAddress(manifest.network, {
      type: "Script",
      hash: scriptHash("withdrawalSpend"),
    }),
    payoutPolicyId: scriptHash("payoutMint"),
    payoutAddress: credentialToAddress(manifest.network, {
      type: "Script",
      hash: scriptHash("payoutSpend"),
    }),
    confirmationDepth: BigInt(manifest.l1Finality.confirmationDepth),
    maxTransactionBytes: limits.maxTransactionBytes,
    maxLineageTransactions: limits.maxLineageTransactions,
  };
  const result = await captureAcceptanceNativeBoundary(
    {
      layout,
      timeoutMs: limits.timeoutMs,
      ...(signal === undefined ? {} : { signal }),
    },
    async (scope) => {
      const kupoUrl = `http://127.0.0.1:${run.kupoPort}`;
      const kupo = openAcceptanceKupoReads(
        scope,
        kupoUrl,
        limits.maxKupoResponseBytes,
      );
      try {
        const readSnapshot = () =>
          readAcceptanceSettlements(
            run,
            scope,
            manifest.manifestId,
            records.map((record) => record.withdrawalEventId),
            limits.maxSettlementRows,
            limits.maxTransactionBytes,
          );
        const snapshot = await readSnapshot();
        const proofs = await collectAcceptancePayoutLineages({
          scope,
          records,
          snapshot,
          config,
          kupoUrl,
          fetchImpl: kupo.fetchImpl,
          blockScanLimit: limits.blockScanLimit,
          maxReferenceInputs: limits.maxReferenceInputs,
        });
        const refs = proofs.map((proof) => proof.beneficiary);
        verifyAcceptanceCurrentPayouts(
          await scope.queryExactOutRefs(refs),
          proofs,
          limits.maxUtxoResponseBytes,
        );
        scope.assertCurrent();
        const refreshed = await readSnapshot();
        requireAcceptance(
          refreshed.generation === snapshot.generation &&
            JSON.stringify(refreshed.attempts) ===
              JSON.stringify(snapshot.attempts),
          "settlement evidence changed during final capture",
        );
        const current = verifyAcceptanceCurrentPayouts(
          await scope.queryExactOutRefs(refs),
          proofs,
          limits.maxUtxoResponseBytes,
        );
        scope.assertCurrent();
        requireAcceptance(
          digest(journeyJournal) === journalDigest &&
            digest(layout.runEnv) === runDigest &&
            createHash("sha256")
              .update(
                readAcceptanceBoundedFile(
                  layout.drillsLog,
                  limits.maxDrillEvidenceBytes,
                ),
              )
              .digest("hex") === drillDigest &&
            !existsSync(owedRestorePath(layout.drillsLog)),
          "run configuration or journey journal changed during capture",
        );
        return {
          deploymentManifestId: manifest.manifestId,
          historyGeneration: refreshed.generation,
          current,
          lineages: proofs.map((proof) => ({
            eventId: proof.eventId,
            eventKey: proof.eventKey,
            order: proof.order,
            currentOrder: proof.currentOrder,
            payout: proof.payout,
            beneficiary: proof.beneficiary,
            lineage: proof.lineage,
          })),
        };
      } finally {
        await kupo.close();
      }
    },
  );
  requireAcceptance(
    result.boundary.deploymentManifestId === manifest.manifestId,
    "native boundary belongs to a different deployment",
  );
  return { ...result.value, boundary: result.boundary };
};
