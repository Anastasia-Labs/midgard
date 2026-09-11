import { appendFile, readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { setTimeout as pause } from "node:timers/promises";

import { DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Kupmios, Lucid, type UTxO } from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

import { nodeRuntimeReferenceScriptTargets } from "../src/transactions/reference-scripts.js";
import { loadRealMidgardContractsForTest } from "./helpers/real-midgard-contracts.js";
import {
  DEFAULT_PUBLICATION_SCHEDULE,
  PublicationJournal,
  publishReferenceChain,
  synchronizePublicationIndexer,
} from "./helpers/reference-publication-chain.js";

const runDirectory = process.env.MIDGARD_PUBLICATION_NODE_RUN_DIR;
const probe = process.env.MIDGARD_PUBLICATION_NODE_PROBE ?? "bounded";

/** An isolated real-node support/recovery probe, using the same publisher. */
it.skipIf(runDirectory === undefined)(
  "publishes a bounded real-node reference chain and reconciles its durable journal",
  async () => {
    if (runDirectory === undefined)
      throw new Error("Probe run directory is required");
    const genesis = JSON.parse(
      await readFile(
        join(runDirectory, "genesis/shelley-genesis.json"),
        "utf8",
      ),
    );
    expect(genesis).toMatchObject({
      networkMagic: 424242,
      slotLength: 1,
      activeSlotsCoeff: 0.05,
      securityParam: 2160,
    });
    const env = Object.fromEntries(
      (await readFile(join(runDirectory, "run.env"), "utf8"))
        .trim()
        .split("\n")
        .map((line) => {
          const separator = line.indexOf("=");
          return [line.slice(0, separator), line.slice(separator + 1)];
        }),
    );
    const ogmiosUrl = `http://127.0.0.1:${env.MIDGARD_PHASE4_OGMIOS_PORT}`;
    const kupoUrl = `http://127.0.0.1:${env.MIDGARD_PHASE4_KUPO_PORT}`;
    const provider = new Kupmios(kupoUrl, ogmiosUrl);
    const lucid = await Lucid(provider, "Custom", {
      slotConfig: {
        zeroTime: Date.parse(genesis.systemStart),
        zeroSlot: 0,
        slotLength: 1000,
      },
    });
    const publisher = JSON.parse(
      await readFile(
        join(runDirectory, "secrets/publication-probe.json"),
        "utf8",
      ),
    );
    lucid.selectWallet.fromSeed(publisher.seedPhrase);
    const preparedPath = join(
      runDirectory,
      `work/publication-probe-${probe}-prepared.json`,
    );
    const journalPath = join(
      runDirectory,
      `work/publication-probe-${probe}.ndjson`,
    );
    let prepared: {
      nonce: Pick<UTxO, "txHash" | "outputIndex">;
      authPolicy: SDK.ReferenceScriptAuthPolicy;
      startedAt: number;
    };
    try {
      prepared = JSON.parse(await readFile(preparedPath, "utf8"));
    } catch (cause) {
      if ((cause as NodeJS.ErrnoException).code !== "ENOENT") throw cause;
      const [nonce] = SDK.selectReferenceScriptFundingUtxos(
        await lucid.wallet().getUtxos(),
        100_000_000n,
      );
      if (nonce === undefined)
        throw new Error("Probe publisher needs confirmed funding");
      prepared = {
        nonce: { txHash: nonce.txHash, outputIndex: nonce.outputIndex },
        authPolicy: SDK.createReferenceScriptAuthPolicy(
          lucid,
          Date.now(),
          probe === "expired" ? 8_137 : 15 * 60_000 + 137,
        ),
        startedAt: Date.now(),
      };
      await writeFile(preparedPath, JSON.stringify(prepared), {
        flag: "wx",
        mode: 0o600,
      });
    }
    if (probe === "expired") {
      const targets = Object.keys(
        DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
      )
        .slice(0, 3)
        .map((name) => ({ name, script: prepared.authPolicy.mintingScript }));
      let failure: unknown;
      try {
        await publishReferenceChain({
          lucid,
          targets,
          authPolicy: prepared.authPolicy,
          journalPath,
          maxTargetsPerBatch: 1,
          schedule: {
            ...DEFAULT_PUBLICATION_SCHEDULE,
            maxUnconfirmedTransactions: 2,
          },
          publicationLimit: () => 15_872,
          synchronize: () => synchronizePublicationIndexer(ogmiosUrl, kupoUrl),
          wait: async () => {
            await pause(1000);
          },
          now: Date.now,
        });
      } catch (cause) {
        failure = cause;
      }
      const journal = await PublicationJournal.open(journalPath);
      try {
        const records = [...journal.records.values()];
        expect(records.length).toBeGreaterThanOrEqual(2);
        expect(records.every(({ outcome }) => outcome === "rejected")).toBe(
          true,
        );
        expect(records[1]!.transaction.dependencies).toContain(
          records[0]!.transaction.hash,
        );
        const eventLines = (await readFile(journalPath, "utf8"))
          .trim()
          .split("\n")
          .map((line) => JSON.parse(line));
        expect(
          eventLines.filter(
            (event) =>
              event.kind === "outcome" && event.outcome === "submitted",
          ).length,
        ).toBeGreaterThanOrEqual(2);
        await writeFile(
          join(runDirectory, "work/publication-probe-expired-evidence.json"),
          JSON.stringify(
            {
              probe,
              authority: prepared.authPolicy.policyId,
              authorityExpiresAtSlot: prepared.authPolicy.expiresAtSlot,
              failure: String(failure),
              records: records.map(({ transaction, outcome, reason }) => ({
                hash: transaction.hash,
                dependencies: transaction.dependencies,
                outcome,
                reason,
              })),
              allSubmissionOutcomesResolved: true,
              duplicateRoles: false,
            },
            null,
            2,
          ),
        );
      } finally {
        await journal.close();
      }
      return;
    }
    const contracts = await loadRealMidgardContractsForTest(
      prepared.nonce,
      prepared.authPolicy,
    );
    const targets = nodeRuntimeReferenceScriptTargets(contracts).slice(0, 8);
    const submit = provider.submitTx.bind(provider);
    let submitted = 0;
    provider.submitTx = async (cbor) => {
      const body = CML.Transaction.from_cbor_hex(cbor).body();
      const inputStatuses = await Promise.all(
        Array.from({ length: body.inputs().len() }, (_, index) =>
          provider.getTransactionStatus(
            body.inputs().get(index).transaction_id().to_hex(),
          ),
        ),
      );
      const hash = await submit(cbor);
      await appendFile(
        join(
          runDirectory,
          `work/publication-probe-${probe}-submissions.ndjson`,
        ),
        JSON.stringify({ hash, submittedAt: Date.now(), inputStatuses }) + "\n",
      );
      submitted += 1;
      if (
        process.env.MIDGARD_PUBLICATION_NODE_INTERRUPT === "after-submit" &&
        submitted === 1
      ) {
        // Deliberate abrupt process loss after the real node accepted the exact
        // journaled bytes. The next invocation must preserve this same identity.
        process.kill(process.pid, "SIGKILL");
      }
      return hash;
    };
    const result = await publishReferenceChain({
      lucid,
      targets,
      authPolicy: prepared.authPolicy,
      journalPath,
      maxTargetsPerBatch: 1,
      schedule: {
        ...DEFAULT_PUBLICATION_SCHEDULE,
        maxUnconfirmedTransactions: ["bytes", "capacity"].includes(probe)
          ? 8
          : 2,
        maxUnconfirmedBytes: probe === "bytes" ? 16_384 : 100_000,
        confirmationAllowanceMs: 180_000,
      },
      publicationLimit: () => 15_872,
      synchronize: () => synchronizePublicationIndexer(ogmiosUrl, kupoUrl),
      wait: async () => {
        await pause(1000);
      },
      now: Date.now,
    });
    expect(result.transactions).toHaveLength(8);
    expect(result.transactions.flatMap(({ roles }) => roles)).toHaveLength(8);
    expect(result.metrics.peakUnconfirmedTransactions).toBeLessThanOrEqual(
      ["bytes", "capacity"].includes(probe) ? 8 : 2,
    );
    expect(result.metrics.peakUnconfirmedBytes).toBeLessThanOrEqual(
      probe === "bytes" ? 16_384 : 100_000,
    );
    const journal = await PublicationJournal.open(journalPath);
    try {
      expect(
        [...journal.records.values()].every(
          ({ outcome }) => outcome === "confirmed",
        ),
      ).toBe(true);
    } finally {
      await journal.close();
    }
    const bytes = result.transactions.reduce(
      (sum, tx) => sum + tx.signedBytes,
      0,
    );
    const durationMs = Date.now() - prepared.startedAt;
    const evidence = {
      network: "isolated devnet with verified Preprod configuration",
      probe,
      ...result.metrics,
      durationMs,
      signedBytes: bytes,
      transactionsPerSecond: result.transactions.length / (durationMs / 1000),
      bytesPerSecond: bytes / (durationMs / 1000),
      constructionTransactionsPerSecond:
        result.transactions.length /
        ((result.metrics.constructionDurationMs +
          result.metrics.submissionDurationMs) /
          1000),
      roles: result.transactions.flatMap((tx) =>
        tx.roles.map(({ role }) => ({
          role,
          contract:
            DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE[
              role as keyof typeof DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE
            ],
          txHash: tx.hash,
        })),
      ),
      transactions: result.transactions.map(
        ({ hash, dependencies, signedBytes }) => ({
          hash,
          dependencies,
          signedBytes,
        }),
      ),
    };
    await writeFile(
      join(runDirectory, `work/publication-probe-${probe}-evidence.json`),
      JSON.stringify(evidence, null, 2),
    );
    console.info(
      JSON.stringify({
        probe,
        ...result.metrics,
        durationMs,
        signedBytes: bytes,
      }),
    );
  },
  12 * 60_000,
);
