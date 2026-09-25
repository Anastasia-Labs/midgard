import { writeSync } from "node:fs";
import { access, readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { setTimeout as pause } from "node:timers/promises";

import * as SDK from "@al-ft/midgard-sdk";
import { CML, Kupmios, Lucid } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect, it } from "vitest";

import { synchronizePublicationIndexer } from "../src/transactions/reference-publication-provider.js";
import { ensureReferenceScriptTargetsProgram } from "../src/transactions/reference-scripts.js";

const runDirectory = process.env.MIDGARD_PUBLICATION_LOCAL_DEVNET_DIR;
const mode = process.env.MIDGARD_PUBLICATION_LOCAL_DEVNET_MODE ?? "chained";
const drill =
  process.env.MIDGARD_PUBLICATION_LOCAL_DEVNET_DRILL ?? "comparison";
const drillOffsets = {
  comparison: 0,
  ambiguous: 20_000,
  expiry: 40_000,
  restart: 60_000,
  rollback: 80_000,
};

it.skipIf(runDirectory === undefined)(
  "publishes and authenticates a bounded workload through local Cardano, Ogmios and Kupo",
  async () => {
    if (runDirectory === undefined || (mode !== "serial" && mode !== "chained"))
      throw new Error(
        "Explicit local devnet directory and serial/chained mode required",
      );
    if (!(drill in drillOffsets))
      throw new Error("Unknown publication recovery drill");
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
          const i = line.indexOf("=");
          return [line.slice(0, i), line.slice(i + 1)];
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
    // Reconstruct only the test deployment identity after a restart. Signed bytes
    // and dependencies remain in memory, including in this probe.
    const authPolicy = await SDK.createReferenceScriptAuthPolicy(
      lucid,
      Date.parse(genesis.systemStart),
      4 * 60 * 60_000 +
        10_000 +
        drillOffsets[drill as keyof typeof drillOffsets] +
        (mode === "chained" ? 1000 : 0),
    );
    const blueprint = JSON.parse(
      await readFile(
        new URL("../../../onchain/aiken/plutus.json", import.meta.url),
        "utf8",
      ),
    );
    const compiledCode: string = blueprint.validators.find(
      (validator: { title: string }) =>
        validator.title ===
        "fraud_proofs/cross_block_duplicate_event/step_01.main.spend",
    ).compiledCode;
    const script = { type: "PlutusV3" as const, script: compiledCode };
    const targets = Object.keys(SDK.REFERENCE_SCRIPT_AUTH_TOKEN_NAMES)
      .slice(0, 8)
      .map((name) => ({ name, script }));
    const address = await lucid.wallet().address();
    const existing = await lucid.utxosAt(address);
    const initialReferenceCount = targets.filter((target) =>
      existing.some((output) =>
        SDK.hasReferenceScriptAuthRole(output, target, authPolicy),
      ),
    ).length;
    const readTip = async (): Promise<string> => {
      const response = await fetch(ogmiosUrl, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          jsonrpc: "2.0",
          method: "queryLedgerState/tip",
          params: {},
          id: "publication-measurement",
        }),
      });
      const body = await response.json();
      expect(response.ok).toBe(true);
      expect(body.result.id).toMatch(/^[0-9a-f]{64}$/);
      return body.result.id;
    };
    const records: {
      hash: string;
      bytes: number;
      submittedAt: number;
      acceptedAt: number;
      indexedAt?: number;
      tipBeforeSubmit: string;
      includedAt?: number;
      expiresAtSlot: number;
      isPublication: boolean;
    }[] = [];
    let unindexedParentSubmissions = 0;
    let unconfirmedParentSubmissions = 0;
    let peakOutstandingCount = 0;
    let peakOutstandingBytes = 0;
    let firstSubmitAt: number | undefined;
    let injectedHash: string | undefined;
    let injectedCbor: string | undefined;
    let injectedExpiry = 0;
    let injectedAttempts = 0;
    let replacementAfterExpiry = false;
    let rollbackStage: "initial" | "observed" | "switched" = "initial";
    let rollbackObserved = false;
    const submit = provider.submitTx.bind(provider);
    const startedAt = Date.now();
    provider.submitTx = async (cbor) => {
      const body = CML.Transaction.from_cbor_hex(cbor).body();
      const isPublication = body.mint() !== undefined;
      const txHash = CML.hash_transaction(body).to_hex();
      if (drill === "expiry" && isPublication) {
        if (injectedHash === undefined) {
          injectedHash = txHash;
          injectedCbor = cbor;
          injectedExpiry = Number(body.ttl());
        }
        if (txHash === injectedHash) {
          expect(cbor).toBe(injectedCbor);
          injectedAttempts += 1;
          throw new Error(
            "Injected publication transport outage until validity expires",
          );
        }
        if (lucid.currentSlot() >= injectedExpiry)
          replacementAfterExpiry = true;
      }
      const inputs = Array.from({ length: body.inputs().len() }, (_, i) => ({
        txHash: body.inputs().get(i).transaction_id().to_hex(),
        outputIndex: Number(body.inputs().get(i).index()),
      }));
      const parents = inputs.filter((input) =>
        records.some((record) => record.hash === input.txHash),
      );
      const visibleParents = await provider.getUtxosByOutRef(parents);
      const tipBeforeSubmit = await readTip();
      const submittedAt = Date.now();
      firstSubmitAt ??= submittedAt;
      const hash = await submit(cbor);
      const tipAfterSubmit = await readTip();
      if (
        parents.some((parent) =>
          records.some(
            (record) =>
              record.hash === parent.txHash &&
              record.tipBeforeSubmit === tipAfterSubmit,
          ),
        )
      )
        unconfirmedParentSubmissions += 1;
      if (parents.length > 0 && visibleParents.length === 0)
        unindexedParentSubmissions += 1;
      if (!records.some((record) => record.hash === hash))
        records.push({
          hash,
          bytes: cbor.length / 2,
          submittedAt,
          acceptedAt: Date.now(),
          tipBeforeSubmit,
          expiresAtSlot: Number(body.ttl()),
          isPublication,
        });
      const outstanding = records.filter(
        (record) =>
          record.indexedAt === undefined &&
          lucid.currentSlot() < record.expiresAtSlot,
      );
      peakOutstandingCount = Math.max(peakOutstandingCount, outstanding.length);
      peakOutstandingBytes = Math.max(
        peakOutstandingBytes,
        outstanding.reduce((sum, record) => sum + record.bytes, 0),
      );
      if (
        drill === "ambiguous" &&
        isPublication &&
        injectedHash === undefined
      ) {
        injectedHash = hash;
        injectedCbor = cbor;
        injectedAttempts += 1;
        throw new Error(
          "Injected lost acknowledgement after real Ogmios acceptance",
        );
      }
      if (hash === injectedHash) {
        expect(cbor).toBe(injectedCbor);
        injectedAttempts += 1;
      }
      if (
        drill === "restart" &&
        isPublication &&
        process.env.MIDGARD_PUBLICATION_CRASH_AFTER_ACCEPT === "1"
      ) {
        writeSync(1, `Injected process death after real acceptance: ${hash}\n`);
        process.kill(process.pid, "SIGKILL");
      }
      return hash;
    };
    const synchronize = async () => {
      if (drill === "rollback" && rollbackStage === "observed") {
        // An external operator switches only this isolated test stack to its
        // preserved pre-publication fork. This marker contains no transaction
        // state and cannot resume or reconstruct the publisher.
        console.log("Ready for isolated chain fork");
        await writeFile(
          join(runDirectory, "work/rollback-observed"),
          "ready\n",
        );
        while (
          await access(join(runDirectory, "work/rollback-fork-ready")).then(
            () => false,
            () => true,
          )
        )
          await pause(1000);
        rollbackStage = "switched";
      }
      const slot = await synchronizePublicationIndexer(ogmiosUrl, kupoUrl);
      for (const record of records) {
        const status = await lucid.transactionStatus(record.hash);
        if (status.status === "confirmed") {
          record.indexedAt ??= Date.now();
          if (status.confirmation.slot !== undefined)
            record.includedAt ??= lucid.slotToUnixTime(
              status.confirmation.slot,
            );
          if (
            drill === "rollback" &&
            record.isPublication &&
            rollbackStage === "initial"
          )
            rollbackStage = "observed";
        } else if (record.indexedAt !== undefined) {
          rollbackObserved = true;
          delete record.indexedAt;
          delete record.includedAt;
        }
      }
      return slot;
    };
    const result = await Effect.runPromise(
      ensureReferenceScriptTargetsProgram(
        lucid,
        "local-devnet-publication",
        targets,
        authPolicy,
        lucid,
        undefined,
        SDK.REFERENCE_SCRIPT_AUTH_MIN_REMAINING_MS,
        new Set(),
        { mode, synchronize },
      ),
    );
    expect(result).toHaveLength(8);
    if (drill === "rollback") expect(rollbackObserved).toBe(true);
    if (drill === "ambiguous") expect(injectedAttempts).toBeGreaterThan(0);
    if (drill === "expiry") {
      expect(injectedAttempts).toBeGreaterThan(1);
      expect(replacementAfterExpiry).toBe(true);
      expect(
        records.some(
          (record) =>
            record.isPublication &&
            record.acceptedAt < lucid.slotToUnixTime(injectedExpiry),
        ),
      ).toBe(true);
    }
    if (records.length > 0) {
      expect(peakOutstandingBytes).toBeLessThanOrEqual(65_536);
      expect(peakOutstandingCount).toBeLessThanOrEqual(
        mode === "chained" ? 6 : 1,
      );
      if (mode === "chained" && drill === "comparison")
        expect(unindexedParentSubmissions).toBeGreaterThan(0);
      if (mode === "chained" && drill === "comparison")
        expect(unconfirmedParentSubmissions).toBeGreaterThan(0);
    }
    await writeFile(
      join(
        runDirectory,
        "work",
        `publication-${mode}-${drill}-measurements.json`,
      ),
      JSON.stringify(
        {
          mode,
          drill,
          initialReferenceCount,
          injectedAttempts,
          replacementAfterExpiry,
          rollbackObserved,
          workload:
            "eight identical real blueprint PlutusV3 reference scripts; authenticated roles; unchanged batching and size splitting",
          recoveredExistingRoster: records.length === 0,
          wallTimeMs: Date.now() - startedAt,
          startupReconciliationMs:
            firstSubmitAt === undefined ? null : firstSubmitAt - startedAt,
          publicationMs:
            firstSubmitAt === undefined ? null : Date.now() - firstSubmitAt,
          transactionCount: records.length,
          signedBytes: records.reduce((sum, record) => sum + record.bytes, 0),
          submissionLatencyMs: records.map(
            (record) => record.acceptedAt - record.submittedAt,
          ),
          indexingObservationLatencyMs: records.map((record) =>
            record.indexedAt === undefined
              ? null
              : record.indexedAt - record.submittedAt,
          ),
          inclusionLatencyMs: records.map((record) =>
            record.includedAt === undefined
              ? null
              : record.includedAt - record.submittedAt,
          ),
          unconfirmedParentSubmissions,
          peakOutstandingCount,
          peakOutstandingBytes,
          unindexedParentSubmissions,
        },
        null,
        2,
      ),
    );
  },
  30 * 60_000,
);
