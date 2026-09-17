import { createHash } from "node:crypto";
import { existsSync } from "node:fs";
import { readFile } from "node:fs/promises";
import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";
import { setTimeout as pause } from "node:timers/promises";

import {
  createLocalKupmiosHttpOgmiosRawSource,
  readAdmittedLocalKupmiosSignedTransactionRecovery,
  rebroadcastAdmittedLocalKupmiosSignedTransaction,
  type SignedWorkflowTransaction,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  paymentCredentialOf,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { activateOperatorProgram } from "midgard-node/transactions/register-active-operator";
import {
  loadWatcherVerifiedDeploymentAuthority,
  parseWatcherProcessConfig,
  watcherDeploymentReleaseFinalityAuthority,
} from "midgard-watcher";
import { expect, it } from "vitest";

import { readJourneyArtifact, writeJourneyArtifact } from "./artifacts.js";
import { loadJourneyContext } from "./live-context.js";

const runDirectory = process.env.MIDGARD_WATCHER_JOURNEY_RUN_DIR;
const selected = process.env.MIDGARD_WATCHER_JOURNEY_ACTIVATE_SUCCESSOR === "1";
const outRef = (utxo: Pick<UTxO, "txHash" | "outputIndex">) =>
  `${utxo.txHash}#${utxo.outputIndex}`;
const outputFacts = (utxo: UTxO) => ({
  address: utxo.address,
  assets: Object.entries(utxo.assets).sort(([a], [b]) => a.localeCompare(b)),
  datum: utxo.datum ?? null,
  datumHash: utxo.datumHash ?? null,
  scriptRef: utxo.scriptRef ?? null,
});

type ActivationJournal = {
  schema: "retained-successor-activation-v1";
  deploymentFingerprint: string;
  blueprintHash: string;
  genesisIdentitySha256: string;
  operatorKeyHash: string;
  registeredUnit: string;
  activeUnit: string;
  transaction: SignedWorkflowTransaction;
  signedBytesSha256: string;
  ordinaryInputs: string[];
  collateralInputs: string[];
  referenceInputs: string[];
  outputs: { outRef: string; outputCbor: string }[];
  validFromSlot: string | null;
  expiresAtSlot: string | null;
  references: [string, UTxO][];
  fundingLeaseOutRefs: string[];
  protectedFunding: UTxO[];
  history: { state: string; observedAt: string; detail: string }[];
};

const transactionFacts = (signedTransactionCborHex: string) => {
  const transaction = CML.Transaction.from_cbor_hex(signedTransactionCborHex);
  if (
    !transaction.is_valid() ||
    transaction.to_cbor_hex() !== signedTransactionCborHex
  )
    throw new Error(
      "Activation requires canonical valid signed transaction bytes",
    );
  const body = transaction.body();
  const transactionHash = CML.hash_transaction(body).to_hex();
  const inputs = (group: ReturnType<typeof body.inputs> | undefined) =>
    group === undefined
      ? []
      : Array.from({ length: group.len() }, (_, index) => {
          const input = group.get(index);
          return `${input.transaction_id().to_hex()}#${input.index()}`;
        });
  const outputs = body.outputs();
  return {
    transaction: { transactionHash, signedTransactionCborHex },
    signedBytesSha256: createHash("sha256")
      .update(Buffer.from(signedTransactionCborHex, "hex"))
      .digest("hex"),
    ordinaryInputs: inputs(body.inputs()),
    collateralInputs: inputs(body.collateral_inputs()),
    referenceInputs: inputs(body.reference_inputs()),
    outputs: Array.from({ length: outputs.len() }, (_, index) => ({
      outRef: `${transactionHash}#${index}`,
      outputCbor: outputs.get(index).to_cbor_hex(),
    })),
    validFromSlot: body.validity_interval_start()?.toString() ?? null,
    expiresAtSlot: body.ttl()?.toString() ?? null,
  };
};

it.skipIf(runDirectory === undefined || !selected)(
  "activates only the retained publisher registration with durable exact-byte recovery",
  async () => {
    const context = await loadJourneyContext(runDirectory!);
    const directory = join(runDirectory!, "work/journeys/transition-trace");
    const journalPath = join(
      runDirectory!,
      "work/retained-successor-activation.json",
    );
    const started = performance.now();
    const manifest = context.deployment.manifest;
    const publisher = context.deployment.publisherLucid;
    const publisherAddress = await publisher.wallet().address();
    const operatorKeyHash = paymentCredentialOf(publisherAddress).hash;
    const activeUnit = toUnit(
      context.deployment.contracts.activeOperators.policyId,
      SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + operatorKeyHash,
    );
    const references = [...context.deployment.references.entries()];
    expect(references).toHaveLength(517);
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
    ).verifyForWorkflow({ deploymentFingerprint: manifest.manifestId });
    if (config.l1.source.sourceMode !== "local_node")
      throw new Error("Activation recovery requires the admitted local node");
    const services = config.l1.source.queryServices;
    const endpoint = (kind: "kupo" | "ogmios") => {
      const service = services.find((entry) => entry.kind === kind);
      if (service === undefined) throw new Error(`Missing ${kind} endpoint`);
      return service.endpoint;
    };
    expect(endpoint("kupo")).toBe(context.kupoUrl);
    expect(endpoint("ogmios")).toBe(context.ogmiosUrl);
    const source = createLocalKupmiosHttpOgmiosRawSource({
      sourceId: "journey-retained-successor-activation",
      kupoHttpUrl: endpoint("kupo"),
      ogmiosUrl: endpoint("ogmios"),
      releaseFinality,
      timeoutMs: config.l1.requestTimeoutMs,
    });
    const binding = {
      schema: "retained-successor-activation-v1" as const,
      deploymentFingerprint: manifest.manifestId,
      blueprintHash: manifest.artifacts.blueprintHash,
      genesisIdentitySha256: context.genesisIdentitySha256,
      operatorKeyHash,
      activeUnit,
    };
    const readLeases = () => {
      const database = new DatabaseSync(
        join(runDirectory!, "work/journeys/runtime/watcher.sqlite"),
        { readOnly: true },
      );
      try {
        return database
          .prepare(
            "SELECT out_ref FROM watcher_prover_funding_lease_v1 ORDER BY out_ref",
          )
          .all()
          .map((row) => {
            if (typeof row.out_ref !== "string")
              throw new Error("Funding lease lacks its exact outref");
            return row.out_ref;
          });
      } finally {
        database.close();
      }
    };
    const fetchProtected = async (utxos: readonly UTxO[]) => {
      const available = new Map<string, UTxO>();
      for (const address of new Set(utxos.map((utxo) => utxo.address))) {
        for (const utxo of await context.provider.getUtxos(address))
          available.set(outRef(utxo), utxo);
      }
      for (const expected of utxos) {
        const actual = available.get(outRef(expected));
        if (actual === undefined)
          throw new Error(`Protected output was spent: ${outRef(expected)}`);
        expect(outputFacts(actual)).toEqual(outputFacts(expected));
      }
    };
    const validate = (journal: ActivationJournal) => {
      for (const [key, value] of Object.entries(binding))
        expect(journal[key as keyof typeof binding]).toBe(value);
      const facts = transactionFacts(
        journal.transaction.signedTransactionCborHex,
      );
      for (const [key, value] of Object.entries(facts))
        expect(journal[key as keyof typeof facts]).toEqual(value);
      expect(journal.references).toEqual(references);
      const protectedOutRefs = new Set([
        ...references.map(([, utxo]) => outRef(utxo)),
        ...journal.fundingLeaseOutRefs,
        ...readLeases(),
      ]);
      for (const input of [
        ...journal.ordinaryInputs,
        ...journal.collateralInputs,
      ]) {
        if (protectedOutRefs.has(input))
          throw new Error(
            `Activation spends a protected reference/funding input: ${input}`,
          );
      }
      if (
        journal.history.length === 0 ||
        journal.history[0]?.state !== "prepared"
      )
        throw new Error("Activation journal lacks its original preparation");
    };
    let journal = existsSync(journalPath)
      ? await readJourneyArtifact<ActivationJournal>(journalPath)
      : undefined;
    const append = async (state: string, detail: string) => {
      if (journal === undefined)
        throw new Error("Activation cannot proceed without its signed journal");
      validate(journal);
      // Keep the complete event history and immutable signed candidate through
      // atomic file replacement; writeJourneyArtifact fsyncs file + directory.
      if (existsSync(journalPath)) {
        const durable =
          await readJourneyArtifact<ActivationJournal>(journalPath);
        expect(durable.transaction).toEqual(journal.transaction);
        expect(durable.signedBytesSha256).toBe(journal.signedBytesSha256);
      }
      journal.history.push({
        state,
        observedAt: new Date().toISOString(),
        detail,
      });
      await writeJourneyArtifact(journalPath, journal);
    };
    if (journal === undefined) {
      await fetchProtected(references.map(([, utxo]) => utxo));
      const registered = [];
      for (const utxo of await context.provider.getUtxos(
        context.deployment.contracts.registeredOperators.spendingScriptAddress,
      )) {
        const node = await Effect.runPromise(
          SDK.getLinkedListNodeViewFromUTxO(utxo),
        );
        if (node.key === "Empty") continue;
        const datum = Data.castFrom(
          node.data as never,
          SDK.RegisteredOperatorDatum,
        );
        if (datum.operator === operatorKeyHash) registered.push(utxo);
      }
      expect(registered).toHaveLength(1);
      const registeredUnit = Object.keys(registered[0]!.assets).find((unit) =>
        unit.startsWith(
          context.deployment.contracts.registeredOperators.policyId,
        ),
      );
      if (registeredUnit === undefined)
        throw new Error("Retained publisher registration has no role token");
      const fundingLeaseOutRefs = readLeases();
      const protectedFunding = await context.provider.getUtxosByOutRef(
        fundingLeaseOutRefs.map((reference) => {
          const [txHash, index] = reference.split("#");
          return { txHash: txHash!, outputIndex: Number(index) };
        }),
      );
      const originalSubmit = context.provider.submitTx;
      let captured = false;
      context.provider.submitTx = async (signedBytes) => {
        const facts = transactionFacts(signedBytes);
        if (journal !== undefined)
          expect(journal.transaction).toEqual(facts.transaction);
        else {
          if (existsSync(journalPath))
            throw new Error(
              "Another activation candidate already owns the durable journal",
            );
          journal = {
            ...binding,
            registeredUnit,
            ...facts,
            references,
            fundingLeaseOutRefs,
            protectedFunding,
            history: [
              {
                state: "prepared",
                observedAt: new Date().toISOString(),
                detail:
                  "Exact builder-signed activation; no transaction submitted",
              },
            ],
          };
          validate(journal);
          await writeJourneyArtifact(journalPath, journal);
        }
        captured = true;
        // Stop the builder at its submit boundary. Canonical reconciliation below
        // exclusively controls every send, including the first one.
        throw new Error(
          "Retained activation durably captured for canonical submission",
        );
      };
      try {
        await Effect.runPromise(
          activateOperatorProgram(
            publisher,
            context.deployment.contracts,
            SDK.getProtocolParameters("Preprod").required_bond,
            publisher,
            publisherAddress,
          ),
        );
      } catch (cause) {
        if (!captured) throw cause;
      } finally {
        context.provider.submitTx = originalSubmit;
      }
    }
    if (journal === undefined)
      throw new Error(
        "Activation builder produced no signed candidate; refusing another operation",
      );
    validate(journal);
    await fetchProtected([
      ...references.map(([, utxo]) => utxo),
      ...journal.protectedFunding,
    ]);
    const deadline = performance.now() + 12 * 60_000;
    let included = false;
    while (performance.now() < deadline) {
      const observation =
        await readAdmittedLocalKupmiosSignedTransactionRecovery({
          source,
          ...journal.transaction,
        });
      await append(observation.status, observation.reason);
      if (observation.status === "included") {
        included = true;
        break;
      }
      if (observation.status === "expired" || observation.status === "conflict")
        throw new Error(
          `Activation ${observation.status}; exact record retained, no replacement permitted`,
        );
      if (observation.status === "rebroadcast") {
        const attempts = journal.history.filter(
          ({ state }) => state === "submission_intent",
        ).length;
        if (attempts >= 3)
          throw new Error(
            "Activation exhausted its three durable identical-byte submission attempts",
          );
        try {
          const hash = await rebroadcastAdmittedLocalKupmiosSignedTransaction({
            source,
            ...journal.transaction,
            authorizeResubmission: async (candidate) => {
              expect(candidate).toEqual(journal!.transaction);
              validate(journal!);
              const current =
                await readAdmittedLocalKupmiosSignedTransactionRecovery({
                  source,
                  ...candidate,
                });
              if (current.status !== "rebroadcast")
                throw new Error(
                  `Activation submission no longer eligible: ${current.status}`,
                );
              await append(
                "submission_intent",
                "Exact signed bytes authorized by current canonical absence, unspent inputs and mempool absence",
              );
            },
          });
          expect(hash).toBe(journal.transaction.transactionHash);
          await append(
            "submitted",
            "Ogmios acknowledged the exact recorded transaction hash",
          );
        } catch (cause) {
          await append(
            "unknown",
            cause instanceof Error
              ? cause.message.slice(0, 1024)
              : "Ambiguous submission outcome",
          );
        }
      }
      await pause(5_000);
    }
    if (!included)
      throw new Error(
        "Activation remains unresolved at the bounded observation deadline; resume the same journal",
      );
    const active = (
      await context.provider.getUtxos(
        context.deployment.contracts.activeOperators.spendingScriptAddress,
      )
    ).filter((utxo) => (utxo.assets[activeUnit] ?? 0n) !== 0n);
    expect(active).toHaveLength(1);
    expect(active[0]!.assets[activeUnit]).toBe(1n);
    const registered = await context.provider.getUtxos(
      context.deployment.contracts.registeredOperators.spendingScriptAddress,
    );
    const registeredUnit = journal.registeredUnit;
    expect(
      registered.filter((utxo) => (utxo.assets[registeredUnit] ?? 0n) !== 0n),
    ).toHaveLength(0);
    await fetchProtected([
      ...references.map(([, utxo]) => utxo),
      ...journal.protectedFunding,
    ]);
    await append(
      "completed",
      `Canonical activation verified; 517 references and protected funding unchanged; durationMs=${Math.round(performance.now() - started)}`,
    );
  },
  16 * 60_000,
);
