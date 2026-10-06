import "node:crypto";
import "node:fs";
import "node:fs/promises";
import "node:os";
import "node:path";
import "node:util";
import "@al-ft/midgard-core/codec";
import "@al-ft/midgard-core/deployment-profile";
import "@al-ft/midgard-core/out-ref";
import "@al-ft/midgard-core/plutus-data-cbor";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "vitest";
import "../src/database/index.js";
import "../src/fibers/fetch-and-insert-deposit-utxos.js";
import "../src/l1-event-history-projection.js";
import "../src/l1-event-history-source.js";
import "./helpers/cardano-protocol-parameters.js";
import "./helpers/history-projection-observations.js";
import "./helpers/mainnet-protocol-parameters.js";
import "./helpers/published-workflow-deployment.js";
import "./helpers/reference-publication-chain.js";
import "./l1-event-history-raw-deposit-emulator.admit-raw.js";

import { mkdirSync, writeFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { inspect } from "node:util";

import { decodeMidgardTxOutput } from "@al-ft/midgard-core/codec";
import { compareOutRefs, outRefLabel } from "@al-ft/midgard-core/out-ref";
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder as ordered,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Data,
  datumToHash,
  Emulator,
  generateEmulatorAccount,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect, it, vi } from "vitest";

import { DepositsDB } from "../src/database/index.js";
import { depositUTxOToEntry } from "../src/fibers/fetch-and-insert-deposit-utxos.js";
import { projectEventHistoryBlock } from "../src/l1-event-history-projection.js";
import {
  decodeBoundEventHistoryLedgerSnapshot,
  eventHistoryGenesisLosslessSha256,
  makeEventHistorySourceBinding,
} from "../src/l1-event-history-source.js";
import { TEST_CARDANO_PROTOCOL_PARAMETERS } from "./helpers/cardano-protocol-parameters.js";
import {
  type AcceptedHistoryObservation,
  historyOutputObservation,
  submitHistoryObservation,
} from "./helpers/history-projection-observations.js";
import {
  createMainnetEmulatorLucid,
  MAINNET_PROTOCOL_PARAMETERS,
  MAINNET_PROTOCOL_PARAMETERS_SOURCE,
} from "./helpers/mainnet-protocol-parameters.js";
import {
  createPublishedWorkflowDeploymentAccounts,
  publishWorkflowDeploymentOnChain,
} from "./helpers/published-workflow-deployment.js";
import { DEFAULT_PUBLICATION_SCHEDULE } from "./helpers/reference-publication-chain.js";
import { makeJournalDirectory } from "./helpers/run-journal-directory.js";
import {
  admitRaw,
  field,
  hash,
  network,
  plain,
} from "./l1-event-history-raw-deposit-emulator.admit-raw.js";

it("preserves actual inline and external Deposit raw map pair order and duplicates through continuation, capture and node conversion", async () => {
  const accounts = createPublishedWorkflowDeploymentAccounts();
  const user = generateEmulatorAccount({ lovelace: 2_000_000_000n });
  const p = MAINNET_PROTOCOL_PARAMETERS;
  const emulator = new Emulator(
    [accounts.operator, accounts.publisher, user],
    p,
  );
  const operator = await createMainnetEmulatorLucid(emulator, network);
  const publisher = await createMainnetEmulatorLucid(emulator, network);
  const lucid = await createMainnetEmulatorLucid(emulator, network);
  operator.selectWallet.fromSeed(accounts.operator.seedPhrase);
  publisher.selectWallet.fromSeed(accounts.publisher.seedPhrase);
  lucid.selectWallet.fromSeed(user.seedPhrase);
  const snapshot = {
    ...TEST_CARDANO_PROTOCOL_PARAMETERS,
    minFeeA: String(p.minFeeA),
    minFeeB: String(p.minFeeB),
    coinsPerUtxoByte: String(p.coinsPerUtxoByte),
    collateralPercentage: String(p.collateralPercentage),
    maxCollateralInputs: String(p.maxCollateralInputs),
    maxTxSize: String(p.maxTxSize),
    maxValueSize: String(p.maxValSize),
    maxTxExUnits: {
      memory: String(p.maxTxExMem),
      steps: String(p.maxTxExSteps),
    },
  };
  const deployment = await publishWorkflowDeploymentOnChain({
    network,
    accounts,
    operatorLucid: operator,
    publisherLucid: publisher,
    chain: {
      now: () => emulator.now(),
      delaySlots: (n) => emulator.awaitSlot(n),
      awaitLedgerTime: (time) => {
        const slots = Math.ceil((time - emulator.now()) / 1000);
        if (slots > 0) emulator.awaitSlot(slots);
      },
    },
    protocolParameters: snapshot,
    publicationJournalPath: join(
      await makeJournalDirectory("midgard-raw-deposit-"),
      "transactions.ndjson",
    ),
    publicationSchedule: DEFAULT_PUBLICATION_SCHEDULE,
    publicationSynchronize: async () => emulator.slot,
  });
  const { contracts, manifest } = deployment;
  const history = SDK.requireEventHistoryContracts(contracts).deposit;
  const historyDeployment = SDK.eventHistoryDeploymentFromContracts(history);
  const receipts: AcceptedHistoryObservation[] = [];
  const stages: unknown[] = [];
  let evidence: Record<string, unknown> = {
    status: "started",
    manifestId: manifest.manifestId,
    blueprintSha256: manifest.artifacts.blueprintHash,
    protocolParameters: p,
    protocolParametersSource: MAINNET_PROTOCOL_PARAMETERS_SOURCE,
  };
  vi.useFakeTimers({ toFake: ["Date"] });
  try {
    vi.setSystemTime(emulator.now());
    const blob = `590258${"ab".repeat(600)}`;
    const cases = [
      { name: "inline-2-1", datum: ordered("a2020a010b"), storage: "Inline" },
      {
        name: "external-2-1",
        datum: ordered(`a202${blob}010b`),
        storage: "External",
      },
      {
        name: "inline-2-1-2",
        datum: ordered("a3020a010b020c"),
        storage: "Inline",
      },
      {
        name: "external-2-1-2",
        datum: ordered(`a302${blob}010b020c`),
        storage: "External",
      },
      { name: "tail", datum: "00", storage: "Inline" },
    ] as const;
    for (const specimen of cases) {
      expect(
        ordered(CML.PlutusData.from_cbor_hex(specimen.datum).to_cbor_hex()),
      ).toBe(specimen.datum);
      if (specimen.name !== "tail")
        expect(
          ordered(
            CML.PlutusData.from_cbor_hex(
              specimen.datum,
            ).to_canonical_cbor_hex(),
          ),
        ).not.toBe(specimen.datum);
    }
    let split = lucid.newTx();
    for (let index = 0; index < cases.length; index++)
      split = split.pay.ToAddress(user.address, { lovelace: 30_000_000n });
    split = split.pay.ToAddress(user.address, { lovelace: 10_000_000n });
    const splitReceipt = await submitHistoryObservation(
      lucid,
      await split.complete({ localUPLCEval: true }),
    );
    receipts.push(splitReceipt);
    const nonces = (await lucid.utxosAt(user.address)).filter(
      (u) =>
        u.txHash === splitReceipt.transaction.txHash &&
        u.assets.lovelace === 30_000_000n,
    );
    expect(nonces).toHaveLength(cases.length);
    const key = (u: UTxO) =>
      Effect.runSync(
        SDK.eventHistoryKey({
          transactionId: u.txHash,
          outputIndex: BigInt(u.outputIndex),
        }),
      );
    nonces.sort((a, b) => key(a).localeCompare(key(b)));
    const reserved = new Set(nonces.map(outRefLabel));
    const selectFunding = async (nonce: UTxO) =>
      lucid.overrideUTxOs(
        (await lucid.utxosAt(user.address)).filter(
          (u) =>
            plain(u) &&
            (!reserved.has(outRefLabel(u)) ||
              outRefLabel(u) === outRefLabel(nonce)),
        ),
      );
    const binding = await Effect.runPromise(
      makeEventHistorySourceBinding({
        contracts,
        identity: {
          kind: "manifest",
          manifest,
          manifestId: manifest.manifestId,
          consensusProfile: manifest.consensusProfile,
        },
        network,
        expectedGenesisLosslessSha256: eventHistoryGenesisLosslessSha256({
          scope: "synthetic raw datum fixture",
          initialization: deployment.initialization.txHash,
        }),
      }),
    );
    const addresses = [
      binding.hubAddress,
      ...Object.values(binding.deployments).flatMap((d) => [
        d.address,
        d.retentionAddress,
      ]),
    ];
    let height = 0;
    const providerCapture = async (point: { slot: number; id: string }) =>
      Effect.runPromise(
        decodeBoundEventHistoryLedgerSnapshot(
          {
            point: { slot: point.slot, id: point.id },
            addresses,
            outputs: (
              await Promise.all(
                addresses.map((address) => lucid.utxosAt(address)),
              )
            )
              .flat()
              .map(historyOutputObservation),
          },
          binding,
        ),
      );
    let capture = await providerCapture({
      slot: emulator.slot,
      id: hash(`raw-initial:${splitReceipt.transaction.txHash}`),
    });
    const project = async (receipt: AcceptedHistoryObservation) => {
      const point = {
        slot: emulator.slot,
        height: ++height,
        id: hash(`raw-block:${receipt.transaction.txHash}`),
      };
      const archive = new Map(
        receipt.historical.map((output) => [outRefLabel(output), output]),
      );
      const projected = await projectEventHistoryBlock({
        previous: capture,
        block: {
          point,
          parent: capture.history.ledger.point.id,
          transactions: [receipt.transaction],
        },
        binding,
        histories: SDK.requireEventHistoryContracts(contracts),
        slotToUnixTime: lucid.slotToUnixTime,
        resolveReference: (_tx, ref) => archive.get(outRefLabel(ref)),
      });
      const actual = await providerCapture(point);
      expect(projected.capture.snapshotDigest).toBe(actual.snapshotDigest);
      expect(projected.capture.history.deposits).toEqual(
        actual.history.deposits,
      );
      capture = projected.capture;
      return {
        point,
        projectedSnapshotDigest: projected.capture.snapshotDigest,
        providerSnapshotDigest: actual.snapshotDigest,
        transitions: projected.transitions,
      };
    };
    const admitted = new Map<
      string,
      {
        payloadCbor: string;
        datum: string;
        factsCbor: string;
        capture: ReturnType<typeof SDK.captureEventHistoryWitness>;
        order: SDK.DepositUTxO;
      }
    >();
    for (const [ordinal, specimen] of cases.entries()) {
      const nonce = nonces[ordinal]!;
      const nodes = SDK.authenticateHistoryNodes(
        await lucid.utxosAt(history.list.spendingScriptAddress),
        historyDeployment,
      );
      const protectedUntil = nodes.reduce(
        (n, { node }) => (node.protected_until > n ? node.protected_until : n),
        0n,
      );
      await deployment.chain.awaitLedgerTime(Number(protectedUntil) + 60_000);
      vi.setSystemTime(emulator.now());
      const prepare = async () => {
        await selectFunding(nonce);
        return Effect.runPromise(
          SDK.prepareDepositSubmissionProgram(lucid, contracts, {
            nonceInput: nonce,
            l2Address: user.address,
            l2Datum: "00",
            lovelace: 5_000_000n,
            additionalAssets: {},
            structuralLovelace: 3_000_000n,
            referenceScripts: {
              depositMinting: deployment.references.get("depositMint")!,
            },
          }),
        );
      };
      let prepared = await prepare();
      const payloadCbor = ordered(
        replacePlutusConstrFieldCbor(
          prepared.request.payloadCbor,
          [0, 1, 2, 0],
          specimen.datum,
        ),
      );
      expect(field(payloadCbor, [0, 1, 2, 0])).toBe(specimen.datum);
      expect(BigInt(payloadCbor.length / 2)).toBeLessThanOrEqual(
        history.recipe.maxPayloadBytes,
      );
      expect(
        BigInt(payloadCbor.length / 2) <= history.recipe.inlineLimitBytes,
      ).toBe(specimen.storage === "Inline");
      let retained: UTxO | undefined;
      if (specimen.storage === "External") {
        const retainedCbor = ordered(
          replacePlutusConstrFieldCbor(
            Data.to(
              {
                event_key: key(nonce),
                event_payload: Data.from(prepared.request.payloadCbor),
                reclaim_auth: prepared.request.reclaimAuth,
              },
              SDK.EventHistoryData,
            ),
            [1],
            payloadCbor,
          ),
        );
        const publication = await prepared.context.lucid
          .newTx()
          .collectFrom([...prepared.context.fundingInputs])
          .pay.ToContract(
            prepared.context.applied.retention.address,
            { kind: "inline", value: retainedCbor },
            {},
          )
          .complete({ coinSelection: false, localUPLCEval: true });
        const receipt = await submitHistoryObservation(lucid, publication);
        receipts.push(receipt);
        expect(
          receipt.transaction.inputs.some(
            (input) => outRefLabel(input) === outRefLabel(nonce),
          ),
        ).toBe(false);
        [retained] = await lucid.utxosByOutRef([
          { txHash: receipt.transaction.txHash, outputIndex: 0 },
        ]);
        expect(ordered(retained!.datum!)).toBe(retainedCbor);
        stages.push({
          name: specimen.name,
          publication: await project(receipt),
          retained,
        });
        prepared = await prepare();
      }
      vi.setSystemTime(emulator.now());
      const validFrom = emulator.now() - 60_000;
      const validTo = SDK.resolveUserEventValidTo(lucid);
      const built = await admitRaw(
        prepared,
        payloadCbor,
        retained,
        validFrom,
        validTo,
      );
      const accepted = await submitHistoryObservation(lucid, built.completed);
      receipts.push(accepted);
      reserved.delete(outRefLabel(nonce));
      expect(accepted.transaction.inputs).toEqual(
        [...built.inputs]
          .sort(compareOutRefs)
          .map(({ txHash, outputIndex }) => ({ txHash, outputIndex })),
      );
      expect(accepted.transaction.references).toEqual(
        [...built.references]
          .sort(compareOutRefs)
          .map(({ txHash, outputIndex }) => ({ txHash, outputIndex })),
      );
      expect(ordered(accepted.transaction.outputs[1]!.datum!)).toBe(
        ordered(built.nodeCbor),
      );
      expect(ordered(accepted.transaction.outputs[0]!.datum!)).toBe(
        ordered(built.predecessorCbor),
      );
      expect(
        accepted.transaction.redeemers.some(
          (redeemer) =>
            redeemer.purpose === "withdraw" &&
            ordered(redeemer.cbor) === ordered(built.observerCbor),
        ),
      ).toBe(true);
      const projected = await project(accepted);
      const orders = await Effect.runPromise(
        SDK.fetchDepositUTxOsProgram(lucid, historyDeployment),
      );
      const order = orders.find((order) => order.assetName === built.key)!;
      expect(order).toBeDefined();
      const captured = SDK.captureEventHistoryWitness(
        order.history,
        history.list.policyId,
        "Deposit",
      );
      expect(order.originalAssets).toEqual({ lovelace: 5_000_000n });
      expect(order.utxo.assets[history.list.policyId + built.key]).toBe(1n);
      expect(order.utxo.assets.lovelace).toBe(8_000_000n);
      expect(order.history.payloadCbor).toBe(payloadCbor);
      expect(order.infoCbor.toString("hex")).toBe(field(payloadCbor, [0, 1]));
      expect(captured.payloadCbor).toBe(payloadCbor);
      expect(captured.factsCbor).toBe(field(order.utxo.datum!, [3, 0]));
      expect(captured.commitment.payload_hash).toBe(datumToHash(payloadCbor));
      expect(field(captured.openingCbor, [0])).toBe(payloadCbor);
      expect(field(captured.openingCbor, [1])).toBe(
        ordered(Data.to(SDK.assetsToValue(order.originalAssets), SDK.Value)),
      );
      expect(
        SDK.opensEventHistoryCommitmentCbor(
          captured.commitment,
          payloadCbor,
          field(captured.openingCbor, [1]),
        ),
      ).toBe(true);
      const entry = await Effect.runPromise(depositUTxOToEntry(order, network));
      const converted = decodeMidgardTxOutput(
        entry[DepositsDB.Columns.LEDGER_OUTPUT],
      );
      expect(converted.datum?.cbor.toString("hex")).toBe(specimen.datum);
      expect(entry[DepositsDB.Columns.INFO]).toEqual(order.infoCbor);
      expect(converted.value).toEqual({
        lovelace: 5_000_000n,
        assets: new Map(),
      });
      admitted.set(built.key, {
        payloadCbor,
        datum: specimen.datum,
        factsCbor: captured.factsCbor,
        capture: captured,
        order,
      });
      let continuation: unknown;
      if (ordinal > 0) {
        const previous = admitted.get(built.anchor.key!)!;
        const refreshed = orders.find(
          (order) => order.assetName === built.anchor.key,
        )!;
        expect(refreshed.utxo.txHash).not.toBe(previous.order.utxo.txHash);
        expect(refreshed.utxo.assets).toEqual(previous.order.utxo.assets);
        const after = SDK.captureEventHistoryWitness(
          refreshed.history,
          history.list.policyId,
          "Deposit",
        );
        expect(after).toEqual(previous.capture);
        expect(field(refreshed.utxo.datum!, [3, 0])).toBe(previous.factsCbor);
        expect(refreshed.infoCbor).toEqual(previous.order.infoCbor);
        expect(refreshed.history.anchor.node.next).toBe(built.key);
        expect(refreshed.history.anchor.node.protected_until).toBeGreaterThan(
          previous.order.history.anchor.node.protected_until,
        );
        const reprojected = await Effect.runPromise(
          depositUTxOToEntry(refreshed, network),
        );
        expect(
          decodeMidgardTxOutput(
            reprojected[DepositsDB.Columns.LEDGER_OUTPUT],
          ).datum?.cbor.toString("hex"),
        ).toBe(previous.datum);
        continuation = {
          before: previous.order.utxo,
          after: refreshed.utxo,
          capture: after,
          nodeEntry: reprojected,
        };
      }
      stages.push({
        name: specimen.name,
        rawDatum: specimen.datum,
        payloadCbor,
        projected,
        order: order.utxo,
        captured,
        nodeEntry: entry,
        converted,
        continuation,
      });
    }
    expect(admitted.size).toBe(5);
    expect(capture.history.deposits).toHaveLength(5);
    expect(capture.history.withdrawals).toHaveLength(0);
    for (const receipt of receipts) {
      expect(receipt.measurement.completeSignedBytes).toBeLessThanOrEqual(
        p.maxTxSize,
      );
      expect(receipt.measurement.executionMemory).toBeLessThanOrEqual(
        p.maxTxExMem,
      );
      expect(receipt.measurement.executionSteps).toBeLessThanOrEqual(
        p.maxTxExSteps,
      );
    }
    evidence = {
      ...evidence,
      status: "passed",
      binding,
      capture,
      scope:
        "Actual raw Deposit admissions/retention publications and continuation; synthetic transport point hashes/heights/genesis; pure node converter, no DB persistence or L2 settlement claim",
    };
  } catch (cause) {
    evidence = {
      ...evidence,
      status: "failed",
      cause: inspect(cause, { depth: 12 }),
    };
    throw cause;
  } finally {
    vi.useRealTimers();
    const path = process.env.MIDGARD_RAW_DEPOSIT_EVIDENCE_PATH;
    if (path !== undefined) {
      mkdirSync(dirname(path), { recursive: true });
      writeFileSync(
        path,
        JSON.stringify(
          { ...evidence, receipts, stages },
          (_key, value) =>
            typeof value === "bigint" ? value.toString() : value,
          2,
        ) + "\n",
      );
    }
  }
});
