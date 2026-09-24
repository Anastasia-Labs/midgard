import { createHash } from "node:crypto";
import { mkdtemp } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Emulator,
  generateEmulatorAccount,
  paymentCredentialOf,
  SLOT_CONFIG_NETWORK,
  unixTimeToEnclosingSlot,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import JSONBig from "json-bigint";
import { expect, vi } from "vitest";

import { projectEventHistoryBlock } from "../../src/l1-event-history-projection.js";
import {
  decodeBoundEventHistoryLedgerSnapshot,
  eventHistoryGenesisLosslessSha256,
  makeEventHistorySourceBinding,
} from "../../src/l1-event-history-source.js";
import type { LedgerSnapshotOutput } from "../../src/l1-ledger-snapshot.js";
import type {
  FetchLike,
  WebSocketLike,
} from "../../src/l1-tx-order-carriage.js";
import { ContractDeploymentIdentity } from "../../src/services/midgard-contracts.js";
import {
  activateOperatorProgram,
  registerOperatorProgram,
} from "../../src/transactions/register-active-operator.js";
import {
  configureEmulatorDaRuntimeManifest,
  type EmulatorFixture,
  initializeNodeRuntime,
  makeGlobalsService,
  makeLucidRuntimeService,
  REGISTRATION_ACTIVATION_DELAY_SLOTS,
  REQUIRED_BOND_LOVELACE,
  resetActiveRuntimePaths,
} from "../deposit-flow-emulator-shared.js";
import { TEST_CARDANO_PROTOCOL_PARAMETERS } from "./cardano-protocol-parameters.js";
import {
  type AcceptedHistoryObservation,
  captureConfirmedHistoryObservations,
  historyOutputObservation,
} from "./history-projection-observations.js";
import {
  createMainnetEmulatorLucid,
  MAINNET_PROTOCOL_PARAMETERS,
} from "./mainnet-protocol-parameters.js";
import {
  createPublishedWorkflowDeploymentAccounts,
  publishWorkflowDeploymentOnChain,
} from "./published-workflow-deployment.js";
import { loadRealMidgardContractsForTest } from "./real-midgard-contracts.js";
import { DEFAULT_PUBLICATION_SCHEDULE } from "./reference-publication-chain.js";

export type RecordedHistoryBatch = {
  observations: readonly AcceptedHistoryObservation[];
  observedSlot: number;
  observedHeight: number;
  outputs: readonly LedgerSnapshotOutput[];
};
const hash = (value: string) =>
  createHash("sha256").update(value).digest("hex");
const label = (value: { txHash: string; outputIndex: number }) =>
  `${value.txHash}#${value.outputIndex}`;

/** Actual published deployment adapted to the existing node pipeline; it is
 * initialized exactly once and uses its own configured DA cosigner. */
export const openHistorySourceOwnerLifecycle = async (
  eventHistoryProtectionDurationMs?: bigint,
) => {
  await resetActiveRuntimePaths();
  await initializeNodeRuntime();
  const accounts = createPublishedWorkflowDeploymentAccounts();
  let onBatch: (
    observations: readonly AcceptedHistoryObservation[],
  ) => Promise<void> = async () => {};
  const p = MAINNET_PROTOCOL_PARAMETERS;
  const emulator = new Emulator([accounts.operator, accounts.publisher], p);
  emulator.time = 1_788_739_200_000;
  emulator.slot = unixTimeToEnclosingSlot(
    emulator.time,
    SLOT_CONFIG_NETWORK.Preprod,
  );
  emulator.blockHeight = Math.floor(emulator.slot / 20);
  const operatorLucid = await createMainnetEmulatorLucid(emulator, "Preprod");
  const publisher = await createMainnetEmulatorLucid(emulator, "Preprod");
  operatorLucid.selectWallet.fromSeed(accounts.operator.seedPhrase);
  publisher.selectWallet.fromSeed(accounts.publisher.seedPhrase);
  const batches: RecordedHistoryBatch[] = [];
  const publications = new Map<
    string,
    { signedCbor: string; observedSlot: number }
  >();
  let preparedContracts: Awaited<
    ReturnType<typeof loadRealMidgardContractsForTest>
  >;
  let observation:
    | ReturnType<typeof captureConfirmedHistoryObservations>
    | undefined;
  const published = await publishWorkflowDeploymentOnChain({
    eventHistoryProtectionDurationMs,
    accounts,
    network: "Preprod",
    operatorLucid,
    publisherLucid: publisher,
    chain: {
      now: () => emulator.now(),
      delaySlots: (slots) => emulator.awaitSlot(slots),
      awaitLedgerTime: (time) => {
        const slots = Math.ceil((time - emulator.now()) / 1000);
        if (slots > 0) emulator.awaitSlot(slots);
      },
    },
    protocolParameters: {
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
    },
    publicationJournalPath: join(
      await mkdtemp(join(tmpdir(), "midgard-history-owner-")),
      "transactions.ndjson",
    ),
    publicationSchedule: DEFAULT_PUBLICATION_SCHEDULE,
    publicationSynchronize: async () => emulator.slot,
    onPrepared: async ({ nonce, authPolicy }) => {
      preparedContracts = await loadRealMidgardContractsForTest(
        nonce,
        authPolicy,
        eventHistoryProtectionDurationMs,
      );
    },
    onPublication: ({ signedCbor, outRef }) => {
      expect(
        CML.hash_transaction(
          CML.Transaction.from_cbor_hex(signedCbor).body(),
        ).to_hex(),
      ).toBe(outRef.txHash);
      publications.set(outRef.txHash, {
        signedCbor,
        observedSlot: emulator.slot,
      });
    },
    onInitialization: () => {
      const pair = SDK.requireEventHistoryContracts(preparedContracts);
      const addresses = [
        preparedContracts.hubOracle.spendingScriptAddress,
        ...Object.values(pair).flatMap((h) => [
          h.list.spendingScriptAddress,
          h.retention.spendingScriptAddress,
        ]),
      ];
      observation = captureConfirmedHistoryObservations(
        operatorLucid,
        emulator,
        async (observations) => {
          batches.push({
            observations,
            observedSlot: emulator.slot,
            observedHeight: emulator.blockHeight,
            outputs: (
              await Promise.all(
                addresses.map((address) => operatorLucid.utxosAt(address)),
              )
            )
              .flat()
              .map(historyOutputObservation),
          });
          await onBatch(observations);
        },
      );
    },
  });
  const deployment = { ...published, emulator };
  const {
    operatorLucid: lucid,
    publisherLucid,
    contracts,
    manifest,
  } = deployment;
  vi.useFakeTimers({ toFake: ["Date"] });
  vi.setSystemTime(emulator.now());
  const deploymentInfoSha256 = hash(JSON.stringify(deployment.deploymentInfo));
  await configureEmulatorDaRuntimeManifest({ manifest, deploymentInfoSha256 });
  const identity = ContractDeploymentIdentity.make({
    kind: "manifest",
    manifest,
    manifestId: manifest.manifestId,
    deploymentMarker: makeDeploymentMarker(manifest.manifestId),
    l1Finality: manifest.l1Finality,
    consensusProfile: manifest.consensusProfile,
  });
  const depositorAccount = generateEmulatorAccount({ lovelace: 0n });
  const depositorLucid = await createMainnetEmulatorLucid(emulator, "Preprod");
  depositorLucid.selectWallet.fromSeed(depositorAccount.seedPhrase);
  const fund = await lucid
    .newTx()
    .pay.ToAddress(depositorAccount.address, { lovelace: 1_000_000_000n })
    .complete({ localUPLCEval: true });
  const funded = await fund.sign.withWallet().complete();
  expect(await lucid.awaitTx(await funded.submit())).toBe(true);
  lucid.overrideUTxOs(await lucid.utxosAt(await lucid.wallet().address()));
  vi.setSystemTime(emulator.now());
  const reference = (role: string) => {
    const result = deployment.references.get(role);
    if (result === undefined)
      throw new Error(`Missing published lifecycle role ${role}`);
    return result;
  };
  const fixture: EmulatorFixture = {
    emulator,
    emulatorCreationTimeMs: emulator.now(),
    contracts,
    operatorAccount: accounts.operator,
    depositorAccount,
    referenceScriptsAccount: accounts.publisher,
    operatorLucid: lucid,
    depositorLucid,
    referenceScriptsLucid: publisherLucid,
    operatorKeyHash: paymentCredentialOf(await lucid.wallet().address()).hash,
    runtimeOverrides: {
      deploymentIdentity: identity,
      daCosignerSeedPhrase: accounts.cosigner.seedPhrase,
    },
    referenceScripts: {
      deposit: { depositMinting: reference("depositMint") },
      withdrawal: { withdrawalMinting: reference("withdrawalMint") },
      init: {
        depositHistory: reference("depositMint"),
        withdrawalHistory: reference("withdrawalMint"),
        daParamsGovernorMinting: reference("daParamsGovernorMint"),
        hubOracleMinting: reference("hubOracleMint"),
        schedulerMinting: reference("schedulerMint"),
        stateQueueMinting: reference("stateQueueMint"),
        registeredOperatorsMinting: reference("registeredOperatorsMint"),
        activeOperatorsMinting: reference("activeOperatorsMint"),
        retiredOperatorsMinting: reference("retiredOperatorsMint"),
        fraudProofCatalogueMinting: reference("fraudProofCatalogueMint"),
      },
    },
  };
  await Effect.runPromise(
    registerOperatorProgram(
      lucid,
      contracts,
      REQUIRED_BOND_LOVELACE,
      publisherLucid,
    ),
  );
  emulator.awaitSlot(REGISTRATION_ACTIVATION_DELAY_SLOTS);
  vi.setSystemTime(emulator.now());
  await Effect.runPromise(
    activateOperatorProgram(
      lucid,
      contracts,
      REQUIRED_BOND_LOVELACE,
      publisherLucid,
    ),
  );
  vi.setSystemTime(emulator.now());
  const lucidService = await makeLucidRuntimeService(fixture);
  const globals = await makeGlobalsService();
  const genesis = {
    scope: "synthetic emulator transport",
    initializationTxHash: deployment.initialization.txHash,
  };
  const binding = await Effect.runPromise(
    makeEventHistorySourceBinding({
      contracts,
      identity,
      network: "Preprod",
      ogmiosUrl: "http://projection-lifecycle-emulator.invalid:1337",
      expectedGenesisLosslessSha256: eventHistoryGenesisLosslessSha256(genesis),
    }),
  );
  const histories = SDK.requireEventHistoryContracts(contracts);
  const addresses = [
    ...new Set([
      binding.hubAddress,
      ...Object.values(binding.deployments).flatMap((item) => [
        item.address,
        item.retentionAddress,
      ]),
    ]),
  ];
  const readCapture = async (point: { slot: number; id: string }) =>
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
  let capture = await readCapture({
    slot: emulator.slot,
    id: hash(
      `projection-lifecycle-start:${deployment.initialization.txHash}:${emulator.slot}`,
    ),
  });
  const receipts: (AcceptedHistoryObservation & {
    projectedSnapshotDigest: string;
    providerSnapshotDigest: string;
  })[] = [];
  const transitions: Awaited<
    ReturnType<typeof projectEventHistoryBlock>
  >["transitions"][number]["transition"][] = [];
  onBatch = async (observations) => {
    const point = {
      slot: emulator.slot,
      height: emulator.blockHeight,
      id: hash(
        `projection-lifecycle-block:${observations.map(({ transaction }) => transaction.txHash).join(":")}`,
      ),
    };
    const archives = new Map(
      observations.map((observation) => [
        observation.transaction.txHash,
        new Map(
          observation.historical.map((output) => [label(output), output]),
        ),
      ]),
    );
    const projected = await projectEventHistoryBlock({
      previous: capture,
      block: {
        parent: capture.history.ledger.point.id,
        point,
        transactions: observations.map(({ transaction }) => transaction),
      },
      binding,
      histories,
      resolveReference: (transactionHash, ref) =>
        archives.get(transactionHash)?.get(label(ref)),
      slotToUnixTime: lucid.slotToUnixTime,
    });
    const actual = await readCapture(point);
    expect(projected.capture.snapshotDigest).toBe(actual.snapshotDigest);
    expect(projected.capture.history.deposits).toEqual(actual.history.deposits);
    expect(projected.capture.history.withdrawals).toEqual(
      actual.history.withdrawals,
    );
    for (const observation of observations) {
      const limits = manifest.cardanoProtocolParameters.snapshot;
      expect(observation.measurement.completeSignedBytes).toBeLessThanOrEqual(
        Number(limits.maxTxSize),
      );
      expect(observation.measurement.executionMemory).toBeLessThanOrEqual(
        BigInt(limits.maxTxExUnits.memory),
      );
      expect(observation.measurement.executionSteps).toBeLessThanOrEqual(
        BigInt(limits.maxTxExUnits.steps),
      );
      receipts.push({
        ...observation,
        projectedSnapshotDigest: projected.capture.snapshotDigest,
        providerSnapshotDigest: actual.snapshotDigest,
      });
    }
    transitions.push(
      ...projected.transitions.map(({ transition }) => transition),
    );
    capture = projected.capture;
  };
  return {
    fixture,
    lucidService,
    globals,
    deployment,
    deploymentInfoSha256,
    observer: observation!,
    batches,
    publications,
    binding,
    genesis,
    receipts,
    transitions,
    capture: () => capture,
  };
};

type HistoryTransportRecording = Pick<
  Awaited<ReturnType<typeof openHistorySourceOwnerLifecycle>>,
  "publications" | "batches" | "genesis"
>;
type HistoryTransportPoint = {
  point: { id: string; slot: number; height: number };
  parent: string;
  transactions: Record<string, unknown>[];
  outputs: readonly LedgerSnapshotOutput[];
};

// These transport records are plain objects/arrays (asset quantities are bigint),
// never Maps. Clone first so freezing cannot mutate the recorder's live buffers.
const immutableTransportRecord = <T>(value: T): T => {
  const copy = structuredClone(value);
  const freeze = (item: unknown): void => {
    if (item !== null && typeof item === "object") {
      for (const nested of Object.values(item)) freeze(nested);
      Object.freeze(item);
    }
  };
  freeze(copy);
  return copy;
};

const historyTransportSlots = (recorded: HistoryTransportRecording) => {
  const slots = new Map<
    number,
    {
      transactions: Map<string, Record<string, unknown>>;
      outputs: readonly LedgerSnapshotOutput[];
      source: unknown[];
      complete: boolean;
    }
  >();
  const at = (observedSlot: number) => {
    if (!Number.isSafeInteger(observedSlot) || observedSlot <= 0)
      throw new Error(
        "History transport requires an actual positive observed slot",
      );
    let slot = slots.get(observedSlot);
    if (slot === undefined) {
      slot = {
        transactions: new Map(),
        outputs: [],
        source: [],
        complete: false,
      };
      slots.set(observedSlot, slot);
    }
    return slot;
  };
  for (const [txHash, publication] of recorded.publications) {
    const slot = at(publication.observedSlot);
    slot.transactions.set(txHash, { id: txHash, cbor: publication.signedCbor });
    slot.source.push({ publication: { txHash, ...publication } });
  }
  for (const batch of recorded.batches) {
    const slot = at(batch.observedSlot);
    for (const observation of batch.observations) {
      const txHash = observation.transaction.txHash;
      const existing = slot.transactions.get(txHash);
      if (existing !== undefined && existing.cbor !== observation.signedCbor)
        throw new Error("History transport creating-body bytes conflict");
      slot.transactions.set(txHash, rawTransaction(observation));
    }
    slot.outputs = batch.outputs;
    slot.source.push({ batch });
    slot.complete = true;
  }
  return [...slots].sort(([a], [b]) => a - b);
};

// Synthetic ancestry/contiguous heights, actual observed slots and complete body
// bytes. This supplies transport only, never ledger-validity/source authority.
const makeHistoryTransport = (
  recorded: HistoryTransportRecording,
  streaming: boolean,
) => {
  const points: HistoryTransportPoint[] = [];
  const sealedSources: string[] = [];
  const creators = new Map<string, number>();
  const origin = {
    id: hash("source-owner-synthetic-parent"),
    slot: 0,
    height: 0,
  };
  const genesis = immutableTransportRecord(recorded.genesis);
  const genesisBytes = lossless.stringify(genesis);
  let closed = false;
  const append = (initial: boolean) => {
    if (closed) throw new Error("History transport is closed");
    if (lossless.stringify(recorded.genesis) !== genesisBytes)
      throw new Error("History transport genesis changed");
    const slots = historyTransportSlots(recorded);
    if (slots.length < points.length)
      throw new Error("History transport sealed history was removed");
    const next: HistoryTransportPoint[] = [];
    const sources: string[] = [];
    const nextCreators = new Map<string, number>();
    for (const [index, [slot, value]] of slots.entries()) {
      const source = lossless.stringify(value.source);
      if (
        index < points.length &&
        (points[index]!.point.slot !== slot || sealedSources[index] !== source)
      )
        throw new Error("History transport cannot mutate a sealed slot");
      if (!initial && index >= points.length && !value.complete)
        throw new Error(
          "History transport requires a completed observation batch before append",
        );
      const parent = next.at(-1)?.point ?? origin;
      const point =
        points[index] ??
        immutableTransportRecord({
          point: {
            slot,
            height: parent.height + 1,
            id: hash(
              `owner:${slot}:${[...value.transactions.keys()].join(":")}`,
            ),
          },
          parent: parent.id,
          transactions: [...value.transactions.values()],
          outputs: value.outputs,
        });
      for (const tx of point.transactions) {
        const id = String(tx.id);
        if (nextCreators.has(id))
          throw new Error(
            "History transport repeats a creating transaction across slots",
          );
        nextCreators.set(id, index);
      }
      next.push(point);
      sources.push(source);
    }
    if (next.length === 0)
      throw new Error("History transport requires an initial recorded point");
    // Validation above is read-only. Publish all new snapshots/archives together.
    points.push(...next.slice(points.length));
    sealedSources.splice(0, sealedSources.length, ...sources);
    creators.clear();
    for (const [id, index] of nextCreators) creators.set(id, index);
  };
  append(true);
  let visible = points.length - 1;
  let holdIntersection = false;
  const sockets = new Set<RecordedSocket>();
  const requests: { socket: number; method: string; params: unknown }[] = [];
  let sequence = 0;
  const tip = () => points[visible]!.point;
  type Request = {
    id: number;
    method: string;
    params: Record<string, unknown>;
  };
  class RecordedSocket implements WebSocketLike {
    listeners = new Map<string, ((event: never) => void)[]>();
    readonly id = ++sequence;
    closed = false;
    authenticated = false;
    acquired: number | undefined;
    cursor = -1;
    pending: Request | undefined;
    held: Request | undefined;
    addEventListener(type: string, listener: (event: never) => void) {
      this.listeners.set(type, [...(this.listeners.get(type) ?? []), listener]);
    }
    emit(type: string, event?: unknown) {
      for (const listener of this.listeners.get(type) ?? [])
        listener(event as never);
    }
    answer(request: Request, result: unknown) {
      queueMicrotask(() => {
        if (!this.closed)
          this.emit("message", {
            data: lossless.stringify({ id: request.id, result }),
          });
      });
    }
    send(data: string) {
      if (this.closed || closed) throw new Error("History transport is closed");
      const request = lossless.parse(data) as Request;
      requests.push({
        socket: this.id,
        method: request.method,
        params: request.params,
      });
      switch (request.method) {
        case "queryNetwork/genesisConfiguration":
          this.authenticated = true;
          this.answer(request, genesis);
          break;
        case "queryNetwork/tip":
          this.answer(request, tip());
          break;
        case "queryLedgerState/tip":
          this.answer(
            request,
            this.acquired === undefined ? tip() : points[this.acquired]!.point,
          );
          break;
        case "acquireLedgerState": {
          const requested = request.params.point as { id: string };
          this.acquired = points.findIndex((p) => p.point.id === requested.id);
          expect(this.acquired).toBeGreaterThanOrEqual(0);
          this.answer(request, {
            acquired: "ledgerState",
            point: points[this.acquired]!.point,
          });
          break;
        }
        case "queryLedgerState/utxo": {
          expect(this.acquired).toBeDefined();
          const addresses = request.params.addresses as string[];
          this.answer(
            request,
            points[this.acquired!]!.outputs.filter((o) =>
              addresses.includes(o.address),
            ).map(rawOutput),
          );
          break;
        }
        case "releaseLedgerState":
          this.acquired = undefined;
          this.answer(request, { released: "ledgerState" });
          break;
        case "findIntersection":
          if (holdIntersection && this.authenticated) this.held = request;
          else this.intersect(request);
          break;
        case "nextBlock":
          expect(this.pending).toBeUndefined();
          this.pending = request;
          this.flush();
          break;
        default:
          throw new Error(`Unexpected fixture RPC ${request.method}`);
      }
    }
    intersect(request: Request) {
      const candidates = request.params.points as {
        id: string;
        slot: number;
      }[];
      const selected = candidates.find(
        (p) => p.id === origin.id || points.some((b) => b.point.id === p.id),
      );
      if (!selected)
        throw new Error("Fixture cannot intersect requested branch");
      this.cursor = points.findIndex((p) => p.point.id === selected.id);
      this.answer(request, { intersection: selected, tip: tip() });
    }
    flush() {
      if (this.pending === undefined || this.cursor >= visible || this.closed)
        return;
      const request = this.pending;
      this.pending = undefined;
      const next = points[++this.cursor]!;
      this.answer(request, {
        direction: "forward",
        tip: tip(),
        block: {
          type: "praos",
          era: "conway",
          ...next.point,
          ancestor: next.parent,
          transactions: next.transactions,
        },
      });
    }
    close() {
      if (!this.closed) {
        this.closed = true;
        sockets.delete(this);
        this.pending = undefined;
        this.held = undefined;
        this.emit("close");
      }
    }
  }
  const fetchImpl: FetchLike = async (url) => {
    if (closed) throw new Error("History transport is closed");
    const parsed = new URL(url);
    if (parsed.pathname.startsWith("/checkpoints/")) {
      const slot = Number(parsed.pathname.split("/").at(-1));
      const found =
        [...points].reverse().find((p) => p.point.slot <= slot)?.point ??
        origin;
      return new Response(
        lossless.stringify({ slot_no: found.slot, header_hash: found.id }),
      );
    }
    const match = /\/matches\/(\d+)@([a-f0-9]{64})/u.exec(parsed.pathname);
    if (match === null) throw new Error(`Unexpected fixture HTTP ${url}`);
    const block = points[creators.get(match[2]!)!];
    if (block === undefined)
      throw new Error(`Unknown actual creator ${match[2]}`);
    return new Response(
      lossless.stringify([
        {
          transaction_id: match[2],
          output_index: Number(match[1]),
          datum: null,
          created_at: {
            slot_no: block.point.slot,
            header_hash: block.point.id,
          },
        },
      ]),
    );
  };
  return {
    get points() {
      return Object.freeze([...points]);
    },
    requests,
    appendAccepted: () => {
      if (!streaming)
        throw new Error("Recorded history transport cannot append");
      append(false);
      visible = points.length - 1;
      for (const socket of sockets) socket.flush();
      return tip();
    },
    close: () => {
      if (closed) return;
      closed = true;
      for (const socket of [...sockets]) socket.close();
    },
    indexOf: (hash: string) => {
      const index = creators.get(hash);
      if (index === undefined) throw new Error("Missing accepted transaction");
      return index;
    },
    reveal: (index: number) => {
      expect(index).toBeGreaterThanOrEqual(0);
      expect(index).toBeLessThan(points.length);
      visible = index;
      for (const socket of sockets) socket.flush();
    },
    hold: () => {
      holdIntersection = true;
    },
    release: () => {
      holdIntersection = false;
      for (const socket of sockets)
        if (socket.held) {
          const request = socket.held;
          socket.held = undefined;
          socket.intersect(request);
        }
    },
    heldCount: () => [...sockets].filter((s) => s.held !== undefined).length,
    options: {
      kupoUrl: "http://source-owner-emulator.invalid:1442",
      ogmiosUrl: "http://projection-lifecycle-emulator.invalid:1337",
      timeoutMs: 20_000,
      blockScanLimit: 1024,
      maximumResponseBytes: 16 * 1024 * 1024,
      maximumTransactionBytes: 16_384,
      fetchImpl,
      webSocketFactory: () => {
        if (closed) throw new Error("History transport is closed");
        const socket = new RecordedSocket();
        sockets.add(socket);
        queueMicrotask(() => {
          if (!socket.closed) socket.emit("open");
        });
        return socket;
      },
    },
  };
};

/** Existing fixed-roster replay controls remain available to existing callers. */
export const makeRecordedHistoryTransport = (
  recorded: HistoryTransportRecording,
) => makeHistoryTransport(recorded, false);

/** Call only after recorded.observer.flush(). Sealed slots cannot be rewritten;
 * successful later observations must have their actual later emulator slot. */
export const makeStreamingHistoryTransport = (
  recorded: HistoryTransportRecording,
) => {
  const transport = makeHistoryTransport(recorded, true);
  return {
    get points() {
      return transport.points;
    },
    requests: transport.requests,
    options: transport.options,
    indexOf: transport.indexOf,
    appendAccepted: transport.appendAccepted,
    close: transport.close,
  };
};

const lossless = JSONBig({ useNativeBigInt: true, strict: true });
const rawRef = (ref: { txHash: string; outputIndex: number }) => ({
  transaction: { id: ref.txHash },
  index: ref.outputIndex,
});
const rawValue = (assets: Readonly<Record<string, bigint>>) => {
  const value: Record<string, Record<string, bigint>> = {
    ada: { lovelace: assets.lovelace ?? 0n },
  };
  for (const [unit, quantity] of Object.entries(assets))
    if (unit !== "lovelace")
      (value[unit.slice(0, 56)] ??= {})[unit.slice(56)] = quantity;
  return value;
};
const rawOutput = (output: LedgerSnapshotOutput) => ({
  ...rawRef(output),
  address: output.address,
  value: rawValue(output.assets),
  ...(output.datum === undefined ? {} : { datum: output.datum }),
  ...(output.datumHash === undefined ? {} : { datumHash: output.datumHash }),
  ...(output.hasReferenceScript ? { script: {} } : {}),
});
const rawTransaction = (
  observation: AcceptedHistoryObservation,
): Record<string, unknown> => {
  const tx = observation.transaction;
  const mint = rawValue(tx.mint);
  delete mint.ada;
  return {
    id: tx.txHash,
    cbor: observation.signedCbor,
    spends: tx.spends,
    inputs: tx.inputs.map(rawRef),
    references: tx.references.map(rawRef),
    collaterals: tx.collaterals.map(rawRef),
    outputs: tx.outputs.map(rawOutput),
    mint,
    withdrawals: Object.fromEntries(
      tx.withdrawals.map((w) => [w.account, { ada: { lovelace: w.amount } }]),
    ),
    redeemers: tx.redeemers.map((r) => ({
      validator: { purpose: r.purpose, index: r.index },
      redeemer: r.cbor,
    })),
    validityInterval: {
      ...(tx.invalidBefore === undefined
        ? {}
        : { invalidBefore: tx.invalidBefore }),
      ...(tx.invalidAfter === undefined
        ? {}
        : { invalidAfter: tx.invalidAfter }),
    },
  };
};
