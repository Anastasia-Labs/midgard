import { createServer, type Socket } from "node:net";

import { computeHash28 } from "@al-ft/midgard-core/codec/hash";
import { readAdmittedLocalKupmiosBoundary } from "@al-ft/midgard-fault-proofs";
import { decodeBlock, openSqliteFactStore } from "@al-ft/midgard-l1-follower";
import { simStoreOptions } from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";

import {
  assertWatcherStateQueueHeaderObservation,
  assertWatcherStateQueueObservation,
  type WatcherAuthenticatedStateQueueObservation,
  type WatcherStateQueueHeaderObservation,
} from "../../src/indexers/authenticated-state-queue-observation.js";
import { RELEASE_FINALITY_DEPTH } from "../../src/indexers/authenticated-state-queue-observation.parse-persisted-header.js";
import {
  createWatcherLocalKupmiosNativeObservationRuntime,
  type WatcherLocalKupmiosNativeObservation,
  type WatcherLocalKupmiosNativeObservationRuntime,
} from "../../src/l1/local-kupmios-native-observation.js";
import {
  admitWatcherNativeRollForwardBlock,
  type WatcherNativeBlockAdmission,
} from "../../src/l1/native-block-admission.js";
import {
  openWatcherNativeExactPointQuery,
  readWatcherNativeExactPointQuery,
  type WatcherNativeChainSyncEventReceipt,
} from "../../src/l1/native-chain-sync.js";
import { readWatcherObservation } from "../../src/l1-follower/observation.js";
import { watcherProjection } from "../../src/l1-follower/projection.js";
import { watcherDeploymentProtocolScriptAuthority } from "../../src/runtime/deployment-identity.js";
import {
  closeAll,
  commitTransaction,
  createSyntheticStateQueueHeader,
  initializationTransaction,
} from "./state-queue-observation-fixture.commit-transaction.js";
import { buildBlock } from "./user-event-origin-fixture.build-block.js";
import {
  createSyntheticUserEventOriginFixture,
  type SyntheticUserEventBlock,
  type SyntheticUserEventOriginFixture,
} from "./user-event-origin-fixture.js";

const openTcpPeer = async () => {
  const sockets = new Set<Socket>();
  const server = createServer((socket) => {
    sockets.add(socket);
    socket.on("error", () => undefined);
    socket.on("close", () => sockets.delete(socket));
  });
  await new Promise<void>((resolve, reject) => {
    server.once("error", reject);
    server.listen(0, "127.0.0.1", () => {
      server.removeListener("error", reject);
      resolve();
    });
  });
  const address = server.address();
  if (address === null || typeof address === "string")
    throw new Error("Synthetic TCP peer has no bound port");
  return {
    port: address.port,
    close: async () => {
      for (const socket of sockets) socket.destroy();
      await new Promise<void>((resolve, reject) =>
        server.close((error) =>
          error === undefined ? resolve() : reject(error),
        ),
      );
    },
  };
};

export type SyntheticStateQueueObservationCapture = Readonly<{
  localRuntime: WatcherLocalKupmiosNativeObservationRuntime;
  initialObservation: WatcherAuthenticatedStateQueueObservation;
  observation: WatcherAuthenticatedStateQueueObservation;
  header: WatcherStateQueueHeaderObservation;
  nativeBlock: WatcherNativeBlockAdmission;
  nativeEventReceipt: WatcherNativeChainSyncEventReceipt;
  localObservation: WatcherLocalKupmiosNativeObservation;
  close(): Promise<void>;
}>;

export type SyntheticStateQueueObservationFixture = Readonly<{
  transport: SyntheticUserEventOriginFixture;
  initializationTransactionCbor: string;
  commitTransactionCbor: string;
  initializationBlock: SyntheticUserEventBlock;
  commitBlock: SyntheticUserEventBlock;
  header: SDK.Header;
  headerHash: string;
  observeFresh(): Promise<SyntheticStateQueueObservationCapture>;
  close(): Promise<void>;
}>;

/** Each call acquires new native queries and reconstructs the queue from Init. */
export const createSyntheticStateQueueObservationFixture = async (
  input: Readonly<{
    header?: SDK.Header;
    ruleBundleCommitment?: string;
    nativeTipMode?: "query_counter" | "controlled";
    composeCommitBlock?: (
      input: Readonly<{
        transport: SyntheticUserEventOriginFixture;
        initializationBlock: SyntheticUserEventBlock;
        commitTransactionCbor: string;
      }>,
    ) => Promise<
      Readonly<{
        transactions: readonly string[];
        creatingBodies?: readonly string[];
        parent?: SyntheticUserEventBlock;
      }>
    >;
  }> = {},
): Promise<SyntheticStateQueueObservationFixture> => {
  const headerCborHex = Data.to(
    input.header ?? createSyntheticStateQueueHeader(),
    SDK.Header,
  );
  const header = Object.freeze(Data.from(headerCborHex, SDK.Header));
  const ruleBundleCommitment = input.ruleBundleCommitment;
  const headerHash = computeHash28(Buffer.from(headerCborHex, "hex")).toString(
    "hex",
  );
  let kupo: Awaited<ReturnType<typeof openTcpPeer>> | undefined;
  let ogmios: Awaited<ReturnType<typeof openTcpPeer>> | undefined;
  const captures = new Set<SyntheticStateQueueObservationCapture>();
  let transport: SyntheticUserEventOriginFixture | undefined;
  let closed = false;
  const close = async () => {
    if (closed) return;
    closed = true;
    await closeAll([
      ...[...captures].map((capture) => () => capture.close()),
      async () => {
        await transport?.close();
      },
      async () => {
        await ogmios?.close();
      },
      async () => {
        await kupo?.close();
      },
    ]);
  };
  try {
    kupo = await openTcpPeer();
    ogmios = await openTcpPeer();
    transport = await createSyntheticUserEventOriginFixture({
      queryEndpoints: {
        kupo: `http://127.0.0.1:${kupo.port}`,
        ogmios: `ws://127.0.0.1:${ogmios.port}`,
      },
      nativeTipBaseDepth: 40,
      ...(input.nativeTipMode === undefined
        ? {}
        : { nativeTipMode: input.nativeTipMode }),
      ...(ruleBundleCommitment === undefined ? {} : { ruleBundleCommitment }),
    });
    const fixtureTransport = transport;
    const authority = watcherDeploymentProtocolScriptAuthority(
      transport.deploymentIdentity,
    );

    const initializationTransactionCbor = initializationTransaction(authority);
    const initializationTxHash = CML.hash_transaction(
      CML.Transaction.from_cbor_hex(initializationTransactionCbor).body(),
    ).to_hex();
    const commitTransactionCbor = commitTransaction(
      authority,
      initializationTxHash,
      header,
    );
    const initializationBlock = await transport.makeBlock({
      transactions: [initializationTransactionCbor],
    });
    // Every block the fixture makes, so a follower store can be fed the
    // whole chain from Init to Commit, including blocks a composer adds.
    const madeBlocks = new Map<string, SyntheticUserEventBlock>([
      [initializationBlock.point.blockHash, initializationBlock],
    ]);
    const recordingTransport: SyntheticUserEventOriginFixture = {
      ...transport,
      makeBlock: async (args) => {
        const block = await fixtureTransport.makeBlock(args);
        madeBlocks.set(block.point.blockHash, block);
        return block;
      },
    };
    const commitContents = await input.composeCommitBlock?.(
      Object.freeze({
        transport: recordingTransport,
        initializationBlock,
        commitTransactionCbor,
      }),
    );
    const commitBlock = await transport.makeBlock({
      transactions: commitContents?.transactions ?? [commitTransactionCbor],
      parent: commitContents?.parent ?? initializationBlock,
      ...(commitContents?.creatingBodies === undefined
        ? {}
        : { creatingBodies: commitContents.creatingBodies }),
    });
    madeBlocks.set(commitBlock.point.blockHash, commitBlock);
    const observeFresh =
      async (): Promise<SyntheticStateQueueObservationCapture> => {
        if (closed) throw new Error("Synthetic SQ fixture is closed");
        const queries: Awaited<
          ReturnType<typeof openWatcherNativeExactPointQuery>
        >[] = [];
        const stores: ReturnType<typeof openSqliteFactStore>[] = [];
        let localRuntime:
          | WatcherLocalKupmiosNativeObservationRuntime
          | undefined;
        let captureClosed = false;
        const closeCapture = async () => {
          if (captureClosed) return;
          captureClosed = true;
          await closeAll([
            () => localRuntime?.close(),
            ...queries.map((query) => () => query.close()),
            ...stores.map((store) => () => store.close()),
          ]);
        };
        const openQuery = async (block: SyntheticUserEventBlock) => {
          const query = await openWatcherNativeExactPointQuery({
            binaryPath: fixtureTransport.l1NodeTransportBinaryPath,
            watcherConfig: fixtureTransport.watcherConfig,
            predecessor: {
              blockHash: block.parentPoint.blockHash,
              blockNo: block.parentPoint.blockNo,
              slot: block.parentPoint.slot,
            },
            target: {
              blockHash: block.point.blockHash,
              blockNo: block.point.blockNo,
              slot: block.point.slot,
            },
            timeoutMs: 60_000,
          });
          queries.push(query);
          return readWatcherNativeExactPointQuery(query.receipt);
        };
        try {
          const initialQuery = await openQuery(initializationBlock);
          localRuntime =
            await createWatcherLocalKupmiosNativeObservationRuntime({
              watcherConfig: fixtureTransport.watcherConfig,
              deploymentIdentity: fixtureTransport.deploymentIdentity,
              nativeAuthority: initialQuery.authority,
            });
          await readAdmittedLocalKupmiosBoundary({
            source: localRuntime.rawSource,
          });
          // The observations come from a watcher follower store fed the
          // fixture's chain from Init to the observed block, then empty
          // blocks up to the native query's tip, read at the release depth as
          // the decision driver reads them. Each observation is a fresh store.
          const origin = initializationBlock.parentPoint;
          const observeAt = async (
            block: SyntheticUserEventBlock,
            depthAtTip: number,
          ) => {
            const store = openSqliteFactStore({
              ...simStoreOptions(
                [
                  watcherProjection({
                    network: fixtureTransport.deploymentIdentity.network,
                    ...authority.protocolScriptHashes,
                  }),
                ],
                RELEASE_FINALITY_DEPTH + 2,
                "sqlite",
              ),
              path: ":memory:",
            });
            stores.push(store);
            const started = await store.start();
            if (started.kind !== "ready")
              throw new Error(`Synthetic SQ follower store: ${started.kind}`);
            const initialized = await store.initialize({
              point: {
                slot: Number(origin.slot),
                hash: Buffer.from(origin.blockHash, "hex"),
              },
              height: Number(origin.blockNo),
            });
            if (initialized.kind !== "initialized")
              throw new Error(
                `Synthetic SQ follower origin: ${initialized.kind}`,
              );
            const chain: SyntheticUserEventBlock[] = [];
            for (let cursor = block; ; ) {
              chain.unshift(cursor);
              if (cursor.parentPoint.blockHash === origin.blockHash) break;
              const parent = madeBlocks.get(cursor.parentPoint.blockHash);
              if (parent === undefined)
                throw new Error(
                  `Synthetic SQ block ${cursor.point.blockHash} does not descend from Init`,
                );
              cursor = parent;
            }
            let tip = block;
            for (let extra = 1; extra < depthAtTip; extra++) {
              tip = buildBlock([], tip.point);
              chain.push(tip);
            }
            for (const next of chain) {
              const result = await store.applyBlock(
                decodeBlock(Buffer.from(next.nativeBlock.rawBlockCbor, "hex")),
              );
              if (result.kind !== "applied")
                throw new Error(`Synthetic SQ block apply: ${result.kind}`);
            }
            const read = await readWatcherObservation(store, {
              authority,
              sourceId: "synthetic-state-queue-observation",
              depth: RELEASE_FINALITY_DEPTH,
              releaseDepth: RELEASE_FINALITY_DEPTH,
            });
            if (read.kind !== "ok")
              throw new Error(
                `Synthetic SQ observation: ${read.reason}: ${read.detail}`,
              );
            return read.observation;
          };
          const initialNative = admitWatcherNativeRollForwardBlock(
            initialQuery.event,
          );
          await localRuntime.observe({
            block: initialNative,
            depth: initialQuery.depthAtObservedTip,
          });
          const initialObservation = await observeAt(
            initializationBlock,
            Number(initialQuery.depthAtObservedTip),
          );
          assertWatcherStateQueueObservation(initialObservation);
          const commitQuery = await openQuery(commitBlock);
          const nativeBlock = admitWatcherNativeRollForwardBlock(
            commitQuery.event,
          );
          const localObservation = await localRuntime.observe({
            block: nativeBlock,
            depth: commitQuery.depthAtObservedTip,
          });
          const observation = await observeAt(
            commitBlock,
            Number(commitQuery.depthAtObservedTip),
          );
          assertWatcherStateQueueObservation(observation);
          const observedHeader = observation.finalizedHeaders.find(
            (item) => item.headerHash === headerHash,
          );
          if (observedHeader === undefined)
            throw new Error("Synthetic SQ commit omitted its requested header");
          assertWatcherStateQueueHeaderObservation(observedHeader);
          const capture = Object.freeze({
            localRuntime,
            initialObservation,
            observation,
            header: observedHeader,
            nativeBlock,
            nativeEventReceipt: commitQuery.eventReceipt,
            localObservation,
            close: closeCapture,
          });
          captures.add(capture);
          return capture;
        } catch (error) {
          await closeCapture();
          throw error;
        }
      };
    return Object.freeze({
      transport,
      initializationTransactionCbor,
      commitTransactionCbor,
      initializationBlock,
      commitBlock,
      header,
      headerHash,
      observeFresh,
      close,
    });
  } catch (error) {
    await close();
    throw error;
  }
};
