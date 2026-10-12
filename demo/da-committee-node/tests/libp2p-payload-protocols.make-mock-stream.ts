import {
  computeDaSha256Hash,
  DaRequestResponseProtocol,
  daRequestResponseProtocolId,
  encodeDaPayloadSubmitRequestCbor,
} from "@al-ft/midgard-core/da-transport";

import type {
  DaLibp2pStream,
  DaLibp2pStreamHandler,
} from "../src/da/libp2p/DaLibp2pNode.js";
import { DaLibp2pPayloadProtocolHandlers } from "../src/da/libp2p/payload-protocols.js";
import type { DaStoredPayloadRootSet, Header } from "../src/domain.js";
import { type PostgresCommitteeStore } from "../src/store/postgres.js";
import { openTestCommitteeStore } from "./helpers/committee-store.js";

export const deploymentFingerprint = "01".repeat(32);

export const deploymentFingerprintBytes = Buffer.from(
  deploymentFingerprint,
  "hex",
);

export const payloadSubmitProtocolId = daRequestResponseProtocolId(
  deploymentFingerprint,
  DaRequestResponseProtocol.payloadSubmit,
);

export const availabilityCommitmentAuthority = {
  deploymentIdentity: "99".repeat(28),
  responseGeometry: {
    chunkByteLength: 4_096,
    trancheByteLength: 4 * 1_024 * 1_024,
    maxTrancheCount: 16,
  },
};

export const encodeSubmit = (
  headerHash: string,
  payloadBytes: Buffer,
): Buffer =>
  encodeDaPayloadSubmitRequestCbor({
    deploymentFingerprint: deploymentFingerprintBytes,
    headerHash: Buffer.from(headerHash, "hex"),
    payloadHash: computeDaSha256Hash(payloadBytes),
    payloadSchemaVersion: 1,
    mode: "inline",
    payloadBytes,
    chunkManifest: null,
  });

export const makeHandlers = async (
  limits: Partial<{
    readonly maxPayloadBytes: number;
    readonly maxInlineResponseBytes: number;
    readonly maxChunkBytes: number;
    readonly maxStreamsPerPeer: number;
    readonly requestTimeoutMs: number;
  }> = {},
): Promise<{
  readonly handlers: DaLibp2pPayloadProtocolHandlers;
  readonly store: PostgresCommitteeStore;
}> => {
  const store = await openTestCommitteeStore();
  return {
    store,
    handlers: new DaLibp2pPayloadProtocolHandlers({
      deploymentFingerprint,
      store,
      limits,
      now: () => new Date("2026-07-24T00:00:00.000Z"),
    }),
  };
};

type MockDaStreamState = {
  readonly sent: Buffer[];
  closed: boolean;
  aborted?: Error;
};

export const makeMockStream = (
  chunks: AsyncIterable<Buffer>,
  onAbort?: (error: Error) => void,
): { readonly stream: DaLibp2pStream; readonly state: MockDaStreamState } => {
  const state: MockDaStreamState = { sent: [], closed: false };
  return {
    state,
    stream: {
      async *[Symbol.asyncIterator](): AsyncGenerator<Buffer> {
        yield* chunks;
      },
      send(data: Uint8Array): boolean {
        state.sent.push(Buffer.from(data));
        return true;
      },
      close(): void {
        state.closed = true;
      },
      abort(error: Error): void {
        state.aborted = error;
        onAbort?.(error);
      },
    },
  };
};

export const invokePayloadSubmitHandler = (
  handler: DaLibp2pStreamHandler,
  stream: DaLibp2pStream,
): Promise<void> =>
  Promise.resolve(
    handler({
      protocolId: payloadSubmitProtocolId,
      protocolName: DaRequestResponseProtocol.payloadSubmit,
      stream,
      connection: {},
    }),
  );

export const waitFor = async (
  predicate: () => boolean,
  label: string,
): Promise<void> => {
  const deadline = Date.now() + 1_000;
  while (!predicate()) {
    if (Date.now() >= deadline) {
      throw new Error(`timed out waiting for ${label}`);
    }
    await new Promise((resolve) => setTimeout(resolve, 1));
  }
};

export const rootSummaryFromHeader = (
  header: Header,
): DaStoredPayloadRootSet => ({
  utxosRoot: header.utxosRoot,
  withdrawalsRoot: header.withdrawalsRoot,
  forcedTransactionsRoot: header.forcedTransactionsRoot,
  transactionsRoot: header.transactionsRoot,
  depositsRoot: header.depositsRoot,
  transitionTraceRoot: header.transitionTraceRoot,
  eventToStepRoot: header.eventToStepRoot,
  validationTracesRoot: header.validationTracesRoot,
});
