import { withDaRequestDeadline } from "@al-ft/midgard-core/da-request-deadline";
import {
  readSingleDaStreamFrame,
  writeDaStreamFrame,
} from "@al-ft/midgard-core/da-stream-codec";
import { DaRequestResponseProtocol } from "@al-ft/midgard-core/da-transport";

import type { Libp2pDaTransportLimits } from "../../config.js";
import type { CommitteeStore } from "../../store.js";
import type { DaLibp2pStreamHandler } from "./DaLibp2pNode.js";
import { createDaProtocolAllowlist } from "./DaProtocols.js";
import {
  DaLibp2pPayloadProtocolHandlers,
  type DaLibp2pPublicRetainedDaPayloadStore,
} from "./payload-protocols.js";
import {
  DaPayloadSubmitAdmission,
  processWideDaPayloadSubmitAdmission,
} from "./payload-source.da-libp2p-payload-source.js";

export const createDaLibp2pPayloadRequestHandlers = ({
  deploymentFingerprint,
  store,
  limits,
  payloadSubmitAdmission = processWideDaPayloadSubmitAdmission,
}: {
  readonly deploymentFingerprint: string;
  readonly store: Pick<CommitteeStore, "getDaPayload" | "saveDaPayload">;
  readonly limits: Libp2pDaTransportLimits;
  readonly payloadSubmitAdmission?: DaPayloadSubmitAdmission;
}): ReadonlyMap<string, DaLibp2pStreamHandler> => {
  const protocolIds = createDaProtocolAllowlist(deploymentFingerprint);
  const handlers = new DaLibp2pPayloadProtocolHandlers({
    deploymentFingerprint,
    store,
    limits,
  });
  const handlerMap = new Map<string, DaLibp2pStreamHandler>();
  const addHandler = (
    protocol: DaRequestResponseProtocol,
    handle: (requestCbor: Uint8Array) => Promise<Buffer>,
    options: { readonly payloadAdmission?: boolean } = {},
  ): void => {
    const protocolId = protocolIds.protocolIdByName.get(protocol)!;
    const execute = async (
      { stream }: Parameters<DaLibp2pStreamHandler>[0],
      signal?: AbortSignal,
    ): Promise<void> => {
      signal?.throwIfAborted();
      const requestCbor = await readSingleDaStreamFrame(stream, {
        maxFrameBytes: limits.maxPayloadBytes,
      });
      signal?.throwIfAborted();
      const responseCbor = await handle(requestCbor);
      signal?.throwIfAborted();
      await writeDaStreamFrame(stream, responseCbor, {
        maxFrameBytes: limits.maxPayloadBytes,
        close: true,
      });
      signal?.throwIfAborted();
    };
    handlerMap.set(
      protocolId,
      options.payloadAdmission
        ? (context) =>
            withDaRequestDeadline({
              timeoutMs: limits.requestTimeoutMs,
              open: async (signal) => ({ context, signal }),
              run: ({ context: admittedContext, signal }) =>
                payloadSubmitAdmission.run(
                  () => execute(admittedContext, signal),
                  { signal },
                ),
              abort: ({ context: admittedContext }, error) =>
                admittedContext.stream.abort?.(error),
            })
        : (context) => execute(context),
    );
  };
  addHandler(DaRequestResponseProtocol.capabilities, (request) =>
    handlers.handleCapabilities(request),
  );
  addHandler(
    DaRequestResponseProtocol.payloadSubmit,
    (request) => handlers.handlePayloadSubmit(request),
    { payloadAdmission: true },
  );
  addHandler(DaRequestResponseProtocol.payloadByHeader, (request) =>
    handlers.handlePayloadByHeader(request),
  );
  addHandler(DaRequestResponseProtocol.payloadChunk, (request) =>
    handlers.handlePayloadChunk(request),
  );
  addHandler(DaRequestResponseProtocol.metadataByHeader, (request) =>
    handlers.handleMetadataByHeader(request),
  );
  return handlerMap;
};

/**
 * Constructs only public read handlers. The public profile cannot submit a
 * payload because this factory accepts no `saveDaPayload` capability and never
 * binds the submit protocol.
 */
export const createDaLibp2pPublicRetainedDaPayloadRequestHandlers = ({
  deploymentFingerprint,
  store,
  limits,
}: {
  readonly deploymentFingerprint: string;
  readonly store: DaLibp2pPublicRetainedDaPayloadStore;
  readonly limits: Libp2pDaTransportLimits;
}): ReadonlyMap<string, DaLibp2pStreamHandler> => {
  const protocolIds = createDaProtocolAllowlist(deploymentFingerprint);
  const handlers = new DaLibp2pPayloadProtocolHandlers({
    deploymentFingerprint,
    store,
    limits,
  });
  const handlerMap = new Map<string, DaLibp2pStreamHandler>();
  const add = (
    protocol: DaRequestResponseProtocol,
    handle: (request: Uint8Array) => Promise<Buffer>,
  ): void => {
    const protocolId = protocolIds.protocolIdByName.get(protocol)!;
    handlerMap.set(protocolId, async ({ stream }) => {
      const request = await readSingleDaStreamFrame(stream, {
        maxFrameBytes: limits.maxPayloadBytes,
      });
      const response = await handle(request);
      await writeDaStreamFrame(stream, response, {
        maxFrameBytes: limits.maxPayloadBytes,
        close: true,
      });
    });
  };
  add(DaRequestResponseProtocol.capabilities, (request) =>
    handlers.handleCapabilities(request),
  );
  add(DaRequestResponseProtocol.payloadByHeader, (request) =>
    handlers.handlePayloadByHeader(request),
  );
  add(DaRequestResponseProtocol.payloadChunk, (request) =>
    handlers.handlePayloadChunk(request),
  );
  add(DaRequestResponseProtocol.metadataByHeader, (request) =>
    handlers.handleMetadataByHeader(request),
  );
  return handlerMap;
};
