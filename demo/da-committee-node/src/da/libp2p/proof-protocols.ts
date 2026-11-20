import {
  DA_TRANSPORT_LIMITS_V1,
  daDeploymentFingerprintFromHex,
  type DaEventToStepByEventResponseV1,
  type DaProofBundleByHeaderResponseV1,
  DaRequestResponseProtocol,
  type DaTraceStepByIndexResponseV1,
  decodeDaEventToStepByEventRequestV1Cbor,
  decodeDaProofBundleByHeaderRequestV1Cbor,
  decodeDaTraceStepByIndexRequestV1Cbor,
  encodeDaEventToStepByEventResponseV1Cbor,
  encodeDaProofBundleByHeaderResponseV1Cbor,
  encodeDaTraceStepByIndexResponseV1Cbor,
  normalizeDaDeploymentFingerprintHex,
} from "@al-ft/midgard-core/da-transport";

import type { Libp2pDaRole, Libp2pDaTransportLimits } from "../../config.js";
import type { TransactionRootValueProjector } from "../payload.js";
import {
  DaProofArtifactDeriver,
  type DaProofArtifactStore,
} from "../proof-artifacts.js";
import type { DaLibp2pStreamHandler } from "./DaLibp2pNode.js";
import type { DaPeerRegistry } from "./DaPeerRegistry.js";
import { createDaProtocolAllowlist } from "./DaProtocols.js";
import {
  readSingleDaStreamFrame,
  writeDaStreamFrame,
} from "./DaStreamCodec.js";

export type DaLibp2pProofProtocolLimits = {
  readonly maxPayloadBytes: number;
  readonly maxInlineResponseBytes: number;
};

export type DaLibp2pProofProtocolHandlersOptions = {
  readonly deploymentFingerprint: string | Uint8Array;
  readonly store: DaProofArtifactStore;
  readonly limits?: Partial<DaLibp2pProofProtocolLimits>;
  readonly registry?: Pick<DaPeerRegistry, "getByPeerId">;
  readonly allowedRequesterRoles?: readonly Libp2pDaRole[];
  readonly transactionProjector?: TransactionRootValueProjector;
};

export type DaLibp2pProofRequestContext = {
  readonly remotePeerId?: string;
};

export class DaLibp2pProofProtocolError extends Error {
  constructor(message: string, options?: ErrorOptions) {
    super(message, options);
    this.name = "DaLibp2pProofProtocolError";
  }
}

export class DaLibp2pProofProtocolHandlers {
  private readonly deploymentFingerprintBytes: Buffer;
  private readonly deriver: DaProofArtifactDeriver;
  private readonly limits: DaLibp2pProofProtocolLimits;
  private readonly registry?: Pick<DaPeerRegistry, "getByPeerId">;
  private readonly allowedRequesterRoles: ReadonlySet<Libp2pDaRole>;

  constructor(options: DaLibp2pProofProtocolHandlersOptions) {
    const deploymentFingerprint = normalizeDaDeploymentFingerprintHex(
      options.deploymentFingerprint,
    );
    this.deploymentFingerprintBytes = daDeploymentFingerprintFromHex(
      deploymentFingerprint,
    );
    this.deriver = new DaProofArtifactDeriver({
      deploymentFingerprint,
      store: options.store,
      transactionProjector: options.transactionProjector,
    });
    this.registry = options.registry;
    this.allowedRequesterRoles = new Set(
      options.allowedRequesterRoles ?? [
        "committee",
        "watcher",
        "challenger",
        "retrieval",
      ],
    );
    this.limits = {
      maxPayloadBytes:
        options.limits?.maxPayloadBytes ??
        DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
      maxInlineResponseBytes:
        options.limits?.maxInlineResponseBytes ??
        DA_TRANSPORT_LIMITS_V1.maxInlineResponseBytes,
    };
    validateLimits(this.limits);
  }

  async handleProofBundleByHeader(
    requestCbor: Uint8Array,
    context: DaLibp2pProofRequestContext = {},
  ): Promise<Buffer> {
    const request = decodeRequest(
      () => decodeDaProofBundleByHeaderRequestV1Cbor(requestCbor),
      "proof-bundle-by-header request",
    );
    if (!this.matchesDeployment(request.deploymentFingerprint)) {
      return encodeDaProofBundleByHeaderResponseV1Cbor({
        ...emptyProofBundleResponse(request.headerHash),
        status: "rejected",
        reasonCode: "deployment_fingerprint_mismatch",
      });
    }
    const authorizationError = this.authorizationError(context);
    if (authorizationError !== undefined) {
      return encodeDaProofBundleByHeaderResponseV1Cbor({
        ...emptyProofBundleResponse(request.headerHash),
        status: "rejected",
        reasonCode: authorizationError,
      });
    }
    const maxInlineBytes = Math.min(
      request.maxInlineBytes,
      this.limits.maxInlineResponseBytes,
    );
    const { response } = await this.deriver.proofBundleByHeader({
      ...request,
      maxInlineBytes,
    });
    return encodeDaProofBundleByHeaderResponseV1Cbor(response);
  }

  async handleTraceStepByIndex(
    requestCbor: Uint8Array,
    context: DaLibp2pProofRequestContext = {},
  ): Promise<Buffer> {
    const request = decodeRequest(
      () => decodeDaTraceStepByIndexRequestV1Cbor(requestCbor),
      "trace-step-by-index request",
    );
    if (!this.matchesDeployment(request.deploymentFingerprint)) {
      return encodeDaTraceStepByIndexResponseV1Cbor(
        emptyTraceStepResponse(
          request.headerHash,
          request.stepIndex,
          "rejected",
        ),
      );
    }
    if (this.authorizationError(context) !== undefined) {
      return encodeDaTraceStepByIndexResponseV1Cbor(
        emptyTraceStepResponse(
          request.headerHash,
          request.stepIndex,
          "rejected",
        ),
      );
    }
    const { response } = await this.deriver.traceStepByIndex(request);
    return encodeDaTraceStepByIndexResponseV1Cbor(response);
  }

  async handleEventToStepByEvent(
    requestCbor: Uint8Array,
    context: DaLibp2pProofRequestContext = {},
  ): Promise<Buffer> {
    const request = decodeRequest(
      () => decodeDaEventToStepByEventRequestV1Cbor(requestCbor),
      "event-to-step-by-event request",
    );
    if (!this.matchesDeployment(request.deploymentFingerprint)) {
      return encodeDaEventToStepByEventResponseV1Cbor(
        emptyEventToStepResponse(
          request.headerHash,
          request.eventKey,
          "rejected",
        ),
      );
    }
    if (this.authorizationError(context) !== undefined) {
      return encodeDaEventToStepByEventResponseV1Cbor(
        emptyEventToStepResponse(
          request.headerHash,
          request.eventKey,
          "rejected",
        ),
      );
    }
    const { response } = await this.deriver.eventToStepByEvent(request);
    return encodeDaEventToStepByEventResponseV1Cbor(
      eventToStepTransportResponse(response),
    );
  }

  private matchesDeployment(value: Buffer): boolean {
    return value.equals(this.deploymentFingerprintBytes);
  }

  private authorizationError(
    context: DaLibp2pProofRequestContext,
  ): string | undefined {
    if (this.registry === undefined) {
      return undefined;
    }
    if (context.remotePeerId === undefined) {
      return "unknown_peer";
    }
    const entry = this.registry.getByPeerId(context.remotePeerId);
    if (entry === undefined) {
      return "unknown_peer";
    }
    return entry.roles.some((role) => this.allowedRequesterRoles.has(role))
      ? undefined
      : "unauthorized_peer_role";
  }
}

export const createDaLibp2pProofRequestHandlers = ({
  deploymentFingerprint,
  store,
  limits,
  registry,
}: {
  readonly deploymentFingerprint: string;
  readonly store: DaProofArtifactStore;
  readonly limits: Libp2pDaTransportLimits;
  readonly registry?: Pick<DaPeerRegistry, "getByPeerId">;
}): ReadonlyMap<string, DaLibp2pStreamHandler> => {
  const protocolIds = createDaProtocolAllowlist(deploymentFingerprint);
  const handlers = new DaLibp2pProofProtocolHandlers({
    deploymentFingerprint,
    store,
    limits,
    registry,
  });
  const handlerMap = new Map<string, DaLibp2pStreamHandler>();
  const addHandler = (
    protocol: DaRequestResponseProtocol,
    handle: (
      requestCbor: Uint8Array,
      context: DaLibp2pProofRequestContext,
    ) => Promise<Buffer>,
  ): void => {
    const protocolId = protocolIds.protocolIdByName.get(protocol)!;
    handlerMap.set(protocolId, async ({ stream, remotePeerId }) => {
      const requestCbor = await readSingleDaStreamFrame(stream, {
        maxFrameBytes: limits.maxPayloadBytes,
      });
      const responseCbor = await handle(requestCbor, { remotePeerId });
      await writeDaStreamFrame(stream, responseCbor, {
        maxFrameBytes: limits.maxPayloadBytes,
        close: true,
      });
    });
  };
  addHandler(
    DaRequestResponseProtocol.proofBundleByHeader,
    (request, context) => handlers.handleProofBundleByHeader(request, context),
  );
  addHandler(DaRequestResponseProtocol.traceStepByIndex, (request, context) =>
    handlers.handleTraceStepByIndex(request, context),
  );
  addHandler(DaRequestResponseProtocol.eventToStepByEvent, (request, context) =>
    handlers.handleEventToStepByEvent(request, context),
  );
  return handlerMap;
};

const decodeRequest = <T>(decode: () => T, label: string): T => {
  try {
    return decode();
  } catch (cause) {
    throw new DaLibp2pProofProtocolError(`invalid ${label}`, { cause });
  }
};

const validateLimits = (limits: DaLibp2pProofProtocolLimits): void => {
  if (
    !Number.isSafeInteger(limits.maxPayloadBytes) ||
    limits.maxPayloadBytes <= 0
  ) {
    throw new Error("maxPayloadBytes must be a positive safe integer");
  }
  if (
    !Number.isSafeInteger(limits.maxInlineResponseBytes) ||
    limits.maxInlineResponseBytes < 0
  ) {
    throw new Error(
      "maxInlineResponseBytes must be a non-negative safe integer",
    );
  }
};

const emptyProofBundleResponse = (
  headerHash: Buffer,
): Omit<DaProofBundleByHeaderResponseV1, "status" | "reasonCode"> => ({
  headerHash,
  proofBundleHash: null,
  proofBundleBytes: null,
  chunkManifest: null,
});

const emptyTraceStepResponse = (
  headerHash: Buffer,
  stepIndex: number,
  status: DaTraceStepByIndexResponseV1["status"],
): DaTraceStepByIndexResponseV1 => ({
  status,
  headerHash,
  stepIndex,
  transitionStepBytes: null,
  membershipProofBytes: null,
});

const emptyEventToStepResponse = (
  headerHash: Buffer,
  eventKey: Buffer,
  status: DaEventToStepByEventResponseV1["status"],
): DaEventToStepByEventResponseV1 => ({
  status,
  headerHash,
  eventKey,
  eventToStepEntryBytes: null,
  membershipOrNonmembershipProofBytes: null,
});

const eventToStepTransportResponse = (
  response: DaEventToStepByEventResponseV1,
): DaEventToStepByEventResponseV1 => response;
