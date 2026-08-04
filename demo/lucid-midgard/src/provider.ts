import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardNativeTxCanonical,
  midgardAddressFromText,
} from "@al-ft/midgard-core/codec";

import {
  ProviderCapabilityError,
  ProviderHttpError,
  ProviderPayloadError,
} from "./core/errors.js";
import type {
  Address,
  MidgardProtocolParameters,
  MidgardUtxo,
  OutRef,
  SubmitTxResult,
  TxStatus,
} from "./core/index.js";
import { normalizeOutRef } from "./core/out-ref.js";
import {
  protocolInfoToParameters,
  requestedOutRefLabels,
  requireAddressQueryUtxos,
  requireBatchUtxosByOutRefs,
  requireUtxoByOutRef,
} from "./provider/convenience.js";
import {
  providerDiagnostics,
  trimTrailingSlash,
} from "./provider/diagnostics.js";
import {
  isObject,
  normalizeTxIdHex,
  parseProtocolInfo,
  parseSubmitTxCanonicalCbor,
  parseSubmitTxResult,
  parseTxStatus,
  parseUtxoResponse,
  parseUtxosResponse,
  txOutRefCborHex,
  validateFallbackProtocolInfo,
} from "./provider/payload.js";
import {
  MidgardNodeTransport,
  unavailableProtocolInfoStatuses,
} from "./provider/transport.js";
import type {
  MidgardNodeProviderOptions,
  MidgardProtocolInfo,
  MidgardProvider,
  ProtocolInfoFallback,
  ProviderDiagnostics,
} from "./provider/types.js";

export type {
  MidgardFetch,
  MidgardNodeProviderOptions,
  MidgardProtocolInfo,
  MidgardProvider,
  ProtocolInfoFallback,
  ProtocolScriptLanguage,
  ProviderDiagnostics,
} from "./provider/types.js";

const midgardNodeProviderConstructorToken = Symbol(
  "MidgardNodeProvider.constructor",
);

const normalizeAddressForUtxoQuery = (
  address: Address,
  endpoint: string,
): Address => {
  try {
    const normalized = address.trim();
    midgardAddressFromText(normalized);
    return normalized;
  } catch (cause) {
    throw new ProviderPayloadError(
      endpoint,
      "address must be a canonical Midgard address",
      cause instanceof Error ? cause.message : String(cause),
    );
  }
};

const submittedTxCanonicalCbor = (
  txCanonicalCborHex: string,
  endpoint: string,
  maxSubmitTxCborBytes: number,
): { readonly txCanonicalCbor: Buffer; readonly txId: string } => {
  const txCanonicalCbor = parseSubmitTxCanonicalCbor(
    txCanonicalCborHex,
    endpoint,
    maxSubmitTxCborBytes,
  );
  try {
    const tx = decodeMidgardNativeTxFullFromCanonicalCbor(txCanonicalCbor);
    if (!encodeMidgardNativeTxCanonical(tx).equals(txCanonicalCbor)) {
      throw new Error("transaction CBOR is not canonical");
    }
    return {
      txCanonicalCbor,
      txId: computeMidgardNativeTxId(tx).toString("hex"),
    };
  } catch (cause) {
    throw new ProviderPayloadError(
      endpoint,
      "tx_canonical_cbor must be a canonical Midgard native transaction",
      cause instanceof Error ? cause.message : String(cause),
    );
  }
};

export class MidgardNodeProvider implements MidgardProvider {
  readonly endpoint: string;
  private readonly transport: MidgardNodeTransport;
  private readonly protocolInfoFallback?: ProtocolInfoFallback;
  private protocolInfoSource: ProviderDiagnostics["protocolInfoSource"] =
    "unknown";
  private protocolInfoFallbackReason: string | undefined;

  private constructor(
    options: MidgardNodeProviderOptions,
    token?: typeof midgardNodeProviderConstructorToken,
  ) {
    if (token !== midgardNodeProviderConstructorToken) {
      throw new ProviderCapabilityError(
        "constructor",
        "Use MidgardNodeProvider.create() so protocol-info is checked before use",
      );
    }
    this.endpoint = trimTrailingSlash(options.endpoint);
    this.transport = new MidgardNodeTransport(
      this.endpoint,
      options.fetch ?? fetch,
    );
    this.protocolInfoFallback = options.protocolInfoFallback;
  }

  static async create(
    options: MidgardNodeProviderOptions,
  ): Promise<MidgardNodeProvider> {
    const provider = new MidgardNodeProvider(
      options,
      midgardNodeProviderConstructorToken,
    );
    await provider.getProtocolInfo();
    return provider;
  }

  diagnostics(): ProviderDiagnostics {
    return providerDiagnostics({
      endpoint: this.endpoint,
      protocolInfoSource: this.protocolInfoSource,
      protocolInfoFallbackReason: this.protocolInfoFallbackReason,
    });
  }

  async getProtocolInfo(): Promise<MidgardProtocolInfo> {
    const endpoint = "/protocol-info";
    const { response, payload } = await this.transport.requestJson(endpoint);
    if (!response.ok) {
      if (unavailableProtocolInfoStatuses.has(response.status)) {
        if (this.protocolInfoFallback === undefined) {
          throw new ProviderCapabilityError(
            endpoint,
            "GET /protocol-info is unavailable and no explicit fallback was supplied",
          );
        }
        this.protocolInfoSource = "fallback";
        this.protocolInfoFallbackReason = this.protocolInfoFallback.reason;
        return validateFallbackProtocolInfo(
          this.protocolInfoFallback.protocolInfo,
          endpoint,
        );
      }
      throw new ProviderHttpError({
        endpoint,
        statusCode: response.status,
        message: "GET /protocol-info failed",
        detail: JSON.stringify(payload ?? null),
      });
    }
    const protocolInfo = parseProtocolInfo(payload, endpoint);
    this.protocolInfoSource = "node";
    this.protocolInfoFallbackReason = undefined;
    return protocolInfo;
  }

  async getProtocolParameters(): Promise<MidgardProtocolParameters> {
    return protocolInfoToParameters(await this.getProtocolInfo());
  }

  async getCurrentSlot(): Promise<bigint> {
    return (await this.getProtocolInfo()).currentSlot;
  }

  async getUtxos(address: Address): Promise<readonly MidgardUtxo[]> {
    const endpoint = "/utxos";
    const requestedAddress = normalizeAddressForUtxoQuery(address, endpoint);
    const { response, payload } = await this.transport.requestJson(
      endpoint,
      undefined,
      {
        address: requestedAddress,
      },
    );
    if (!response.ok) {
      throw new ProviderHttpError({
        endpoint,
        statusCode: response.status,
        message: "GET /utxos failed",
        detail: JSON.stringify(payload ?? null),
      });
    }
    return requireAddressQueryUtxos(
      parseUtxosResponse(
        payload,
        endpoint,
        "GET /utxos response must contain an utxos array",
      ),
      requestedAddress,
      endpoint,
    );
  }

  async getUtxoByOutRef(outRef: OutRef): Promise<MidgardUtxo | undefined> {
    const endpoint = "/utxo";
    const requestedOutRef = normalizeOutRef(outRef);
    const { response, payload } = await this.transport.requestJson(
      endpoint,
      undefined,
      {
        txOutRef: txOutRefCborHex(requestedOutRef),
      },
    );
    if (response.status === 404) {
      return undefined;
    }
    if (!response.ok) {
      throw new ProviderHttpError({
        endpoint,
        statusCode: response.status,
        message: "GET /utxo failed",
        detail: JSON.stringify(payload ?? null),
      });
    }
    return requireUtxoByOutRef(
      parseUtxoResponse(payload, endpoint),
      requestedOutRef,
      endpoint,
    );
  }

  async getUtxosByOutRefs(
    outRefs: readonly OutRef[],
  ): Promise<readonly MidgardUtxo[]> {
    const endpoint = "/utxos?by-outrefs";
    const requestedLabels = requestedOutRefLabels(outRefs, endpoint);
    const { response, payload } = await this.transport.requestJson(
      "/utxos",
      {
        method: "POST",
        headers: { "content-type": "application/json" },
        body: JSON.stringify([...requestedLabels]),
      },
      {
        "by-outrefs": "",
      },
    );
    if (!response.ok) {
      throw new ProviderHttpError({
        endpoint,
        statusCode: response.status,
        message: "POST /utxos?by-outrefs failed",
        detail: JSON.stringify(payload ?? null),
      });
    }
    return requireBatchUtxosByOutRefs(
      parseUtxosResponse(
        payload,
        endpoint,
        "POST /utxos?by-outrefs response must contain an utxos array",
      ),
      requestedLabels,
      endpoint,
    );
  }

  async submitTx(txCanonicalCborHex: string): Promise<SubmitTxResult> {
    const endpoint = "/submit";
    const protocolInfo = await this.getProtocolInfo();
    const submittedTx = submittedTxCanonicalCbor(
      txCanonicalCborHex,
      endpoint,
      protocolInfo.submissionLimits.maxSubmitTxCborBytes,
    );
    const { response, payload } = await this.transport.requestJson(endpoint, {
      method: "POST",
      headers: { "content-type": "application/cbor" },
      body: submittedTx.txCanonicalCbor,
    });
    if (response.status !== 200 && response.status !== 202) {
      const message = isObject(payload)
        ? typeof payload.error === "string"
          ? payload.error
          : `POST /submit failed with status ${response.status.toString()}`
        : `POST /submit failed with status ${response.status.toString()}`;
      throw new ProviderHttpError({
        endpoint,
        statusCode: response.status,
        message,
        detail: JSON.stringify(payload ?? null),
        retryable: response.status === 503 || response.status >= 500,
      });
    }
    const admission = parseSubmitTxResult(payload, response.status, endpoint);
    if (admission.txId !== submittedTx.txId) {
      throw new ProviderPayloadError(
        endpoint,
        "POST /submit returned a different tx id than the submitted tx",
        `expected=${submittedTx.txId} actual=${admission.txId}`,
      );
    }
    return admission;
  }

  async getTxStatus(txId: string): Promise<TxStatus> {
    const endpoint = "/tx-status";
    const requestedTxId = normalizeTxIdHex(txId, endpoint);
    const { response, payload } = await this.transport.requestJson(
      endpoint,
      undefined,
      {
        tx_hash: requestedTxId,
      },
    );
    const hasNotFoundStatus =
      response.status === 404 &&
      isObject(payload) &&
      payload.status === "not_found";
    if (response.ok || hasNotFoundStatus) {
      const status = parseTxStatus(payload, endpoint);
      if (status.txId !== requestedTxId) {
        throw new ProviderPayloadError(
          endpoint,
          "GET /tx-status returned a different tx id than requested",
          `expected=${requestedTxId} actual=${status.txId}`,
        );
      }
      return status;
    }
    if ([404, 405, 501].includes(response.status)) {
      throw new ProviderCapabilityError(endpoint, "GET /tx-status unavailable");
    }
    throw new ProviderHttpError({
      endpoint,
      statusCode: response.status,
      message: "GET /tx-status failed",
      detail: JSON.stringify(payload ?? null),
    });
  }
}
