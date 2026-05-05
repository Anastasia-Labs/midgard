import { CML } from "@lucid-evolution/lucid";
import {
  MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
  midgardAddressFromText,
  midgardAddressToText,
  scriptLanguageTagToName,
  type ScriptLanguageName,
  type ScriptLanguageTag,
} from "@al-ft/midgard-core/codec";
import {
  decodeMidgardUtxo,
  utxoAddress,
  isSubmitAdmissionStatus,
  type Address,
  type MidgardProtocolParameters,
  type MidgardUtxo,
  type OutRef,
  type SubmitTxResult,
  type TxStatus,
} from "./core/index.js";
import {
  ProviderCapabilityError,
  ProviderHttpError,
  ProviderPayloadError,
  ProviderTransportError,
} from "./core/errors.js";
import { normalizeOutRef, outRefLabel } from "./core/out-ref.js";

export type ProtocolScriptLanguage = {
  readonly name: ScriptLanguageName;
  readonly tag: ScriptLanguageTag;
};

export type MidgardProtocolInfo = {
  readonly apiVersion: number;
  readonly network: string;
  readonly midgardNativeTxVersion: number;
  readonly currentSlot: bigint;
  readonly supportedScriptLanguages: readonly ProtocolScriptLanguage[];
  readonly protocolFeeParameters: {
    readonly minFeeA: bigint;
    readonly minFeeB: bigint;
  };
  readonly submissionLimits: {
    readonly maxSubmitTxCborBytes: number;
  };
  readonly validation: {
    readonly strictnessProfile: string;
    readonly localValidationIsAuthoritative: false;
  };
};

export type ProviderDiagnostics = {
  readonly endpoint: string;
  readonly protocolInfoSource: "node" | "fallback" | "unknown";
  readonly protocolInfoFallbackReason?: string;
};

export type ProtocolInfoFallback = {
  readonly protocolInfo: MidgardProtocolInfo;
  readonly reason: string;
};

export type MidgardProvider = {
  getUtxos(address: Address): Promise<readonly MidgardUtxo[]>;
  getUtxoByOutRef(outRef: OutRef): Promise<MidgardUtxo | undefined>;
  getUtxosByOutRefs?(
    outRefs: readonly OutRef[],
  ): Promise<readonly MidgardUtxo[]>;
  getUtxosByUnit?(unit: string): Promise<readonly MidgardUtxo[]>;
  getProtocolInfo(): Promise<MidgardProtocolInfo>;
  getProtocolParameters(): Promise<MidgardProtocolParameters>;
  getCurrentSlot(): Promise<bigint>;
  submitTx(txCborHex: string): Promise<SubmitTxResult>;
  getTxStatus(txId: string): Promise<TxStatus>;
  diagnostics(): ProviderDiagnostics;
};

export type MidgardFetch = (
  input: string | URL,
  init?: RequestInit,
) => Promise<Response>;

const knownNetworkId = (network: string): bigint => {
  switch (network) {
    case "Mainnet":
      return 1n;
    case "Preprod":
    case "Preview":
      return 0n;
    default:
      throw new ProviderPayloadError(
        "/protocol-info",
        "unsupported protocol network",
        network,
      );
  }
};

export type MidgardNodeProviderOptions = {
  readonly endpoint: string;
  readonly fetch?: MidgardFetch;
  readonly protocolInfoFallback?: ProtocolInfoFallback;
};

const midgardNodeProviderConstructorToken = Symbol(
  "MidgardNodeProvider.constructor",
);

type EncodedStoredUtxo = {
  readonly outref: string;
  readonly outputCbor: string;
};

const unavailableProtocolInfoStatuses = new Set([404, 405, 501]);

const trimTrailingSlash = (endpoint: string): string => {
  const normalized = endpoint.trim();
  if (normalized.length === 0) {
    throw new ProviderPayloadError("constructor", "Provider endpoint is empty");
  }
  return normalized.replace(/\/+$/, "");
};

const redactEndpoint = (endpoint: string): string => {
  try {
    const url = new URL(endpoint);
    url.username = "";
    url.password = "";
    url.search = "";
    url.hash = "";
    return url.toString().replace(/\/+$/, "");
  } catch {
    return "<redacted-invalid-endpoint>";
  }
};

const isObject = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null && !Array.isArray(value);

const requireString = (
  value: unknown,
  fieldName: string,
  endpoint: string,
): string => {
  if (typeof value !== "string") {
    throw new ProviderPayloadError(endpoint, `${fieldName} must be a string`);
  }
  return value;
};

const requireNumber = (
  value: unknown,
  fieldName: string,
  endpoint: string,
): number => {
  if (typeof value !== "number" || !Number.isSafeInteger(value) || value <= 0) {
    throw new ProviderPayloadError(
      endpoint,
      `${fieldName} must be a positive safe integer`,
    );
  }
  return value;
};

const requireNonNegativeSafeInteger = (
  value: unknown,
  fieldName: string,
  endpoint: string,
): number => {
  if (typeof value !== "number" || !Number.isSafeInteger(value) || value < 0) {
    throw new ProviderPayloadError(
      endpoint,
      `${fieldName} must be a non-negative safe integer`,
    );
  }
  return value;
};

const parseNonNegativeBigInt = (
  value: unknown,
  fieldName: string,
  endpoint: string,
): bigint => {
  const raw = requireString(value, fieldName, endpoint);
  if (!/^(0|[1-9][0-9]*)$/.test(raw)) {
    throw new ProviderPayloadError(
      endpoint,
      `${fieldName} must be a non-negative integer string`,
    );
  }
  return BigInt(raw);
};

const cloneSupportedScriptLanguages = (
  languages: readonly ProtocolScriptLanguage[],
): readonly ProtocolScriptLanguage[] =>
  languages.map((language) => ({
    name: language.name,
    tag: language.tag,
  }));

const expectedScriptLanguageLabel = (
  language: ProtocolScriptLanguage,
): string => `${language.name}:${language.tag.toString(10)}`;

const validateSupportedScriptLanguages = (
  languages: unknown,
  endpoint: string,
  fieldName = "supportedScriptLanguages",
): readonly ProtocolScriptLanguage[] => {
  if (!Array.isArray(languages)) {
    throw new ProviderPayloadError(endpoint, `${fieldName} must be an array`);
  }
  const normalized = languages.map((raw, index) => {
    if (!isObject(raw)) {
      throw new ProviderPayloadError(
        endpoint,
        `${fieldName}[${index.toString()}] must be an object`,
      );
    }
    const name = requireString(
      raw.name,
      `${fieldName}[${index.toString()}].name`,
      endpoint,
    );
    const tag = requireNonNegativeSafeInteger(
      raw.tag,
      `${fieldName}[${index.toString()}].tag`,
      endpoint,
    );
    let canonicalName: ScriptLanguageName;
    try {
      canonicalName = scriptLanguageTagToName(tag as ScriptLanguageTag);
    } catch (cause) {
      throw new ProviderPayloadError(
        endpoint,
        `unsupported script language tag ${tag.toString(10)}`,
        cause instanceof Error ? cause.message : String(cause),
      );
    }
    if (name !== canonicalName) {
      throw new ProviderPayloadError(
        endpoint,
        `script language tag/name mismatch for ${fieldName}[${index.toString()}]`,
        `${name}:${tag.toString(10)}`,
      );
    }
    return {
      name: canonicalName,
      tag: tag as ScriptLanguageTag,
    };
  });
  const expected = MIDGARD_SUPPORTED_SCRIPT_LANGUAGES.map(
    expectedScriptLanguageLabel,
  ).sort();
  const actual = normalized.map(expectedScriptLanguageLabel).sort();
  if (
    expected.length !== actual.length ||
    expected.some((label, index) => actual[index] !== label)
  ) {
    throw new ProviderPayloadError(
      endpoint,
      "supported script languages must exactly match the Midgard protocol profile",
      `expected=${expected.join(",")} actual=${actual.join(",")}`,
    );
  }
  return cloneSupportedScriptLanguages(MIDGARD_SUPPORTED_SCRIPT_LANGUAGES);
};

const fromHex = (hex: string, fieldName: string, endpoint: string): Buffer => {
  const normalized = hex.trim().toLowerCase();
  if (normalized.length === 0 || normalized.length % 2 !== 0) {
    throw new ProviderPayloadError(endpoint, `${fieldName} must be hex`);
  }
  if (!/^[0-9a-f]+$/.test(normalized)) {
    throw new ProviderPayloadError(endpoint, `${fieldName} must be hex`);
  }
  return Buffer.from(normalized, "hex");
};

const normalizeSubmitTxCborHex = (
  txCborHex: string,
  endpoint: string,
  maxSubmitTxCborBytes?: number,
): string => {
  const bytes = fromHex(txCborHex, "tx_cbor", endpoint);
  if (
    maxSubmitTxCborBytes !== undefined &&
    bytes.length > maxSubmitTxCborBytes
  ) {
    throw new ProviderPayloadError(
      endpoint,
      "tx_cbor exceeds protocol submit size limit",
      `size=${bytes.length.toString()} max=${maxSubmitTxCborBytes.toString()}`,
    );
  }
  return bytes.toString("hex");
};

export const outRefToCbor = (outRef: OutRef): Buffer => {
  const normalized = normalizeOutRef(outRef);
  return Buffer.from(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(normalized.txHash),
      BigInt(normalized.outputIndex),
    ).to_cbor_bytes(),
  );
};

const normalizeTxIdHex = (txId: string, endpoint: string): string => {
  const normalized = txId.trim().toLowerCase();
  if (normalized.length !== 64 || !/^[0-9a-f]+$/.test(normalized)) {
    throw new ProviderPayloadError(
      endpoint,
      "transaction id must be a 32-byte hex string",
    );
  }
  return normalized;
};

const decodeEncodedUtxo = (raw: unknown, endpoint: string): MidgardUtxo => {
  if (!isObject(raw)) {
    throw new ProviderPayloadError(endpoint, "UTxO entry must be an object");
  }
  const outRefCbor = fromHex(
    requireString(raw.outref, "utxo.outref", endpoint),
    "utxo.outref",
    endpoint,
  );
  const outputCbor = fromHex(
    requireString(raw.outputCbor, "utxo.outputCbor", endpoint),
    "utxo.outputCbor",
    endpoint,
  );
  let input: InstanceType<typeof CML.TransactionInput>;
  try {
    input = CML.TransactionInput.from_cbor_bytes(outRefCbor);
  } catch (cause) {
    throw new ProviderPayloadError(
      endpoint,
      "utxo.outref is not canonical TxOutRef CBOR",
      cause instanceof Error ? cause.message : String(cause),
    );
  }
  const outputIndex = Number(input.index());
  if (!Number.isSafeInteger(outputIndex)) {
    throw new ProviderPayloadError(
      endpoint,
      "utxo.outref output index exceeds safe integer range",
    );
  }
  try {
    return decodeMidgardUtxo({
      outRef: {
        txHash: input.transaction_id().to_hex(),
        outputIndex,
      },
      outRefCbor,
      outputCbor,
    });
  } catch (cause) {
    throw new ProviderPayloadError(
      endpoint,
      "utxo.outputCbor is not valid Midgard output CBOR",
      cause instanceof Error ? cause.message : String(cause),
    );
  }
};

const parseProtocolInfo = (
  payload: unknown,
  endpoint: string,
): MidgardProtocolInfo => {
  if (!isObject(payload)) {
    throw new ProviderPayloadError(endpoint, "protocol-info must be an object");
  }
  const protocolFeeParameters = payload.protocolFeeParameters;
  const submissionLimits = payload.submissionLimits;
  const validation = payload.validation;
  if (!isObject(protocolFeeParameters)) {
    throw new ProviderPayloadError(
      endpoint,
      "protocolFeeParameters must be an object",
    );
  }
  if (!isObject(submissionLimits)) {
    throw new ProviderPayloadError(
      endpoint,
      "submissionLimits must be an object",
    );
  }
  if (!isObject(validation)) {
    throw new ProviderPayloadError(endpoint, "validation must be an object");
  }
  if (validation.localValidationIsAuthoritative !== false) {
    throw new ProviderPayloadError(
      endpoint,
      "validation.localValidationIsAuthoritative must be false",
    );
  }
  return {
    apiVersion: requireNumber(payload.apiVersion, "apiVersion", endpoint),
    network: requireString(payload.network, "network", endpoint),
    midgardNativeTxVersion: requireNumber(
      payload.midgardNativeTxVersion,
      "midgardNativeTxVersion",
      endpoint,
    ),
    currentSlot: parseNonNegativeBigInt(
      payload.currentSlot,
      "currentSlot",
      endpoint,
    ),
    supportedScriptLanguages: validateSupportedScriptLanguages(
      payload.supportedScriptLanguages,
      endpoint,
    ),
    protocolFeeParameters: {
      minFeeA: parseNonNegativeBigInt(
        protocolFeeParameters.minFeeA,
        "protocolFeeParameters.minFeeA",
        endpoint,
      ),
      minFeeB: parseNonNegativeBigInt(
        protocolFeeParameters.minFeeB,
        "protocolFeeParameters.minFeeB",
        endpoint,
      ),
    },
    submissionLimits: {
      maxSubmitTxCborBytes: requireNumber(
        submissionLimits.maxSubmitTxCborBytes,
        "submissionLimits.maxSubmitTxCborBytes",
        endpoint,
      ),
    },
    validation: {
      strictnessProfile: requireString(
        validation.strictnessProfile,
        "validation.strictnessProfile",
        endpoint,
      ),
      localValidationIsAuthoritative: false,
    },
  };
};

const validateFallbackProtocolInfo = (
  protocolInfo: MidgardProtocolInfo,
  endpoint: string,
): MidgardProtocolInfo => {
  if (!isObject(protocolInfo)) {
    throw new ProviderPayloadError(
      endpoint,
      "fallback protocolInfo must be an object",
    );
  }
  if (
    typeof protocolInfo.apiVersion !== "number" ||
    !Number.isSafeInteger(protocolInfo.apiVersion) ||
    protocolInfo.apiVersion <= 0
  ) {
    throw new ProviderPayloadError(
      endpoint,
      "fallback apiVersion must be a positive safe integer",
    );
  }
  if (typeof protocolInfo.network !== "string") {
    throw new ProviderPayloadError(endpoint, "fallback network must be string");
  }
  if (
    typeof protocolInfo.midgardNativeTxVersion !== "number" ||
    !Number.isSafeInteger(protocolInfo.midgardNativeTxVersion) ||
    protocolInfo.midgardNativeTxVersion <= 0
  ) {
    throw new ProviderPayloadError(
      endpoint,
      "fallback midgardNativeTxVersion must be a positive safe integer",
    );
  }
  if (
    typeof protocolInfo.currentSlot !== "bigint" ||
    protocolInfo.currentSlot < 0n
  ) {
    throw new ProviderPayloadError(
      endpoint,
      "fallback currentSlot must be a non-negative bigint",
    );
  }
  const supportedScriptLanguages = validateSupportedScriptLanguages(
    protocolInfo.supportedScriptLanguages,
    endpoint,
    "fallback supportedScriptLanguages",
  );
  if (!isObject(protocolInfo.protocolFeeParameters)) {
    throw new ProviderPayloadError(
      endpoint,
      "fallback protocolFeeParameters must be an object",
    );
  }
  if (
    typeof protocolInfo.protocolFeeParameters.minFeeA !== "bigint" ||
    protocolInfo.protocolFeeParameters.minFeeA < 0n ||
    typeof protocolInfo.protocolFeeParameters.minFeeB !== "bigint" ||
    protocolInfo.protocolFeeParameters.minFeeB < 0n
  ) {
    throw new ProviderPayloadError(
      endpoint,
      "fallback fee parameters must be non-negative bigints",
    );
  }
  if (!isObject(protocolInfo.submissionLimits)) {
    throw new ProviderPayloadError(
      endpoint,
      "fallback submissionLimits must be an object",
    );
  }
  if (
    typeof protocolInfo.submissionLimits.maxSubmitTxCborBytes !== "number" ||
    !Number.isSafeInteger(protocolInfo.submissionLimits.maxSubmitTxCborBytes) ||
    protocolInfo.submissionLimits.maxSubmitTxCborBytes <= 0
  ) {
    throw new ProviderPayloadError(
      endpoint,
      "fallback maxSubmitTxCborBytes must be a positive safe integer",
    );
  }
  if (!isObject(protocolInfo.validation)) {
    throw new ProviderPayloadError(
      endpoint,
      "fallback validation must be an object",
    );
  }
  if (
    typeof protocolInfo.validation.strictnessProfile !== "string" ||
    protocolInfo.validation.localValidationIsAuthoritative !== false
  ) {
    throw new ProviderPayloadError(
      endpoint,
      "fallback validation facts are invalid",
    );
  }
  return {
    ...protocolInfo,
    supportedScriptLanguages,
  };
};

const parseTxStatus = (payload: unknown, endpoint: string): TxStatus => {
  if (!isObject(payload)) {
    throw new ProviderPayloadError(endpoint, "tx-status must be an object");
  }
  const txId = requireString(payload.txId, "txId", endpoint);
  const status = requireString(payload.status, "status", endpoint);
  if (status === "rejected") {
    const timestamps = payload.timestamps;
    const createdAt = isObject(timestamps)
      ? typeof timestamps.createdAt === "string"
        ? timestamps.createdAt
        : undefined
      : undefined;
    return {
      kind: "rejected",
      txId,
      code: requireString(payload.reasonCode, "reasonCode", endpoint),
      detail:
        typeof payload.reasonDetail === "string" ? payload.reasonDetail : null,
      createdAt,
    };
  }
  if (
    status === "committed" ||
    status === "accepted" ||
    status === "pending_commit" ||
    status === "awaiting_local_recovery" ||
    status === "validating" ||
    status === "queued" ||
    status === "not_found"
  ) {
    return { kind: status, txId };
  }
  throw new ProviderPayloadError(endpoint, `unsupported tx status ${status}`);
};

export class MidgardNodeProvider implements MidgardProvider {
  readonly endpoint: string;
  private readonly fetchImpl: MidgardFetch;
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
    this.fetchImpl = options.fetch ?? fetch;
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
    return {
      endpoint: redactEndpoint(this.endpoint),
      protocolInfoSource: this.protocolInfoSource,
      protocolInfoFallbackReason: this.protocolInfoFallbackReason,
    };
  }

  private url(path: string, params?: Record<string, string>): string {
    const url = new URL(`${this.endpoint}${path}`);
    for (const [key, value] of Object.entries(params ?? {})) {
      url.searchParams.set(key, value);
    }
    return url.toString();
  }

  private async requestJson(
    path: string,
    init?: RequestInit,
    params?: Record<string, string>,
  ): Promise<{
    readonly response: Response;
    readonly payload: unknown;
    readonly bodyText: string;
  }> {
    const endpoint = `${path}${params === undefined ? "" : `?${new URLSearchParams(params).toString()}`}`;
    let response: Response;
    try {
      response = await this.fetchImpl(this.url(path, params), init);
    } catch (cause) {
      throw new ProviderTransportError(endpoint, cause);
    }
    let bodyText: string;
    try {
      bodyText = await response.text();
    } catch (cause) {
      throw new ProviderPayloadError(
        endpoint,
        "response body could not be read",
        cause instanceof Error ? cause.message : String(cause),
      );
    }

    if (bodyText.trim().length === 0) {
      return { response, payload: undefined, bodyText };
    }

    try {
      return { response, payload: JSON.parse(bodyText) as unknown, bodyText };
    } catch (cause) {
      if (!response.ok) {
        return {
          response,
          payload: { rawBody: bodyText },
          bodyText,
        };
      }
      throw new ProviderPayloadError(
        endpoint,
        "response body must be JSON",
        cause instanceof Error ? cause.message : String(cause),
      );
    }
  }

  async getProtocolInfo(): Promise<MidgardProtocolInfo> {
    const endpoint = "/protocol-info";
    const { response, payload } = await this.requestJson(endpoint);
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
    const info = await this.getProtocolInfo();
    return {
      apiVersion: info.apiVersion,
      network: info.network,
      midgardNativeTxVersion: info.midgardNativeTxVersion,
      currentSlot: info.currentSlot,
      supportedScriptLanguages: cloneSupportedScriptLanguages(
        info.supportedScriptLanguages,
      ),
      minFeeA: info.protocolFeeParameters.minFeeA,
      minFeeB: info.protocolFeeParameters.minFeeB,
      networkId: knownNetworkId(info.network),
      maxSubmitTxCborBytes: info.submissionLimits.maxSubmitTxCborBytes,
      strictnessProfile: info.validation.strictnessProfile,
    };
  }

  async getCurrentSlot(): Promise<bigint> {
    return (await this.getProtocolInfo()).currentSlot;
  }

  async getUtxos(address: Address): Promise<readonly MidgardUtxo[]> {
    const endpoint = "/utxos";
    let requestedAddress: string;
    try {
      requestedAddress = midgardAddressToText(midgardAddressFromText(address));
    } catch (cause) {
      throw new ProviderPayloadError(
        endpoint,
        "address must be a valid Midgard bech32 address",
        cause instanceof Error ? cause.message : String(cause),
      );
    }
    const { response, payload } = await this.requestJson(endpoint, undefined, {
      address: requestedAddress,
    });
    if (!response.ok) {
      throw new ProviderHttpError({
        endpoint,
        statusCode: response.status,
        message: "GET /utxos failed",
        detail: JSON.stringify(payload ?? null),
      });
    }
    if (!isObject(payload) || !Array.isArray(payload.utxos)) {
      throw new ProviderPayloadError(
        endpoint,
        "GET /utxos response must contain an utxos array",
      );
    }
    return payload.utxos.map((entry) => {
      const decoded = decodeEncodedUtxo(entry, endpoint);
      if (utxoAddress(decoded) !== requestedAddress) {
        throw new ProviderPayloadError(
          endpoint,
          "GET /utxos returned an output for a different address",
        );
      }
      return decoded;
    });
  }

  async getUtxoByOutRef(outRef: OutRef): Promise<MidgardUtxo | undefined> {
    const endpoint = "/utxo";
    const normalized = normalizeOutRef(outRef);
    const { response, payload } = await this.requestJson(endpoint, undefined, {
      txOutRef: outRefToCbor(normalized).toString("hex"),
    });
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
    if (!isObject(payload) || payload.utxo === undefined) {
      throw new ProviderPayloadError(
        endpoint,
        "GET /utxo response must contain utxo",
      );
    }
    const decoded = decodeEncodedUtxo(payload.utxo, endpoint);
    if (
      decoded.txHash !== normalized.txHash ||
      decoded.outputIndex !== normalized.outputIndex
    ) {
      throw new ProviderPayloadError(
        endpoint,
        "GET /utxo returned a different outref than requested",
      );
    }
    return decoded;
  }

  async getUtxosByOutRefs(
    outRefs: readonly OutRef[],
  ): Promise<readonly MidgardUtxo[]> {
    const endpoint = "/utxos?by-outrefs";
    const requestedLabels = new Set(
      outRefs.map((outRef) => outRefLabel(outRef)),
    );
    if (requestedLabels.size !== outRefs.length) {
      throw new ProviderPayloadError(endpoint, "duplicate requested outref");
    }
    const { response, payload } = await this.requestJson(
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
    if (!isObject(payload) || !Array.isArray(payload.utxos)) {
      throw new ProviderPayloadError(
        endpoint,
        "POST /utxos?by-outrefs response must contain an utxos array",
      );
    }
    const seen = new Set<string>();
    return payload.utxos.map((entry) => {
      const decoded = decodeEncodedUtxo(entry, endpoint);
      const label = outRefLabel(decoded);
      if (!requestedLabels.has(label)) {
        throw new ProviderPayloadError(
          endpoint,
          `POST /utxos?by-outrefs returned unrequested outref ${label}`,
        );
      }
      if (seen.has(label)) {
        throw new ProviderPayloadError(
          endpoint,
          `POST /utxos?by-outrefs returned duplicate outref ${label}`,
        );
      }
      seen.add(label);
      return decoded;
    });
  }

  async submitTx(txCborHex: string): Promise<SubmitTxResult> {
    const endpoint = "/submit";
    const protocolInfo = await this.getProtocolInfo();
    const normalizedTxCborHex = normalizeSubmitTxCborHex(
      txCborHex,
      endpoint,
      protocolInfo.submissionLimits.maxSubmitTxCborBytes,
    );
    const { response, payload } = await this.requestJson(endpoint, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({ tx_cbor: normalizedTxCborHex }),
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
    if (!isObject(payload)) {
      throw new ProviderPayloadError(
        endpoint,
        "submit response must be object",
      );
    }
    const duplicate =
      typeof payload.duplicate === "boolean" ? payload.duplicate : undefined;
    if (duplicate === undefined) {
      throw new ProviderPayloadError(
        endpoint,
        "submit response must contain duplicate boolean",
      );
    }
    if (response.status === 202 && duplicate) {
      throw new ProviderPayloadError(
        endpoint,
        "new submit admission cannot be marked duplicate",
      );
    }
    if (response.status === 200 && !duplicate) {
      throw new ProviderPayloadError(
        endpoint,
        "duplicate submit admission must be marked duplicate",
      );
    }
    const status = requireString(payload.status, "status", endpoint);
    if (!isSubmitAdmissionStatus(status)) {
      throw new ProviderPayloadError(
        endpoint,
        "submit response status is not a supported durable admission status",
        status,
      );
    }
    if (response.status === 202 && status !== "queued") {
      throw new ProviderPayloadError(
        endpoint,
        "new submit admission must start queued",
      );
    }
    if (
      payload.firstSeenAt !== undefined &&
      typeof payload.firstSeenAt !== "string"
    ) {
      throw new ProviderPayloadError(
        endpoint,
        "submit response firstSeenAt must be string when present",
      );
    }
    if (
      payload.lastSeenAt !== undefined &&
      typeof payload.lastSeenAt !== "string"
    ) {
      throw new ProviderPayloadError(
        endpoint,
        "submit response lastSeenAt must be string when present",
      );
    }
    return {
      txId: normalizeTxIdHex(
        requireString(payload.txId, "txId", endpoint),
        endpoint,
      ),
      status,
      httpStatus: response.status,
      firstSeenAt: payload.firstSeenAt,
      lastSeenAt: payload.lastSeenAt,
      duplicate,
    };
  }

  async getTxStatus(txId: string): Promise<TxStatus> {
    const endpoint = "/tx-status";
    const requestedTxId = normalizeTxIdHex(txId, endpoint);
    const { response, payload } = await this.requestJson(endpoint, undefined, {
      tx_hash: requestedTxId,
    });
    if (
      response.status === 404 &&
      isObject(payload) &&
      payload.status === "not_found"
    ) {
      const status = parseTxStatus(payload, endpoint);
      if (status.txId !== requestedTxId) {
        throw new ProviderPayloadError(
          endpoint,
          "GET /tx-status returned a different tx id than requested",
        );
      }
      return status;
    }
    if (!response.ok) {
      if (
        response.status === 404 ||
        response.status === 405 ||
        response.status === 501
      ) {
        throw new ProviderCapabilityError(
          endpoint,
          "GET /tx-status unavailable",
        );
      }
      throw new ProviderHttpError({
        endpoint,
        statusCode: response.status,
        message: "GET /tx-status failed",
        detail: JSON.stringify(payload ?? null),
      });
    }
    const status = parseTxStatus(payload, endpoint);
    if (status.txId !== requestedTxId) {
      throw new ProviderPayloadError(
        endpoint,
        "GET /tx-status returned a different tx id than requested",
      );
    }
    return status;
  }
}
