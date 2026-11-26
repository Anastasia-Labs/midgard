import {
  ProviderCapabilityError,
  ProviderHttpError,
  ProviderPayloadError,
} from "../core/errors.js";
import { isObject } from "./payload.validate-supported-script-languages.js";
import type { MidgardNodeTransport } from "./transport.js";
import type {
  MidgardBlocksPage,
  MidgardBlocksQueryOptions,
  MidgardBlockSummary,
} from "./types.js";

const BLOCKS_ENDPOINT = "/blocks";

export const buildBlocksQuery = (
  options: MidgardBlocksQueryOptions | undefined,
  endpoint: string = BLOCKS_ENDPOINT,
): Record<string, string> | undefined => {
  if (options === undefined) return undefined;
  const query: Record<string, string> = {};
  if (options.limit !== undefined) {
    if (!Number.isSafeInteger(options.limit) || options.limit <= 0) {
      throw new ProviderPayloadError(
        endpoint,
        "limit must be a positive safe integer",
        `limit=${String(options.limit)}`,
      );
    }
    query.limit = options.limit.toString();
  }
  if (options.cursor !== undefined) {
    const trimmed = options.cursor.trim();
    if (trimmed.length === 0) {
      throw new ProviderPayloadError(
        endpoint,
        "cursor must be a non-empty string",
      );
    }
    query.cursor = trimmed;
  }
  return Object.keys(query).length === 0 ? undefined : query;
};

export const parseBlocksPage = (
  payload: unknown,
  endpoint: string = BLOCKS_ENDPOINT,
): MidgardBlocksPage => {
  const invalid = (detail: string) =>
    new ProviderPayloadError(endpoint, "Invalid /blocks payload", detail);
  if (!isObject(payload) || !Array.isArray(payload.blocks)) {
    throw invalid("expected { blocks: [...], nextCursor: string | null }");
  }
  if (payload.nextCursor !== null && typeof payload.nextCursor !== "string") {
    throw invalid("nextCursor must be a string or null");
  }
  const blocks: MidgardBlockSummary[] = payload.blocks.map(
    (entry: unknown, index) => {
      if (!isObject(entry)) {
        throw invalid(`blocks[${index.toString()}] must be an object`);
      }
      return { ...entry };
    },
  );
  return {
    blocks,
    nextCursor: payload.nextCursor,
  };
};

export const fetchBlocksPage = async (
  transport: MidgardNodeTransport,
  options?: MidgardBlocksQueryOptions,
): Promise<MidgardBlocksPage> => {
  const query = buildBlocksQuery(options, BLOCKS_ENDPOINT);
  const { response, payload } = await transport.requestJson(
    BLOCKS_ENDPOINT,
    undefined,
    query,
  );
  if (response.ok) {
    return parseBlocksPage(payload, BLOCKS_ENDPOINT);
  }
  if ([404, 405, 501].includes(response.status)) {
    throw new ProviderCapabilityError(
      BLOCKS_ENDPOINT,
      "GET /blocks unavailable",
    );
  }
  throw new ProviderHttpError({
    endpoint: BLOCKS_ENDPOINT,
    statusCode: response.status,
    message: "GET /blocks failed",
    detail: JSON.stringify(payload ?? null),
    retryable: response.status === 503 || response.status >= 500,
  });
};
