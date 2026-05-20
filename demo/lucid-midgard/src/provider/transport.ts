import {
  ProviderPayloadError,
  ProviderTransportError,
} from "../core/errors.js";
import type { MidgardFetch } from "./types.js";

export type ProviderJsonResponse = {
  readonly response: Response;
  readonly payload: unknown;
  readonly bodyText: string;
};

export const unavailableProtocolInfoStatuses: ReadonlySet<number> = new Set([
  404, 405, 501,
]);

const requestEndpointLabel = (
  path: string,
  params?: Record<string, string>,
): string =>
  `${path}${params === undefined ? "" : `?${new URLSearchParams(params).toString()}`}`;

export class MidgardNodeTransport {
  constructor(
    private readonly endpoint: string,
    private readonly fetchImpl: MidgardFetch,
  ) {}

  private url(path: string, params?: Record<string, string>): string {
    const url = new URL(`${this.endpoint}${path}`);
    for (const [key, value] of Object.entries(params ?? {})) {
      url.searchParams.set(key, value);
    }
    return url.toString();
  }

  async requestJson(
    path: string,
    init?: RequestInit,
    params?: Record<string, string>,
  ): Promise<ProviderJsonResponse> {
    const endpoint = requestEndpointLabel(path, params);
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
}
