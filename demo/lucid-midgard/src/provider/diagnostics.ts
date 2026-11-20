import { ProviderPayloadError } from "../core/errors.js";
import type { ProviderDiagnostics } from "./types.js";

export const trimTrailingSlash = (endpoint: string): string => {
  const normalized = endpoint.trim();
  if (normalized.length === 0) {
    throw new ProviderPayloadError("constructor", "Provider endpoint is empty");
  }
  return normalized.replace(/\/+$/, "");
};

export const redactEndpoint = (endpoint: string): string => {
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

export const providerDiagnostics = ({
  endpoint,
  protocolInfoSource,
  protocolInfoFallbackReason,
}: {
  readonly endpoint: string;
  readonly protocolInfoSource: ProviderDiagnostics["protocolInfoSource"];
  readonly protocolInfoFallbackReason?: string;
}): ProviderDiagnostics => ({
  endpoint: redactEndpoint(endpoint),
  protocolInfoSource,
  protocolInfoFallbackReason,
});
