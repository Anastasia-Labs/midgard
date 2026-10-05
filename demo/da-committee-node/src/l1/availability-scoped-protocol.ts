import { createHash } from "node:crypto";

import {
  type CanonicalJsonValue,
  canonicalJsonValue,
  compareCanonicalJsonKeys,
} from "@al-ft/midgard-core/canonical-json";
import { isPlainRecord, isUnknownArray } from "@al-ft/midgard-core/narrowing";
import type { DaAvailabilityReadScope } from "@al-ft/midgard-sdk";

import {
  committeeScopedFetch,
  type CommitteeSourceReadLimits,
} from "../availability/scoped-transports.js";
import { getRecord } from "./provider.parse-persisted-chain-sync-state.js";

/** Native parameter JSON also carries finite decimal coefficients, e.g. 1.2. */
const protocolJsonValue = (value: unknown): CanonicalJsonValue => {
  const subject = "native protocol parameters";
  if (typeof value === "number") {
    if (
      !Number.isFinite(value) ||
      (Number.isInteger(value) && !Number.isSafeInteger(value))
    )
      throw new TypeError(
        `${subject} numbers must be finite and integers must be safe`,
      );
    return value;
  }
  if (isUnknownArray(value)) {
    if (Object.keys(value).length !== value.length)
      throw new TypeError(`${subject} arrays must be dense`);
    return value.map(protocolJsonValue);
  }
  if (isPlainRecord(value))
    return Object.fromEntries(
      Object.entries(value)
        .sort(([left], [right]) => compareCanonicalJsonKeys(left, right))
        .map(([key, child]) => [key, protocolJsonValue(child)]),
    );
  return canonicalJsonValue(value, subject);
};

/** Fresh native parameters, never Lucid's cached startup configuration. */
export const committeeScopedProtocolDigest =
  (input: {
    ogmiosUrl: string;
    limits: CommitteeSourceReadLimits;
    fetchImpl?: typeof fetch;
  }) =>
  async (scope: DaAvailabilityReadScope): Promise<string> => {
    const endpoint = new URL(input.ogmiosUrl);
    if (endpoint.protocol === "ws:") endpoint.protocol = "http:";
    else if (endpoint.protocol === "wss:") endpoint.protocol = "https:";
    if (endpoint.protocol !== "http:" && endpoint.protocol !== "https:")
      throw new Error("Invalid protocol parameter endpoint");
    const response = await committeeScopedFetch(
      scope,
      input.limits,
      input.fetchImpl,
    )(endpoint, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({
        jsonrpc: "2.0",
        method: "queryLedgerState/protocolParameters",
        params: {},
        id: "committee-promise-protocol",
      }),
    });
    if (!response.ok)
      throw new Error(`Protocol parameter read failed: ${response.status}`);
    const reply = getRecord(
      await response.json(),
      "Ogmios protocol parameter reply",
    );
    if (
      reply.id !== "committee-promise-protocol" ||
      reply.jsonrpc !== "2.0" ||
      reply.error !== undefined
    )
      throw new Error(
        "Ogmios protocol parameter reply is not the requested successful result",
      );
    const parameters = getRecord(reply.result, "Ogmios protocol parameters");
    if (Object.keys(parameters).length === 0)
      throw new Error("Ogmios protocol parameters are empty");
    scope.assertCurrent();
    return createHash("sha256")
      .update(JSON.stringify(protocolJsonValue(parameters)))
      .digest("hex");
  };
