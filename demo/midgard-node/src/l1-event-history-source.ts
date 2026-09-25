import { createHash } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import { Data, type Network, toUnit } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import JSONBig from "json-bigint";

import {
  followEventHistoryChain,
  type HistoryChainBlock,
} from "./l1-event-history-chain.js";
import {
  decodeEventHistoryLedgerSnapshot,
  type NodeEventHistoryDeployments,
} from "./l1-event-history-snapshot.js";
import {
  decodeHistoryChainTransaction,
  type HistoryChainTransaction,
} from "./l1-event-history-transaction.js";
import {
  type AcquiredLedgerSnapshot,
  readAcquiredLedgerSnapshot,
} from "./l1-ledger-snapshot.js";
import {
  normalizeOgmiosWebSocketUrl,
  openOgmiosSession,
  type WebSocketFactory,
  type WebSocketLike,
} from "./l1-tx-order-carriage.js";
import { ogmiosEndpointIdentitySha256 } from "./local-ledger-slot.js";
import type { ContractDeploymentIdentityValue } from "./services/midgard-contracts.js";

export const HISTORY_GENESIS_DIGEST_ALGORITHM =
  "ogmios-shelley-result-lossless-v1";
const lossless = JSONBig({ useNativeBigInt: true, strict: true });
const sha256 = (value: string) =>
  createHash("sha256").update(value).digest("hex");
const sourceError = (cause: unknown): Error =>
  cause instanceof Error
    ? cause
    : new Error("History source verification failed", { cause });
const requireDigest = (value: string) => {
  if (!/^[0-9a-f]{64}$/u.test(value))
    throw new Error("History source requires an approved lowercase SHA256 pin");
};

/** The Shelley response uses integer JSON quantities and string ratios. Reject
 * already-rounded Number values rather than assigning them an exact identity.
 * Bigints are serialized as integer tokens, distinctly from JSON strings. */
const canonicalValue = (value: unknown): unknown => {
  if (
    value === null ||
    typeof value === "string" ||
    typeof value === "boolean" ||
    typeof value === "bigint"
  )
    return value;
  if (typeof value === "number" && Number.isSafeInteger(value)) return value;
  if (Array.isArray(value)) return value.map(canonicalValue);
  if (
    typeof value === "object" &&
    value !== null &&
    (Object.getPrototypeOf(value) === Object.prototype ||
      Object.getPrototypeOf(value) === null)
  )
    return Object.fromEntries(
      Object.keys(value)
        .sort()
        .map((key) => [
          key,
          canonicalValue((value as Record<string, unknown>)[key]),
        ]),
    );
  throw new Error(
    "History source identity requires losslessly decoded JSON values",
  );
};
export const eventHistoryCanonicalJson = (value: unknown): string =>
  lossless.stringify(canonicalValue(value));

/** Hash only the losslessly decoded Shelley query RESULT, not its RPC envelope
 * or genesis-file bytes. This deliberately does not reuse configurationSha256
 * from local-ledger-slot, whose existing slot artifacts use native JSON numbers. */
export const eventHistoryGenesisLosslessSha256 = (result: unknown): string => {
  if (typeof result !== "object" || result === null || Array.isArray(result))
    throw new Error("History source genesis result must be an object");
  return sha256(eventHistoryCanonicalJson(result));
};

export type EventHistorySourceBinding = Readonly<{
  digest: string;
  manifestId: string;
  network: Network;
  endpointIdentitySha256: string;
  genesisAlgorithm: typeof HISTORY_GENESIS_DIGEST_ALGORITHM;
  genesisSha256: string;
  hubAddress: string;
  hubUnit: string;
  hubDatumCbor: string;
  deployments: NodeEventHistoryDeployments;
}>;

/** Inputs come from the shared, parser-admitted MidgardContractServices layer.
 * A derived scaffold, a missing operator-approved pin, or a network mismatch
 * cannot acquire the production history authority under this binding. */
export const makeEventHistorySourceBinding = (input: {
  readonly contracts: SDK.MidgardValidators;
  readonly identity: ContractDeploymentIdentityValue;
  readonly network: Network;
  readonly ogmiosUrl: string;
  readonly expectedGenesisLosslessSha256: string;
}) =>
  Effect.gen(function* () {
    const admitted = yield* Effect.try({
      try: () => {
        const { identity } = input;
        if (
          identity.kind !== "manifest" ||
          identity.manifest === undefined ||
          identity.manifestId === undefined ||
          identity.manifestId !== identity.manifest.manifestId
        )
          throw new Error(
            "History source requires one admitted deployment manifest",
          );
        if (identity.manifest.network !== input.network)
          throw new Error(
            "History source network differs from the deployment manifest",
          );
        requireDigest(identity.manifestId);
        requireDigest(input.expectedGenesisLosslessSha256);
        const pair = SDK.requireEventHistoryContracts(input.contracts);
        for (const kind of ["deposit", "withdrawal"] as const) {
          if (
            pair[kind].recipe.hubPolicyId !==
              input.contracts.hubOracle.policyId ||
            pair[kind].list.policyId !== input.contracts[kind].policyId ||
            pair[kind].list.spendingScriptAddress !==
              input.contracts[kind].spendingScriptAddress
          )
            throw new Error(
              "History source contract views disagree on their deployment",
            );
        }
        return {
          manifestId: identity.manifestId,
          network: input.network,
          endpointIdentitySha256: ogmiosEndpointIdentitySha256(input.ogmiosUrl),
          genesisAlgorithm: HISTORY_GENESIS_DIGEST_ALGORITHM,
          genesisSha256: input.expectedGenesisLosslessSha256,
          hubAddress: input.contracts.hubOracle.spendingScriptAddress,
          hubUnit: toUnit(
            input.contracts.hubOracle.policyId,
            SDK.HUB_ORACLE_ASSET_NAME,
          ),
          deployments: Object.freeze({
            deposit: Object.freeze(
              SDK.eventHistoryDeploymentFromContracts(pair.deposit),
            ),
            withdrawal: Object.freeze(
              SDK.eventHistoryDeploymentFromContracts(pair.withdrawal),
            ),
          }),
        } satisfies Omit<EventHistorySourceBinding, "digest" | "hubDatumCbor">;
      },
      catch: sourceError,
    });
    const hubDatum = yield* SDK.makeHubOracleDatum(input.contracts);
    const value = {
      ...admitted,
      hubDatumCbor: Data.to(hubDatum, SDK.HubOracleDatum),
    };
    return Object.freeze({
      ...value,
      digest: sha256(
        eventHistoryCanonicalJson({
          domain: "midgard-node-history-source-v1",
          ...value,
        }),
      ),
    }) satisfies EventHistorySourceBinding;
  });

/** Must run on EACH exact socket before its capture or ChainSync observations
 * are admitted. Its request implementation must use the lossless JSON parser.
 * A separate HTTP preflight or equal endpoint URL is not this receipt. */
export const authenticateEventHistorySession = async (
  session: {
    readonly request: (
      method: string,
      params: Record<string, unknown>,
    ) => Promise<unknown>;
  },
  binding: EventHistorySourceBinding,
): Promise<Readonly<{ bindingDigest: string; genesisSha256: string }>> => {
  requireDigest(binding.genesisSha256);
  if (binding.genesisAlgorithm !== HISTORY_GENESIS_DIGEST_ALGORITHM)
    throw new Error("Unsupported history source genesis digest algorithm");
  const observed = eventHistoryGenesisLosslessSha256(
    await session.request("queryNetwork/genesisConfiguration", {
      era: "shelley",
    }),
  );
  if (observed !== binding.genesisSha256)
    throw new Error("History source genesis does not match the approved pin");
  return Object.freeze({
    bindingDigest: binding.digest,
    genesisSha256: observed,
  });
};

/** Strictly validate every hub-policy-bearing output at the captured hub
 * address. Generic SDK list helpers intentionally filter invalid outputs and
 * cannot establish this exact singleton deployment binding. */
export const verifyEventHistoryHubOutputs = (
  outputs: AcquiredLedgerSnapshot["outputs"],
  binding: Pick<
    EventHistorySourceBinding,
    "hubAddress" | "hubUnit" | "hubDatumCbor"
  >,
) => {
  const policy = binding.hubUnit.slice(0, 56);
  const hubs = outputs.filter(
    (output) =>
      output.address === binding.hubAddress &&
      Object.keys(output.assets).some((unit) => unit.startsWith(policy)),
  );
  if (hubs.length !== 1)
    throw new Error("History capture requires exactly one authenticated hub");
  const hub = hubs[0]!;
  if (
    hub.assets[binding.hubUnit] !== 1n ||
    (hub.assets.lovelace ?? 0n) <= 0n ||
    Object.keys(hub.assets).some(
      (unit) => unit !== "lovelace" && unit !== binding.hubUnit,
    ) ||
    hub.datum === undefined ||
    hub.datumHash !== undefined ||
    hub.hasReferenceScript
  )
    throw new Error("History capture has a malformed authenticated hub");
  const decoded = Data.from(hub.datum, SDK.HubOracleDatum);
  if (Data.to(decoded, SDK.HubOracleDatum) !== binding.hubDatumCbor)
    throw new Error("History capture hub datum differs from its deployment");
  return hub;
};

/** A complete capture must also include the hub in its declared scope. */
export const verifyEventHistoryCaptureHub = (
  ledger: AcquiredLedgerSnapshot,
  binding: Parameters<typeof verifyEventHistoryHubOutputs>[1],
) => {
  if (!ledger.addresses.includes(binding.hubAddress))
    throw new Error("History capture omitted its deployment hub address");
  return verifyEventHistoryHubOutputs(ledger.outputs, binding);
};

/** This receipt binds one captured deployment and both lists. Canonical point,
 * origin/retirement provenance and current generation remain owner obligations. */
export const decodeBoundEventHistoryLedgerSnapshot = (
  ledger: AcquiredLedgerSnapshot,
  binding: EventHistorySourceBinding,
) =>
  Effect.gen(function* () {
    const hub = yield* Effect.try({
      try: () => verifyEventHistoryCaptureHub(ledger, binding),
      catch: sourceError,
    });
    const history = yield* decodeEventHistoryLedgerSnapshot(
      ledger,
      binding.deployments,
    );
    const snapshotDigest = sha256(
      eventHistoryCanonicalJson({
        domain: "midgard-node-history-capture-v1",
        bindingDigest: binding.digest,
        point: ledger.point,
        addresses: [...ledger.addresses].sort(),
        outputs: [...ledger.outputs].sort(
          (a, b) =>
            a.txHash.localeCompare(b.txHash) || a.outputIndex - b.outputIndex,
        ),
      }),
    );
    return Object.freeze({
      history,
      hub,
      bindingDigest: binding.digest,
      snapshotDigest,
    });
  });

const requireBoundEndpoint = (
  ogmiosUrl: string,
  binding: EventHistorySourceBinding,
) => {
  if (
    ogmiosEndpointIdentitySha256(ogmiosUrl) !== binding.endpointIdentitySha256
  )
    throw new Error(
      "History source endpoint differs from its approved binding",
    );
};

/** One exact-socket authenticated capture of the complete five-address scope.
 * This entry point permits pre-initialization state. It does not assert that
 * the deployment is activated, that the point remains canonical, or that the
 * capture can be used as an origin without whole-block initialization replay.
 * A requested historical point must still be available to local-state-query.
 */
export const readBoundEventHistoryRawLedgerSnapshot = async ({
  binding,
  ...options
}: Omit<
  Parameters<typeof readAcquiredLedgerSnapshot>[0],
  "addresses" | "verifySession"
> & {
  readonly binding: EventHistorySourceBinding;
}) => {
  requireBoundEndpoint(options.ogmiosUrl, binding);
  const ledger = await readAcquiredLedgerSnapshot({
    ...options,
    addresses: [
      binding.hubAddress,
      ...Object.values(binding.deployments).flatMap((deployment) => [
        deployment.address,
        deployment.retentionAddress,
      ]),
    ],
    verifySession: async (session) => {
      await authenticateEventHistorySession(session, binding);
    },
  });
  options.signal?.throwIfAborted();
  return Object.freeze({ ledger, bindingDigest: binding.digest });
};

/** Each invocation authenticates its actual capture socket, then reads the hub
 * and both complete lists at one acquired point. The receipt is still subject
 * to the owner's canonical-path and generation checks before publication. */
export const readBoundEventHistoryLedgerSnapshot = async (
  options: Parameters<typeof readBoundEventHistoryRawLedgerSnapshot>[0],
) => {
  const { ledger } = await readBoundEventHistoryRawLedgerSnapshot(options);
  const snapshot = await Effect.runPromise(
    decodeBoundEventHistoryLedgerSnapshot(ledger, options.binding),
    { signal: options.signal },
  );
  options.signal?.throwIfAborted();
  return snapshot;
};

/** Source liveness only: one exact-socket genesis authentication and one
 * network tip read. Never Kupo and never a cached receipt. This is a constant
 * cost query, never a ledger-state scan, and establishes no branch or capture.
 */
export const readBoundEventHistoryNetworkTip = async ({
  binding,
  ogmiosUrl,
  timeoutMs,
  signal,
  webSocketFactory = (url) => new WebSocket(url) as unknown as WebSocketLike,
}: {
  readonly binding: EventHistorySourceBinding;
  readonly ogmiosUrl: string;
  readonly timeoutMs: number;
  readonly signal: AbortSignal;
  readonly webSocketFactory?: WebSocketFactory;
}): Promise<unknown> => {
  requireBoundEndpoint(ogmiosUrl, binding);
  const session = await openOgmiosSession({
    url: normalizeOgmiosWebSocketUrl(ogmiosUrl),
    timeoutMs,
    webSocketFactory,
    signal: AbortSignal.any([signal, AbortSignal.timeout(timeoutMs)]),
    parseMessage: (text) => lossless.parse(text) as unknown,
  });
  try {
    await authenticateEventHistorySession(session, binding);
    const tip = await session.request("queryNetwork/tip", {});
    signal.throwIfAborted();
    return tip;
  } finally {
    session.close();
  }
};

/** Additional recovery state at the exact canonical checkpoint selected by the
 * source owner. Authenticate this socket and the deployed hub again; neither a
 * provider's current queue scan nor absence from that queue proves noninclusion.
 * The caller validates the requested protocol outputs and checks its source
 * generation after this read, before using the captured state for recovery.
 */
export const readBoundRecoveryLedgerSnapshot = async ({
  binding,
  addresses,
  ...options
}: Omit<
  Parameters<typeof readAcquiredLedgerSnapshot>[0],
  "addresses" | "verifySession" | "at"
> & {
  readonly binding: EventHistorySourceBinding;
  readonly at: AcquiredLedgerSnapshot["point"];
  readonly addresses: readonly string[];
}) => {
  requireBoundEndpoint(options.ogmiosUrl, binding);
  const ledger = await readAcquiredLedgerSnapshot({
    ...options,
    addresses: [binding.hubAddress, ...addresses],
    verifySession: async (session) => {
      await authenticateEventHistorySession(session, binding);
    },
  });
  options.signal?.throwIfAborted();
  verifyEventHistoryCaptureHub(ledger, binding);
  return Object.freeze({ ledger, bindingDigest: binding.digest });
};

export type BoundHistoryChainBlock = Readonly<{
  point: HistoryChainBlock["point"];
  parent: string;
  transactions: readonly HistoryChainTransaction[];
}>;

/** Reconnection always performs a fresh genesis handshake. Equal URL routing
 * does not allow a receipt from a previous capture or follower to be reused. */
export const followBoundEventHistoryChain = async ({
  binding,
  ...options
}: Omit<
  Parameters<typeof followEventHistoryChain>[0],
  "verifySession" | "onForward"
> & {
  readonly binding: EventHistorySourceBinding;
  readonly onForward: (block: BoundHistoryChainBlock) => void | Promise<void>;
}): Promise<void> => {
  try {
    requireBoundEndpoint(options.ogmiosUrl, binding);
  } catch (cause) {
    options.onUnavailable(cause);
    throw cause;
  }
  return followEventHistoryChain({
    ...options,
    onForward: ({ point, parent, body }) => {
      if (!Array.isArray(body.transactions))
        throw new Error("History block omitted its complete transaction array");
      // Decode the whole block before admitting any observation. A malformed
      // later transaction cannot leave an owner with a partially applied block.
      const transactions = body.transactions.map(decodeHistoryChainTransaction);
      if (
        new Set(transactions.map((transaction) => transaction.txHash)).size !==
        transactions.length
      )
        throw new Error("History block repeats a transaction identity");
      return options.onForward(
        Object.freeze({
          point,
          parent,
          transactions: Object.freeze(transactions),
        }),
      );
    },
    verifySession: async (session) => {
      await authenticateEventHistorySession(session, binding);
    },
  });
};
