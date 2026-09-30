import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import {
  computeDaSha256Hash,
  DaRequestResponseProtocol,
} from "@al-ft/midgard-core/da-transport";
import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  DA_PAYLOAD_VERSION,
  type DaPayload,
  EMPTY_MERKLE_TREE_ROOT,
  encodeDaPayload,
} from "@al-ft/midgard-sdk";

import type { WatcherConfig } from "../../src/runtime/config.js";
import { WATCHER_CONFIG_SCHEMA_VERSION } from "../../src/runtime/config.js";
import type { VerifiedWatcherDeploymentIdentity } from "../../src/runtime/deployment-identity.js";
import {
  type WatcherPublicDaClock,
  type WatcherPublicDaLibp2pTransportV1,
  type WatcherPublicDaRequest,
} from "../../src/storage/public-da-client.js";

// ---------------------------------------------------------------------------
// Fixtures
// ---------------------------------------------------------------------------

export const repeatHex = (value: number, length: number): string =>
  value.toString(16).padStart(2, "0").repeat(length);

export const FINGERPRINT = repeatHex(0x1a, 32);

export const OTHER_FINGERPRINT = repeatHex(0x2b, 32);

export const HEADER_HASH = repeatHex(0xab, 28);

export const OTHER_HEADER_HASH = repeatHex(0xcd, 28);

export const PEERS = [
  "da-peer-a",
  "da-peer-b",
  "da-peer-c",
  "da-peer-d",
  "da-peer-e",
];

// Peer ids must stay inside the base58 alphabet the watcher config enforces.
const PEER_ID_SUFFIX = ["A", "B", "C", "D", "E"];

export const multiaddrFor = (index: number): string =>
  `/dns4/da-${String.fromCharCode(97 + index)}.example/tcp/443/p2p/12D3KooWAbcdefghijkmnopqrstuvwxyz1234${PEER_ID_SUFFIX[index]!}`;

export const rawConfig = (options?: {
  readonly peerCount?: number;
  readonly maxConcurrency?: number;
  readonly requestTimeoutMs?: number;
  readonly daFetchMs?: number;
  readonly targetNetwork?: string;
}): Record<string, unknown> => ({
  schemaVersion: WATCHER_CONFIG_SCHEMA_VERSION,
  mode: "acceptance",
  targetNetwork: options?.targetNetwork ?? "Preprod",
  l1: {
    source: {
      sourceMode: "external_providers",
      providers: [
        {
          identity: "provider-a",
          operatorIdentitySha256: repeatHex(0x11, 32),
          endpoint: "https://cardano-a.example",
        },
        {
          identity: "provider-b",
          operatorIdentitySha256: repeatHex(0x22, 32),
          endpoint: "https://cardano-b.example",
        },
      ],
    },
    requestTimeoutMs: 10_000,
    maxConcurrency: 8,
    finality: {
      depth: 15,
      rollback: {
        beforeFinality: "rewind",
        afterFinality: "quarantine",
        maxDepth: 15,
      },
    },
  },
  da: {
    peers: Array.from({ length: options?.peerCount ?? 1 }, (_, index) => ({
      identity: PEERS[index],
      multiaddr: multiaddrFor(index),
    })),
    requestTimeoutMs: options?.requestTimeoutMs ?? 10_000,
    maxConcurrency: options?.maxConcurrency ?? 8,
  },
  storage: {
    driver: "sqlite",
    path: "/var/lib/midgard-watcher/watcher.sqlite",
    rollbackAuthorityKeySource: {
      kind: "environment",
      variable: "MIDGARD_WATCHER_ROLLBACK_AUTHORITY_KEY",
    },
  },
  proverWallet: {
    keySource: {
      kind: "environment",
      variable: "MIDGARD_WATCHER_PROVER_KEY",
    },
  },
  deadlines: {
    daFetchMs: options?.daFetchMs ?? 60_000,
    daPublishMs: 60_000,
    proofConstructMs: 300_000,
    proofSubmitMs: 120_000,
  },
});

export const configOf = (
  options?: Parameters<typeof rawConfig>[0],
): WatcherConfig => rawConfig(options) as unknown as WatcherConfig;

export const identityOf = (options?: {
  readonly manifestId?: string;
  readonly markerManifestId?: string;
  readonly network?: "Mainnet" | "Preprod" | "Preview";
}): VerifiedWatcherDeploymentIdentity => {
  const manifestId = options?.manifestId ?? FINGERPRINT;
  return {
    manifestId,
    network: options?.network ?? "Preprod",
    trustRootId: "trust-root-a",
    fundingProfileBundleDigest: "ab".repeat(32),
    blueprintHash: repeatHex(0x33, 32),
    ruleBundleCommitment: repeatHex(0x44, 32),
    programCommitments: {},
    durableMarker: makeDeploymentMarker(
      options?.markerManifestId ?? manifestId,
    ),
  };
};

const daPayload = (headerHash: string): DaPayload => {
  const counts = {
    withdrawalCount: 0n,
    forcedTransactionCount: 0n,
    l2TransactionCount: 1n,
    depositCount: 0n,
    totalEventCount: 1n,
    transitionStepCount: 1n,
    validationTraceCount: 1n,
  };
  return {
    version: DA_PAYLOAD_VERSION,
    block_body: {
      header_hash: headerHash,
      header: {
        prevUtxosRoot: repeatHex(0x01, 32),
        utxosRoot: repeatHex(0x02, 32),
        withdrawalsRoot: EMPTY_MERKLE_TREE_ROOT,
        forcedTransactionsRoot: EMPTY_MERKLE_TREE_ROOT,
        transactionsRoot: repeatHex(0x03, 32),
        depositsRoot: EMPTY_MERKLE_TREE_ROOT,
        transitionTraceRoot: repeatHex(0x04, 32),
        eventToStepRoot: repeatHex(0x05, 32),
        validationTracesRoot: repeatHex(0x06, 32),
        ...counts,
        startTime: 1_000n,
        endTime: 1_999n,
        blockSlot: 42n,
        expectedNetworkId: 0n,
        minFeeA: 0n,
        minFeeB: 0n,
        prevHeaderHash: repeatHex(0x07, 28),
        operatorVkey: repeatHex(0x08, 28),
        protocolVersion: 1n,
      },
      utxos: [],
      withdrawals: [],
      forced_transactions: [],
      transactions: [[repeatHex(0x09, 32), repeatHex(0x0a, 40)]],
      transaction_preimages: [[repeatHex(0x09, 32), repeatHex(0x0b, 64)]],
      forced_transaction_preimages: [],
      cek_program_material: [],
      deposits: [],
      transition_trace: [[repeatHex(0x0c, 32), repeatHex(0x0d, 48)]],
      event_to_step: [[repeatHex(0x0e, 32), repeatHex(0x0f, 8)]],
      validation_traces: [[repeatHex(0x10, 32), repeatHex(0x11, 24)]],
      validation_trace_witnesses: [],
      counts,
    },
  };
};

export type PayloadFixture = Readonly<{
  innerCbor: Buffer;
  envelope: Buffer;
  payloadHash: Buffer;
}>;

export const makePayloadFixture = async (
  headerHash: string,
): Promise<PayloadFixture> => {
  const innerCbor = encodeDaPayload(daPayload(headerHash));
  const envelope = await wrapDaPayload(innerCbor, { mode: "identity" });
  return {
    innerCbor,
    envelope,
    payloadHash: computeDaSha256Hash(envelope),
  };
};

// ---------------------------------------------------------------------------
// Scripted libp2p transport
// ---------------------------------------------------------------------------

type ProtocolHandler = (
  request: WatcherPublicDaRequest,
) => Promise<Uint8Array> | Uint8Array;

export type PeerScript = Partial<
  Record<DaRequestResponseProtocol, ProtocolHandler>
>;

export class ScriptedTransport implements WatcherPublicDaLibp2pTransportV1 {
  readonly calls: WatcherPublicDaRequest[] = [];

  constructor(private readonly script: Readonly<Record<string, PeerScript>>) {}

  async request(request: WatcherPublicDaRequest): Promise<Uint8Array> {
    this.calls.push(request);
    const peer = this.script[request.peerIdentity];
    if (peer === undefined) {
      throw new Error(`unscripted peer ${request.peerIdentity}`);
    }
    const handler = peer[request.protocol];
    if (handler === undefined) {
      throw new Error(
        `unscripted protocol ${request.protocol} for ${request.peerIdentity}`,
      );
    }
    return handler(request);
  }

  protocolsFor(peerIdentity: string): DaRequestResponseProtocol[] {
    return this.calls
      .filter((call) => call.peerIdentity === peerIdentity)
      .map((call) => call.protocol);
  }
}

/**
 * A deterministic replacement for the real clock and timer queue.
 *
 * Deadline behaviour must be decided by state, not by racing wall-clock
 * timers against peer failures: under full-suite load real timers drift in
 * both directions, and a fetch that has actually spent its budget can still
 * observe a sliver of remaining time and burn another peer on a clamped
 * 1ms dial (see #535). Virtual time only ever moves when the earliest pending
 * timer fires, so elapsed time is exactly the sum of the deadlines the client
 * itself chose. The drain runs on `setImmediate` so every pending microtask
 * settles between two firings — real time is used for ordering only, never
 * for measurement.
 */
export const makeVirtualClock = (): WatcherPublicDaClock => {
  type VirtualTimer = { readonly dueAt: number; readonly callback: () => void };
  const timers = new Map<number, VirtualTimer>();
  let now = 0;
  let nextId = 0;
  let scheduled = false;

  const drain = (): void => {
    if (scheduled) {
      return;
    }
    scheduled = true;
    setImmediate(step);
  };

  const step = (): void => {
    scheduled = false;
    let dueId: number | undefined;
    let due: VirtualTimer | undefined;
    // Earliest deadline first; ties break on insertion order, which the Map
    // preserves, so the firing order is a pure function of the schedule.
    for (const [id, timer] of timers) {
      if (due === undefined || timer.dueAt < due.dueAt) {
        dueId = id;
        due = timer;
      }
    }
    if (dueId === undefined || due === undefined) {
      return;
    }
    timers.delete(dueId);
    now = Math.max(now, due.dueAt);
    due.callback();
    drain();
  };

  return {
    now: () => now,
    setTimeout: (callback: () => void, delayMs: number) => {
      const id = (nextId += 1);
      timers.set(id, { dueAt: now + delayMs, callback });
      drain();
      return id;
    },
    clearTimeout: (handle: unknown) => {
      timers.delete(handle as number);
    },
  };
};

/** A transport call that never settles until the peer aborts it. */
export const hangUntilAborted: ProtocolHandler = async (request) =>
  new Promise<Uint8Array>((_, reject) => {
    request.signal.addEventListener("abort", () => {
      reject(new Error("aborted"));
    });
  });
