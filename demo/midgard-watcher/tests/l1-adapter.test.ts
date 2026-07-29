import { createHash } from "node:crypto";

import { describe, expect, it } from "vitest";

import { computeHash32 } from "../../midgard-core/src/codec/hash.js";
import {
  encodeWatcherNormalizedL1BlockV1,
  makeWatcherL1PublicBytesV1,
  normalizeWatcherL1BlockV1,
  WATCHER_AUTHENTICATED_L1_PROVIDER_V1_SCHEMA_VERSION,
  WATCHER_L1_ADAPTER_V1_BOUNDS,
  WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION,
  WATCHER_NORMALIZED_L1_BLOCK_V1_SCHEMA_VERSION,
  watcherL1AdapterDiagnostic,
  WatcherL1AdapterError,
  type WatcherL1AdapterErrorCode,
} from "../src/l1-adapter.js";

type MutableRecord = Record<string, any>;

const blake2b256 = (bytesHex: string): string =>
  computeHash32(Buffer.from(bytesHex, "hex")).toString("hex");

const provider = (
  providerId = "provider-a",
  identityByte = "aa",
): MutableRecord => ({
  schemaVersion: WATCHER_AUTHENTICATED_L1_PROVIDER_V1_SCHEMA_VERSION,
  network: "Preprod",
  providerId,
  source: {
    sourceMode: "external_providers",
    operatorIdentitySha256: identityByte.repeat(32),
  },
  authentication: {
    kind: "https_tls_identity_v1",
    publicIdentitySha256: identityByte.repeat(32),
  },
});

const localProvider = (
  surface: "chain_sync" | "ogmios" | "kupo" | "kupmios" | "db_sync",
  providerId = surface.replace("_", "-"),
  identityByte = surface === "chain_sync" ? "cc" : "dd",
): MutableRecord => ({
  schemaVersion: WATCHER_AUTHENTICATED_L1_PROVIDER_V1_SCHEMA_VERSION,
  network: "Preprod",
  providerId,
  source: {
    sourceMode: "local_node",
    authorityNodeId: "watcher-node-a",
    surface,
  },
  authentication: {
    kind:
      surface === "chain_sync"
        ? "cardano_node_genesis_v1"
        : "https_tls_identity_v1",
    publicIdentitySha256: identityByte.repeat(32),
  },
});

const publicBytes = (bytesHex: string): MutableRecord => ({
  ...makeWatcherL1PublicBytesV1(bytesHex),
});

const transaction = (bodyBytes: string, outputIndex: string): MutableRecord => {
  const txHash = blake2b256(bodyBytes);
  const datumBytes = `d8${outputIndex.padStart(2, "0")}`;
  return {
    txHash,
    body: publicBytes(bodyBytes),
    utxos: [
      {
        outRef: `${txHash}#${outputIndex}`,
        outputIndex,
        output: publicBytes(`82${outputIndex.padStart(2, "0")}`),
        datum: {
          datumHash: blake2b256(datumBytes),
          bytes: publicBytes(datumBytes),
        },
        referenceScript: {
          scriptHash: "33".repeat(28),
          language: "PlutusV3",
          bytes: publicBytes("4e4d01000033222220051200120011"),
        },
      },
    ],
    scripts: [
      {
        scriptHash: "22".repeat(28),
        language: "Native",
        bytes: publicBytes("8200581c"),
      },
    ],
    datums: [
      {
        datumHash: blake2b256("01"),
        bytes: publicBytes("01"),
      },
    ],
    redeemers: [
      {
        purpose: "mint",
        index: "10",
        bytes: publicBytes("d87980"),
      },
      {
        purpose: "spend",
        index: outputIndex,
        bytes: publicBytes("d8799f01ff"),
      },
    ],
  };
};

const observation = (): MutableRecord => ({
  schemaVersion: WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION,
  network: "Preprod",
  providerId: "provider-a",
  chainPoint: {
    blockHash: "11".repeat(32),
    slot: "76543210",
    blockNo: "2345678",
    depth: "15",
  },
  transactions: [transaction("a20081825820", "10"), transaction("a100", "2")],
});

const rejected = (
  action: () => unknown,
  code: WatcherL1AdapterErrorCode,
  path: string,
): WatcherL1AdapterError => {
  try {
    action();
  } catch (error) {
    expect(error).toBeInstanceOf(WatcherL1AdapterError);
    const adapterError = error as WatcherL1AdapterError;
    expect(adapterError).toMatchObject({ code, path });
    return adapterError;
  }
  throw new Error("Expected L1 adapter rejection");
};

describe("provider-neutral authenticated L1 adapter", () => {
  it("normalizes a complete authenticated Cardano observation", () => {
    const normalized = normalizeWatcherL1BlockV1(provider(), observation());

    expect(normalized).toMatchObject({
      schemaVersion: WATCHER_NORMALIZED_L1_BLOCK_V1_SCHEMA_VERSION,
      network: "Preprod",
      provider: {
        providerId: "provider-a",
        source: {
          sourceMode: "external_providers",
          operatorIdentitySha256: "aa".repeat(32),
        },
        authentication: {
          kind: "https_tls_identity_v1",
          publicIdentitySha256: "aa".repeat(32),
        },
      },
      chainPoint: {
        blockHash: "11".repeat(32),
        slot: "76543210",
        blockNo: "2345678",
        depth: "15",
      },
    });
    expect(normalized.transactions.map(({ txHash }) => txHash)).toEqual(
      [...normalized.transactions.map(({ txHash }) => txHash)].sort(),
    );
    expect(
      normalized.transactions[0]?.redeemers.map(({ purpose }) => purpose),
    ).toEqual(["spend", "mint"]);
    expect(normalized.chainPoint.chainPointId).toMatch(/^[0-9a-f]{64}$/u);
    expect(normalized.chainPoint.pointDigest).toMatch(/^[0-9a-f]{64}$/u);
    expect(normalized.blockContentDigest).toMatch(/^[0-9a-f]{64}$/u);
    expect(normalized.observationDigest).toMatch(/^[0-9a-f]{64}$/u);
    expect(Object.isFrozen(normalized)).toBe(true);
    expect(Object.isFrozen(normalized.transactions)).toBe(true);
    expect(Object.isFrozen(normalized.transactions[0]?.utxos)).toBe(true);
  });

  it("emits one byte-identical deterministic canonical fixture", () => {
    const first = normalizeWatcherL1BlockV1(provider(), observation());
    const reordered = observation();
    reordered.transactions.reverse();
    for (const tx of reordered.transactions) {
      tx.redeemers.reverse();
    }
    const second = normalizeWatcherL1BlockV1(provider(), reordered);
    const firstBytes = encodeWatcherNormalizedL1BlockV1(first);
    const secondBytes = encodeWatcherNormalizedL1BlockV1(second);

    expect(second).toEqual(first);
    expect(secondBytes.equals(firstBytes)).toBe(true);
    expect(createHash("sha256").update(firstBytes).digest("hex")).toBe(
      "f301c7aeac421c6df1550fb52501e7c98995dcaed10b42534ec858ecba464c1b",
    );
    expect(first.blockContentDigest).toBe(
      "a5e34702f4bcb3c843303e694e92e3d6e773bdf780328f969090cb40fe20b3cc",
    );
    expect(first.observationDigest).toBe(
      "b0132b6974d588fe194a2fa34fac5a2f972687bae04fcf3ea3d67ad8f8d01135",
    );
  });

  it("keeps provider-neutral content stable while provider evidence stays distinct", () => {
    const first = normalizeWatcherL1BlockV1(provider(), observation());
    const secondInput = observation();
    secondInput.providerId = "provider-b";
    const second = normalizeWatcherL1BlockV1(
      provider("provider-b", "bb"),
      secondInput,
    );

    expect(second.chainPoint.pointDigest).toBe(first.chainPoint.pointDigest);
    expect(second.blockContentDigest).toBe(first.blockContentDigest);
    expect(second.chainPoint.chainPointId).not.toBe(
      first.chainPoint.chainPointId,
    );
    expect(second.observationDigest).not.toBe(first.observationDigest);
  });

  it("binds a local chain-sync authority and aligned query surfaces without changing actual Cardano bytes", () => {
    const chainSync = normalizeWatcherL1BlockV1(localProvider("chain_sync"), {
      ...observation(),
      providerId: "chain-sync",
    });
    const ogmios = normalizeWatcherL1BlockV1(localProvider("ogmios"), {
      ...observation(),
      providerId: "ogmios",
    });

    expect(chainSync.provider.source).toEqual({
      sourceMode: "local_node",
      authorityNodeId: "watcher-node-a",
      surface: "chain_sync",
    });
    expect(ogmios.provider.source).toEqual({
      sourceMode: "local_node",
      authorityNodeId: "watcher-node-a",
      surface: "ogmios",
    });
    expect(ogmios.blockContentDigest).toBe(chainSync.blockContentDigest);
    expect(ogmios.transactions).toEqual(chainSync.transactions);
    expect(ogmios.observationDigest).not.toBe(chainSync.observationDigest);
  });

  it("rejects mismatched network and provider claims", () => {
    const wrongNetwork = observation();
    wrongNetwork.network = "Preview";
    rejected(
      () => normalizeWatcherL1BlockV1(provider(), wrongNetwork),
      "network_mismatch",
      "$.network",
    );

    const wrongProvider = observation();
    wrongProvider.providerId = "provider-b";
    rejected(
      () => normalizeWatcherL1BlockV1(provider(), wrongProvider),
      "provider_mismatch",
      "$.providerId",
    );
  });

  it("rejects unsupported schemas, authentication modes, and malformed identities", () => {
    const wrongSchema = observation();
    wrongSchema.schemaVersion = "midgard-watcher-l1-block-observation-v2";
    rejected(
      () => normalizeWatcherL1BlockV1(provider(), wrongSchema),
      "unsupported_schema",
      "$.schemaVersion",
    );

    const wrongAuthentication = provider();
    wrongAuthentication.authentication.kind = "bearer_token";
    rejected(
      () => normalizeWatcherL1BlockV1(wrongAuthentication, observation()),
      "invalid_field",
      "$.authenticatedProvider.authentication.kind",
    );

    const missingSource = provider();
    delete missingSource.source;
    rejected(
      () => normalizeWatcherL1BlockV1(missingSource, observation()),
      "missing_field",
      "$.authenticatedProvider.source",
    );

    const chainSyncOverTls = localProvider("chain_sync");
    chainSyncOverTls.authentication.kind = "https_tls_identity_v1";
    rejected(
      () => normalizeWatcherL1BlockV1(chainSyncOverTls, observation()),
      "identity_mismatch",
      "$.authenticatedProvider.authentication.kind",
    );

    const malformedPoint = observation();
    malformedPoint.chainPoint.slot = "076543210";
    rejected(
      () => normalizeWatcherL1BlockV1(provider(), malformedPoint),
      "invalid_field",
      "$.chainPoint.slot",
    );
  });

  it("rejects unknown fields and unsafe object shapes exactly", () => {
    const unknown = observation();
    unknown.apiToken = "never-report-this-secret";
    const error = rejected(
      () => normalizeWatcherL1BlockV1(provider(), unknown),
      "unknown_field",
      "$.apiToken",
    );
    expect(error.message).not.toContain("never-report-this-secret");

    const unsafe = observation();
    Object.defineProperty(unsafe.chainPoint, "depth", {
      enumerable: true,
      get: () => "15",
    });
    rejected(
      () => normalizeWatcherL1BlockV1(provider(), unsafe),
      "unsafe_value",
      "$.chainPoint",
    );

    const unsafeArray = observation();
    Object.defineProperty(unsafeArray.transactions, "0", {
      enumerable: true,
      get: () => transaction("a100", "2"),
    });
    rejected(
      () => normalizeWatcherL1BlockV1(provider(), unsafeArray),
      "unsafe_value",
      "$.transactions",
    );
  });

  it("rejects forged content digests and Cardano transaction or datum identities", () => {
    const badDigest = observation();
    badDigest.transactions[0].body.sha256 = "00".repeat(32);
    rejected(
      () => normalizeWatcherL1BlockV1(provider(), badDigest),
      "content_digest_mismatch",
      "$.transactions[0].body.sha256",
    );

    const badTransaction = observation();
    badTransaction.transactions[0].txHash = "00".repeat(32);
    rejected(
      () => normalizeWatcherL1BlockV1(provider(), badTransaction),
      "identity_mismatch",
      "$.transactions[0].txHash",
    );

    const badDatum = observation();
    badDatum.transactions[0].datums[0].datumHash = "00".repeat(32);
    rejected(
      () => normalizeWatcherL1BlockV1(provider(), badDatum),
      "identity_mismatch",
      "$.transactions[0].datums[0].datumHash",
    );
  });

  it("rejects forged UTxO outrefs and duplicate canonical identities", () => {
    const badOutRef = observation();
    badOutRef.transactions[0].utxos[0].outRef = `${badOutRef.transactions[0].txHash}#11`;
    rejected(
      () => normalizeWatcherL1BlockV1(provider(), badOutRef),
      "identity_mismatch",
      "$.transactions[0].utxos[0].outRef",
    );

    const duplicate = observation();
    duplicate.transactions.push(structuredClone(duplicate.transactions[0]));
    rejected(
      () => normalizeWatcherL1BlockV1(provider(), duplicate),
      "duplicate_identity",
      "$.transactions[2]",
    );
  });

  it("rejects multiplicative nested collections before parsing their members", () => {
    const hostile = observation();
    const transactionCount =
      Math.floor(
        WATCHER_L1_ADAPTER_V1_BOUNDS.totalCollectionMembers /
          WATCHER_L1_ADAPTER_V1_BOUNDS.arrayMembers,
      ) + 1;
    hostile.transactions = Array.from(
      { length: transactionCount },
      (_, index) => ({
        ...transaction(index.toString(16).padStart(4, "0"), "0"),
        utxos: Array.from(
          { length: WATCHER_L1_ADAPTER_V1_BOUNDS.arrayMembers },
          () => null,
        ),
        scripts: [],
        datums: [],
        redeemers: [],
      }),
    );

    rejected(
      () => normalizeWatcherL1BlockV1(provider(), hostile),
      "out_of_bounds",
      "$.transactions",
    );
  });

  it("uses secret-safe diagnostics for recognized and foreign errors", () => {
    const input = observation();
    input.secret = "super-secret-provider-credential";
    const error = rejected(
      () => normalizeWatcherL1BlockV1(provider(), input),
      "unknown_field",
      "$.secret",
    );
    const known = watcherL1AdapterDiagnostic(error);
    const foreign = watcherL1AdapterDiagnostic(
      new Error("super-secret-provider-credential"),
    );

    expect(known).toEqual({
      code: "unknown_field",
      path: "$.secret",
      message: "Watcher L1 observation rejected: unknown_field at $.secret",
    });
    expect(JSON.stringify([known, foreign])).not.toContain(
      "super-secret-provider-credential",
    );
    expect(foreign).toEqual({
      code: "invalid_field",
      path: "$",
      message: "Watcher L1 observation rejected: invalid_field at $",
    });
  });
});
