import { createHash } from "node:crypto";

import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { computeHash32 } from "../../midgard-core/src/codec/hash.js";
import {
  encodeWatcherNormalizedL1BlockV1,
  makeWatcherL1NormalizationSessionV1,
  makeWatcherL1PublicBytesV1,
  normalizeWatcherL1BlockV1,
  WATCHER_AUTHENTICATED_L1_PROVIDER_V1_SCHEMA_VERSION,
  WATCHER_L1_ADAPTER_V1_BOUNDS,
  WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION,
  WATCHER_NORMALIZED_L1_BLOCK_V1_SCHEMA_VERSION,
  watcherL1AdapterDiagnostic,
  WatcherL1AdapterError,
  type WatcherL1AdapterErrorCode,
  watcherL1NormalizationSessionStatsV1,
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

const transaction = (
  seedHex: string,
  outputIndex: string,
  redeemerEncoding: "legacy" | "map" = "legacy",
  includeScriptDataHash = true,
  isValid = true,
  includeCollateralReturn = false,
): MutableRecord => {
  const nativeScript = CML.NativeScript.new_script_all(
    CML.NativeScriptList.new(),
  );
  const nativeScripts = CML.NativeScriptList.new();
  nativeScripts.add(nativeScript);
  const datum = CML.PlutusData.from_cbor_hex("01");
  const datums = CML.PlutusDataList.new();
  datums.add(datum);
  const address = CML.Address.from_raw_bytes(
    Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x44)]),
  );
  const output = CML.TransactionOutput.new(
    address,
    CML.Value.from_coin(2_000_000n + BigInt(outputIndex)),
    CML.DatumOption.new_datum(datum),
    CML.Script.new_native(nativeScript),
  );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(output);
  const body = CML.TransactionBody.new(
    CML.TransactionInputList.new(),
    outputs,
    BigInt(`0x${seedHex}`),
  );
  const collateralReturn = CML.TransactionOutput.new(
    address,
    CML.Value.from_coin(1_500_000n),
    undefined,
    undefined,
  );
  if (includeCollateralReturn) {
    body.set_collateral_return(collateralReturn);
    body.set_total_collateral(500_000n);
  }
  if (includeScriptDataHash) {
    body.set_script_data_hash(
      CML.ScriptDataHash.from_raw_bytes(
        Buffer.alloc(32, Number(BigInt(outputIndex) % 256n)),
      ),
    );
  }
  const mintData = CML.PlutusData.from_cbor_hex("d87980");
  const spendData = CML.PlutusData.from_cbor_hex("d8798101");
  const witnessSet = CML.TransactionWitnessSet.new();
  witnessSet.set_native_scripts(nativeScripts);
  witnessSet.set_plutus_datums(datums);
  if (redeemerEncoding === "legacy") {
    const redeemers = CML.LegacyRedeemerList.new();
    redeemers.add(
      CML.LegacyRedeemer.new(
        CML.RedeemerTag.Mint,
        10n,
        mintData,
        CML.ExUnits.new(5n, 7n),
      ),
    );
    redeemers.add(
      CML.LegacyRedeemer.new(
        CML.RedeemerTag.Spend,
        BigInt(outputIndex),
        spendData,
        CML.ExUnits.new(11n, 13n),
      ),
    );
    witnessSet.set_redeemers(CML.Redeemers.new_arr_legacy_redeemer(redeemers));
  } else {
    const redeemers = CML.MapRedeemerKeyToRedeemerVal.new();
    redeemers.insert(
      CML.RedeemerKey.new(CML.RedeemerTag.Mint, 10n),
      CML.RedeemerVal.new(mintData, CML.ExUnits.new(5n, 7n)),
    );
    redeemers.insert(
      CML.RedeemerKey.new(CML.RedeemerTag.Spend, BigInt(outputIndex)),
      CML.RedeemerVal.new(spendData, CML.ExUnits.new(11n, 13n)),
    );
    witnessSet.set_redeemers(
      CML.Redeemers.new_map_redeemer_key_to_redeemer_val(redeemers),
    );
  }
  const fullTransaction = CML.Transaction.new(
    body,
    witnessSet,
    isValid,
    undefined,
  );
  const bodyBytes = body.to_canonical_cbor_hex();
  const txHash = blake2b256(bodyBytes);
  const datumBytes = datum.to_canonical_cbor_hex();
  const scriptBytes = nativeScript.to_canonical_cbor_hex();
  return {
    txHash,
    fullTransaction: publicBytes(fullTransaction.to_canonical_cbor_hex()),
    body: publicBytes(bodyBytes),
    witnessSet: publicBytes(witnessSet.to_canonical_cbor_hex()),
    utxos: isValid
      ? [
          {
            outRef: `${txHash}#0`,
            outputIndex: "0",
            output: publicBytes(output.to_canonical_cbor_hex()),
            datum: {
              datumHash: blake2b256(datumBytes),
              bytes: publicBytes(datumBytes),
            },
            referenceScript: {
              scriptHash: nativeScript.hash().to_hex(),
              language: "Native",
              bytes: publicBytes(scriptBytes),
            },
          },
        ]
      : includeCollateralReturn
        ? [
            {
              outRef: `${txHash}#1`,
              outputIndex: "1",
              output: publicBytes(collateralReturn.to_canonical_cbor_hex()),
              datum: null,
              referenceScript: null,
            },
          ]
        : [],
    scripts: [
      {
        scriptHash: nativeScript.hash().to_hex(),
        language: "Native",
        bytes: publicBytes(scriptBytes),
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
        bytes: publicBytes("d8798101"),
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
    parentBlockHash: "10".repeat(32),
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
        parentBlockHash: "10".repeat(32),
        slot: "76543210",
        blockNo: "2345678",
        depth: "15",
      },
    });
    expect(normalized.transactions.map(({ txHash }) => txHash)).toEqual(
      observation().transactions.map(
        ({ txHash }: { txHash: string }) => txHash,
      ),
    );
    expect(
      normalized.transactions[0]?.redeemers.map(({ purpose }) => purpose),
    ).toEqual(["spend", "mint"]);
    expect(normalized.transactions.map(({ isValid }) => isValid)).toEqual([
      true,
      true,
    ]);
    expect(normalized.chainPoint.chainPointId).toMatch(/^[0-9a-f]{64}$/u);
    expect(normalized.chainPoint.pointDigest).toMatch(/^[0-9a-f]{64}$/u);
    expect(normalized.blockContentDigest).toMatch(/^[0-9a-f]{64}$/u);
    expect(normalized.observationDigest).toMatch(/^[0-9a-f]{64}$/u);
    expect(Object.isFrozen(normalized)).toBe(true);
    expect(Object.isFrozen(normalized.transactions)).toBe(true);
    expect(Object.isFrozen(normalized.transactions[0]?.utxos)).toBe(true);
  });

  it("derives deterministic redeemer pointers from Conway map witnesses", () => {
    const input = observation();
    input.transactions[0] = transaction("a20081825820", "10", "map");
    const normalized = normalizeWatcherL1BlockV1(provider(), input);

    expect(
      normalized.transactions[0]?.redeemers.map(({ purpose, index }) => ({
        purpose,
        index,
      })),
    ).toEqual([
      { purpose: "spend", index: "10" },
      { purpose: "mint", index: "10" },
    ]);
  });

  it("preserves the authenticated node transaction sequence in the canonical bytes", () => {
    const first = normalizeWatcherL1BlockV1(provider(), observation());
    const reordered = observation();
    reordered.transactions.reverse();
    for (const tx of reordered.transactions) {
      tx.redeemers.reverse();
    }
    const second = normalizeWatcherL1BlockV1(provider(), reordered);
    const firstBytes = encodeWatcherNormalizedL1BlockV1(first);
    const secondBytes = encodeWatcherNormalizedL1BlockV1(second);

    expect(second.transactions.map(({ txHash }) => txHash)).toEqual(
      reordered.transactions.map(({ txHash }: { txHash: string }) => txHash),
    );
    expect(secondBytes.equals(firstBytes)).toBe(false);
    expect(second.blockContentDigest).not.toBe(first.blockContentDigest);
    expect(second.observationDigest).not.toBe(first.observationDigest);
    expect(createHash("sha256").update(firstBytes).digest("hex")).toBe(
      "6ecf05a35440ee2c7bec1e81c7296b40930b8ef8bbfdb2812c2c4e3d4d94dd11",
    );
    expect(first.blockContentDigest).toBe(
      "7686e78a8abaf53b3d8041705db0b6f96f62698766134fea45b611bf145a9185",
    );
    expect(first.observationDigest).toBe(
      "5c0c63e326e2b9b643b9152d7fcc083fae1da51b5e0837f21ce669a855810061",
    );
  });

  it("rejects transaction reordering once the node supplies ledger ordinals", () => {
    const indexed = observation();
    indexed.transactions = indexed.transactions.map(
      (candidate: MutableRecord, index: number) => ({
        ...candidate,
        transactionIndex: index.toString(),
      }),
    );
    expect(
      normalizeWatcherL1BlockV1(provider(), indexed).transactions.map(
        ({ transactionIndex }) => transactionIndex,
      ),
    ).toEqual(["0", "1"]);

    const reordered = structuredClone(indexed);
    reordered.transactions.reverse();
    rejected(
      () => normalizeWatcherL1BlockV1(provider(), reordered),
      "identity_mismatch",
      "$.transactions[0].transactionIndex",
    );

    const transactionParent = structuredClone(indexed);
    transactionParent.transactions[0].blockParentHash = "12".repeat(32);
    rejected(
      () => normalizeWatcherL1BlockV1(provider(), transactionParent),
      "unknown_field",
      "$.transactions[0].blockParentHash",
    );
  });

  it("authenticates parent metadata for empty blocks and rejects detached parent claims", () => {
    const empty = observation();
    empty.transactions = [];
    const normalized = normalizeWatcherL1BlockV1(provider(), empty);
    expect(normalized.chainPoint.parentBlockHash).toBe("10".repeat(32));

    const detached = structuredClone(empty);
    detached.chainPoint.parentBlockHash = "12".repeat(32);
    const detachedNormalized = normalizeWatcherL1BlockV1(provider(), detached);
    expect(detachedNormalized.chainPoint.pointDigest).not.toBe(
      normalized.chainPoint.pointDigest,
    );
    expect(detachedNormalized.blockContentDigest).not.toBe(
      normalized.blockContentDigest,
    );

    const missing = structuredClone(empty);
    delete missing.chainPoint.parentBlockHash;
    rejected(
      () => normalizeWatcherL1BlockV1(provider(), missing),
      "missing_field",
      "$.chainPoint.parentBlockHash",
    );

    const genesis = structuredClone(empty);
    genesis.chainPoint.parentBlockHash = null;
    expect(
      normalizeWatcherL1BlockV1(provider(), genesis).chainPoint.parentBlockHash,
    ).toBeNull();
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

  it("binds the body and script-data identity to the full transaction", () => {
    const detachedBody = observation();
    const otherTransaction = transaction("1234", "10");
    detachedBody.transactions[0].body = otherTransaction.body;
    detachedBody.transactions[0].txHash = otherTransaction.txHash;
    rejected(
      () => normalizeWatcherL1BlockV1(provider(), detachedBody),
      "identity_mismatch",
      "$.transactions[0].body.bytesHex",
    );

    const missingScriptDataHash = observation();
    missingScriptDataHash.transactions[0] = transaction(
      "1234",
      "10",
      "legacy",
      false,
    );
    rejected(
      () => normalizeWatcherL1BlockV1(provider(), missingScriptDataHash),
      "identity_mismatch",
      "$.transactions[0].body.bytesHex",
    );
  });

  it.each([
    {
      mode: "external_providers",
      authenticatedProvider: provider(),
      providerId: "provider-a",
    },
    {
      mode: "local_node",
      authenticatedProvider: localProvider("chain_sync"),
      providerId: "chain-sync",
    },
  ])(
    "rejects detached script and redeemer views in $mode mode while transaction identity stays unchanged",
    ({ authenticatedProvider, providerId }) => {
      const alteredScript = observation();
      alteredScript.providerId = providerId;
      alteredScript.transactions[0].scripts[0].bytes = publicBytes("00");
      rejected(
        () => normalizeWatcherL1BlockV1(authenticatedProvider, alteredScript),
        "identity_mismatch",
        "$.transactions[0].scripts",
      );

      const alteredRedeemer = observation();
      alteredRedeemer.providerId = providerId;
      alteredRedeemer.transactions[0].redeemers[0].bytes = publicBytes("00");
      rejected(
        () => normalizeWatcherL1BlockV1(authenticatedProvider, alteredRedeemer),
        "identity_mismatch",
        "$.transactions[0].redeemers",
      );
    },
  );

  it("rejects a detached witness-set snapshot under an unchanged full transaction", () => {
    const alteredWitnessSet = observation();
    alteredWitnessSet.transactions[0].witnessSet = publicBytes("a0");
    rejected(
      () => normalizeWatcherL1BlockV1(provider(), alteredWitnessSet),
      "identity_mismatch",
      "$.transactions[0].witnessSet",
    );
  });

  it("derives every applied output and rejects missing, extra, or forged views", () => {
    const session = makeWatcherL1NormalizationSessionV1();
    const warm = normalizeWatcherL1BlockV1(provider(), observation(), session);
    expect(warm.transactions).toHaveLength(2);
    expect(watcherL1NormalizationSessionStatsV1(session)).toMatchObject({
      retainedEntries: 2,
      maximumEntries: WATCHER_L1_ADAPTER_V1_BOUNDS.normalizationSessionEntries,
      maximumBytes: WATCHER_L1_ADAPTER_V1_BOUNDS.normalizationSessionBytes,
    });
    expect(
      watcherL1NormalizationSessionStatsV1(session).retainedBytes,
    ).toBeLessThanOrEqual(
      WATCHER_L1_ADAPTER_V1_BOUNDS.normalizationSessionBytes,
    );

    const missing = observation();
    missing.transactions[0].utxos = [];
    rejected(
      () => normalizeWatcherL1BlockV1(provider(), missing, session),
      "identity_mismatch",
      "$.transactions[0].utxos",
    );

    const extra = observation();
    extra.transactions[0].utxos.push({
      ...structuredClone(extra.transactions[0].utxos[0]),
      outRef: `${extra.transactions[0].txHash}#1`,
      outputIndex: "1",
    });
    rejected(
      () => normalizeWatcherL1BlockV1(provider(), extra, session),
      "identity_mismatch",
      "$.transactions[0].utxos",
    );

    const forgedOutput = observation();
    forgedOutput.transactions[0].utxos[0].output = publicBytes("00");
    rejected(
      () => normalizeWatcherL1BlockV1(provider(), forgedOutput, session),
      "identity_mismatch",
      "$.transactions[0].utxos",
    );

    const forgedDatum = observation();
    forgedDatum.transactions[0].utxos[0].datum = {
      datumHash: blake2b256("00"),
      bytes: publicBytes("00"),
    };
    rejected(
      () => normalizeWatcherL1BlockV1(provider(), forgedDatum, session),
      "identity_mismatch",
      "$.transactions[0].utxos",
    );

    const forgedReferenceScript = observation();
    forgedReferenceScript.transactions[0].utxos[0].referenceScript.bytes =
      publicBytes("00");
    rejected(
      () =>
        normalizeWatcherL1BlockV1(provider(), forgedReferenceScript, session),
      "identity_mismatch",
      "$.transactions[0].utxos",
    );

    const digestCollisionClaim = observation();
    digestCollisionClaim.transactions[0].fullTransaction.bytesHex = "00";
    rejected(
      () =>
        normalizeWatcherL1BlockV1(provider(), digestCollisionClaim, session),
      "content_digest_mismatch",
      "$.transactions[0].fullTransaction.sha256",
    );

    expect(
      watcherL1NormalizationSessionStatsV1(
        makeWatcherL1NormalizationSessionV1(),
      ),
    ).toMatchObject({
      retainedEntries: 0,
      retainedBytes: 0,
    });
  });

  it("retains invalid transaction evidence without exposing phantom applied outputs", () => {
    const invalidTransaction = transaction("1234", "10", "legacy", true, false);
    const phantom = observation();
    phantom.transactions = [structuredClone(invalidTransaction)];
    phantom.transactions[0].utxos = transaction(
      "1234",
      "10",
      "legacy",
      true,
      true,
    ).utxos;
    rejected(
      () => normalizeWatcherL1BlockV1(provider(), phantom),
      "identity_mismatch",
      "$.transactions[0].utxos",
    );

    const accepted = observation();
    accepted.transactions = [invalidTransaction];
    const normalized = normalizeWatcherL1BlockV1(provider(), accepted);
    expect(normalized.transactions[0]).toMatchObject({
      isValid: false,
      utxos: [],
    });
    expect(normalized.transactions[0]?.redeemers).toHaveLength(2);
    expect(normalized.transactions[0]?.witnessSet.bytesHex).not.toBe("");

    const spoofed = observation();
    spoofed.transactions[0].isValid = false;
    rejected(
      () => normalizeWatcherL1BlockV1(provider(), spoofed),
      "unknown_field",
      "$.transactions[0].isValid",
    );
  });

  it("indexes only the ledger-created collateral return for an invalid transaction", () => {
    const invalidTransaction = transaction(
      "1235",
      "10",
      "legacy",
      true,
      false,
      true,
    );
    const accepted = observation();
    accepted.transactions = [invalidTransaction];
    const normalized = normalizeWatcherL1BlockV1(provider(), accepted);
    expect(normalized.transactions[0]).toMatchObject({
      isValid: false,
      utxos: [
        {
          outRef: `${invalidTransaction.txHash}#1`,
          outputIndex: "1",
          datum: null,
          referenceScript: null,
        },
      ],
    });
    expect(normalized.transactions[0]?.utxos[0]?.output).toEqual(
      invalidTransaction.utxos[0].output,
    );

    const phantomRegularOutput = structuredClone(accepted);
    phantomRegularOutput.transactions[0].utxos[0] = {
      ...transaction("1235", "10", "legacy", true, true).utxos[0],
      outRef: `${invalidTransaction.txHash}#0`,
    };
    rejected(
      () => normalizeWatcherL1BlockV1(provider(), phantomRegularOutput),
      "identity_mismatch",
      "$.transactions[0].utxos",
    );

    const forgedCollateralReturn = structuredClone(accepted);
    forgedCollateralReturn.transactions[0].utxos[0].output = publicBytes("00");
    rejected(
      () => normalizeWatcherL1BlockV1(provider(), forgedCollateralReturn),
      "identity_mismatch",
      "$.transactions[0].utxos",
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
