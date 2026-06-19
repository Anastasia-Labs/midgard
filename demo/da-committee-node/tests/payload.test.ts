import * as SDK from "@al-ft/midgard-sdk";
import {
  computeMidgardNativeTxId,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardNativeTxCanonical,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core/codec";
import { describe, expect, it } from "vitest";

import {
  computeDaPayloadRoots,
  DaPayloadValidationError,
  decodeDaPayloadV1Strict,
  verifyDaPayloadAgainstHeader,
} from "../src/da/payload.js";
import { hashBlockHeader } from "../src/l1/state-queue-scanner.js";
import { JsonFileWatcherStore } from "../src/store.js";
import {
  fixtureHeaderBase,
  IDENTITY_TX_PROJECTOR,
  makePayloadFixture,
  tempDir,
} from "./helpers.js";

describe("DA payload verification", () => {
  it("strictly decodes and verifies roots against the L1 header", async () => {
    const { payloadCbor, header, headerHash } = await makePayloadFixture();
    const decoded = decodeDaPayloadV1Strict(payloadCbor);
    expect(decoded.header_hash).toBe(headerHash);
    const verified = await verifyDaPayloadAgainstHeader(
      payloadCbor,
      headerHash,
      header,
      {
        stateQueueOutRef: "tx#0",
        transactionProjector: IDENTITY_TX_PROJECTOR,
      },
    );
    expect(verified.validation.rootsMatch).toBe(true);
    expect(verified.roots.utxosRoot).toBe(header.utxosRoot);
  });

  it("verifies transaction roots with the production Midgard-native projector", async () => {
    const nativeTx = materializeMidgardNativeTxFromCanonical({
      version: MIDGARD_NATIVE_TX_VERSION,
      validity: "TxIsValid",
      body: {
        spendInputsPreimageCbor: EMPTY_CBOR_LIST,
        referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
        outputsPreimageCbor: EMPTY_CBOR_LIST,
        fee: 0n,
        validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
        validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
        requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
        requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
        mintPreimageCbor: EMPTY_CBOR_LIST,
        scriptIntegrityHash: EMPTY_NULL_ROOT,
        auxiliaryDataHash: EMPTY_NULL_ROOT,
        networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
      },
      witnessSet: {
        addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
        scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
        redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      },
    });
    const txCbor = encodeMidgardNativeTxCanonical(nativeTx);
    const payloadWithoutHash: SDK.DaPayloadV1 = {
      version: SDK.DA_PAYLOAD_V1_VERSION,
      header_hash: "00".repeat(28),
      block_body: {
        utxos: [],
        transactions: [
          [
            Buffer.from(computeMidgardNativeTxId(nativeTx)).toString("hex"),
            txCbor.toString("hex"),
          ],
        ],
        deposits: [],
        withdrawals: [],
      },
    };
    const roots = await computeDaPayloadRoots(payloadWithoutHash);
    const header = {
      ...fixtureHeaderBase(),
      utxosRoot: roots.utxosRoot,
      transactionsRoot: roots.transactionsRoot,
      depositsRoot: roots.depositsRoot,
      withdrawalsRoot: roots.withdrawalsRoot,
    };
    const headerHash = hashBlockHeader(header);
    const payload = {
      ...payloadWithoutHash,
      header_hash: headerHash,
    };

    await expect(
      verifyDaPayloadAgainstHeader(
        SDK.encodeDaPayloadV1(payload),
        headerHash,
        header,
        { stateQueueOutRef: "tx#0" },
      ),
    ).resolves.toMatchObject({
      validation: {
        headerHash,
        rootsMatch: true,
      },
    });
  });

  it("rejects wrong versions, duplicate keys, unsorted keys, and root mismatches", async () => {
    const { payload, header, headerHash } = await makePayloadFixture();
    expect(() =>
      decodeDaPayloadV1Strict(
        SDK.encodeDaPayloadV1({ ...payload, version: 2n }),
      ),
    ).toThrow(/version/);
    expect(() =>
      decodeDaPayloadV1Strict(
        SDK.encodeDaPayloadV1({
          ...payload,
          block_body: {
            ...payload.block_body,
            deposits: [
              ["30", "aa"],
              ["30", "bb"],
            ],
          },
        }),
      ),
    ).toThrow(/duplicate/);
    expect(() =>
      decodeDaPayloadV1Strict(
        SDK.encodeDaPayloadV1({
          ...payload,
          block_body: {
            ...payload.block_body,
            withdrawals: [
              ["ff", "aa"],
              ["01", "bb"],
            ],
          },
        }),
      ),
    ).toThrow(/sorted/);
    await expect(
      verifyDaPayloadAgainstHeader(
        SDK.encodeDaPayloadV1(payload),
        headerHash,
        { ...header, depositsRoot: "ff".repeat(32) },
        {
          stateQueueOutRef: "tx#0",
          transactionProjector: IDENTITY_TX_PROJECTOR,
        },
      ),
    ).rejects.toMatchObject({ code: "root_mismatch" });
  });

  it("rejects malformed transaction values with the default projector", async () => {
    const { payloadCbor, header, headerHash } = await makePayloadFixture();
    await expect(
      verifyDaPayloadAgainstHeader(payloadCbor, headerHash, header, {
        stateQueueOutRef: "tx#0",
      }),
    ).rejects.toBeInstanceOf(DaPayloadValidationError);
  });

  it("detects conflicting payload bytes durably", async () => {
    const dir = await tempDir();
    const store = await JsonFileWatcherStore.open(dir);
    const first = await store.saveDaPayload({
      deploymentFingerprint: "dep",
      headerHash: "01".repeat(28),
      payloadCborHex: "aa",
      payloadSha256: "11".repeat(32),
      sourceEndpoint: "a",
      fetchedAt: new Date().toISOString(),
      validationStatus: "fetched",
    });
    const second = await store.saveDaPayload({
      ...first,
      payloadCborHex: "bb",
      payloadSha256: "22".repeat(32),
    });
    expect(second.validationStatus).toBe("conflicted");
    expect(second.conflictStatus).toBe("conflicting_bytes");
  });
});
