import {
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardNativeTxCanonical,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  computeDaPayloadRoots,
  DaPayloadValidationError,
  decodeDaPayloadV2Strict,
  verifyDaPayloadAgainstHeader,
} from "../src/da/payload.js";
import { hashBlockHeader } from "../src/l1/state-queue-scanner.js";
import { JsonFileWatcherStore } from "../src/store.js";
import {
  IDENTITY_TX_PROJECTOR,
  makePayloadFixture,
  tempDir,
} from "./helpers.js";

describe("DA payload verification", () => {
  it("strictly decodes and verifies roots against the L1 header", async () => {
    const { payloadCbor, header, headerHash } = await makePayloadFixture();
    const decoded = decodeDaPayloadV2Strict(payloadCbor);
    expect(decoded.block_body.header_hash).toBe(headerHash);
    expect(decoded.block_body.forced_transactions).toHaveLength(1);
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
    expect(verified.roots.transitionTraceRoot).toBe(header.transitionTraceRoot);
    expect(verified.roots.forcedTransactionsRoot).toBe(
      header.forcedTransactionsRoot,
    );
    expect(verified.counts.totalEventCount).toBe(header.totalEventCount);
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
    const base = await makePayloadFixture();
    const payloadWithoutHash: SDK.DaPayloadV2 = {
      ...base.payload,
      block_body: {
        ...base.payload.block_body,
        header_hash: "00".repeat(28),
        transactions: base.payload.block_body.transactions.map(([key]) => [
          key,
          txCbor.toString("hex"),
        ]),
      },
    };
    const roots = await computeDaPayloadRoots(payloadWithoutHash);
    const header = {
      ...base.header,
      utxosRoot: roots.utxosRoot,
      forcedTransactionsRoot: roots.forcedTransactionsRoot,
      transactionsRoot: roots.transactionsRoot,
      depositsRoot: roots.depositsRoot,
      withdrawalsRoot: roots.withdrawalsRoot,
      transitionTraceRoot: roots.transitionTraceRoot,
      eventToStepRoot: roots.eventToStepRoot,
    };
    const headerHash = hashBlockHeader(header);
    const payload = {
      ...payloadWithoutHash,
      block_body: {
        ...payloadWithoutHash.block_body,
        header_hash: headerHash,
        header,
      },
    };

    await expect(
      verifyDaPayloadAgainstHeader(
        SDK.encodeDaPayloadV2(payload),
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
      decodeDaPayloadV2Strict(
        SDK.encodeDaPayloadV2({ ...payload, version: 1n }),
      ),
    ).toThrow(/version/);
    expect(() =>
      decodeDaPayloadV2Strict(
        SDK.encodeDaPayloadV2({
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
      decodeDaPayloadV2Strict(
        SDK.encodeDaPayloadV2({
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
    const [depositKey] = payload.block_body.deposits[0]!;
    const mismatchedPayload: SDK.DaPayloadV2 = {
      ...payload,
      block_body: {
        ...payload.block_body,
        deposits: [[depositKey, "ff"]],
      },
    };
    await expect(
      verifyDaPayloadAgainstHeader(
        SDK.encodeDaPayloadV2(mismatchedPayload),
        headerHash,
        header,
        {
          stateQueueOutRef: "tx#0",
          transactionProjector: IDENTITY_TX_PROJECTOR,
        },
      ),
    ).rejects.toMatchObject({ code: "root_mismatch" });
  });

  it("rejects missing trace and event-to-step members", async () => {
    const { payload } = await makePayloadFixture();
    expect(() =>
      decodeDaPayloadV2Strict(
        SDK.encodeDaPayloadV2({
          ...payload,
          block_body: {
            ...payload.block_body,
            transition_trace: payload.block_body.transition_trace.slice(1),
          },
        }),
      ),
    ).toThrow(/counts/);
    expect(() =>
      decodeDaPayloadV2Strict(
        SDK.encodeDaPayloadV2({
          ...payload,
          block_body: {
            ...payload.block_body,
            event_to_step: payload.block_body.event_to_step.slice(1),
          },
        }),
      ),
    ).toThrow(/event_to_step/);
  });

  it("rejects header payload divergence before attestation", async () => {
    const { payload, headerHash, header } = await makePayloadFixture();
    const divergent = {
      ...payload,
      block_body: {
        ...payload.block_body,
        header: {
          ...payload.block_body.header,
          depositsRoot: "ff".repeat(32),
        },
      },
    };
    await expect(
      verifyDaPayloadAgainstHeader(
        SDK.encodeDaPayloadV2(divergent),
        headerHash,
        header,
        {
          stateQueueOutRef: "tx#0",
          transactionProjector: IDENTITY_TX_PROJECTOR,
        },
      ),
    ).rejects.toMatchObject({ code: "header_hash_mismatch" });
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
