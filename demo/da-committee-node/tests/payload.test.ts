import {
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardNativeTxCanonical,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core/codec";
import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS_V1 } from "@al-ft/midgard-core/da-transport";
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
  it("does not misclassify canonical raw DaPayloadV2 when schema is omitted", async () => {
    const { payloadCbor } = await makePayloadFixture();
    const unwrapped = await unwrapDaPayload(payloadCbor, {
      maxPayloadBytes: DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
    });
    expect(unwrapped.schemaVersion).toBe(2);
    expect(unwrapped.innerBytes).toEqual(payloadCbor);
  });

  it("strictly decodes and verifies roots against the L1 header", async () => {
    const { payloadCbor, header, headerHash } = await makePayloadFixture();
    const stages: string[] = [];
    let now = 0;
    const timing = {
      monotonicNow: () => {
        now += 1;
        return now;
      },
      onStageTiming: (stage: string, durationMs: number) => {
        expect(durationMs).toBe(1);
        stages.push(stage);
      },
    };
    const decoded = decodeDaPayloadV2Strict(payloadCbor, timing);
    expect(decoded.block_body.header_hash).toBe(headerHash);
    expect(decoded.block_body.forced_transactions).toHaveLength(1);
    const verified = await verifyDaPayloadAgainstHeader(
      payloadCbor,
      headerHash,
      header,
      {
        stateQueueOutRef: "tx#0",
        transactionProjector: IDENTITY_TX_PROJECTOR,
        timing,
      },
    );
    expect(verified.validation.rootsMatch).toBe(true);
    expect(verified.roots.utxosRoot).toBe(header.utxosRoot);
    expect(verified.roots.transitionTraceRoot).toBe(header.transitionTraceRoot);
    expect(verified.roots.forcedTransactionsRoot).toBe(
      header.forcedTransactionsRoot,
    );
    expect(verified.counts.totalEventCount).toBe(header.totalEventCount);
    expect(stages).toEqual([
      "inner_decode",
      "payload_structure_validation",
      "stored_hash",
      "inner_decode",
      "payload_structure_validation",
      "semantic_validation",
    ]);
  });

  it("strictly rejects non-minimal inner payload CBOR without re-encoding", async () => {
    const { payloadCbor } = await makePayloadFixture();
    const nonMinimalVersion = Buffer.concat([
      payloadCbor.subarray(0, 3),
      Buffer.from([0x18, 0x02]),
      payloadCbor.subarray(4),
    ]);

    try {
      decodeDaPayloadV2Strict(nonMinimalVersion);
      throw new Error("expected strict decode to reject");
    } catch (error) {
      expect(error).toBeInstanceOf(DaPayloadValidationError);
      expect((error as DaPayloadValidationError).code).toBe("non_canonical");
    }
  });

  it("keeps strict validation semantics when the optional timing clock throws", async () => {
    const { payloadCbor } = await makePayloadFixture();
    expect(() =>
      decodeDaPayloadV2Strict(payloadCbor, {
        monotonicNow: () => {
          throw new Error("clock unavailable");
        },
        onStageTiming: () => {
          throw new Error("sink unavailable");
        },
      }),
    ).not.toThrow();
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
      sourcePeerId: "a",
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

  it("does not let missing DA status erase stored payload bytes", async () => {
    const dir = await tempDir();
    const store = await JsonFileWatcherStore.open(dir);
    const stored = await store.saveDaPayload({
      deploymentFingerprint: "dep",
      headerHash: "01".repeat(28),
      payloadCborHex: "aa",
      payloadSha256: "11".repeat(32),
      sourcePeerId: "libp2p:payload-submit",
      fetchedAt: "2026-06-13T00:00:01.000Z",
      validationStatus: "fetched",
    });
    const missing = await store.saveDaPayload({
      deploymentFingerprint: "dep",
      headerHash: stored.headerHash,
      payloadCborHex: "",
      payloadSha256: "",
      sourcePeerId: "",
      fetchedAt: "2026-06-13T00:00:02.000Z",
      validationStatus: "missing_da",
      validationError: "producer:transport_error",
    });

    expect(missing).toEqual(stored);
    await expect(store.getDaPayload(stored.headerHash)).resolves.toEqual(
      stored,
    );
  });

  it("does not downgrade verified payload bytes to fetched", async () => {
    const dir = await tempDir();
    const store = await JsonFileWatcherStore.open(dir);
    const verified = await store.saveDaPayload({
      deploymentFingerprint: "dep",
      headerHash: "01".repeat(28),
      payloadCborHex: "aa",
      payloadSha256: "11".repeat(32),
      sourcePeerId: "libp2p:payload-submit",
      fetchedAt: "2026-06-13T00:00:01.000Z",
      verifiedAt: "2026-06-13T00:00:02.000Z",
      validationStatus: "verified",
      payloadFetchStatus: "available",
    });

    const fetched = await store.saveDaPayload({
      ...verified,
      validationStatus: "fetched",
    });

    expect(fetched).toEqual(verified);
    await expect(store.getDaPayload(verified.headerHash)).resolves.toEqual(
      verified,
    );
  });
});
