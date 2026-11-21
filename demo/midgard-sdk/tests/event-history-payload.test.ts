import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import { Constr, Data, datumToHash } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  countHistoryDataNodes,
  EventHistoryData,
  EventHistoryPayload,
  prepareEventHistoryPayload,
  prepareEventHistoryPayloadCbor,
} from "../src/user-events/index.js";

const owner = "aa".repeat(28);
const auth = { PublicKeyCredential: [owner] as [string] };
const payload = (datum: Data | null): EventHistoryPayload => ({
  DepositPayload: {
    event: {
      id: { transactionId: "bb".repeat(32), outputIndex: 0n },
      info: {
        l2_address: { paymentCredential: auth, stakeCredential: null },
        l2_network_id: 0n,
        l2_datum: datum,
      },
    },
  },
});
const bounds = {
  inlineLimitBytes: 512n,
  maxPayloadBytes: 15000n,
  maxPayloadNodes: 1024n,
};

describe("authenticated history payload preparation", () => {
  it("plans exact raw Data with duplicate keys and normalized semantic framing", () => {
    const base = Data.to(payload(0n), EventHistoryPayload);
    const raw = replacePlutusConstrFieldCbor(
      base,
      [0, 1, 2, 0],
      "a302c2410101d86682008002c2420001",
    );
    const expected = aikenSerialisedPlutusDataCborPreservingMapOrder(
      replacePlutusConstrFieldCbor(base, [0, 1, 2, 0], "a3020101d879800201"),
    );
    const plan = prepareEventHistoryPayloadCbor(raw, auth, bounds);
    expect(plan.payloadCbor).toBe(expected);
    expect(plutusConstrFieldCbor(plan.payloadCbor, [0, 1, 2, 0])).toBe(
      "a3020101d879800201",
    );
    expect(plan.payloadNodes).toBe(
      countHistoryDataNodes(Data.from(base), bounds.maxPayloadNodes) + 6n,
    );
    expect(plan.payloadNodes).toBe(
      countHistoryDataNodes(
        Data.from(plan.payloadCbor),
        bounds.maxPayloadNodes,
      ) + 2n,
    );
    expect(
      prepareEventHistoryPayloadCbor(raw, auth, {
        ...bounds,
        maxPayloadNodes: plan.payloadNodes,
      }).payloadNodes,
    ).toBe(plan.payloadNodes);
    expect(() =>
      prepareEventHistoryPayloadCbor(raw, auth, {
        ...bounds,
        maxPayloadNodes: plan.payloadNodes - 1n,
      }),
    ).toThrow("Data-node bound");
    expect(
      prepareEventHistoryPayloadCbor(raw, auth, {
        ...bounds,
        inlineLimitBytes: plan.payloadBytes,
      }).kind,
    ).toBe("Inline");
    const external = prepareEventHistoryPayloadCbor(raw, auth, {
      ...bounds,
      inlineLimitBytes: plan.payloadBytes - 1n,
    });
    if (external.kind !== "External") throw new Error("Expected external plan");
    expect(external.key).toBe(plan.key);
    expect(plutusConstrFieldCbor(external.datumCbor, [1])).toBe(
      plan.payloadCbor,
    );
    expect(external.location.External.storage_datum_hash).toBe(
      datumToHash(external.datumCbor),
    );
    const reordered = replacePlutusConstrFieldCbor(
      raw,
      [0, 1, 2, 0],
      "a301d8798002010201",
    );
    expect(
      prepareEventHistoryPayloadCbor(reordered, auth, {
        ...bounds,
        inlineLimitBytes: 1n,
      }).location,
    ).not.toEqual(external.location);
  });

  it("counts constructors, map keys and nested lists consistently with Aiken", () => {
    const data = new Constr(0, [new Map([[1n, ["abcd"]]])]);
    expect(countHistoryDataNodes(data, 5n)).toBe(5n);
    expect(() => countHistoryDataNodes(data, 4n)).toThrow("Data-node bound");
  });
  it("accepts the exact node boundary and stops on wide data", () => {
    expect(
      countHistoryDataNodes(
        Array.from({ length: 1023 }, () => 0n),
        1024n,
      ),
    ).toBe(1024n);
    expect(() =>
      countHistoryDataNodes(
        Array.from({ length: 14000 }, () => 0n),
        1024n,
      ),
    ).toThrow("Data-node bound");
  });
  it("chooses inline at the exact serialized byte bound and external just below it", () => {
    const event = payload(null);
    const size = prepareEventHistoryPayload(event, auth, bounds).payloadBytes;
    expect(
      prepareEventHistoryPayload(event, auth, {
        ...bounds,
        inlineLimitBytes: size,
      }).kind,
    ).toBe("Inline");
    expect(
      prepareEventHistoryPayload(event, auth, {
        ...bounds,
        inlineLimitBytes: size - 1n,
      }).kind,
    ).toBe("External");
  });
  it("binds reclaim authorization in external storage while preserving the event key", () => {
    const event = payload("ab".repeat(4000));
    const first = prepareEventHistoryPayload(event, auth, bounds);
    const second = prepareEventHistoryPayload(
      event,
      { ScriptCredential: ["cc".repeat(28)] },
      bounds,
    );
    expect(first.kind).toBe("External");
    expect(second.kind).toBe("External");
    if (first.kind !== "External" || second.kind !== "External")
      throw new Error("Expected external plans");
    expect(first.key).toBe(second.key);
    expect(first.location.External.storage_datum_hash).not.toBe(
      second.location.External.storage_datum_hash,
    );
    expect(Data.from(first.datumCbor, EventHistoryData)).toEqual(first.datum);
  });
  it("rejects byte and node excess before proposing publication", () => {
    expect(() =>
      prepareEventHistoryPayload(payload("ab".repeat(15000)), auth, bounds),
    ).toThrow("byte bound");
    expect(() =>
      prepareEventHistoryPayload(
        payload(Array.from({ length: 1024 }, () => 0n)),
        auth,
        bounds,
      ),
    ).toThrow("Data-node bound");
  });
});
