import "@al-ft/midgard-core/codec";
import "@al-ft/midgard-core/da-payload-envelope";
import "@al-ft/midgard-sdk";
import "vitest";
import "../src/da/payload.js";
import "../src/da/source.js";
import "./helpers.js";

import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  computeDaPayloadUtxosRoot,
  ParentStateUnavailableError,
  resolvePreBlockUtxos,
} from "../src/da/payload.js";
import type { DaPayloadSource } from "../src/da/source.js";
import { makePayloadFixture, payloadSourceFromBytes } from "./helpers.js";

const PARENT_HASH = "11".repeat(28);

const parentUtxos: readonly SDK.DaPayloadEntry[] = [
  [
    encodeMidgardSpendInputItem({
      txId: Buffer.alloc(32, 7),
      outputIndex: 0,
    }).toString("hex"),
    encodeMidgardTxOutput({
      address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 9)]),
      value: { lovelace: 2_000_000n, assets: new Map() },
    }).toString("hex"),
  ],
];

/** A parent payload whose post-state is `utxos`. */
const parentPayloadCbor = async (
  utxos: readonly SDK.DaPayloadEntry[],
): Promise<Buffer> => {
  const { payload } = await makePayloadFixture(1);
  return wrapDaPayload(
    SDK.encodeDaPayload({
      ...payload,
      block_body: { ...payload.block_body, utxos: [...utxos] },
    }),
    { mode: "identity" },
  );
};

const childHeader = async (
  prevUtxosRoot: string,
  prevHeaderHash = PARENT_HASH,
): Promise<SDK.Header> => ({
  ...(await makePayloadFixture(1)).header,
  prevHeaderHash,
  prevUtxosRoot,
});

const noPeers: DaPayloadSource = {
  fetchPayloadCandidates: async () => ({ ok: false, attempts: [] }),
};

/**
 * The state before a block is the parent block's post-state, accepted only
 * when its root is the block's `prev_utxos_root`; the first block starts from
 * the empty set.
 */
describe("the UTxO set immediately before a block", () => {
  it("is the empty set before the first block", async () => {
    const header = await childHeader(
      await computeDaPayloadUtxosRoot([]),
      SDK.GENESIS_HEADER_HASH,
    );
    await expect(
      resolvePreBlockUtxos({
        header,
        getDaPayload: async () => undefined,
        payloadSource: noPeers,
      }),
    ).resolves.toEqual([]);
  });

  it("refuses a first block whose prev_utxos_root is not the empty set's", async () => {
    const header = await childHeader(
      await computeDaPayloadUtxosRoot(parentUtxos),
      SDK.GENESIS_HEADER_HASH,
    );
    await expect(
      resolvePreBlockUtxos({
        header,
        getDaPayload: async () => undefined,
        payloadSource: noPeers,
      }),
    ).rejects.toBeInstanceOf(ParentStateUnavailableError);
  });

  it("is the retained parent payload's post-state when its root matches", async () => {
    const header = await childHeader(
      await computeDaPayloadUtxosRoot(parentUtxos),
    );
    const retained = (await parentPayloadCbor(parentUtxos)).toString("hex");
    const requested: string[] = [];
    const utxos = await resolvePreBlockUtxos({
      header,
      getDaPayload: async (headerHash) => {
        requested.push(headerHash);
        return { payloadCborHex: retained };
      },
      payloadSource: noPeers,
    });
    expect(requested).toEqual([PARENT_HASH]);
    expect(
      utxos.map(([key, value]) => [key, Buffer.from(value).toString("hex")]),
    ).toEqual(parentUtxos);
  });

  it("fetches the parent from peers when the retained payload's root does not match", async () => {
    const header = await childHeader(
      await computeDaPayloadUtxosRoot(parentUtxos),
    );
    const utxos = await resolvePreBlockUtxos({
      header,
      getDaPayload: async () => ({
        payloadCborHex: (await parentPayloadCbor([])).toString("hex"),
      }),
      payloadSource: payloadSourceFromBytes(
        await parentPayloadCbor(parentUtxos),
      ),
    });
    expect(utxos.map(([key]) => key)).toEqual(parentUtxos.map(([key]) => key));
  });

  it("is unavailable when no retained or fetched parent carries the bound root", async () => {
    const header = await childHeader(
      await computeDaPayloadUtxosRoot(parentUtxos),
    );
    await expect(
      resolvePreBlockUtxos({
        header,
        getDaPayload: async () => undefined,
        payloadSource: payloadSourceFromBytes(await parentPayloadCbor([])),
      }),
    ).rejects.toBeInstanceOf(ParentStateUnavailableError);
    await expect(
      resolvePreBlockUtxos({
        header,
        getDaPayload: async () => undefined,
        payloadSource: noPeers,
      }),
    ).rejects.toBeInstanceOf(ParentStateUnavailableError);
  });
});
