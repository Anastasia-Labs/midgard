import { readAdmittedLocalKupmiosReferenceBodiesAtPoint } from "@al-ft/midgard-fault-proofs";
import { assetsToValue, CML } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import { createWatcherLocalKupmiosRawSource } from "../../src/l1/local-kupmios-raw-source.js";
import { createSyntheticUserEventOriginFixture } from "../support/user-event-origin-fixture.js";

const policy = "d1".repeat(28);
const unit = `${policy}.abcd`;
const address = CML.Address.from_raw_bytes(
  Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0xe1)]),
).to_bech32();

const makeTransaction = (inputHashes: readonly string[] = [], fee = 1n) => {
  const inputs = CML.TransactionInputList.new();
  for (const hash of inputHashes) {
    inputs.add(
      CML.TransactionInput.new(CML.TransactionHash.from_hex(hash), 0n),
    );
  }
  const datum = CML.PlutusData.from_cbor_hex("9f01ff");
  const datumHash = CML.hash_plutus_data(datum).to_hex();
  const inlineCbor = datum.to_cbor_hex();
  const nativeScript = CML.NativeScript.new_script_all(
    CML.NativeScriptList.new(),
  );
  const script = CML.Script.new_native(nativeScript);
  const scriptHash = script.hash().to_hex();
  const scriptCbor = nativeScript.to_cbor_hex();
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(address),
      assetsToValue({ lovelace: 3_000_000n, [`${policy}abcd`]: 7n }),
      CML.DatumOption.new_datum(datum),
      script,
    ),
  );
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(address),
      CML.Value.from_coin(2_000_000n),
      CML.DatumOption.new_hash(CML.DatumHash.from_hex(datumHash)),
    ),
  );
  const body = CML.TransactionBody.new(inputs, outputs, fee);
  const transaction = CML.Transaction.new(
    body,
    CML.TransactionWitnessSet.new(),
    true,
  );
  return {
    txHash: CML.hash_transaction(body).to_hex(),
    cbor: transaction.to_cbor_hex(),
    datumHash,
    inlineCbor,
    scriptHash,
    scriptCbor,
  };
};

const matches = async (pattern: string) => {
  const response = await fetch(
    `http://127.0.0.1:1442/matches/${encodeURIComponent(pattern)}`,
  );
  expect(response.headers.get("X-Most-Recent-Checkpoint")).toBe("999999");
  return (await response.json()) as readonly {
    readonly transaction_id: string;
    readonly output_index: number;
    readonly spent_at: unknown;
  }[];
};

describe("synthetic user-event origin query indexes", () => {
  it("keeps all pre-initialization reference heights before the native chain", async () => {
    const template = await createSyntheticUserEventOriginFixture();
    const deployment = template.deployment;
    const initialization = template.activationTransactionCbor;
    await template.close();
    const creating = Array.from({ length: 80 }, (_, index) =>
      makeTransaction([], BigInt(index + 1)),
    );
    const fixture = await createSyntheticUserEventOriginFixture({
      published: {
        deployment,
        transactionCbor: initialization,
        creatingTransactions: creating.map(({ cbor }) => ({
          transactionCbor: cbor,
        })),
      },
    });
    try {
      const referenced = creating.at(-1)!;
      const body = CML.Transaction.from_cbor_hex(
        makeTransaction([], 1000n).cbor,
      ).body();
      const references = CML.TransactionInputList.new();
      references.add(
        CML.TransactionInput.new(
          CML.TransactionHash.from_hex(referenced.txHash),
          0n,
        ),
      );
      body.set_reference_inputs(references);
      const target = await fixture.makeBlock({
        transactions: [
          CML.Transaction.new(
            body,
            CML.TransactionWitnessSet.new(),
            true,
          ).to_cbor_hex(),
        ],
      });
      const source = createWatcherLocalKupmiosRawSource({
        watcherConfig: fixture.watcherConfig,
        deploymentIdentity: fixture.deploymentIdentity,
      });
      const captured = await readAdmittedLocalKupmiosReferenceBodiesAtPoint({
        source,
        point: target.point,
      });
      expect(captured.creatingTransactionBodies).toEqual([
        CML.Transaction.from_cbor_hex(referenced.cbor).body().to_cbor_hex(),
      ]);
      expect(BigInt(target.parentPoint.blockNo) + 1n).toBe(
        BigInt(target.point.blockNo),
      );
      expect(BigInt(fixture.activationBlock.point.blockNo)).toBeGreaterThan(
        2n * BigInt(creating.length),
      );
    } finally {
      await fixture.close();
    }
  });

  it("preserves exact metadata and serves repeated queries without CML decoding", async () => {
    const originalFetch = globalThis.fetch;
    const fixture = await createSyntheticUserEventOriginFixture();
    try {
      const first = makeTransaction();
      const second = makeTransaction([], 2n);
      const block = await fixture.makeBlock({
        transactions: [first.cbor, second.cbor],
      });
      const expected = {
        transaction_index: 0,
        transaction_id: first.txHash,
        output_index: 0,
        address,
        value: { coins: "3000000", assets: { [unit]: "7" } },
        datum_hash: first.datumHash,
        datum_type: "inline",
        script_hash: first.scriptHash,
        created_at: {
          slot_no: Number(block.point.slot),
          header_hash: block.point.blockHash,
        },
        spent_at: null,
        datum: first.inlineCbor,
        script: { language: "native", script: first.scriptCbor },
      };
      const decode = vi.spyOn(CML.Transaction, "from_cbor_hex");
      try {
        expect(vi.isMockFunction(globalThis.fetch)).toBe(false);
        expect(await matches(`0@${first.txHash}`)).toEqual([expected]);
        expect(await matches(`*@${first.txHash}`)).toEqual([
          expected,
          {
            ...expected,
            output_index: 1,
            value: { coins: "2000000", assets: {} },
            datum_type: "hash",
            datum: null,
            script_hash: null,
            script: null,
          },
        ]);
        for (let query = 0; query < 64; query++) {
          expect(await matches(unit)).toEqual([
            expected,
            {
              ...expected,
              transaction_index: 1,
              transaction_id: second.txHash,
            },
          ]);
          expect(await matches(address)).toHaveLength(4);
        }
        expect(await matches(`${"d2".repeat(28)}.`)).toEqual([]);
        await expect(matches(`0@${"ee".repeat(32)}`)).rejects.toThrow(
          "no requested creating frame",
        );
        await expect(matches(`2@${first.txHash}`)).rejects.toThrow(
          "no requested output",
        );
        expect(decode).not.toHaveBeenCalled();
      } finally {
        decode.mockRestore();
      }
    } finally {
      await fixture.close();
    }
    expect(globalThis.fetch).toBe(originalFetch);
  });

  it("indexes append-time consumption while exact outref queries retain the unspent view", async () => {
    const fixture = await createSyntheticUserEventOriginFixture({
      nativeTipMode: "controlled",
    });
    try {
      const created = makeTransaction();
      const creation = await fixture.makeBlock({
        transactions: [created.cbor],
      });
      await fixture.setNativeTip(creation.point);
      // Input coordinates refer to the actual ordered transaction input list.
      const spending = makeTransaction(["00".repeat(32), created.txHash], 3n);
      const spent = await fixture.appendNativeBlock({
        transactions: [spending.cbor],
        slot: Number(creation.point.slot) + 1,
      });
      const expectedSpent = {
        transaction_id: spending.txHash,
        input_index: 1,
        slot_no: Number(spent.point.slot),
        header_hash: spent.point.blockHash,
      };
      const row = (await matches(unit)).find(
        (value: { transaction_id: string }) =>
          value.transaction_id === created.txHash,
      );
      expect(row?.spent_at).toEqual(expectedSpent);
      expect(
        (await matches(address)).find(
          (value: { transaction_id: string; output_index: number }) =>
            value.transaction_id === created.txHash && value.output_index === 0,
        )?.spent_at,
      ).toEqual(expectedSpent);
      expect((await matches(`0@${created.txHash}`))[0]?.spent_at).toBeNull();
      // The rollback command affects native stream delivery, not the fixture's
      // retained query history; this is the same behavior as before indexing.
      await fixture.rollbackNativeStream(creation.point);
      expect((await matches(unit))[0]).toEqual(row);
    } finally {
      await fixture.close();
    }
  });

  it("still refuses duplicate consumption in history without changing exact queries", async () => {
    const fixture = await createSyntheticUserEventOriginFixture();
    try {
      const created = makeTransaction();
      await fixture.makeBlock({ transactions: [created.cbor] });
      const spending = makeTransaction([created.txHash], 4n);
      await fixture.makeBlock({ transactions: [spending.cbor] });
      // A duplicate creating transaction must still index its second inclusion.
      await fixture.makeBlock({ transactions: [spending.cbor] });
      await expect(matches(unit)).rejects.toThrow("duplicate consumption");
      await expect(matches(address)).rejects.toThrow("duplicate consumption");
      expect((await matches(`0@${created.txHash}`))[0]?.spent_at).toBeNull();
    } finally {
      await fixture.close();
    }
  });
});
