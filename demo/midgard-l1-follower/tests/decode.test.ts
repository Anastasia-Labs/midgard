import { readFileSync } from "node:fs";

import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { blake2b256, BlockDecodeError, decodeBlock } from "../src/index.js";
import {
  encodeBlock,
  encodeTxBody,
  encodeWitnessSet,
} from "../src/testing/block-cbor.js";
import * as c from "../src/testing/cbor-writer.js";

const fixture = (name: string): Buffer =>
  Buffer.from(
    readFileSync(new URL(`./fixtures/${name}`, import.meta.url), "utf8").trim(),
    "hex",
  );

const hex = (bytes: Uint8Array): string => Buffer.from(bytes).toString("hex");

describe("decodeBlock on a real Conway mainnet block", () => {
  const raw = fixture("conway-block.hex");
  const block = decodeBlock(raw);
  const cml = CML.Block.from_cbor_bytes(raw);

  it("reads the header point, height and parent", () => {
    expect(hex(block.point.hash)).toBe(
      "27807a70215e3e018eec9be8c619c692e06a78ebcb63daf90d7abe823f3bbf47",
    );
    expect(block.point.slot).toBe(159_835_207);
    expect(block.height).toBe(12_069_665);
    expect(block.parentHash?.toString("hex")).toBe(
      "ff51732269af51a2efaa2a7ad4a2ff5647af5629013a446511249e837be617a0",
    );
  });

  it("keeps each tx's exact body, witness and aux bytes and agrees with CML field by field", () => {
    const bodies = cml.transaction_bodies();
    const witnesses = cml.transaction_witness_sets();
    const invalid = new Set(cml.invalid_transactions());
    const aux = cml.auxiliary_data_set();
    expect(block.txs.length).toBe(bodies.len());
    expect(block.txs.length).toBeGreaterThan(0);
    for (const tx of block.txs) {
      const body = bodies.get(tx.index);
      expect(tx.bodyCbor.toString("hex")).toBe(body.to_cbor_hex());
      expect(tx.hash.toString("hex")).toBe(CML.hash_transaction(body).to_hex());
      expect(tx.hash.equals(blake2b256(tx.bodyCbor))).toBe(true);
      expect(raw.includes(tx.bodyCbor)).toBe(true);
      expect(tx.witnessCbor.toString("hex")).toBe(
        witnesses.get(tx.index).to_cbor_hex(),
      );
      expect(tx.isValid).toBe(!invalid.has(tx.index));
      const auxData = aux.get(tx.index);
      expect(tx.auxCbor?.toString("hex") ?? null).toBe(
        auxData?.to_cbor_hex() ?? null,
      );
      const inputs = body.inputs();
      const cmlInputs = Array.from({ length: inputs.len() }, (_, i) => {
        const input = inputs.get(i);
        return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
      }).sort();
      expect(
        tx.inputs.map((o) => `${o.txHash.toString("hex")}#${o.index}`),
      ).toEqual(cmlInputs);
      const outputs = body.outputs();
      expect(tx.outputs.length).toBe(outputs.len());
      tx.outputs.forEach((output, i) => {
        const cmlOutput = outputs.get(i);
        expect(output.address.toString("hex")).toBe(
          hex(cmlOutput.address().to_raw_bytes()),
        );
        expect(output.lovelace).toBe(cmlOutput.amount().coin());
        const multi = cmlOutput.amount().multi_asset();
        expect(
          [...output.assets.values()].reduce((n, names) => n + names.size, 0),
        ).toBe(
          Array.from(
            { length: multi.keys().len() },
            (_, p) => multi.get_assets(multi.keys().get(p))?.len() ?? 0,
          ).reduce((a, b) => a + b, 0),
        );
        const datum = cmlOutput.datum();
        expect(output.datumHash?.toString("hex") ?? null).toBe(
          datum?.as_hash()?.to_hex() ?? null,
        );
        expect(output.datum !== null).toBe(
          datum?.as_datum() !== undefined && datum.as_datum() !== null,
        );
        const scriptRef = cmlOutput.script_ref();
        expect(output.scriptRef?.hash.toString("hex") ?? null).toBe(
          scriptRef?.hash().to_hex() ?? null,
        );
      });
      expect(tx.collaterals.length).toBe(body.collateral_inputs()?.len() ?? 0);
      expect(tx.referenceInputs.length).toBe(
        body.reference_inputs()?.len() ?? 0,
      );
      expect(tx.mint.size).toBe(body.mint()?.keys().len() ?? 0);
      expect(tx.invalidAfter ?? undefined).toBe(body.ttl());
      expect(tx.invalidBefore ?? undefined).toBe(
        body.validity_interval_start(),
      );
      expect(tx.redeemers.length).toBe(
        (() => {
          const redeemers = witnesses.get(tx.index).redeemers();
          if (redeemers === undefined) return 0;
          const legacy = redeemers.as_arr_legacy_redeemer();
          return legacy !== undefined
            ? legacy.len()
            : (redeemers.as_map_redeemer_key_to_redeemer_val()?.keys().len() ??
                0);
        })(),
      );
    }
  });
});

describe("decodeBlock on the devnet origin block", () => {
  it("decodes a genesis-successor block with no parent hash", () => {
    const block = decodeBlock(fixture("devnet-origin-block.hex"));
    expect(block.point.hash.toString("hex")).toBe(
      "cc1bb6f53517ad80ee0ffd405f1d50f4a8784b5f42bc494015d1e87720df3b96",
    );
    expect(block.point.slot).toBe(47);
    expect(block.height).toBe(0);
    expect(block.parentHash).toBeNull();
  });
});

/** A hand-built block whose tx body uses indefinite and non-minimal encodings. */
const handBuiltBlock = () => {
  const inputHash = Buffer.alloc(32, 0xab);
  const address = Buffer.concat([Buffer.from([0x70]), Buffer.alloc(28, 0x11)]);
  const policy = Buffer.alloc(28, 0x22);
  const body = c.mapIndef(
    [
      c.uint(0),
      c.tag(
        258,
        c.arrayIndef(
          c.array(c.bytes(inputHash), c.uint(1, 2)),
          c.array(c.bytes(inputHash), c.uint(0)),
        ),
      ),
    ],
    [
      c.uint(1),
      c.arrayIndef(
        c.mapIndef(
          [
            c.uint(0),
            c.bytesIndef(address.subarray(0, 10), address.subarray(10)),
          ],
          [
            c.uint(1),
            c.array(
              c.uint(2_000_000, 4),
              c.map([
                c.bytes(policy),
                c.map([c.bytes(Buffer.from("tok")), c.uint(5)]),
              ]),
            ),
          ],
          [
            c.uint(2),
            c.array(
              c.uint(1),
              c.tag(24, c.bytes(Buffer.from([0xd8, 0x79, 0x9f, 0xff]))),
            ),
          ],
        ),
        c.array(c.bytes(address), c.uint(3_000_000)),
      ),
    ],
    [c.uint(2), c.uint(170_000)],
    [c.uint(3, 4), c.uint(99_999)],
    [
      c.uint(9),
      c.map([
        c.bytes(policy),
        c.map([c.bytes(Buffer.from("tok")), c.nint(-2)]),
      ]),
    ],
  );
  const witness = c.map([
    c.uint(5),
    c.map([
      c.array(c.uint(0), c.uint(0)),
      c.array(c.arrayIndef(c.uint(7)), c.array(c.uint(1), c.uint(2))),
    ]),
  ]);
  const headerBody = c.array(
    c.uint(77),
    c.uint(123_456),
    c.bytes(Buffer.alloc(32, 0x01)),
    c.bytes(Buffer.alloc(32)),
  );
  const header = c.array(headerBody, c.bytes(Buffer.alloc(64)));
  const raw = c.array(
    header,
    c.arrayIndef(body),
    c.array(witness),
    c.map(),
    c.array(),
  );
  return { raw, body, witness, header, inputHash, policy };
};

describe("decodeBlock on non-canonical encodings", () => {
  it("keeps the exact body slice and reads every field", () => {
    const { raw, body, witness, header, inputHash, policy } = handBuiltBlock();
    const block = decodeBlock(raw);
    expect(block.point.hash.equals(blake2b256(header))).toBe(true);
    expect(block.height).toBe(77);
    expect(block.point.slot).toBe(123_456);
    const [tx] = block.txs;
    expect(tx?.bodyCbor.equals(body)).toBe(true);
    expect(tx?.witnessCbor.equals(witness)).toBe(true);
    expect(tx?.hash.equals(blake2b256(body))).toBe(true);
    expect(tx?.hash.toString("hex")).toBe(
      CML.hash_transaction(CML.TransactionBody.from_cbor_bytes(body)).to_hex(),
    );
    expect(tx?.inputs).toEqual([
      { txHash: inputHash, index: 0 },
      { txHash: inputHash, index: 1 },
    ]);
    expect(tx?.outputs[0]?.lovelace).toBe(2_000_000n);
    expect(
      tx?.outputs[0]?.assets
        .get(policy.toString("hex"))
        ?.get(Buffer.from("tok").toString("hex")),
    ).toBe(5n);
    expect(tx?.outputs[0]?.datum?.toString("hex")).toBe("d8799fff");
    expect(tx?.outputs[1]?.lovelace).toBe(3_000_000n);
    expect(
      tx?.mint
        .get(policy.toString("hex"))
        ?.get(Buffer.from("tok").toString("hex")),
    ).toBe(-2n);
    expect(tx?.invalidAfter).toBe(99_999n);
    expect(tx?.redeemers).toEqual([
      { purpose: "spend", index: 0, data: c.arrayIndef(c.uint(7)) },
    ]);
    expect(tx?.isValid).toBe(true);
  });

  it("refuses truncated, trailing and inconsistent blocks with BlockDecodeError", () => {
    const { raw } = handBuiltBlock();
    expect(() => decodeBlock(raw.subarray(0, raw.length - 3))).toThrow(
      BlockDecodeError,
    );
    expect(() =>
      decodeBlock(Buffer.concat([raw, Buffer.from([0x00])])),
    ).toThrow(BlockDecodeError);
    const real = fixture("conway-block.hex");
    expect(() =>
      decodeBlock(real.subarray(0, Math.floor(real.length / 2))),
    ).toThrow(BlockDecodeError);
    const mismatched = c.array(
      c.array(c.array(c.uint(1), c.uint(2), c.nul), c.bytes(Buffer.alloc(1))),
      c.array(c.map()),
      c.array(),
      c.map(),
      c.array(),
    );
    expect(() => decodeBlock(mismatched)).toThrow(
      /body and witness counts differ/u,
    );
  });

  it("skips a deeply nested datum without exhausting the stack", () => {
    const depth = 200_000;
    const deep = Buffer.concat([
      Buffer.alloc(depth, 0x81),
      Buffer.from([0x00]),
    ]);
    const address = Buffer.concat([
      Buffer.from([0x70]),
      Buffer.alloc(28, 0x11),
    ]);
    const body = c.map(
      [c.uint(0), c.array(c.array(c.bytes(Buffer.alloc(32, 1)), c.uint(0)))],
      [
        c.uint(1),
        c.array(
          c.map(
            [c.uint(0), c.bytes(address)],
            [c.uint(1), c.uint(1)],
            [c.uint(2), c.array(c.uint(1), c.tag(24, c.bytes(deep)))],
          ),
        ),
      ],
      [c.uint(2), c.uint(1)],
    );
    const witness = c.map([
      c.uint(5),
      c.array(
        c.array(c.uint(0), c.uint(0), deep, c.array(c.uint(1), c.uint(1))),
      ),
    ]);
    const header = c.array(
      c.array(c.uint(1), c.uint(2), c.nul),
      c.bytes(Buffer.alloc(1)),
    );
    const block = decodeBlock(
      c.array(header, c.array(body), c.array(witness), c.map(), c.array()),
    );
    expect(block.txs[0]?.redeemers[0]?.data.length).toBe(deep.length);
  });
});

describe("decodeBlock phase-2 failures and reference scripts", () => {
  it("marks invalid txs, reads the collateral return and hashes reference scripts like CML", () => {
    const address = Buffer.concat([
      Buffer.from([0x00]),
      Buffer.alloc(28, 0x33),
      Buffer.alloc(28, 0x44),
    ]);
    const plutus = Buffer.from("4e4d01000033222220051200120011", "hex");
    const native = c.array(c.uint(0), c.bytes(Buffer.alloc(28, 0x55)));
    const output = (script: Buffer) =>
      c.map(
        [c.uint(0), c.bytes(address)],
        [c.uint(1), c.uint(5_000_000)],
        [c.uint(3), c.tag(24, c.bytes(script))],
      );
    const plutusOutput = output(c.array(c.uint(3), c.bytes(plutus)));
    const nativeOutput = output(c.array(c.uint(0), native));
    const input = c.array(c.bytes(Buffer.alloc(32, 9)), c.uint(0));
    const body = (outputs: Buffer[]) =>
      c.map(
        [c.uint(0), c.array(input)],
        [c.uint(1), c.array(...outputs)],
        [c.uint(2), c.uint(1)],
        [c.uint(13), c.array(input)],
        [c.uint(16), c.array(c.bytes(address), c.uint(4_000_000))],
      );
    const header = c.array(
      c.array(c.uint(5), c.uint(50), c.bytes(Buffer.alloc(32, 2))),
      c.bytes(Buffer.alloc(1)),
    );
    const bodies = [body([plutusOutput]), body([nativeOutput])];
    const block = decodeBlock(
      c.array(
        header,
        c.array(...bodies),
        c.array(c.map(), c.map()),
        c.map([c.uint(1), c.map()]),
        c.array(c.uint(1)),
      ),
    );
    expect(block.txs.map((tx) => tx.isValid)).toEqual([true, false]);
    expect(block.txs[1]?.auxCbor?.toString("hex")).toBe("a0");
    expect(block.txs[1]?.collateralReturn?.lovelace).toBe(4_000_000n);
    expect(block.txs[1]?.collaterals).toHaveLength(1);
    expect(block.txs[0]?.outputs[0]?.stakeCredential?.toString("hex")).toBe(
      Buffer.alloc(28, 0x44).toString("hex"),
    );
    for (const [tx, encoded] of [
      [block.txs[0], plutusOutput],
      [block.txs[1], nativeOutput],
    ] as const) {
      const scriptRef = tx?.outputs[0]?.scriptRef;
      expect(scriptRef?.hash.toString("hex")).toBe(
        CML.TransactionOutput.from_cbor_bytes(encoded)
          .script_ref()
          ?.hash()
          .to_hex(),
      );
    }
    expect(block.txs[0]?.outputs[0]?.scriptRef?.type).toBe("plutus_v3");
    expect(block.txs[0]?.outputs[0]?.scriptRef?.bytes.equals(plutus)).toBe(
      true,
    );
    expect(block.txs[1]?.outputs[0]?.scriptRef?.type).toBe("native");
    expect(block.txs[1]?.outputs[0]?.scriptRef?.bytes.equals(native)).toBe(
      true,
    );
  });
});

describe("decodeBlock on simulator redeemers", () => {
  it("reads the simulator's Conway-map redeemers, which CML also reads, with a script_data_hash in the body", () => {
    const data = c.array(c.uint(7), c.bytes(Buffer.from("ab", "hex")));
    const plain = {
      inputs: [{ txHash: Buffer.alloc(32, 1), index: 0 }],
      outputs: [{ address: Buffer.alloc(29, 0x70), lovelace: 2_000_000n }],
      nonce: 1,
    };
    const redeemed = {
      ...plain,
      redeemers: [
        { purpose: "mint" as const, index: 0, data },
        { purpose: "spend" as const, index: 2, data: c.uint(5) },
      ],
    };
    const encoded = encodeBlock({
      height: 1,
      slot: 10,
      prevHash: null,
      branch: 0,
      txs: [plain, redeemed],
    });
    const block = decodeBlock(encoded.raw);
    expect(block.txs[0]?.redeemers).toEqual([]);
    expect(
      block.txs[1]?.redeemers.map((r) => [r.purpose, r.index, hex(r.data)]),
    ).toEqual([
      ["mint", 0, hex(data)],
      ["spend", 2, hex(c.uint(5))],
    ]);
    expect(block.txs[1]?.hash.equals(block.txs[0]?.hash as Buffer)).toBe(false);
    expect(
      CML.TransactionBody.from_cbor_bytes(
        encodeTxBody(plain),
      ).script_data_hash(),
    ).toBeUndefined();
    expect(
      CML.TransactionBody.from_cbor_bytes(encodeTxBody(redeemed))
        .script_data_hash()
        ?.to_hex(),
    ).toHaveLength(64);
    const witness = CML.TransactionWitnessSet.from_cbor_bytes(
      encodeWitnessSet(redeemed),
    );
    expect(
      witness.redeemers()?.as_map_redeemer_key_to_redeemer_val()?.keys().len(),
    ).toBe(2);
  });
});
