/**
 * Raw ledger outputs whose multi-asset value repeats a policy id or an asset
 * name. The core encoders build values from maps and cannot repeat a key, so
 * these bytes are written by hand: `{0: address, 1: [lovelace, assets]}` with
 * every head in its shortest form and every key in the order given.
 */

type Asset = readonly [name: Buffer, quantity: bigint];
type Policy = readonly [id: Buffer, assets: readonly Asset[]];

const head = (major: number, value: bigint): Buffer => {
  const type = major << 5;
  if (value < 24n) return Buffer.from([type | Number(value)]);
  if (value < 0x100n) return Buffer.from([type | 24, Number(value)]);
  if (value < 0x1_0000n) {
    const bytes = Buffer.alloc(3);
    bytes[0] = type | 25;
    bytes.writeUInt16BE(Number(value), 1);
    return bytes;
  }
  if (value < 0x1_0000_0000n) {
    const bytes = Buffer.alloc(5);
    bytes[0] = type | 26;
    bytes.writeUInt32BE(Number(value), 1);
    return bytes;
  }
  const bytes = Buffer.alloc(9);
  bytes[0] = type | 27;
  bytes.writeBigUInt64BE(value, 1);
  return bytes;
};

const byteString = (bytes: Buffer): Buffer =>
  Buffer.concat([head(2, BigInt(bytes.length)), bytes]);

/** An enterprise key address; its key hash byte varies the owner. */
export const enterpriseAddress = (byte = 0x47): Buffer =>
  Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, byte)]);

/** A policy id whose last two bytes hold `index`, so ids ascend with it. */
export const policyId = (index: number, fill = 0x11): Buffer => {
  const id = Buffer.alloc(28, fill);
  id.writeUInt16BE(index, 26);
  return id;
};

/** One byte name per index 0..255; index -1 is the empty name. */
export const assetName = (index: number): Buffer =>
  index < 0 ? Buffer.alloc(0) : Buffer.from([index]);

/** The output bytes for `policies`, in exactly the order given. */
export const rawMultiAssetOutput = ({
  address = enterpriseAddress(),
  lovelace = 2_000_000n,
  policies,
}: {
  readonly address?: Buffer;
  readonly lovelace?: bigint;
  readonly policies: readonly Policy[];
}): Buffer =>
  Buffer.concat([
    Buffer.from([0xa2, 0x00]),
    byteString(address),
    Buffer.from([0x01, 0x82]),
    head(0, lovelace),
    head(5, BigInt(policies.length)),
    ...policies.flatMap(([id, assets]) => [
      byteString(id),
      head(5, BigInt(assets.length)),
      ...assets.flatMap(([name, quantity]) => [
        byteString(name),
        head(0, quantity),
      ]),
    ]),
  ]);

/** Two policies, the second repeating the first's id. */
export const duplicatePolicyOutput = (lovelace?: bigint): Buffer =>
  rawMultiAssetOutput({
    lovelace,
    policies: [
      [policyId(1), [[assetName(1), 1n]]],
      [policyId(1), [[assetName(2), 1n]]],
    ],
  });

/** One policy whose second asset repeats the first's name. */
export const duplicateAssetNameOutput = (lovelace?: bigint): Buffer =>
  rawMultiAssetOutput({
    lovelace,
    policies: [
      [
        policyId(1),
        [
          [assetName(1), 1n],
          [assetName(1), 2n],
        ],
      ],
    ],
  });

/** The honest twin: distinct policies, distinct names, both ascending. */
export const distinctAscendingOutput = (lovelace?: bigint): Buffer =>
  rawMultiAssetOutput({
    lovelace,
    policies: [
      [
        policyId(1),
        [
          [assetName(1), 1n],
          [assetName(2), 2n],
        ],
      ],
      [policyId(2), [[assetName(2), 1n]]],
    ],
  });

/**
 * An output of exactly `targetBytes` holding as many assets as fit: full
 * policies of 257 names (the empty name, then every one-byte name), then a
 * partial last policy. With `duplicateLast` the final asset repeats the name
 * before it; otherwise it takes the next name, so both shapes have the same
 * length and differ only in that one byte. Leftover bytes widen leading
 * quantities from one byte to two.
 */
export const maximumAssetOutput = ({
  targetBytes,
  duplicateLast,
  lovelace = 2_000_000n,
}: {
  readonly targetBytes: number;
  readonly duplicateLast: boolean;
  readonly lovelace?: bigint;
}): Buffer => {
  const build = (assetCount: number, widened: number) => {
    const policies: Policy[] = [];
    let remaining = assetCount;
    let asset = 0;
    for (let index = 0; remaining > 0; index += 1) {
      const count = Math.min(257, remaining);
      remaining -= count;
      const assets: Asset[] = [];
      for (let name = -1; name < count - 1; name += 1) {
        assets.push([assetName(name), asset < widened ? 24n : 1n]);
        asset += 1;
      }
      policies.push([policyId(index), assets]);
    }
    const last = policies.at(-1)![1] as Asset[];
    if (duplicateLast) last[last.length - 1] = [last[last.length - 2]![0], 1n];
    return rawMultiAssetOutput({ lovelace, policies });
  };
  // The last policy keeps at least three assets, so the repeated name is a
  // one-byte name the same width as the name it replaces.
  let low = 3;
  let high = 1 << 16;
  while (low < high) {
    const middle = Math.ceil((low + high) / 2);
    if (build(middle, 0).length <= targetBytes) low = middle;
    else high = middle - 1;
  }
  let assetCount = low;
  while (assetCount % 257 < 3) assetCount -= 1;
  const shortfall = targetBytes - build(assetCount, 0).length;
  const output = build(assetCount, shortfall);
  if (output.length !== targetBytes)
    throw new Error(
      `maximum asset output is ${output.length.toString()} bytes, not ${targetBytes.toString()}`,
    );
  return output;
};
