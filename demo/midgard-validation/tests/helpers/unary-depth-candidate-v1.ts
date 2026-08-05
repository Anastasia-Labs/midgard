import { computeHash32 } from "@al-ft/midgard-core";
import { CML } from "@lucid-evolution/lucid";

/**
 * Raw (CML-free) builder for the canonical V1 unary-depth Cardano candidate.
 *
 * Extracted from `plutus-data-unary-depth-boundary-v1.test.ts` so the boundary
 * suite and the out-of-process maximum-depth admission share one builder. The
 * transaction is assembled byte by byte rather than through CML precisely so
 * that constructing the candidate never depends on CML's recursive decoder —
 * that dependency is what made the maximum unreachable in the first place, and
 * the boundary suite pins the depth-one output against CML byte for byte.
 */

const unaryConstructorPrefixHex = "d8799f";
const unaryConstructorSuffixHex = "ff";
const unaryLeafHex = "00";

export const cardanoUnaryConstructorDataCborV1 = (depth: number): string => {
  if (!Number.isSafeInteger(depth) || depth <= 0) {
    throw new Error("Cardano unary Data depth must be positive");
  }
  return (
    unaryConstructorPrefixHex.repeat(depth) +
    unaryLeafHex +
    unaryConstructorSuffixHex.repeat(depth)
  );
};

export const measureExactUnaryConstructorDataV1 = (
  datumCborHex: string,
): {
  readonly depth: number;
  readonly nodeCount: number;
  readonly scalarCount: number;
} => {
  const bytes = Buffer.from(datumCborHex, "hex");
  let cursor = 0;
  let depth = 0;
  while (
    cursor + 3 <= bytes.length &&
    bytes[cursor] === 0xd8 &&
    bytes[cursor + 1] === 0x79 &&
    bytes[cursor + 2] === 0x9f
  ) {
    depth += 1;
    cursor += 3;
  }
  if (depth === 0 || bytes[cursor] !== 0x00) {
    throw new Error("Unary Data must terminate in the exact integer-zero leaf");
  }
  cursor += 1;
  for (let index = 0; index < depth; index += 1) {
    if (bytes[cursor + index] !== 0xff) {
      throw new Error("Unary Data constructor must have exactly one child");
    }
  }
  if (cursor + depth !== bytes.length) {
    throw new Error("Unary Data contains trailing bytes");
  }
  return {
    depth,
    nodeCount: depth + 1,
    scalarCount: 1,
  };
};

const encodeCborHead = (major: number, value: bigint): Buffer => {
  if (
    !Number.isSafeInteger(major) ||
    major < 0 ||
    major > 7 ||
    value < 0n ||
    value > 0xffff_ffff_ffff_ffffn
  ) {
    throw new Error("CBOR head is outside the supported uint64 range");
  }
  if (value < 24n) {
    return Buffer.from([(major << 5) | Number(value)]);
  }
  const widths = [
    { limit: 0xffn, additional: 24, bytes: 1 },
    { limit: 0xffffn, additional: 25, bytes: 2 },
    { limit: 0xffff_ffffn, additional: 26, bytes: 4 },
    {
      limit: 0xffff_ffff_ffff_ffffn,
      additional: 27,
      bytes: 8,
    },
  ] as const;
  const width = widths.find(({ limit }) => value <= limit)!;
  const encoded = Buffer.alloc(1 + width.bytes);
  encoded[0] = (major << 5) | width.additional;
  let remaining = value;
  for (let index = width.bytes; index > 0; index -= 1) {
    encoded[index] = Number(remaining & 0xffn);
    remaining >>= 8n;
  }
  return encoded;
};

const encodeCborUint = (value: bigint): Buffer => encodeCborHead(0, value);

const encodeCborInt = (value: bigint): Buffer =>
  value < 0n ? encodeCborHead(1, -1n - value) : encodeCborHead(0, value);

const encodeCborBytes = (bytes: Uint8Array): Buffer =>
  Buffer.concat([encodeCborHead(2, BigInt(bytes.length)), Buffer.from(bytes)]);

const encodeCborArrayRaw = (items: readonly Uint8Array[]): Buffer =>
  Buffer.concat([
    encodeCborHead(4, BigInt(items.length)),
    ...items.map((item) => Buffer.from(item)),
  ]);

const encodeCborMapRaw = (
  entries: readonly (readonly [Uint8Array, Uint8Array])[],
): Buffer =>
  Buffer.concat([
    encodeCborHead(5, BigInt(entries.length)),
    ...entries.flatMap(([key, value]) => [
      Buffer.from(key),
      Buffer.from(value),
    ]),
  ]);

const encodeCborTagRaw = (tag: bigint, item: Uint8Array): Buffer =>
  Buffer.concat([encodeCborHead(6, tag), Buffer.from(item)]);

export type RawSignedCardanoUnaryCandidateV1 = {
  readonly requestedItemCount: number;
  readonly cborHex: string;
  readonly signedBytes: number;
  readonly fee: bigint;
  readonly bodyHash: Buffer;
  readonly signature: Buffer;
  readonly datumCbor: Buffer;
};

/**
 * Builds the same simple Alonzo-map transaction shape as CML, but keeps the
 * tag-24 inline datum bytes opaque. This avoids treating the CML/WASM call
 * stack as a Cardano protocol depth limit. The depth-one result is compared
 * byte-for-byte with CML in the boundary suite, including the deterministic
 * vkey witness.
 */
export const buildRawSignedCardanoUnaryCandidateV1 = ({
  privateKey,
  inputTransactionId,
  inputLovelace,
  recipientAddress,
  requestedDepth,
  minFeeA,
  minFeeB,
}: {
  readonly privateKey: CML.PrivateKey;
  readonly inputTransactionId: string;
  readonly inputLovelace: bigint;
  readonly recipientAddress: string;
  readonly requestedDepth: number;
  readonly minFeeA: number;
  readonly minFeeB: number;
}): RawSignedCardanoUnaryCandidateV1 => {
  const datumCbor = Buffer.from(
    cardanoUnaryConstructorDataCborV1(requestedDepth),
    "hex",
  );
  const input = encodeCborArrayRaw([
    encodeCborBytes(Buffer.from(inputTransactionId, "hex")),
    encodeCborUint(0n),
  ]);
  const inputs = encodeCborTagRaw(258n, encodeCborArrayRaw([input]));
  const address = Buffer.from(
    CML.Address.from_bech32(recipientAddress).to_raw_bytes(),
  );
  const datumOption = encodeCborArrayRaw([
    encodeCborUint(1n),
    encodeCborTagRaw(24n, encodeCborBytes(datumCbor)),
  ]);
  const publicKey = Buffer.from(privateKey.to_public().to_raw_bytes());

  let fee = BigInt(minFeeB);
  for (let attempt = 0; attempt < 10; attempt += 1) {
    const outputLovelace = inputLovelace - fee;
    if (outputLovelace <= 0n) {
      throw new Error("Unary-depth candidate exhausts its funding input");
    }
    const output = encodeCborMapRaw([
      [encodeCborUint(0n), encodeCborBytes(address)],
      [encodeCborUint(1n), encodeCborUint(outputLovelace)],
      [encodeCborUint(2n), datumOption],
    ]);
    const body = encodeCborMapRaw([
      [encodeCborUint(0n), inputs],
      [encodeCborUint(1n), encodeCborArrayRaw([output])],
      [encodeCborUint(2n), encodeCborUint(fee)],
    ]);
    const bodyHash = computeHash32(body);
    const signature = Buffer.from(privateKey.sign(bodyHash).to_raw_bytes());
    const vkeyWitness = encodeCborArrayRaw([
      encodeCborBytes(publicKey),
      encodeCborBytes(signature),
    ]);
    const witnessSet = encodeCborMapRaw([
      [
        encodeCborUint(0n),
        encodeCborTagRaw(258n, encodeCborArrayRaw([vkeyWitness])),
      ],
    ]);
    const signed = encodeCborArrayRaw([
      body,
      witnessSet,
      Buffer.from([0xf5]),
      Buffer.from([0xf6]),
    ]);
    const nextFee = BigInt(minFeeA) * BigInt(signed.length) + BigInt(minFeeB);
    if (nextFee === fee) {
      if (
        !privateKey
          .to_public()
          .verify(bodyHash, CML.Ed25519Signature.from_raw_bytes(signature))
      ) {
        throw new Error("Unary-depth Cardano vkey witness did not verify");
      }
      return {
        requestedItemCount: requestedDepth,
        cborHex: signed.toString("hex"),
        signedBytes: signed.length,
        fee,
        bodyHash,
        signature,
        datumCbor,
      };
    }
    fee = nextFee;
  }
  throw new Error("Unary-depth Cardano fee did not converge");
};

/**
 * Alonzo/Conway `language_views` for a single Plutus V3 cost model.
 *
 * Only the V1 legacy shape needs the double-bytestring wrapping; V2 and V3 use
 * the plain `{ language_id => [cost model integers] }` map. Written out here so
 * the script-data hash below never has to route through
 * `CML.calc_script_data_hash`, which materializes the redeemer `PlutusData` and
 * therefore traps on maximum-depth unary Data.
 */
const plutusV3LanguageViewsCborV1 = (
  costModel: readonly number[],
): Buffer =>
  encodeCborMapRaw([
    [
      encodeCborUint(2n),
      Buffer.concat([
        encodeCborHead(4, BigInt(costModel.length)),
        ...costModel.map((entry) => encodeCborInt(BigInt(entry))),
      ]),
    ],
  ]);

/**
 * Conway script-data hash with an empty datum witness list.
 *
 * `blake2b-256(redeemers || language_views)`: the datum component is omitted
 * entirely — not encoded as an empty list — whenever no datum witnesses are
 * present, which is the case for every candidate this builder produces.
 */
const rawScriptDataHashV1 = ({
  redeemersCbor,
  costModel,
}: {
  readonly redeemersCbor: Buffer;
  readonly costModel: readonly number[];
}): Buffer =>
  computeHash32(
    Buffer.concat([redeemersCbor, plutusV3LanguageViewsCborV1(costModel)]),
  );

export type RawSignedCardanoUnaryRedeemerCandidateV1 = {
  readonly requestedItemCount: number;
  readonly cborHex: string;
  readonly signedBytes: number;
  readonly fee: bigint;
  readonly redeemerDataCbor: Buffer;
  readonly redeemersCbor: Buffer;
  readonly scriptDataHash: Buffer;
};

/**
 * Raw (CML-free) builder for the canonical V1 unary-depth Cardano *redeemer*
 * candidate — the field-8 counterpart of
 * `buildRawSignedCardanoUnaryCandidateV1`.
 *
 * `buildSignedCardanoSpendRedeemersCandidateV1` cannot reach the genuine
 * maximum: it routes the redeemer through `CML.PlutusData.from_cbor_hex` and
 * derives the script-data hash through `CML.calc_script_data_hash`, and both
 * recurse over the Data tree. This builder assembles the same Conway map
 * transaction byte by byte and keeps the redeemer Data opaque, so the only
 * limit left is the signed Cardano byte count. The depth-one result is pinned
 * byte-for-byte against the CML builder in the boundary suite, which is what
 * makes "the same transaction, only deeper" a measured claim rather than an
 * assumption.
 */
export const buildRawSignedCardanoUnaryRedeemersCandidateV1 = ({
  privateKey,
  spendInputs,
  scriptInputIndex,
  collateralInput,
  recipientAddress,
  plutusV3ScriptCborHex,
  requestedDepth,
  executionMemory,
  executionSteps,
  totalCollateral,
  minFeeA,
  minFeeB,
  priceMem,
  priceStep,
  plutusV3CostModel,
}: {
  readonly privateKey: CML.PrivateKey;
  readonly spendInputs: readonly {
    readonly txHash: string;
    readonly outputIndex: number;
    readonly lovelace: bigint;
  }[];
  readonly scriptInputIndex: number;
  readonly collateralInput: {
    readonly txHash: string;
    readonly outputIndex: number;
    readonly lovelace: bigint;
  };
  readonly recipientAddress: string;
  readonly plutusV3ScriptCborHex: string;
  readonly requestedDepth: number;
  readonly executionMemory: bigint;
  readonly executionSteps: bigint;
  readonly totalCollateral: bigint;
  readonly minFeeA: number;
  readonly minFeeB: number;
  readonly priceMem: number;
  readonly priceStep: number;
  readonly plutusV3CostModel: readonly number[];
}): RawSignedCardanoUnaryRedeemerCandidateV1 => {
  if (spendInputs.length === 0) {
    throw new Error("Unary-depth redeemer candidate needs a spend input");
  }
  if (
    !Number.isSafeInteger(scriptInputIndex) ||
    scriptInputIndex < 0 ||
    scriptInputIndex >= spendInputs.length
  ) {
    throw new Error("Unary-depth redeemer script input index is out of range");
  }
  const redeemerDataCbor = Buffer.from(
    cardanoUnaryConstructorDataCborV1(requestedDepth),
    "hex",
  );
  const address = Buffer.from(
    CML.Address.from_bech32(recipientAddress).to_raw_bytes(),
  );
  const publicKey = Buffer.from(privateKey.to_public().to_raw_bytes());
  const encodeInput = (input: {
    readonly txHash: string;
    readonly outputIndex: number;
  }): Buffer =>
    encodeCborArrayRaw([
      encodeCborBytes(Buffer.from(input.txHash, "hex")),
      encodeCborUint(BigInt(input.outputIndex)),
    ]);
  const inputs = encodeCborTagRaw(
    258n,
    encodeCborArrayRaw(spendInputs.map(encodeInput)),
  );
  const collateralInputs = encodeCborTagRaw(
    258n,
    encodeCborArrayRaw([encodeInput(collateralInput)]),
  );
  const collateralReturn = encodeCborArrayRaw([
    encodeCborBytes(address),
    encodeCborUint(collateralInput.lovelace - totalCollateral),
  ]);
  const redeemersCbor = encodeCborMapRaw([
    [
      encodeCborArrayRaw([
        encodeCborUint(0n),
        encodeCborUint(BigInt(scriptInputIndex)),
      ]),
      encodeCborArrayRaw([
        redeemerDataCbor,
        encodeCborArrayRaw([
          encodeCborUint(executionMemory),
          encodeCborUint(executionSteps),
        ]),
      ]),
    ],
  ]);
  const scriptDataHash = rawScriptDataHashV1({
    redeemersCbor,
    costModel: plutusV3CostModel,
  });
  const scripts = encodeCborTagRaw(
    258n,
    encodeCborArrayRaw([Buffer.from(plutusV3ScriptCborHex, "hex")]),
  );
  const selectedLovelace = spendInputs.reduce(
    (total, input) => total + input.lovelace,
    0n,
  );
  // `CML.min_fee` adds the execution-unit component as one exact rational sum
  // and rounds up once; it never varies with the redeemer depth, so the whole
  // depth sweep shares this single constant.
  const executionUnitFeeScale = 10_000_000_000n;
  const executionUnitFee =
    (BigInt(Math.round(priceMem * Number(executionUnitFeeScale))) *
      executionMemory +
      BigInt(Math.round(priceStep * Number(executionUnitFeeScale))) *
        executionSteps +
      executionUnitFeeScale -
      1n) /
    executionUnitFeeScale;

  let fee = BigInt(minFeeB);
  for (let attempt = 0; attempt < 10; attempt += 1) {
    const outputLovelace = selectedLovelace - fee;
    if (outputLovelace <= 0n) {
      throw new Error(
        "Unary-depth redeemer candidate exhausts its selected inputs",
      );
    }
    const output = encodeCborArrayRaw([
      encodeCborBytes(address),
      encodeCborUint(outputLovelace),
    ]);
    const body = encodeCborMapRaw([
      [encodeCborUint(0n), inputs],
      [encodeCborUint(1n), encodeCborArrayRaw([output])],
      [encodeCborUint(2n), encodeCborUint(fee)],
      [encodeCborUint(11n), encodeCborBytes(scriptDataHash)],
      [encodeCborUint(13n), collateralInputs],
      [encodeCborUint(16n), collateralReturn],
      [encodeCborUint(17n), encodeCborUint(totalCollateral)],
    ]);
    const bodyHash = computeHash32(body);
    const signature = Buffer.from(privateKey.sign(bodyHash).to_raw_bytes());
    const vkeyWitness = encodeCborArrayRaw([
      encodeCborBytes(publicKey),
      encodeCborBytes(signature),
    ]);
    const witnessSet = encodeCborMapRaw([
      [
        encodeCborUint(0n),
        encodeCborTagRaw(258n, encodeCborArrayRaw([vkeyWitness])),
      ],
      [encodeCborUint(5n), redeemersCbor],
      [encodeCborUint(7n), scripts],
    ]);
    const signed = encodeCborArrayRaw([
      body,
      witnessSet,
      Buffer.from([0xf5]),
      Buffer.from([0xf6]),
    ]);
    const nextFee =
      BigInt(minFeeA) * BigInt(signed.length) +
      BigInt(minFeeB) +
      executionUnitFee;
    if (nextFee === fee) {
      if (
        !privateKey
          .to_public()
          .verify(bodyHash, CML.Ed25519Signature.from_raw_bytes(signature))
      ) {
        throw new Error(
          "Unary-depth redeemer Cardano vkey witness did not verify",
        );
      }
      return {
        requestedItemCount: requestedDepth,
        cborHex: signed.toString("hex"),
        signedBytes: signed.length,
        fee,
        redeemerDataCbor,
        redeemersCbor,
        scriptDataHash,
      };
    }
    fee = nextFee;
  }
  throw new Error("Unary-depth redeemer Cardano fee did not converge");
};
