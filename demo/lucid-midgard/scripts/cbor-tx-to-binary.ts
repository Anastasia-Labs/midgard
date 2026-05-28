/**
 * Re-encode a Midgard native tx from the legacy CBOR canonical format to the
 * current binary canonical format.
 *
 * The CBOR codec was removed in commit 7dfa0656 ("wip"). This script preserves
 * the old decoder under ./legacy-cbor-codec/ so checked-in CBOR fixtures (e.g.
 * tests/fixtures/native-size-balanced-15_5k.json) can be re-emitted as binary.
 *
 * Usage:
 *   tsup scripts/cbor-tx-to-binary.ts --format esm --out-dir .tmp/cbor-bin --clean
 *   node .tmp/cbor-bin/cbor-tx-to-binary.js <input.hex|input.json> [output.hex]
 *     - .hex input is hex-encoded CBOR bytes
 *     - .json input is read as the size-balanced fixture shape (uses fullTxCborHex)
 *     - if output is omitted, the binary hex is printed to stdout.
 */

import fs from "node:fs";
import path from "node:path";

import {
  encodeMidgardNativeTxCanonical,
  type MidgardMint,
  type MidgardNativeTxBodyCanonical,
  type MidgardNativeTxCanonical,
  type MidgardNativeTxWitnessSetCanonical,
  type MidgardTxOutput,
  type MidgardVersionedScript,
  type OutputReference,
  type VKeyWitness,
} from "@al-ft/midgard-core/codec";

import {
  asArray,
  asBytes,
  asMap,
  decodeSingleCbor,
  readCborArrayHeader,
  readCborBytes,
  readCborUnsigned,
} from "./legacy-cbor-codec/cbor.js";
import { decodeMidgardNativeTxFullFromCanonicalCbor } from "./legacy-cbor-codec/native.js";
import { decodeMidgardTxOutput } from "./legacy-cbor-codec/output.js";
import { decodeMidgardVersionedScriptListPreimage } from "./legacy-cbor-codec/versioned-script.js";

const HASH28 = 28;
const HASH32 = 32;
const VKEY_LEN = 32;
const SIG_LEN = 64;

/** Decode a CBOR `[bytes(32), uint]` as an OutputReference. */
const decodeOutputReferenceCbor = (
  bytes: Uint8Array,
  fieldName: string,
): OutputReference => {
  const header = readCborArrayHeader(bytes, 0, fieldName);
  if (header.length !== 2) {
    throw new Error(`${fieldName}: expected 2-element array, got ${header.length}`);
  }
  const txId = readCborBytes(bytes, header.nextOffset, `${fieldName}.tx_id`);
  if (txId.value.length !== HASH32) {
    throw new Error(`${fieldName}.tx_id: expected 32 bytes`);
  }
  const index = readCborUnsigned(bytes, txId.nextOffset, `${fieldName}.index`);
  if (index.value > 0xffffn) {
    throw new Error(`${fieldName}.index: exceeds u16`);
  }
  return { txId: txId.value, index: Number(index.value) };
};

/** Decode a CML Vkeywitness CBOR `[vkey(32), signature(64)]`. */
const decodeVKeyWitnessCbor = (
  bytes: Uint8Array,
  fieldName: string,
): VKeyWitness => {
  const header = readCborArrayHeader(bytes, 0, fieldName);
  if (header.length !== 2) {
    throw new Error(`${fieldName}: expected 2-element array, got ${header.length}`);
  }
  const vkey = readCborBytes(bytes, header.nextOffset, `${fieldName}.vkey`);
  if (vkey.value.length !== VKEY_LEN) {
    throw new Error(`${fieldName}.vkey: expected 32 bytes`);
  }
  const sig = readCborBytes(bytes, vkey.nextOffset, `${fieldName}.signature`);
  if (sig.value.length !== SIG_LEN) {
    throw new Error(`${fieldName}.signature: expected 64 bytes`);
  }
  return { vkey: vkey.value, signature: sig.value };
};

/** Decode the mint preimage CBOR. Empty array → empty mint; map → MidgardMint. */
const decodeMintPreimage = (preimageCbor: Uint8Array): MidgardMint => {
  const decoded = decodeSingleCbor(preimageCbor);
  if (Array.isArray(decoded)) {
    if (decoded.length !== 0) {
      throw new Error("mint preimage array must be empty");
    }
    return new Map();
  }
  const policies = asMap(decoded, "mint");
  const out = new Map<string, Map<string, bigint>>();
  for (const [policyKey, assetsValue] of policies.entries()) {
    const policy = asBytes(policyKey, "mint.policy");
    if (policy.length !== HASH28) {
      throw new Error("mint policy must be 28 bytes");
    }
    const assets = asMap(assetsValue, "mint.assets");
    const inner = new Map<string, bigint>();
    for (const [nameKey, qtyValue] of assets.entries()) {
      const name = asBytes(nameKey, "mint.asset_name");
      if (typeof qtyValue !== "bigint" && typeof qtyValue !== "number") {
        throw new Error("mint quantity must be int");
      }
      inner.set(name.toString("hex"), BigInt(qtyValue));
    }
    out.set(policy.toString("hex"), inner);
  }
  return out;
};

/** Decode a `cbor([bytes, bytes, ...])` preimage and parse each entry. */
const decodePreimageList = (
  preimageCbor: Uint8Array,
  fieldName: string,
): Buffer[] => {
  const decoded = decodeSingleCbor(preimageCbor);
  return asArray(decoded, fieldName).map((item, i) =>
    asBytes(item, `${fieldName}[${i}]`),
  );
};

const ensureHash28 = (b: Buffer, fieldName: string): Buffer => {
  if (b.length !== HASH28) {
    throw new Error(`${fieldName}: expected 28 bytes, got ${b.length}`);
  }
  return b;
};

const convertCborToBinary = (cborBytes: Uint8Array): Buffer => {
  const legacy = decodeMidgardNativeTxFullFromCanonicalCbor(cborBytes);

  const spendInputs: OutputReference[] = decodePreimageList(
    legacy.body.spendInputsPreimageCbor,
    "spend_inputs",
  ).map((b, i) => decodeOutputReferenceCbor(b, `spend_inputs[${i}]`));

  const referenceInputs: OutputReference[] = decodePreimageList(
    legacy.body.referenceInputsPreimageCbor,
    "reference_inputs",
  ).map((b, i) => decodeOutputReferenceCbor(b, `reference_inputs[${i}]`));

  const outputs: MidgardTxOutput[] = decodePreimageList(
    legacy.body.outputsPreimageCbor,
    "outputs",
  ).map((b) => decodeMidgardTxOutput(b) as MidgardTxOutput);

  const requiredObservers: Buffer[] = decodePreimageList(
    legacy.body.requiredObserversPreimageCbor,
    "required_observers",
  );

  const requiredSigners: Buffer[] = decodePreimageList(
    legacy.body.requiredSignersPreimageCbor,
    "required_signers",
  ).map((b, i) => ensureHash28(b, `required_signers[${i}]`));

  const mint = decodeMintPreimage(legacy.body.mintPreimageCbor);

  const addrTxWits: VKeyWitness[] = decodePreimageList(
    legacy.witnessSet.addrTxWitsPreimageCbor,
    "addr_tx_wits",
  ).map((b, i) => decodeVKeyWitnessCbor(b, `addr_tx_wits[${i}]`));

  const scriptTxWits = decodeMidgardVersionedScriptListPreimage(
    legacy.witnessSet.scriptTxWitsPreimageCbor,
    "script_tx_wits",
  ) as readonly MidgardVersionedScript[];

  // redeemerTxWits preimage is the CBOR-encoded redeemer set blob —
  // the legacy canonical bytes already contain it verbatim, so pass through.
  const redeemerTxWits = Buffer.from(legacy.witnessSet.redeemerTxWitsPreimageCbor);

  const body: MidgardNativeTxBodyCanonical = {
    spendInputs,
    referenceInputs,
    outputs,
    fee: legacy.body.fee,
    validityIntervalStart: legacy.body.validityIntervalStart,
    validityIntervalEnd: legacy.body.validityIntervalEnd,
    requiredObservers,
    requiredSigners,
    mint,
    scriptIntegrityHash: legacy.body.scriptIntegrityHash,
    auxiliaryDataHash: legacy.body.auxiliaryDataHash,
    networkId: legacy.body.networkId,
  };

  const witnessSet: MidgardNativeTxWitnessSetCanonical = {
    addrTxWits,
    scriptTxWits,
    redeemerTxWits,
  };

  const canonical: MidgardNativeTxCanonical = {
    version: legacy.version,
    validity: legacy.validity,
    body,
    witnessSet,
  };

  return encodeMidgardNativeTxCanonical(canonical);
};

/** Public API for tests / programmatic use. */
export const cborMidgardNativeTxToBinary = (
  cborBytes: Uint8Array,
): Buffer => convertCborToBinary(cborBytes);

const readInput = (inputPath: string): Buffer => {
  const raw = fs.readFileSync(inputPath, "utf8").trim();
  if (inputPath.endsWith(".json")) {
    const j = JSON.parse(raw) as { readonly fullTxCborHex?: string };
    if (typeof j.fullTxCborHex !== "string") {
      throw new Error(`${inputPath}: JSON missing fullTxCborHex`);
    }
    return Buffer.from(j.fullTxCborHex, "hex");
  }
  return Buffer.from(raw.replace(/\s+/g, ""), "hex");
};

const cli = (): void => {
  const [inputPath, outputPath] = process.argv.slice(2);
  if (!inputPath) {
    process.stderr.write(
      "usage: cbor-tx-to-binary <input.hex|input.json> [output.hex]\n",
    );
    process.exit(1);
  }
  const cbor = readInput(path.resolve(inputPath));
  const binary = convertCborToBinary(cbor);
  const hex = binary.toString("hex");
  if (outputPath) {
    fs.writeFileSync(path.resolve(outputPath), hex);
    process.stderr.write(
      `wrote ${binary.length} bytes (${hex.length} hex chars) → ${outputPath}\n`,
    );
  } else {
    process.stdout.write(hex);
    process.stdout.write("\n");
  }
};

if (process.env.CBOR_TX_TO_BINARY_RUN === "1") {
  cli();
}
