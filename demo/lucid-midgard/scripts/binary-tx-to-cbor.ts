/**
 * Re-encode a Midgard native tx from the current binary canonical format
 * back to the legacy CBOR canonical format. Inverse of cbor-tx-to-binary.ts.
 *
 * Lossless for the body/witness CONTENTS, but the resulting CBOR's txId will
 * be the legacy CBOR-era id (different from the binary-era id), because the
 * compact-body hashes are taken over the preimage encoding.
 */

import fs from "node:fs";
import path from "node:path";

import {
  decodeMidgardNativeTxFullFromCanonicalBinary,
  type MidgardMint,
  type MidgardTxOutput as NewMidgardTxOutput,
  type MidgardVersionedScript as NewMidgardVersionedScript,
  type OutputReference,
  type VKeyWitness,
} from "@al-ft/midgard-core/codec";

import {
  encodeCbor,
  encodeCborArrayRaw,
  encodeCborBytes,
  encodeCborUnsigned,
} from "./legacy-cbor-codec/cbor.js";
import {
  encodeMidgardNativeTxCanonical as encodeLegacyCanonical,
  type MidgardNativeTxBodyCanonical as LegacyBody,
  type MidgardNativeTxCanonical as LegacyCanonical,
  type MidgardNativeTxWitnessSetCanonical as LegacyWitnessSet,
} from "./legacy-cbor-codec/native.js";
import {
  encodeMidgardTxOutput,
  type MidgardTxOutput as LegacyMidgardTxOutput,
} from "./legacy-cbor-codec/output.js";
import {
  encodeMidgardVersionedScriptListPreimage,
  type MidgardVersionedScript as LegacyMidgardVersionedScript,
} from "./legacy-cbor-codec/versioned-script.js";

/** Encode a CBOR `[bytes(32), uint]` for OutputReference. */
const encodeOutputReferenceCbor = (outref: OutputReference): Buffer =>
  encodeCborArrayRaw([
    encodeCborBytes(outref.txId),
    encodeCborUnsigned(BigInt(outref.index)),
  ]);

const encodeVKeyWitnessCbor = (w: VKeyWitness): Buffer =>
  encodeCborArrayRaw([encodeCborBytes(w.vkey), encodeCborBytes(w.signature)]);

/**
 * Build `cbor([bytes(item1), bytes(item2), …])` — the preimage shape used by
 * spendInputs, referenceInputs, outputs, requiredObservers, requiredSigners,
 * and addrTxWits in the legacy codec.
 */
const encodeBytesListPreimage = (items: readonly Buffer[]): Buffer =>
  encodeCbor(items);

/** Mint preimage: `cbor(Map<policyBytes, Map<nameBytes, qty>>)`, empty mint → `cbor([])`. */
const encodeMintPreimage = (mint: MidgardMint): Buffer => {
  if (mint.size === 0) return encodeCbor([]);
  const policies = new Map<Buffer, Map<Buffer, bigint>>();
  for (const [policyHex, assets] of mint.entries()) {
    const inner = new Map<Buffer, bigint>();
    for (const [nameHex, qty] of assets.entries()) {
      inner.set(Buffer.from(nameHex, "hex"), qty);
    }
    policies.set(Buffer.from(policyHex, "hex"), inner);
  }
  return encodeCbor(policies);
};

const convertBinaryToCbor = (binaryBytes: Uint8Array): Buffer => {
  const tx = decodeMidgardNativeTxFullFromCanonicalBinary(binaryBytes);

  const spendInputsPreimageCbor = encodeBytesListPreimage(
    tx.body.spendInputs.map(encodeOutputReferenceCbor),
  );
  const referenceInputsPreimageCbor = encodeBytesListPreimage(
    tx.body.referenceInputs.map(encodeOutputReferenceCbor),
  );
  const outputsPreimageCbor = encodeBytesListPreimage(
    tx.body.outputs.map((o: NewMidgardTxOutput) =>
      encodeMidgardTxOutput(o as LegacyMidgardTxOutput),
    ),
  );
  const requiredObserversPreimageCbor = encodeBytesListPreimage(
    tx.body.requiredObservers.map((b) => Buffer.from(b)),
  );
  const requiredSignersPreimageCbor = encodeBytesListPreimage(
    tx.body.requiredSigners.map((b) => Buffer.from(b)),
  );
  const mintPreimageCbor = encodeMintPreimage(tx.body.mint);

  const addrTxWitsPreimageCbor = encodeBytesListPreimage(
    tx.witnessSet.addrTxWits.map(encodeVKeyWitnessCbor),
  );
  const scriptTxWitsPreimageCbor = encodeMidgardVersionedScriptListPreimage(
    tx.witnessSet.scriptTxWits.map(
      (s: NewMidgardVersionedScript) => s as LegacyMidgardVersionedScript,
    ),
  );
  const redeemerTxWitsPreimageCbor = Buffer.from(tx.witnessSet.redeemerTxWits);

  const body: LegacyBody = {
    spendInputsPreimageCbor,
    referenceInputsPreimageCbor,
    outputsPreimageCbor,
    fee: tx.body.fee,
    validityIntervalStart: tx.body.validityIntervalStart,
    validityIntervalEnd: tx.body.validityIntervalEnd,
    requiredObserversPreimageCbor,
    requiredSignersPreimageCbor,
    mintPreimageCbor,
    scriptIntegrityHash: tx.body.scriptIntegrityHash,
    auxiliaryDataHash: tx.body.auxiliaryDataHash,
    networkId: tx.body.networkId,
  };

  const witnessSet: LegacyWitnessSet = {
    addrTxWitsPreimageCbor,
    scriptTxWitsPreimageCbor,
    redeemerTxWitsPreimageCbor,
  };

  const canonical: LegacyCanonical = {
    version: tx.version,
    validity: tx.validity,
    body,
    witnessSet,
  };

  return encodeLegacyCanonical(canonical);
};

export const binaryMidgardNativeTxToCbor = (
  binaryBytes: Uint8Array,
): Buffer => convertBinaryToCbor(binaryBytes);

const readInput = (inputPath: string): Buffer => {
  const raw = fs.readFileSync(inputPath, "utf8").trim();
  if (inputPath.endsWith(".json")) {
    const j = JSON.parse(raw) as { readonly fullTxBinaryHex?: string };
    if (typeof j.fullTxBinaryHex !== "string") {
      throw new Error(`${inputPath}: JSON missing fullTxBinaryHex`);
    }
    return Buffer.from(j.fullTxBinaryHex, "hex");
  }
  return Buffer.from(raw.replace(/\s+/g, ""), "hex");
};

const cli = (): void => {
  const [inputPath, outputPath] = process.argv.slice(2);
  if (!inputPath) {
    process.stderr.write(
      "usage: binary-tx-to-cbor <input.hex|input.json> [output.hex]\n",
    );
    process.exit(1);
  }
  const binary = readInput(path.resolve(inputPath));
  const cbor = convertBinaryToCbor(binary);
  const hex = cbor.toString("hex");
  if (outputPath) {
    fs.writeFileSync(path.resolve(outputPath), hex);
    process.stderr.write(
      `wrote ${cbor.length} bytes (${hex.length} hex chars) → ${outputPath}\n`,
    );
  } else {
    process.stdout.write(hex);
    process.stdout.write("\n");
  }
};

if (process.env.BINARY_TX_TO_CBOR_RUN === "1") {
  cli();
}
