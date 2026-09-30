import { buildMidgardBoundedItem } from "./bounded-item.js";
import { encodeCbor } from "./codec/cbor.js";
import { ensureHash32, type Hash32 } from "./codec/hash.js";
import {
  EXECUTION_LEAF_DOMAIN,
  hash32,
  MINT_ASSET_LEAF_DOMAIN,
  OUTPUT_DESCRIPTOR_LEAF_DOMAIN,
  OUTPUT_ITEM_LEAF_DOMAIN,
  PURPOSE_LEAF_DOMAIN,
  REDEEMER_LEAF_DOMAIN,
  RESOLVED_CONTEXT_ITEM_LEAF_DOMAIN,
  SCRIPT_CONTEXT_ITEM_LEAF_DOMAIN,
  SIGNER_LEAF_DOMAIN,
} from "./script-proof.hash-midgard-script-source-leaf.js";

export const hashMidgardRedeemerItemLeaf = (input: {
  readonly redeemerIndex: number;
  readonly itemCommitment: Uint8Array;
}): Hash32 => {
  if (!Number.isSafeInteger(input.redeemerIndex) || input.redeemerIndex < 0) {
    throw new Error("redeemer item index must be a non-negative safe integer");
  }
  const itemCommitment = ensureHash32(
    input.itemCommitment,
    "redeemer item commitment",
  );
  return hash32(
    Buffer.concat([
      REDEEMER_LEAF_DOMAIN,
      encodeCbor(BigInt(input.redeemerIndex)),
      encodeCbor(itemCommitment),
    ]),
  );
};

export const hashMidgardRedeemerLeaf = (input: {
  readonly redeemerIndex: number;
  readonly canonicalRedeemerWitnessCbor: Uint8Array;
}): Hash32 => {
  const item = buildMidgardBoundedItem({
    fieldIndex: 8,
    itemIndex: input.redeemerIndex,
    bytes: input.canonicalRedeemerWitnessCbor,
  });
  return hashMidgardRedeemerItemLeaf({
    redeemerIndex: input.redeemerIndex,
    itemCommitment: item.commitment,
  });
};

export const hashMidgardScriptPurposeLeaf = (input: {
  readonly purposeKind: 0 | 1 | 2 | 3;
  readonly purposeIndex: bigint;
  readonly scriptHash: Uint8Array;
  readonly subject: Uint8Array;
}): Hash32 => {
  if (input.purposeIndex < 0n) {
    throw new Error("script purpose index must be non-negative");
  }
  const scriptHash = Buffer.from(input.scriptHash);
  if (scriptHash.length !== 28) {
    throw new Error("script purpose hash must contain exactly 28 bytes");
  }
  return hash32(
    Buffer.concat([
      PURPOSE_LEAF_DOMAIN,
      encodeCbor(BigInt(input.purposeKind)),
      encodeCbor(input.purposeIndex),
      encodeCbor(scriptHash),
      encodeCbor(Buffer.from(input.subject)),
    ]),
  );
};

export const hashMidgardSignerLeaf = (signerHash: Uint8Array): Hash32 => {
  const exactSignerHash = Buffer.from(signerHash);
  if (exactSignerHash.length !== 28) {
    throw new Error("signer hash must contain exactly 28 bytes");
  }
  return hash32(
    Buffer.concat([SIGNER_LEAF_DOMAIN, encodeCbor(exactSignerHash)]),
  );
};

export const hashMidgardOutputItemLeaf = (input: {
  readonly outputIndex: number;
  readonly itemCommitment: Uint8Array;
}): Hash32 => {
  if (!Number.isSafeInteger(input.outputIndex) || input.outputIndex < 0) {
    throw new Error("output index must be a non-negative safe integer");
  }
  const itemCommitment = ensureHash32(
    input.itemCommitment,
    "output item commitment",
  );
  return hash32(
    Buffer.concat([
      OUTPUT_ITEM_LEAF_DOMAIN,
      encodeCbor(BigInt(input.outputIndex)),
      encodeCbor(itemCommitment),
    ]),
  );
};

export const hashMidgardOutputLeaf = (input: {
  readonly outputIndex: number;
  readonly outputCbor: Uint8Array;
}): Hash32 => {
  const item = buildMidgardBoundedItem({
    fieldIndex: 2,
    itemIndex: input.outputIndex,
    bytes: input.outputCbor,
  });
  return hashMidgardOutputItemLeaf({
    outputIndex: input.outputIndex,
    itemCommitment: item.commitment,
  });
};

/**
 * Commits the exact compact ledger descriptor derived by the bounded output
 * proof. Full output bytes remain available through transaction DA.
 */
export const hashMidgardOutputDescriptorLeaf = (input: {
  readonly outputIndex: number;
  readonly descriptorCbor: Uint8Array;
}): Hash32 => {
  if (!Number.isSafeInteger(input.outputIndex) || input.outputIndex < 0) {
    throw new Error("output index must be a non-negative safe integer");
  }
  return hash32(
    Buffer.concat([
      OUTPUT_DESCRIPTOR_LEAF_DOMAIN,
      encodeCbor(BigInt(input.outputIndex)),
      encodeCbor(Buffer.from(input.descriptorCbor)),
    ]),
  );
};

export const hashMidgardScriptExecutionLeaf = (input: {
  readonly languageTag: 0 | 3 | 128;
  readonly purposeLeaf: Uint8Array;
  readonly sourceLeaf: Uint8Array;
  readonly redeemerLeaf?: Uint8Array;
}): Hash32 => {
  const purposeLeaf = Buffer.from(input.purposeLeaf);
  const sourceLeaf = Buffer.from(input.sourceLeaf);
  const redeemerLeaf = Buffer.from(input.redeemerLeaf ?? []);
  if (purposeLeaf.length !== 32 || sourceLeaf.length !== 32) {
    throw new Error("script execution leaves must contain exactly 32 bytes");
  }
  if (redeemerLeaf.length !== 0 && redeemerLeaf.length !== 32) {
    throw new Error(
      "script execution redeemer leaf must be empty or exactly 32 bytes",
    );
  }
  return hash32(
    Buffer.concat([
      EXECUTION_LEAF_DOMAIN,
      encodeCbor(BigInt(input.languageTag)),
      encodeCbor(purposeLeaf),
      encodeCbor(sourceLeaf),
      encodeCbor(redeemerLeaf),
    ]),
  );
};

export const hashMidgardMintAssetLeaf = (input: {
  readonly policyId: Uint8Array;
  readonly assetName: Uint8Array;
  readonly quantity: bigint;
}): Hash32 => {
  const policyId = Buffer.from(input.policyId);
  const assetName = Buffer.from(input.assetName);
  if (policyId.length !== 28) {
    throw new Error("mint policy id must contain exactly 28 bytes");
  }
  if (assetName.length > 32) {
    throw new Error("mint asset name must contain at most 32 bytes");
  }
  if (input.quantity === 0n) {
    throw new Error("mint quantity must be non-zero");
  }
  return hash32(
    Buffer.concat([
      MINT_ASSET_LEAF_DOMAIN,
      encodeCbor(policyId),
      encodeCbor(assetName),
      encodeCbor(input.quantity),
    ]),
  );
};

export const hashMidgardScriptContextItemLeaf = (input: {
  readonly collectionKind: number;
  readonly itemIndex: number;
  readonly semanticRoot: Uint8Array;
  readonly cborLength: bigint;
  readonly memory: bigint;
}): Hash32 => {
  if (
    !Number.isSafeInteger(input.collectionKind) ||
    input.collectionKind < 0 ||
    input.collectionKind > 7
  ) {
    throw new Error(
      "script-context collection kind must be between zero and seven",
    );
  }
  if (!Number.isSafeInteger(input.itemIndex) || input.itemIndex < 0) {
    throw new Error(
      "script-context item index must be a non-negative safe integer",
    );
  }
  const root = Buffer.from(input.semanticRoot);
  if (root.length !== 32) {
    throw new Error(
      "script-context semantic root must contain exactly 32 bytes",
    );
  }
  if (input.cborLength < 0n || input.memory < 0n) {
    throw new Error("script-context item length and memory must be unsigned");
  }
  return hash32(
    Buffer.concat([
      SCRIPT_CONTEXT_ITEM_LEAF_DOMAIN,
      encodeCbor(BigInt(input.collectionKind)),
      encodeCbor(BigInt(input.itemIndex)),
      encodeCbor(root),
      encodeCbor(input.cborLength),
      encodeCbor(input.memory),
    ]),
  );
};

export const hashMidgardResolvedContextItemLeaf = (input: {
  readonly sourceKind: "spend" | "reference";
  readonly itemIndex: number;
  readonly key: Uint8Array;
  readonly outputCbor: Uint8Array;
}): Hash32 => {
  if (!Number.isSafeInteger(input.itemIndex) || input.itemIndex < 0) {
    throw new Error(
      "resolved context item index must be a non-negative safe integer",
    );
  }
  return hash32(
    Buffer.concat([
      RESOLVED_CONTEXT_ITEM_LEAF_DOMAIN,
      encodeCbor(input.sourceKind === "spend" ? 0n : 1n),
      encodeCbor(BigInt(input.itemIndex)),
      encodeCbor(Buffer.from(input.key)),
      encodeCbor(Buffer.from(input.outputCbor)),
    ]),
  );
};
