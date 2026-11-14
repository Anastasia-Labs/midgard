import { computeHash32 } from "@al-ft/midgard-core";
import { CML } from "@lucid-evolution/lucid";

import { CARDANO_BOUNDARY_OBSERVER_EXPIRY_BASE } from "./parameters.js";

const CARDANO_BOUNDARY_SIGNER_KEY_DOMAIN = Buffer.from(
  "CardanoBoundarySignerKeyV1",
  "utf8",
);

export const deterministicCardanoBoundaryPrivateKey = (
  signerIndex: number,
): CML.PrivateKey => {
  if (
    !Number.isSafeInteger(signerIndex) ||
    signerIndex < 0 ||
    signerIndex > 0xffff_ffff
  ) {
    throw new Error("Deterministic Cardano signer index must fit uint32");
  }
  const encodedIndex = Buffer.alloc(4);
  encodedIndex.writeUInt32BE(signerIndex);
  return CML.PrivateKey.from_normal_bytes(
    computeHash32(
      Buffer.concat([CARDANO_BOUNDARY_SIGNER_KEY_DOMAIN, encodedIndex]),
    ),
  );
};

export const deriveCardanoGenesisInputSupply = (maxTxSize: number): number => {
  if (!Number.isSafeInteger(maxTxSize) || maxTxSize <= 0) {
    throw new Error("Cardano maxTxSize must be a positive safe integer");
  }
  const transactionIdBytesPerInput = 32;
  const adjacentCandidateReserve = 2;
  return (
    Math.floor(maxTxSize / transactionIdBytesPerInput) +
    adjacentCandidateReserve
  );
};

export const makeCardanoBoundaryNativeScript = ({
  signerHash,
  scriptIndex,
}: {
  readonly signerHash: CML.Ed25519KeyHash;
  readonly scriptIndex: number;
}): CML.NativeScript => {
  const clauses = CML.NativeScriptList.new();
  clauses.add(CML.NativeScript.new_script_pubkey(signerHash));
  clauses.add(
    CML.NativeScript.new_script_invalid_hereafter(
      CARDANO_BOUNDARY_OBSERVER_EXPIRY_BASE + BigInt(scriptIndex),
    ),
  );
  return CML.NativeScript.new_script_all(clauses);
};
