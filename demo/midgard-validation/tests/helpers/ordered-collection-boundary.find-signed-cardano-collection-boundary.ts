import { computeHash32 } from "@al-ft/midgard-core";
import { CML, PROTOCOL_PARAMETERS_DEFAULT } from "@lucid-evolution/lucid";

export const CARDANO_BOUNDARY_MAX_TX_SIZE = 16_384;

export const CARDANO_BOUNDARY_MAX_VALUE_SIZE = 5_000;

export const CARDANO_BOUNDARY_PROTOCOL_MAJOR = 11;

export const CARDANO_BOUNDARY_NESTED_VALUE_ASSET_COUNT = 1_592;

export const CARDANO_BOUNDARY_NESTED_VALUE_LOVELACE = 30_000_000n;

export const CARDANO_BOUNDARY_NESTED_VALUE_POLICY_ID_HEXES = Array.from(
  { length: 7 },
  (_, policyIndex) =>
    (0x11 + policyIndex).toString(16).padStart(2, "0").repeat(28),
);

export const cardanoBoundaryNestedDataCbor = (
  nestedLeafCount: number,
): string => {
  if (!Number.isSafeInteger(nestedLeafCount) || nestedLeafCount <= 0) {
    throw new Error("Cardano nested Data leaf count must be positive");
  }
  const balancedList = (firstLeafIndex: number, leafCount: number): string => {
    if (leafCount === 1) {
      return firstLeafIndex === 0 ? "4101" : "00";
    }
    const leftCount = Math.floor(leafCount / 2);
    return [
      "9f",
      balancedList(firstLeafIndex, leftCount),
      balancedList(firstLeafIndex + leftCount, leafCount - leftCount),
      "ff",
    ].join("");
  };
  return [
    "d8668218809f",
    "a1",
    "d87980",
    balancedList(0, nestedLeafCount),
    "ff",
  ].join("");
};

export const CARDANO_BOUNDARY_OBSERVER_TTL = 10_000n;

export const CARDANO_BOUNDARY_OBSERVER_EXPIRY_BASE = 20_000n;

export const CARDANO_BOUNDARY_MINT_ADA_PER_EXTRA_OUTPUT = 100_000_000n;

export const CARDANO_BOUNDARY_TOTAL_COLLATERAL = 5_000_000n;

export const CARDANO_BOUNDARY_MINT_ASSET_NAME = Buffer.from(
  "MidgardV1",
  "utf8",
);

export const PREPROD_EPOCH_303_BOUNDARY_PARAMETERS = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  minFeeA: 44,
  minFeeB: 155_381,
  maxTxSize: CARDANO_BOUNDARY_MAX_TX_SIZE,
  maxValSize: CARDANO_BOUNDARY_MAX_VALUE_SIZE,
  maxTxExMem: 16_500_000n,
  maxTxExSteps: 10_000_000_000n,
  priceMem: 0.0577,
  priceStep: 0.0000721,
  coinsPerUtxoByte: 4_310n,
  collateralPercentage: 150,
  maxCollateralInputs: 3,
  minFeeRefScriptCostPerByte: 15,
} as const;

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

export type SignedCardanoCollectionCandidate = {
  readonly requestedItemCount: number;
  readonly cborHex: string;
  readonly signedBytes: number;
  readonly fee: bigint;
};

export type SignedCardanoCollectionBoundary = {
  readonly accepted: SignedCardanoCollectionCandidate;
  readonly adjacent: SignedCardanoCollectionCandidate;
  readonly adjacentFailure: string;
};

export type CardanoBoundaryNestedValueAsset = {
  readonly policyIdHex: string;
  readonly assetNameHex: string;
  readonly quantity: bigint;
};

export const cardanoBoundaryNestedValueAssets = (
  requestedValueCborBytes: number,
): readonly CardanoBoundaryNestedValueAsset[] => {
  if (
    requestedValueCborBytes !== CARDANO_BOUNDARY_MAX_VALUE_SIZE &&
    requestedValueCborBytes !== CARDANO_BOUNDARY_MAX_VALUE_SIZE + 1
  ) {
    throw new Error(
      "Nested Cardano Value boundary shape only supports 5,000 or 5,001 bytes",
    );
  }
  const adjacent =
    requestedValueCborBytes === CARDANO_BOUNDARY_MAX_VALUE_SIZE + 1;
  return CARDANO_BOUNDARY_NESTED_VALUE_POLICY_ID_HEXES.flatMap(
    (policyIdHex, policyIndex) => {
      const assetCount = policyIndex < 3 ? 228 : 227;
      return Array.from(
        { length: assetCount },
        (_, policyAssetIndex): CardanoBoundaryNestedValueAsset => ({
          policyIdHex,
          assetNameHex:
            policyAssetIndex === 0
              ? ""
              : Buffer.from([policyAssetIndex - 1]).toString("hex"),
          quantity:
            adjacent &&
            policyIndex + 1 ===
              CARDANO_BOUNDARY_NESTED_VALUE_POLICY_ID_HEXES.length &&
            policyAssetIndex + 1 === assetCount
              ? 24n
              : 1n,
        }),
      );
    },
  );
};

export type MidgardOrderedCollectionBoundaryMeasurement = {
  readonly nativeCanonicalBytes: number;
  readonly fieldBytes: number;
  readonly fieldCommitmentHex: string;
  readonly fieldPreimageCborHex: string;
  readonly fieldPreimageHashHex: string;
  readonly itemCount: number;
  readonly revealStepCount: number;
  readonly completeFoldStepCount: number;
  readonly maxRevealBytes: number;
  readonly maxChunkBytes: number;
  readonly terminalFoldVector: {
    readonly transactionIdHex: string;
    readonly transactionCommitmentHex: string;
    readonly compactCborHex: string;
    readonly witnessSetCompactCborHex: string;
    readonly fieldPreimageLengthsCborHex: string;
    readonly validationContextCborHex: string;
    readonly workWitnessCborHex: string;
    readonly compactBindingWitnessCborHex: string;
    readonly successorPhase: "canonicalDecode" | "compactBinding";
    readonly successorWitnessCborHex: string;
    readonly preWorkRootHex: string;
    readonly postWorkRootHex: string;
    readonly encodedLengthBeforeItem: number;
    readonly collectionProof: {
      readonly fieldIndex: number;
      readonly itemCount: number;
      readonly itemIndex: number;
      readonly itemLength: number;
      readonly itemCommitmentHex: string;
      readonly frontier: readonly {
        readonly height: number;
        readonly hashHex: string;
      }[];
      readonly siblingHexes: readonly string[];
    };
    readonly chunkProof: {
      readonly fieldIndex: number;
      readonly itemIndex: number;
      readonly totalLength: number;
      readonly chunkIndex: number;
      readonly chunkHex: string;
      readonly frontier: readonly {
        readonly height: number;
        readonly hashHex: string;
      }[];
      readonly siblingHexes: readonly string[];
    };
  };
};

type FindSignedCardanoCollectionBoundaryOptions = {
  readonly buildSignedCandidate: (
    requestedItemCount: number,
  ) => Promise<SignedCardanoCollectionCandidate>;
  readonly maxTxSize: number;
};

/**
 * Finds a transaction-shape boundary without introducing a Midgard count cap.
 *
 * The shape builder produces fully signed Cardano CBOR on both sides of the
 * boundary. Exact signed bytes are compared with the preserved maxTxSize;
 * provider behavior and a Midgard count are deliberately not gate inputs.
 */
export const findSignedCardanoCollectionBoundary = async ({
  buildSignedCandidate,
  maxTxSize,
}: FindSignedCardanoCollectionBoundaryOptions): Promise<SignedCardanoCollectionBoundary> => {
  if (!Number.isSafeInteger(maxTxSize) || maxTxSize <= 0) {
    throw new Error("Cardano maxTxSize must be a positive safe integer");
  }

  const buildMeasured = async (
    requestedItemCount: number,
  ): Promise<SignedCardanoCollectionCandidate> => {
    const candidate = await buildSignedCandidate(requestedItemCount);
    if (candidate.requestedItemCount !== requestedItemCount) {
      throw new Error(
        `Cardano collection builder returned cardinality ${candidate.requestedItemCount.toString()} for requested cardinality ${requestedItemCount.toString()}`,
      );
    }
    return candidate;
  };

  let accepted = await buildMeasured(1);
  if (accepted.signedBytes > maxTxSize) {
    throw new Error("One-item signed Cardano collection exceeds maxTxSize");
  }
  let rejectedItemCount = 2;
  for (;;) {
    const candidate = await buildMeasured(rejectedItemCount);
    if (candidate.signedBytes > maxTxSize) {
      break;
    }
    accepted = candidate;
    if (rejectedItemCount > Math.floor(Number.MAX_SAFE_INTEGER / 2)) {
      throw new Error("Cardano collection boundary search overflowed");
    }
    rejectedItemCount *= 2;
  }

  let acceptedItemCount = accepted.requestedItemCount;
  while (acceptedItemCount + 1 < rejectedItemCount) {
    const midpoint = Math.floor((acceptedItemCount + rejectedItemCount) / 2);
    const candidate = await buildMeasured(midpoint);
    if (candidate.signedBytes <= maxTxSize) {
      accepted = candidate;
      acceptedItemCount = midpoint;
    } else {
      rejectedItemCount = midpoint;
    }
  }

  const adjacent = await buildMeasured(acceptedItemCount + 1);
  if (adjacent.signedBytes <= maxTxSize) {
    throw new Error(
      `Adjacent Cardano shape with ${adjacent.requestedItemCount.toString()} requested items unexpectedly fit maxTxSize`,
    );
  }

  return {
    accepted,
    adjacent,
    adjacentFailure:
      `Exact signed Cardano CBOR is ${adjacent.signedBytes.toString()} bytes, ` +
      `above snapshot maxTxSize ${maxTxSize.toString()}`,
  };
};
