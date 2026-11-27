/** Pinned Cardano collection-boundary parameters. Candidate construction and boundary search have separate owners. */
import { PROTOCOL_PARAMETERS_DEFAULT } from "@lucid-evolution/lucid";

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

export type SignedCardanoCollectionCandidate = {
  readonly requestedItemCount: number;
  readonly cborHex: string;
  readonly signedBytes: number;
  readonly fee: bigint;
};
