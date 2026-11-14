import {
  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type DeploymentManifestFraudProofCatalogueCategory,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data, fromText } from "@lucid-evolution/lucid";

import type { FraudProofs, MerkleRoot } from "../common.js";

export const FRAUD_PROOF_CATALOGUE_ASSET_NAME = fromText(
  "MIDGARD_FRAUD_PROOF_CATALOGUE",
);

export const FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT = 4;

// Category order and four-byte identities are owned by the core deployment
// registry. Identity is carried by the id map, never by array position.
export const FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER =
  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER satisfies readonly (keyof FraudProofs)[];

export type FraudProofCatalogueCategoryName =
  DeploymentManifestFraudProofCatalogueCategory;

export const FRAUD_PROOF_CATALOGUE_CATEGORY_IDS =
  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_IDS;

export type FraudProofCatalogueCategoryDeploymentInfo = {
  readonly categoryId: string;
  readonly scriptHash: string;
  readonly membershipProofCbor: string;
};

/**
 * A deployment's catalogue. `categories` holds the categories its profile
 * carries; an excluded category is absent and has no membership proof.
 */
export type FraudProofCatalogueDeploymentInfo<
  CategoryName extends string = FraudProofCatalogueCategoryName,
> = {
  readonly root: MerkleRoot;
  readonly categories: Readonly<
    Partial<Record<CategoryName, FraudProofCatalogueCategoryDeploymentInfo>>
  >;
};

export const FraudProofCatalogueDatumSchema = Data.Bytes();

export type FraudProofCatalogueDatum = Data.Static<
  typeof FraudProofCatalogueDatumSchema
>;
export const FraudProofCatalogueDatum = asDataType<FraudProofCatalogueDatum>(
  FraudProofCatalogueDatumSchema,
);

export const FraudProofCatalogueMintRedeemerSchema = Data.Enum([
  Data.Literal("Init"),
  Data.Literal("Deinit"),
  Data.Literal("NewFraudCategory"),
  Data.Literal("RemoveFraudCategory"),
]);
export type FraudProofCatalogueMintRedeemer = Data.Static<
  typeof FraudProofCatalogueMintRedeemerSchema
>;
export const FraudProofCatalogueMintRedeemer =
  asDataType<FraudProofCatalogueMintRedeemer>(
    FraudProofCatalogueMintRedeemerSchema,
  );

export const FraudProofCatalogueSpendRedeemerSchema = Data.Object({
  fraudProofCatalogueAssetName: Data.Bytes(),
});
export type FraudProofCatalogueSpendRedeemer = Data.Static<
  typeof FraudProofCatalogueSpendRedeemerSchema
>;
export const FraudProofCatalogueSpendRedeemer =
  asDataType<FraudProofCatalogueSpendRedeemer>(
    FraudProofCatalogueSpendRedeemerSchema,
  );
