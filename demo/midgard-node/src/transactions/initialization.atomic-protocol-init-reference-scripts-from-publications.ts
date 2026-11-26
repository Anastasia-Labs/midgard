import { deploymentFraudProofCatalogueCategories } from "@al-ft/midgard-core/deployment-profile";
import { asLucidSchema } from "@al-ft/midgard-core/lucid-data";
import * as SDK from "@al-ft/midgard-sdk/core";
import type * as SDKDeployment from "@al-ft/midgard-sdk/deployment";
import { Data as LucidData, UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  type DaLocalSigner,
  isStrictlyAscending,
  splitPackedHex,
  VERIFICATION_KEY_HEX_LENGTH,
} from "../da/local-signers.js";
import { MpfError } from "../mpf/errors.js";
import { MidgardMpf } from "../mpf/store.midgard-mpf.js";
import { MpfBatchOp } from "../mpf/types.js";

/**
 * Deployment helpers for the protocol's initial on-chain contract state.
 *
 * Canonical real deployment is atomic: hub-oracle, scheduler, state-queue,
 * operator-set roots, and fraud-proof catalogue are minted in one transaction.
 */

/**
 * Converts a fraud-proof catalogue index into the fixed-width key used by the
 * catalogue MPF.
 */
export const uint32ToFraudProofID = (index: number): Buffer => {
  const buf = Buffer.alloc(4);
  buf.writeUInt32BE(index);
  return buf;
};

const FraudProofCatalogueIdSchema = LucidData.Bytes({
  minLength: SDK.FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
  maxLength: SDK.FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
});

type IndexedFraudProof<
  CategoryName extends
    SDK.FraudProofCatalogueCategoryName = SDK.FraudProofCatalogueCategoryName,
> = readonly [
  categoryId: Buffer,
  validator: SDK.SpendingValidator,
  categoryName: CategoryName,
];

/**
 * The deployment's catalogue entries: every category the deployment profile
 * does not exclude, in presentation order. Uses the frozen wire ID map;
 * category presentation order is not identity.
 */
export const fraudProofsToIndexedValidators = (
  fraudProofs: SDK.FraudProofs,
  catalogueCategories: readonly SDK.FraudProofCatalogueCategoryName[] = deploymentFraudProofCatalogueCategories(),
): IndexedFraudProof<SDK.FraudProofCatalogueCategoryName>[] => {
  return catalogueCategories.map((fraudProofTitle) => {
    const categoryIdHex =
      SDK.FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[fraudProofTitle];
    const categoryId = Buffer.from(categoryIdHex, "hex");
    if (
      categoryId.length !== SDK.FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT ||
      categoryId.toString("hex") !== categoryIdHex
    ) {
      throw new Error(
        `Invalid fraud-proof category ID for ${fraudProofTitle}: ${categoryIdHex}`,
      );
    }
    return [categoryId, fraudProofs[fraudProofTitle], fraudProofTitle];
  });
};

const encodeFraudProofCatalogueKey = (categoryId: Buffer): Buffer =>
  Buffer.from(
    LucidData.to(
      categoryId.toString("hex"),
      asLucidSchema(FraudProofCatalogueIdSchema),
    ),
    "hex",
  );

const encodeFraudProofCatalogueValue = (
  fraudProofValidator: SDK.SpendingValidator,
): Buffer =>
  Buffer.from(
    LucidData.to(
      fraudProofValidator.spendingScriptHash,
      asLucidSchema(SDK.ScriptHashSchema),
    ),
    "hex",
  );

/**
 * Builds the Merkle Patricia Forestry root used as the fraud-proof catalogue.
 */
export const createFraudProofCatalogueMpf = <
  CategoryName extends SDK.FraudProofCatalogueCategoryName,
>(
  indexedFraudProofs: readonly IndexedFraudProof<CategoryName>[],
): Effect.Effect<MidgardMpf, MpfError> =>
  Effect.gen(function* () {
    const batchOps = indexedFraudProofs.map(
      ([i, fraudProofValidator]): MpfBatchOp => ({
        type: "insert",
        key: encodeFraudProofCatalogueKey(i),
        value: encodeFraudProofCatalogueValue(fraudProofValidator),
      }),
    );
    const mpf = yield* MidgardMpf.createScratch("fraud_proof_catalogue");
    yield* mpf.applyBatch(batchOps);
    return mpf;
  });

export const buildFraudProofCatalogueDeploymentInfo = <
  CategoryName extends SDK.FraudProofCatalogueCategoryName,
>(
  indexedFraudProofs: readonly IndexedFraudProof<CategoryName>[],
): Effect.Effect<
  SDK.FraudProofCatalogueDeploymentInfo<CategoryName>,
  MpfError
> =>
  Effect.gen(function* () {
    const mpf = yield* createFraudProofCatalogueMpf(indexedFraudProofs);
    const root = yield* mpf.rootHex();
    const categories: Partial<
      Record<CategoryName, SDK.FraudProofCatalogueCategoryDeploymentInfo>
    > = {};

    for (const [categoryId, validator, categoryName] of indexedFraudProofs) {
      const key = encodeFraudProofCatalogueKey(categoryId);
      const proof = yield* mpf.prove(key);
      categories[categoryName] = {
        categoryId: categoryId.toString("hex"),
        scriptHash: validator.spendingScriptHash,
        membershipProofCbor: proof.cbor.toString("hex"),
      };
    }

    return {
      root,
      categories:
        categories as SDK.FraudProofCatalogueDeploymentInfo<CategoryName>["categories"],
    };
  });

export const DEFAULT_DEPLOYMENT_VALIDITY_WINDOW_MS = 7n * 60n * 1000n;

export const DEFAULT_DEPLOYMENT_VALIDITY_BACKOFF_MS = 60_000;

export const DEPLOYMENT_VISIBILITY_REFRESH_MAX_RETRIES = 12;

export const DEPLOYMENT_VISIBILITY_REFRESH_DELAY = "2 seconds";

export type AtomicProtocolInitReferenceScripts =
  SDKDeployment.AtomicProtocolInitReferenceScripts;

type ReferenceScriptPublicationLike = {
  readonly name: string;
  readonly utxo: UTxO;
};

const requireReferenceScriptPublication = (
  publications: readonly ReferenceScriptPublicationLike[],
  name: string,
): UTxO => {
  const publication = publications.find((candidate) => candidate.name === name);
  if (publication === undefined) {
    throw new Error(`Missing published reference script ${name}`);
  }
  return publication.utxo;
};

export const atomicProtocolInitReferenceScriptsFromPublications = (
  publications: readonly ReferenceScriptPublicationLike[],
): AtomicProtocolInitReferenceScripts => ({
  depositHistory: requireReferenceScriptPublication(
    publications,
    "deposit minting",
  ),
  withdrawalHistory: requireReferenceScriptPublication(
    publications,
    "withdrawal minting",
  ),
  daParamsGovernorMinting: requireReferenceScriptPublication(
    publications,
    "da-params-governor minting",
  ),
  hubOracleMinting: requireReferenceScriptPublication(
    publications,
    "hub-oracle minting",
  ),
  schedulerMinting: requireReferenceScriptPublication(
    publications,
    "scheduler minting",
  ),
  stateQueueMinting: requireReferenceScriptPublication(
    publications,
    "state-queue minting",
  ),
  registeredOperatorsMinting: requireReferenceScriptPublication(
    publications,
    "registered-operators minting",
  ),
  activeOperatorsMinting: requireReferenceScriptPublication(
    publications,
    "active-operators minting",
  ),
  retiredOperatorsMinting: requireReferenceScriptPublication(
    publications,
    "retired-operators minting",
  ),
  fraudProofCatalogueMinting: requireReferenceScriptPublication(
    publications,
    "fraud-proof-catalogue minting",
  ),
  daBondPoolMinting: requireReferenceScriptPublication(
    publications,
    "da-bond-pool minting",
  ),
  validationCaseConfigMinting: requireReferenceScriptPublication(
    publications,
    "V1 validation-trace case configuration minting",
  ),
});

/**
 * Committee size at or above which the single-key attestation warning is not
 * emitted.
 *
 * Advice, not a bound. The governor represents a committee of one: the
 * single-key attest loop runs with a rate-limited explanatory log, and
 * two-key committees are the standing configuration, so nothing here
 * refuses a committee below this size.
 */
export const MIN_RECOMMENDED_DA_COMMITTEE_SIZE = 2;

/**
 * Owner-set size at or above which the single-key governance warning is not
 * emitted.
 *
 * Advice, not a bound, and deliberately a separate constant from
 * {@link MIN_RECOMMENDED_DA_COMMITTEE_SIZE}: one covers who can attest, the
 * other who can rotate the parameters, and a future change to either must not
 * silently move the other. A lone owner is representable, so this drives a warning and nothing else.
 */
export const MIN_RECOMMENDED_DA_OWNER_COUNT = 2;

export const resolveDaCommittee = (
  nodeConfig: {
    readonly DA_COMMITTEE_HEX?: string;
  },
  signers: readonly DaLocalSigner[],
): Effect.Effect<string, SDK.HashingError> =>
  Effect.try({
    try: () => {
      const configured = (nodeConfig.DA_COMMITTEE_HEX ?? "").trim();
      if (configured.length > 0) {
        return validatedPackedSet(
          configured,
          VERIFICATION_KEY_HEX_LENGTH,
          "DA_COMMITTEE_HEX",
          "packed 32-byte verification keys",
        ).join("");
      }
      // No arity refusal. A locally derived committee of one is the single-key
      // attest loop; `deriveOperatorDaParams` warns about it rather than
      // failing closed here.
      return [...new Set(signers.map((signer) => signer.verificationKeyHex))]
        .sort()
        .join("");
    },
    catch: (cause) =>
      new SDK.HashingError({
        message: "Invalid DA committee configuration",
        cause,
      }),
  });

/**
 * Parses a packed hex set and enforces what `valid_datum` needs of it: correct
 * element width and strictly ascending order (its sorted-unique encoding).
 *
 * It checks no arity: both call sites accept a single element, and
 * `splitPackedHex` already rejects an empty field.
 *
 * Order is rejected rather than repaired. Committee position *is* the signer
 * index every attestation witness and attested-signer bit is keyed on, so
 * silently reordering a configured committee would desynchronise this node from
 * its peers.
 */
export const validatedPackedSet = (
  packed: string,
  chunkHexLength: number,
  fieldName: string,
  shape: string,
): string[] => {
  let elements: readonly string[];
  try {
    elements = splitPackedHex(packed, chunkHexLength, fieldName);
  } catch {
    throw new Error(`${fieldName} must be ${shape} as hex`);
  }
  if (!isStrictlyAscending(elements)) {
    throw new Error(
      `${fieldName} must be sorted ascending with no duplicates, matching the governor's sorted-unique encoding`,
    );
  }
  return [...elements];
};
