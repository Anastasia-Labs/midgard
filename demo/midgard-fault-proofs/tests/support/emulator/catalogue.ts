import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import { compareOutRefs, findOutRefIndex } from "@al-ft/midgard-core";
import { asLucidSchema } from "@al-ft/midgard-core/lucid-data";
import {
  EMPTY_MERKLE_TREE_ROOT,
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
  type FraudProofCatalogueDeploymentInfo,
  type FraudProofs,
  ScriptHashSchema,
} from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";

export const categoryIdSchema = Data.Bytes({
  minLength: FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
  maxLength: FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
});

export const categoryId = (index: number): string => {
  const buf = Buffer.alloc(FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT);
  buf.writeUInt32BE(index);
  return buf.toString("hex");
};

export const encodeCatalogueKey = (id: string): Buffer =>
  Buffer.from(Data.to(id, asLucidSchema(categoryIdSchema)), "hex");

export const encodeCatalogueValue = (scriptHash: string): Buffer =>
  Buffer.from(Data.to(scriptHash, asLucidSchema(ScriptHashSchema)), "hex");

export const trieRootHex = (trie: Trie): string =>
  trie.hash == null
    ? EMPTY_MERKLE_TREE_ROOT
    : Buffer.from(trie.hash).toString("hex");

export const ledgerOrderedIndex = (
  candidates: readonly UTxO[],
  target: UTxO,
  label: string,
): bigint => {
  const index = findOutRefIndex([...candidates].sort(compareOutRefs), target);
  if (index === undefined) {
    throw new Error(`Missing ${label} in candidate set`);
  }
  return BigInt(index);
};

/**
 * A catalogue category registered on top of the canonical registered set —
 * pre-registration families whose production registration is still pending.
 * With no extras the emitted root and every base proof are byte-identical to
 * the one-argument behaviour, so no measured fixture moves.
 */
export type CatalogueExtraCategory = {
  readonly categoryId: string;
  readonly scriptHash: string;
  readonly membershipProofCbor: string;
};

export const buildCatalogueDeploymentInfo = async (
  fraudProofs: FraudProofs,
  extraCategories: Readonly<
    Record<string, { readonly categoryId: string; readonly scriptHash: string }>
  > = {},
): Promise<
  FraudProofCatalogueDeploymentInfo & {
    readonly extraCategories: Readonly<Record<string, CatalogueExtraCategory>>;
  }
> => {
  const categories = Object.fromEntries(
    FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map((name) => [
      name,
      {
        categoryId: FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[name],
        scriptHash: fraudProofs[name].spendingScriptHash,
        membershipProofCbor: "",
      },
    ]),
  ) as FraudProofCatalogueDeploymentInfo["categories"];

  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  for (const name of FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
    const category = categories[name];
    await trie.insert(
      encodeCatalogueKey(category.categoryId),
      encodeCatalogueValue(category.scriptHash),
    );
  }
  for (const extra of Object.values(extraCategories)) {
    await trie.insert(
      encodeCatalogueKey(extra.categoryId),
      encodeCatalogueValue(extra.scriptHash),
    );
  }
  const categoriesWithProofs = { ...categories };
  for (const name of FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
    const category = categories[name];
    const proof = await trie.prove(encodeCatalogueKey(category.categoryId));
    categoriesWithProofs[name] = {
      ...category,
      membershipProofCbor: proof.toCBOR().toString("hex"),
    };
  }
  const extraCategoriesWithProofs: Record<string, CatalogueExtraCategory> = {};
  for (const [name, extra] of Object.entries(extraCategories)) {
    const proof = await trie.prove(encodeCatalogueKey(extra.categoryId));
    extraCategoriesWithProofs[name] = {
      ...extra,
      membershipProofCbor: proof.toCBOR().toString("hex"),
    };
  }
  return {
    root: trieRootHex(trie),
    categories: categoriesWithProofs,
    extraCategories: extraCategoriesWithProofs,
  };
};
