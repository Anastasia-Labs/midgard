import { Data } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import { encodeCbor } from ".././codec/cbor.js";
import { asLucidSchema } from ".././lucid-data.js";
import {
  buildMidgardMpfProofFoldTrace,
  type MidgardMpfProofStep,
} from ".././mpf-proof-fold.js";
import {
  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type DeploymentManifestFraudProofCatalogueCategory,
  type DeploymentManifestFraudProofCatalogueCategoryIdentity,
  type DeploymentManifestFraudProofCatalogueIdentity,
} from "./catalogue-roles.js";
import { requireExactKeys, requireHex, requireRecord } from "./primitives.js";

const FraudProofCatalogueProofNeighborSchema = Data.Object({
  nibble: Data.Integer(),
  prefix: Data.Bytes(),
  root: Data.Bytes(),
});

const FraudProofCatalogueProofStepSchema = Data.Enum([
  Data.Object({
    Branch: Data.Object({
      skip: Data.Integer(),
      neighbors: Data.Bytes(),
    }),
  }),
  Data.Object({
    Fork: Data.Object({
      skip: Data.Integer(),
      neighbor: FraudProofCatalogueProofNeighborSchema,
    }),
  }),
  Data.Object({
    Leaf: Data.Object({
      skip: Data.Integer(),
      key: Data.Bytes(),
      value: Data.Bytes(),
    }),
  }),
]);

const FraudProofCatalogueProofSchema = Data.Array(
  FraudProofCatalogueProofStepSchema,
);

type FraudProofCatalogueProofData = Data.Static<
  typeof FraudProofCatalogueProofSchema
>;

const MPF_NULL_HASH = Buffer.alloc(32);
const MPF_PATH_NIBBLE_COUNT = 64;

const mpfHash = (bytes: Uint8Array): Buffer =>
  Buffer.from(blake2b(bytes, { dkLen: 32 }));

const mpfCombine = (left: Uint8Array, right: Uint8Array): Buffer =>
  mpfHash(Buffer.concat([Buffer.from(left), Buffer.from(right)]));

const mpfNibbleAt = (path: Uint8Array, index: number): number => {
  if (
    !Number.isSafeInteger(index) ||
    index < 0 ||
    index >= MPF_PATH_NIBBLE_COUNT
  ) {
    throw new Error("Fraud-proof catalogue MPF path cursor is invalid");
  }
  const byte = path[Math.floor(index / 2)]!;
  return index % 2 === 0 ? Math.floor(byte / 16) : byte % 16;
};

const mpfPathNibbles = (
  path: Uint8Array,
  start: number,
  end: number,
): Buffer => {
  const result: number[] = [];
  for (let cursor = start; cursor < end; cursor += 1) {
    result.push(mpfNibbleAt(path, cursor));
  }
  return Buffer.from(result);
};

const mpfSuffix = (path: Uint8Array, cursor: number): Buffer => {
  if (
    !Number.isSafeInteger(cursor) ||
    cursor < 0 ||
    cursor > MPF_PATH_NIBBLE_COUNT
  ) {
    throw new Error("Fraud-proof catalogue MPF suffix cursor is invalid");
  }
  if (cursor % 2 === 0) {
    return Buffer.concat([
      Buffer.from([0xff]),
      Buffer.from(path).subarray(cursor / 2),
    ]);
  }
  return Buffer.concat([
    Buffer.from([0, mpfNibbleAt(path, cursor)]),
    Buffer.from(path).subarray((cursor + 1) / 2),
  ]);
};

const mpfSparseChildrenRoot = (
  children: ReadonlyMap<number, Uint8Array>,
): Buffer => {
  let level = Array.from<Uint8Array>({ length: 16 }).fill(MPF_NULL_HASH);
  for (const [nibble, root] of children) {
    if (!Number.isSafeInteger(nibble) || nibble < 0 || nibble > 15) {
      throw new Error("Fraud-proof catalogue MPF child nibble is invalid");
    }
    level[nibble] = root;
  }
  while (level.length > 1) {
    const next: Buffer[] = [];
    for (let index = 0; index < level.length; index += 2) {
      next.push(mpfCombine(level[index]!, level[index + 1]!));
    }
    level = next;
  }
  return Buffer.from(level[0]!);
};

type FraudProofCatalogueMpfEntry = {
  readonly path: Buffer;
  readonly valueHash: Buffer;
};

const reconstructFraudProofCatalogueMpfNode = (
  entries: readonly FraudProofCatalogueMpfEntry[],
  cursor: number,
): Buffer => {
  if (entries.length === 0) {
    throw new Error("Fraud-proof catalogue MPF node must not be empty");
  }
  if (entries.length === 1) {
    return mpfCombine(
      mpfSuffix(entries[0]!.path, cursor),
      entries[0]!.valueHash,
    );
  }

  let branchCursor = cursor;
  while (
    branchCursor < MPF_PATH_NIBBLE_COUNT &&
    entries.every(
      ({ path }) =>
        mpfNibbleAt(path, branchCursor) ===
        mpfNibbleAt(entries[0]!.path, branchCursor),
    )
  ) {
    branchCursor += 1;
  }
  if (branchCursor >= MPF_PATH_NIBBLE_COUNT) {
    throw new Error("Fraud-proof catalogue MPF contains duplicate key paths");
  }

  const grouped = new Map<number, FraudProofCatalogueMpfEntry[]>();
  for (const entry of entries) {
    const nibble = mpfNibbleAt(entry.path, branchCursor);
    const group = grouped.get(nibble) ?? [];
    group.push(entry);
    grouped.set(nibble, group);
  }
  const childRoots = new Map<number, Buffer>();
  for (const [nibble, group] of grouped) {
    childRoots.set(
      nibble,
      reconstructFraudProofCatalogueMpfNode(group, branchCursor + 1),
    );
  }
  return mpfCombine(
    mpfPathNibbles(entries[0]!.path, cursor, branchCursor),
    mpfSparseChildrenRoot(childRoots),
  );
};

const encodeFraudProofCatalogueKey = (categoryId: string): Buffer =>
  encodeCbor(Buffer.from(categoryId, "hex"));

const encodeFraudProofCatalogueValue = (scriptHash: string): Buffer =>
  encodeCbor(Buffer.from(scriptHash, "hex"));

const proofDataToMpfSteps = (
  proof: FraudProofCatalogueProofData,
): readonly MidgardMpfProofStep[] =>
  proof.map((step) => {
    if ("Branch" in step) {
      return {
        kind: "branch",
        skip: Number(step.Branch.skip),
        neighbors: Buffer.from(step.Branch.neighbors, "hex"),
      };
    }
    if ("Fork" in step) {
      return {
        kind: "fork",
        skip: Number(step.Fork.skip),
        neighbor: {
          nibble: Number(step.Fork.neighbor.nibble),
          prefix: Buffer.from(step.Fork.neighbor.prefix, "hex"),
          root: Buffer.from(step.Fork.neighbor.root, "hex"),
        },
      };
    }
    return {
      kind: "leaf",
      skip: Number(step.Leaf.skip),
      key: Buffer.from(step.Leaf.key, "hex"),
      value: Buffer.from(step.Leaf.value, "hex"),
    };
  });

export const verifyDeploymentManifestFraudProofCatalogueIdentity = (
  catalogue: DeploymentManifestFraudProofCatalogueIdentity,
): DeploymentManifestFraudProofCatalogueIdentity => {
  requireExactKeys(
    catalogue as unknown as Record<string, unknown>,
    ["root", "categories"],
    [],
    "contracts.fraudProofCatalogueMint.fraudProofCatalogue",
  );
  requireExactKeys(
    catalogue.categories as unknown as Record<string, unknown>,
    DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
    [],
    "contracts.fraudProofCatalogueMint.fraudProofCatalogue.categories",
  );
  const declaredRoot = requireHex(
    catalogue.root,
    32,
    "contracts.fraudProofCatalogueMint.fraudProofCatalogue.root",
  );
  const entries: FraudProofCatalogueMpfEntry[] = [];
  const encodedEntries = new Map<
    DeploymentManifestFraudProofCatalogueCategory,
    { readonly key: Buffer; readonly value: Buffer }
  >();
  const parsedCategories = {} as Record<
    DeploymentManifestFraudProofCatalogueCategory,
    DeploymentManifestFraudProofCatalogueCategoryIdentity
  >;

  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.forEach(
    (categoryName) => {
      const field = `contracts.fraudProofCatalogueMint.fraudProofCatalogue.categories.${categoryName}`;
      const candidate = requireRecord(
        (catalogue.categories as unknown as Record<string, unknown>)[
          categoryName
        ],
        `Deployment manifest ${field}`,
      );
      requireExactKeys(
        candidate,
        ["categoryId", "scriptHash", "membershipProofCbor"],
        [],
        field,
      );
      const category = {
        categoryId: requireHex(candidate.categoryId, 4, `${field}.categoryId`),
        scriptHash: requireHex(candidate.scriptHash, 28, `${field}.scriptHash`),
        membershipProofCbor: requireHex(
          candidate.membershipProofCbor,
          undefined,
          `${field}.membershipProofCbor`,
        ),
      } satisfies DeploymentManifestFraudProofCatalogueCategoryIdentity;
      parsedCategories[categoryName] = category;
      const expectedCategoryId =
        DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[categoryName];
      if (category.categoryId !== expectedCategoryId) {
        throw new Error(
          `Deployment manifest ${field}.categoryId must be ${expectedCategoryId}`,
        );
      }
      const key = encodeFraudProofCatalogueKey(category.categoryId);
      const value = encodeFraudProofCatalogueValue(category.scriptHash);
      encodedEntries.set(categoryName, { key, value });
      entries.push({
        path: mpfHash(key),
        valueHash: mpfHash(value),
      });
    },
  );

  const reconstructedRoot = reconstructFraudProofCatalogueMpfNode(
    entries,
    0,
  ).toString("hex");
  if (reconstructedRoot !== declaredRoot) {
    throw new Error(
      `Deployment manifest fraud-proof catalogue root mismatch: declared=${declaredRoot}, reconstructed=${reconstructedRoot}`,
    );
  }

  for (const categoryName of DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
    const field = `contracts.fraudProofCatalogueMint.fraudProofCatalogue.categories.${categoryName}`;
    const category = parsedCategories[categoryName];
    const encoded = encodedEntries.get(categoryName)!;
    let proof: FraudProofCatalogueProofData;
    try {
      proof = Data.from(
        category.membershipProofCbor,
        FraudProofCatalogueProofSchema,
      ) as unknown as FraudProofCatalogueProofData;
    } catch (cause) {
      throw new Error(
        `Deployment manifest ${field}.membershipProofCbor is not exact Proof CBOR: ${String(cause)}`,
      );
    }
    const canonicalProofCbor = Data.to(
      proof,
      asLucidSchema(FraudProofCatalogueProofSchema),
    );
    if (canonicalProofCbor !== category.membershipProofCbor) {
      throw new Error(
        `Deployment manifest ${field}.membershipProofCbor is not canonical`,
      );
    }
    let proofRoot: string;
    try {
      proofRoot = buildMidgardMpfProofFoldTrace({
        key: encoded.key,
        value: encoded.value,
        steps: proofDataToMpfSteps(proof),
      }).terminal.includingRoot.toString("hex");
    } catch (cause) {
      throw new Error(
        `Deployment manifest ${field}.membershipProofCbor is invalid: ${String(cause)}`,
      );
    }
    if (proofRoot !== declaredRoot) {
      throw new Error(
        `Deployment manifest ${field}.membershipProofCbor does not prove membership in catalogue root`,
      );
    }
  }
  return catalogue;
};
