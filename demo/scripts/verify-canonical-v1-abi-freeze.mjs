#!/usr/bin/env node
/**
 * IG1 — ABI gate, plus the consolidated IG2 proof-family gate review.
 *
 * This gate does NOT re-pin the surfaces the program already pins. F02 owns the
 * 132-row format registry, the HeaderV1 gate owns the three-way HeaderV1
 * contract, `sdk-abi-fixtures.test.ts` owns the byte-exact golden encodings,
 * and F20 owns the fault-proof coverage reconciliation. Re-pinning any of them
 * here would create a second copy of a number that can disagree with the first
 * — which is the defect an ABI freeze exists to prevent.
 *
 * What this gate adds is the BINDING layer over those surfaces: it decides
 * that the identities the program is about to freeze still agree *with each
 * other* and with the tree they are derived from. Concretely it closes the two
 * holes a per-package pin cannot see:
 *
 *   1. The deployment ABI identity is declared three times — in
 *      `midgard-core`, in `midgard-node` and in `midgard-sdk` — and until this
 *      gate existed nothing compared the copies. A 54-entry contract vector, a
 *      36-role reference-script map, a 37-role token map, an 11-entry
 *      catalogue order and a 6-entry step list were duplicated verbatim across
 *      packages with no cross-package equality anywhere in CI, so the #547
 *      registration could have landed in one package and not another.
 *   2. The frozen catalogue order was never bound to the validator tree that
 *      implements it. A family directory could be added, removed or renamed
 *      without any gate noticing that the catalogue no longer covers it.
 *
 * Every number this gate enforces is re-derived from a source of truth and
 * compared against the artifact; the artifact is never the authority for its
 * own claims. Identity sources are read from the git INDEX so a freeze
 * measures committed bytes rather than an uncommitted working tree.
 *
 * Fail-closed reading of GOAL_SPEC §11 IG1/IG2, mirroring the WG1/IG3 gate:
 * a freeze may only be published as PASS when every identity, relation,
 * blueprint binding and bound gate is measured — never inferred — and IG2
 * refuses to admit any integrated proof family that lacks one of its six
 * required evidence dimensions.
 *
 * Modes:
 *   --print-bootstrap   emit every derived value as JSON and exit 0. This is
 *                       how the artifact is regenerated after a legitimate ABI
 *                       change; it never reads or trusts the artifact.
 */

import { createHash } from "node:crypto";
import { execFileSync } from "node:child_process";
import { existsSync, readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const repositoryRoot = resolve(scriptDirectory, "../..");

const FREEZE_PATH = "docs/exec-plans/evidence/canonical-v1-abi-freeze-v1.json";
const BLUEPRINT_PATH = "onchain/aiken/plutus.json";
const REGISTRY_PATH =
  "docs/exec-plans/evidence/canonical-v1-format-registry-v1.json";
const GOLDEN_FIXTURE_PATH =
  "demo/midgard-node/tests/fixtures/transition-trace-abi.json";

/* ------------------------------------------------------------------ */
/* Inputs. Only the artifact and the generated blueprint are           */
/* redirectable, plus one identity source at a time: a self-test must  */
/* judge a seeded claim against the real tree, never against a second  */
/* seeded copy of it.                                                  */
/* ------------------------------------------------------------------ */

const flagValue = (name) => {
  const prefix = `--${name}=`;
  const found = process.argv.find((argument) => argument.startsWith(prefix));
  return found === undefined ? null : found.slice(prefix.length);
};

const inputPath = (flag, environmentVariable, fallback) => {
  const explicit = flagValue(flag) ?? process.env[environmentVariable] ?? null;
  return explicit === null
    ? resolve(repositoryRoot, fallback)
    : resolve(repositoryRoot, explicit);
};

/** `--source-<key>=<path>` redirects exactly one identity source to disk. */
const sourceOverrides = new Map(
  process.argv
    .filter((argument) => argument.startsWith("--source-"))
    .map((argument) => {
      const [name, ...rest] = argument.slice("--source-".length).split("=");
      return [name, resolve(repositoryRoot, rest.join("="))];
    }),
);

const errors = [];
const fail = (message) => {
  errors.push(message);
};

const execGit = (args) =>
  execFileSync("git", args, {
    cwd: repositoryRoot,
    encoding: "utf8",
    maxBuffer: 256 * 1024 * 1024,
    stdio: ["ignore", "pipe", "pipe"],
  });

/** Committed bytes are the authority for every identity source. */
const readIndexedText = (path) => execGit(["show", `:${path}`]);

const sha256Hex = (value) =>
  createHash("sha256")
    .update(typeof value === "string" ? value : JSON.stringify(value))
    .digest("hex");

const sameJson = (left, right) =>
  JSON.stringify(left) === JSON.stringify(right);

/* ------------------------------------------------------------------ */
/* A minimal TypeScript literal reader.                                */
/*                                                                     */
/* These identity vectors are `export const X = Object.freeze([...])`  */
/* declarations of plain string literals. Reading them with a scanner  */
/* that understands strings and comments is exact for that shape and   */
/* keeps this gate dependency-free; importing the built dist instead   */
/* would make the gate depend on a build step and would measure the    */
/* compiler's output rather than the committed source.                 */
/* ------------------------------------------------------------------ */

const skipStringLiteral = (text, start) => {
  const quote = text[start];
  let index = start + 1;
  while (index < text.length) {
    const character = text[index];
    if (character === "\\") {
      index += 2;
      continue;
    }
    if (character === quote) return index + 1;
    index += 1;
  }
  throw new Error("unterminated string literal");
};

/** Advance past comments/strings so bracket depth is never fooled by text. */
const skipNonCode = (text, index) => {
  const character = text[index];
  if (character === '"' || character === "'" || character === "`") {
    return skipStringLiteral(text, index);
  }
  if (character === "/" && text[index + 1] === "/") {
    const end = text.indexOf("\n", index);
    return end < 0 ? text.length : end;
  }
  if (character === "/" && text[index + 1] === "*") {
    const end = text.indexOf("*/", index + 2);
    return end < 0 ? text.length : end + 2;
  }
  return index;
};

const findLiteralRegion = (text, exportName) => {
  const declaration = `export const ${exportName}`;
  const declaredAt = text.indexOf(declaration);
  if (declaredAt < 0) {
    throw new Error(`${exportName} is not exported by this source`);
  }
  let index = declaredAt + declaration.length;
  while (index < text.length) {
    const skipped = skipNonCode(text, index);
    if (skipped !== index) {
      index = skipped;
      continue;
    }
    if (text[index] === "[" || text[index] === "{") break;
    index += 1;
  }
  if (index >= text.length) {
    throw new Error(`${exportName} has no array or object literal`);
  }
  const open = text[index];
  const close = open === "[" ? "]" : "}";
  let depth = 0;
  let cursor = index;
  while (cursor < text.length) {
    const skipped = skipNonCode(text, cursor);
    if (skipped !== cursor) {
      cursor = skipped;
      continue;
    }
    const character = text[cursor];
    if (character === open) depth += 1;
    else if (character === close) {
      depth -= 1;
      if (depth === 0) {
        return {
          kind: open === "[" ? "array" : "object",
          body: text.slice(index + 1, cursor),
        };
      }
    }
    cursor += 1;
  }
  throw new Error(`${exportName} literal is unbalanced`);
};

/** Drop comments so a commented-out entry can never be read as a real one. */
const withoutComments = (text) => {
  let output = "";
  let index = 0;
  while (index < text.length) {
    const character = text[index];
    if (character === '"' || character === "'" || character === "`") {
      const end = skipStringLiteral(text, index);
      output += text.slice(index, end);
      index = end;
      continue;
    }
    if (
      character === "/" &&
      (text[index + 1] === "/" || text[index + 1] === "*")
    ) {
      index = skipNonCode(text, index);
      continue;
    }
    output += character;
    index += 1;
  }
  return output;
};

const readStringVector = (text, exportName) => {
  const region = findLiteralRegion(text, exportName);
  if (region.kind !== "array") {
    throw new Error(`${exportName} must be an array literal`);
  }
  return [
    ...withoutComments(region.body).matchAll(/"((?:[^"\\]|\\.)*)"/gu),
  ].map((match) => match[1]);
};

const readStringRecord = (text, exportName) => {
  const region = findLiteralRegion(text, exportName);
  if (region.kind !== "object") {
    throw new Error(`${exportName} must be an object literal`);
  }
  return [
    ...withoutComments(region.body).matchAll(
      /"((?:[^"\\]|\\.)*)"\s*:\s*"((?:[^"\\]|\\.)*)"/gu,
    ),
  ].map((match) => [match[1], match[2]]);
};

/* ------------------------------------------------------------------ */
/* Declarations under test.                                            */
/* ------------------------------------------------------------------ */

const IDENTITY_SOURCE_PATHS = {
  core: "demo/midgard-core/src/deployment-manifest-identity-v1.ts",
  node: "demo/midgard-node/src/deployment-manifest-v1.ts",
  sdkCatalogue: "demo/midgard-sdk/src/fraud-proof/catalogue.ts",
  sdkReferenceScripts: "demo/midgard-sdk/src/reference-scripts.ts",
};

/**
 * The seven duplicated deployment-ABI identities. `sources` names every
 * package that declares the identity; a vector is frozen only when every
 * declaration is element-wise identical, so a single-package edit can never
 * drift silently.
 */
const CROSS_PACKAGE_IDENTITIES = [
  {
    id: "ABI-01",
    title: "Deployment contract-name vector",
    kind: "vector",
    sources: [
      { source: "core", exportName: "DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES" },
      { source: "node", exportName: "DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES" },
    ],
  },
  {
    id: "ABI-02",
    title: "Fraud-proof catalogue category order",
    kind: "vector",
    sources: [
      {
        source: "core",
        exportName:
          "DEPLOYMENT_MANIFEST_V1_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER",
      },
      {
        source: "sdkCatalogue",
        exportName: "FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER",
      },
    ],
  },
  {
    id: "ABI-03",
    title: "Reference-script role to contract map",
    kind: "record",
    sources: [
      {
        source: "core",
        exportName: "DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE",
      },
      {
        source: "node",
        exportName: "DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE",
      },
    ],
  },
  {
    id: "ABI-04",
    title: "Reference-script auth token names",
    kind: "record",
    sources: [
      {
        source: "core",
        exportName: "DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES",
      },
      {
        source: "sdkReferenceScripts",
        exportName: "REFERENCE_SCRIPT_AUTH_TOKEN_NAMES",
      },
    ],
  },
  {
    id: "ABI-05",
    title: "Deployment manifest root keys",
    kind: "vector",
    sources: [
      { source: "core", exportName: "DEPLOYMENT_MANIFEST_V1_ROOT_KEYS" },
    ],
  },
  {
    id: "ABI-06",
    title: "Deployment step names",
    kind: "vector",
    sources: [
      { source: "core", exportName: "DEPLOYMENT_MANIFEST_V1_STEP_NAMES" },
      { source: "node", exportName: "DEPLOYMENT_MANIFEST_V1_STEP_NAMES" },
    ],
  },
  {
    id: "ABI-07",
    title: "Required transaction-order contracts",
    kind: "vector",
    sources: [
      { source: "node", exportName: "REQUIRED_TRANSACTION_ORDER_CONTRACTS" },
    ],
  },
];

/**
 * The frozen bijection between the catalogue order and the validator tree that
 * implements it. This map is gate-owned on purpose: it is the claim under
 * test, so it may not live in the artifact it judges.
 */
const CATEGORY_BY_VALIDATOR_DIRECTORY = new Map([
  ["double-spend", "doubleSpend"],
  ["no-input", "nonExistentInput"],
  ["input-no-idx", "nonExistentInputNoIndex"],
  ["invalid-range", "invalidRange"],
  ["transition-trace", "transitionTrace"],
  ["zero-input", "zeroInput"],
  ["validation-trace", "validationTraceDispute"],
  ["da-hash-preimage", "daHashPreimage"],
  ["no-reference-input", "noReferenceInput"],
  ["reference-input-no-idx", "referenceInputNoIdx"],
  ["invalid-signature", "invalidSignature"],
  ["fabricated-deposit", "fabricatedDeposit"],
  ["fabricated-withdrawal", "fabricatedWithdrawal"],
  ["native-script-decoding", "nativeScriptDecoding"],
  ["missing-signature", "missingSignature"],
  ["missing-native-script-tx", "missingNativeScriptTx"],
  ["withdrawn-reference-input", "withdrawnReferenceInput"],
  ["canonical-decodability", "canonicalDecodability"],
  ["committed-field-shape", "committedFieldShape"],
  ["min-fee", "minFee"],
  ["withdrawal-mistag", "withdrawalMistag"],
  ["double-withdraw", "doubleWithdraw"],
  ["cross-block-duplicate-event", "crossBlockDuplicateEvent"],
  ["l2-tx-mistag", "l2TxMistag"],
  ["withdrawn-input", "withdrawnInput"],
]);

/**
 * Directories under `validators/fraud-proofs/` that are deliberately not
 * catalogue categories. `mpf-chunked-proof` is the #545 chunked-carriage
 * verifier shared by every family rather than a family of its own. All actual
 * fault-proof validator directories are registered in the production
 * catalogue by the canonical V1 registration wave.
 */
// `mpf-chunked-proof` is shared verifier infrastructure. The other three are
// PRE-REGISTRATION families (2026-08-28 tier-2 wave): built and compiled with
// category ids 00000019/0000001a/0000001b reserved, deliberately absent from
// `FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER` until the registration wave, so the
// catalogue cannot cover them yet.
const UNCATEGORISED_VALIDATOR_DIRECTORIES = [
  "input-set-uniqueness",
  "mint-authorization",
  "mpf-chunked-proof",
  "value-not-preserved",
];

/** GOAL_SPEC §11 IG2, in the order the clause names them. */
const IG2_DIMENSION_IDS = [
  "reachability",
  "tooling",
  "valid_block_rejection",
  "maximum_proof_fit",
  "correction",
  "matrix_evidence",
];

const STATUSES = new Set(["PASS", "BLOCKED"]);

/* ================================================================== */
/* A. Derivation. Nothing below reads the artifact.                    */
/* ================================================================== */

const sourceText = new Map();
const readIdentitySource = (key) => {
  if (sourceText.has(key)) return sourceText.get(key);
  const override = sourceOverrides.get(key);
  const text = override
    ? readFileSync(override, "utf8")
    : readIndexedText(IDENTITY_SOURCE_PATHS[key]);
  sourceText.set(key, text);
  return text;
};

/**
 * A `vector` identity is positional — its order IS the ABI — so it is compared
 * as declared. A `record` identity is a dictionary: every consumer in the tree
 * reads it by key (`MAP[role]`) or compares its key set with `requireExactKeys`,
 * so declaration order is not part of the ABI and equality is measured over the
 * key-sorted mapping. The declaration order is still measured and published as
 * `declarationOrder`, because two packages listing the same mapping in
 * different orders is worth seeing even when it changes no wire byte.
 */
const canonicalise = (kind, value) =>
  kind === "record"
    ? [...value].sort(([left], [right]) =>
        left < right ? -1 : left > right ? 1 : 0,
      )
    : value;

const deriveIdentity = (identity) => {
  const readings = identity.sources.map(({ source, exportName }) => {
    const text = readIdentitySource(source);
    const value =
      identity.kind === "vector"
        ? readStringVector(text, exportName)
        : readStringRecord(text, exportName);
    return { source, exportName, value };
  });
  const [first, ...rest] = readings;
  const canonical = canonicalise(identity.kind, first.value);
  const disagreeing = rest
    .filter(
      (reading) =>
        !sameJson(canonicalise(identity.kind, reading.value), canonical),
    )
    .map((reading) => `${reading.source}.${reading.exportName}`);
  const declarationOrder = rest.every((reading) =>
    sameJson(reading.value, first.value),
  )
    ? "IDENTICAL"
    : "DIVERGENT";
  return {
    id: identity.id,
    title: identity.title,
    kind: identity.kind,
    cardinality: first.value.length,
    declaredBy: readings.map(
      ({ source, exportName }) =>
        `${IDENTITY_SOURCE_PATHS[source]}:${exportName}`,
    ),
    digest: sha256Hex(canonical),
    declarationOrder,
    disagreeing,
    value: first.value,
  };
};

const derivedIdentities = new Map();
for (const identity of CROSS_PACKAGE_IDENTITIES) {
  try {
    derivedIdentities.set(identity.id, deriveIdentity(identity));
  } catch (error) {
    fail(`${identity.id} could not be derived from source: ${error.message}`);
  }
}

const identityValue = (id) => derivedIdentities.get(id)?.value ?? [];
const contractNames = new Set(identityValue("ABI-01"));
const categoryOrder = identityValue("ABI-02");
const roleMap = identityValue("ABI-03");
const tokenMap = identityValue("ABI-04");
const transactionOrderContracts = identityValue("ABI-07");

/** A 4-byte big-endian category index, hex-encoded, exactly as core derives it. */
const categoryIdForIndex = (index) => {
  const bytes = Buffer.alloc(4);
  bytes.writeUInt32BE(index, 0);
  return bytes.toString("hex");
};
const categoryIds = categoryOrder.map((_, index) => categoryIdForIndex(index));

/** Family directories carrying verifier logic, derived from the index. */
const validatorFilesByDirectory = new Map();
for (const path of execGit([
  "ls-files",
  "onchain/aiken/validators/fraud-proofs/",
])
  .split("\n")
  .filter((line) => line.length > 0)) {
  const directory = path.split("/")[4];
  if (directory === undefined || directory.endsWith(".ak")) continue;
  const files = validatorFilesByDirectory.get(directory) ?? [];
  files.push(path);
  validatorFilesByDirectory.set(directory, files);
}
const validatorDirectories = [...validatorFilesByDirectory.keys()].sort();

const mappedCategories = [...CATEGORY_BY_VALIDATOR_DIRECTORY.values()];
const roleNames = new Set(roleMap.map(([role]) => role));

/**
 * The relations that make the seven identities one ABI rather than seven
 * lists. Each entry measures its own violations from the tree; the artifact
 * may only publish what is measured here.
 */
const measuredRelations = new Map([
  [
    "every-reference-script-role-targets-a-frozen-contract",
    roleMap
      .filter(([, contract]) => !contractNames.has(contract))
      .map(([role, contract]) => `${role} -> ${contract}`),
  ],
  [
    "token-map-covers-every-reference-script-role",
    roleMap
      .filter(([role]) => !tokenMap.some(([key]) => key === role))
      .map(([role]) => role),
  ],
  [
    "transaction-order-contracts-are-frozen-contracts",
    transactionOrderContracts.filter(
      (contract) => !contractNames.has(contract),
    ),
  ],
  [
    "catalogue-order-covers-the-validator-tree",
    categoryOrder.filter((category) => !mappedCategories.includes(category)),
  ],
  [
    "validator-tree-is-covered-by-the-catalogue-or-declared-uncategorised",
    validatorDirectories.filter(
      (directory) =>
        !CATEGORY_BY_VALIDATOR_DIRECTORY.has(directory) &&
        !UNCATEGORISED_VALIDATOR_DIRECTORIES.includes(directory),
    ),
  ],
  [
    "declared-directories-still-exist",
    [
      ...CATEGORY_BY_VALIDATOR_DIRECTORY.keys(),
      ...UNCATEGORISED_VALIDATOR_DIRECTORIES,
    ].filter((directory) => !validatorDirectories.includes(directory)),
  ],
]);

/**
 * `token-map-extra-roles` is a measurement, not a violation: the SDK's token
 * map legitimately carries one role more than the contract map (the direct CEK
 * resolver has an auth token but no deployment contract of its own). It is
 * published so the delta cannot grow unnoticed.
 */
const tokenMapExtraRoles = tokenMap
  .filter(([role]) => !roleNames.has(role))
  .map(([role]) => role);

/* --- IG2 per-family measurement ----------------------------------- */

/**
 * The offchain builder/submitter each family is proven to have. IG2's "tooling"
 * clause is about a family being *buildable and submittable*, so it is measured
 * against these modules rather than against the SDK surface, which several
 * families reach through a shared generic module. Gate-owned: a declared module
 * that has left the index fails this gate rather than silently downgrading the
 * family, and a family with no module measures BLOCKED.
 */
const FAMILY_OFFCHAIN_MODULES = new Map([
  // The two #579-era families keep their verifier logic in the SDK rather than
  // behind a `midgard-fault-proofs` `prepare-*` builder, so that is what this
  // table names for them. IG2's tooling clause asks whether a family is
  // buildable and submittable, not which package it is buildable from.
  [
    "canonical-decodability",
    ["demo/midgard-sdk/src/fraud-proof/canonical-decodability-v1.ts"],
  ],
  [
    "committed-field-shape",
    ["demo/midgard-sdk/src/fraud-proof/committed-field-shape-v1.ts"],
  ],
  [
    "da-hash-preimage",
    ["demo/midgard-fault-proofs/src/prepare-da-hash-preimage.ts"],
  ],
  ["double-spend", ["demo/midgard-fault-proofs/src/prepare-double-spend.ts"]],
  // #482 landed the two `fabricated-*` families' verifier trees and their
  // offchain builders, but the #617 ABI wave is the first regeneration to
  // measure them here, so this table names them now. Both build through a
  // `midgard-fault-proofs` `prepare-*` builder like the rest of that group;
  // the four `submit-*-step-0N.ts` modules and the SDK byte twins sit behind
  // them and are not separately named, exactly as for `double-spend`.
  [
    "fabricated-deposit",
    ["demo/midgard-fault-proofs/src/prepare-fabricated-deposit.ts"],
  ],
  [
    "fabricated-withdrawal",
    ["demo/midgard-fault-proofs/src/prepare-fabricated-withdrawal.ts"],
  ],
  ["input-no-idx", ["demo/midgard-fault-proofs/src/prepare-input-no-idx.ts"]],
  // The three 2026-08-28 pre-registration families keep the whole offchain
  // lane under a family directory (no `prepare-*` shim): the named module is
  // the family's builder entry point, with the submit-* modules behind it.
  [
    "input-set-uniqueness",
    ["demo/midgard-fault-proofs/src/input-set-uniqueness/scan-v1.ts"],
  ],
  ["invalid-range", ["demo/midgard-fault-proofs/src/prepare-invalid-range.ts"]],
  [
    "invalid-signature",
    ["demo/midgard-fault-proofs/src/prepare-invalid-signature.ts"],
  ],
  [
    "mint-authorization",
    ["demo/midgard-fault-proofs/src/mint-authorization/prover-v1.ts"],
  ],
  [
    "cross-block-duplicate-event",
    ["demo/midgard-fault-proofs/src/cross-block-duplicate-event/prepare-v1.ts"],
  ],
  [
    "double-withdraw",
    ["demo/midgard-fault-proofs/src/prepare-double-withdraw.ts"],
  ],
  [
    "l2-tx-mistag",
    ["demo/midgard-fault-proofs/src/l2-tx-mistag/prepare-l2-tx-mistag-v1.ts"],
  ],
  ["min-fee", ["demo/midgard-fault-proofs/src/prepare-min-fee.ts"]],
  [
    "missing-native-script-tx",
    ["demo/midgard-fault-proofs/src/missing-native-script-tx/prepare-v1.ts"],
  ],
  [
    "missing-signature",
    ["demo/midgard-fault-proofs/src/missing-signature/prover-v1.ts"],
  ],
  [
    "mpf-chunked-proof",
    ["demo/midgard-fault-proofs/src/publish-proof-chunks.ts"],
  ],
  ["no-input", ["demo/midgard-fault-proofs/src/prepare-non-existent-input.ts"]],
  [
    "no-reference-input",
    ["demo/midgard-fault-proofs/src/prepare-no-reference-input.ts"],
  ],
  [
    "native-script-decoding",
    ["demo/midgard-fault-proofs/src/native-script-decoding/prover-v1.ts"],
  ],
  [
    "reference-input-no-idx",
    ["demo/midgard-fault-proofs/src/prepare-reference-input-no-idx.ts"],
  ],
  [
    "transition-trace",
    ["demo/midgard-fault-proofs/src/transition-trace/detect.ts"],
  ],
  [
    "validation-trace",
    ["demo/midgard-fault-proofs/src/validation-dispute/submit.ts"],
  ],
  [
    "value-not-preserved",
    ["demo/midgard-fault-proofs/src/value-not-preserved/prover-v1.ts"],
  ],
  [
    "withdrawal-mistag",
    [
      "demo/midgard-fault-proofs/src/withdrawal-mistag/prepare-withdrawal-mistag.ts",
    ],
  ],
  [
    "withdrawn-input",
    ["demo/midgard-fault-proofs/src/prepare-withdrawn-input.ts"],
  ],
  [
    "withdrawn-reference-input",
    [
      "demo/midgard-fault-proofs/src/withdrawn-reference-input/prepare-withdrawn-reference-input-v1.ts",
    ],
  ],
  ["zero-input", ["demo/midgard-fault-proofs/src/prepare-zero-input.ts"]],
]);

const COVERAGE_MATRIX_PATH = "docs/fault-proofs/coverage-matrix.md";
const coverageMatrix = readIndexedText(COVERAGE_MATRIX_PATH);

/**
 * One snapshot of the index rather than a `git cat-file` per citation: this
 * gate checks a few hundred cited paths and a process spawn each would make it
 * slow enough to be skipped, which is its own failure mode.
 */
const indexedPaths = new Set(
  execGit(["ls-files"])
    .split("\n")
    .filter((line) => line.length > 0),
);
const indexHas = (path) => indexedPaths.has(path);

const registeredCategories = new Set(categoryOrder);

/**
 * Three of the six IG2 dimensions are mechanically decidable from the tree, so
 * this gate decides them rather than reading them out of the artifact. The
 * other three — valid-block rejection, maximum proof fit and correction — are
 * measurements the Q-family rows own; the artifact declares them and this gate
 * requires every cited path to exist, so a PASS can never rest on a file that
 * is not there.
 */
const DERIVED_IG2_DIMENSIONS = ["reachability", "tooling", "matrix_evidence"];

const measuredFamilies = new Map(
  validatorDirectories.map((directory) => {
    const category = CATEGORY_BY_VALIDATOR_DIRECTORY.get(directory) ?? null;
    const integrated = category !== null && registeredCategories.has(category);
    const validators = validatorFilesByDirectory.get(directory) ?? [];
    const offchainModules = FAMILY_OFFCHAIN_MODULES.get(directory) ?? null;
    const missingModules =
      offchainModules === null
        ? []
        : offchainModules.filter((path) => !indexHas(path));
    return [
      directory,
      {
        directory,
        category,
        integrated,
        validatorFiles: validators.length,
        entryModule: [...validators].sort()[0] ?? null,
        offchainModules,
        missingModules,
        derived: {
          reachability: integrated && validators.length > 0,
          tooling:
            offchainModules !== null &&
            offchainModules.length > 0 &&
            missingModules.length === 0,
          matrix_evidence: coverageMatrix.includes(directory),
        },
      },
    ];
  }),
);

for (const family of measuredFamilies.values()) {
  if (family.offchainModules === null) {
    fail(
      `${family.directory} carries verifier logic but this gate declares no offchain tooling for it; extend FAMILY_OFFCHAIN_MODULES`,
    );
  }
  for (const path of family.missingModules) {
    fail(
      `${family.directory} declares the offchain module ${path}, which is not in the index`,
    );
  }
}

/* --- Blueprint --------------------------------------------------- */

const blueprintPath = inputPath(
  "blueprint-under-test",
  "MIDGARD_REAL_BLUEPRINT_PATH",
  BLUEPRINT_PATH,
);
const aikenToml = readIndexedText("onchain/aiken/aiken.toml");
const declaredCompiler =
  /^compiler\s*=\s*"([^"]+)"/mu.exec(aikenToml)?.[1] ?? null;

let measuredBlueprint = null;
if (existsSync(blueprintPath)) {
  const blueprintBytes = readFileSync(blueprintPath);
  const blueprint = JSON.parse(blueprintBytes.toString("utf8"));
  const validators = Array.isArray(blueprint.validators)
    ? blueprint.validators
    : [];
  const titles = [...new Set(validators.map(({ title }) => title))].sort();
  measuredBlueprint = {
    path: BLUEPRINT_PATH,
    validatorCount: validators.length,
    uniqueValidatorTitles: titles.length,
    validatorTitleDigest: sha256Hex(titles),
    compilerVersion: blueprint.preamble?.compiler?.version ?? null,
    plutusVersion: blueprint.preamble?.plutusVersion ?? null,
    declaredCompiler,
    measuredSha256: createHash("sha256").update(blueprintBytes).digest("hex"),
  };
}

/* --- Bound gates -------------------------------------------------- */

const registry = JSON.parse(readIndexedText(REGISTRY_PATH));
const goldenFixtures = JSON.parse(readIndexedText(GOLDEN_FIXTURE_PATH));
const headerRow = registry.formats?.find(({ id }) => id === "L01");

const measuredBoundGates = new Map([
  [
    "format-registry",
    {
      script: "demo/scripts/verify-canonical-v1-format-registry.mjs",
      artifact: REGISTRY_PATH,
      counts: {
        rows: Array.isArray(registry.formats) ? registry.formats.length : -1,
        forbiddenActivePatterns: Array.isArray(registry.forbiddenActivePatterns)
          ? registry.forbiddenActivePatterns.length
          : -1,
        withCounterpartTests:
          registry.crossLanguageSummary?.withCounterpartTests ?? -1,
        notApplicable: registry.crossLanguageSummary?.notApplicable ?? -1,
        unverified: Array.isArray(registry.formats)
          ? registry.formats.filter(({ auditStatus }) => auditStatus !== "PASS")
              .length
          : -1,
      },
    },
  ],
  [
    "header-v1-abi",
    {
      script: "demo/scripts/verify-canonical-v1-header-v1-abi.mjs",
      artifact: REGISTRY_PATH,
      counts: {
        constructorTag: headerRow?.canonicalForms?.[0]?.constructor?.tag ?? -1,
        constructorArity:
          headerRow?.canonicalForms?.[0]?.constructorArity ?? -1,
      },
    },
  ],
  [
    "sdk-abi-fixtures",
    {
      script: "demo/midgard-node/tests/sdk-abi-fixtures.test.ts",
      artifact: GOLDEN_FIXTURE_PATH,
      counts: {
        goldenFixtures: Object.keys(goldenFixtures.fixtures ?? {}).length,
        goldenVersion: goldenFixtures.version ?? -1,
      },
    },
  ],
]);

/* --- Drift against the reconciliation baseline -------------------- */

/**
 * The F20 reconciliation is the program's inventory of what is deployed. It is
 * a captured artifact, so it can fall behind the tree it describes — and an ABI
 * freeze is exactly the moment that must be visible. These findings are
 * measured here, not asserted in the artifact, so the freeze cannot be
 * published clean by deleting a finding from a file.
 */
const RECONCILIATION_PATH =
  "docs/exec-plans/evidence/canonical-v1-fault-proof-reconciliation-v1.json";
const reconciliation = JSON.parse(readIndexedText(RECONCILIATION_PATH));
const reconciledCategories =
  reconciliation.bindingInventory?.registeredCategoryNames ?? [];
const reconciledDirectories =
  reconciliation.bindingInventory?.deployedValidatorDirectories ?? [];

const measuredDriftFindings = [];
if (!sameJson(reconciledCategories, categoryOrder)) {
  measuredDriftFindings.push({
    id: "f20-registered-categories-behind-the-frozen-catalogue",
    surface: `${RECONCILIATION_PATH}:bindingInventory.registeredCategoryNames`,
    measured: `reconciliation records ${String(reconciledCategories.length)} registered categories; the frozen catalogue order is ${String(categoryOrder.length)}`,
    owner: "F20",
  });
}
if (!sameJson([...reconciledDirectories].sort(), validatorDirectories)) {
  measuredDriftFindings.push({
    id: "f20-validator-directories-behind-the-index",
    surface: `${RECONCILIATION_PATH}:bindingInventory.deployedValidatorDirectories`,
    measured: `reconciliation records ${String(reconciledDirectories.length)} validator directories; the index carries ${String(validatorDirectories.length)}`,
    owner: "F20",
  });
}

/* ================================================================== */
/* B. Bootstrap mode.                                                  */
/* ================================================================== */

if (process.argv.includes("--print-bootstrap")) {
  process.stdout.write(
    `${JSON.stringify(
      {
        crossPackageIdentities: [...derivedIdentities.values()].map(
          ({
            id,
            title,
            kind,
            cardinality,
            declaredBy,
            digest,
            declarationOrder,
          }) => ({
            id,
            title,
            kind,
            cardinality,
            declaredBy,
            digest,
            agreement: "EXACT",
            declarationOrder,
          }),
        ),
        derivedRelations: [...measuredRelations.entries()].map(
          ([id, violations]) => ({
            id,
            status: violations.length === 0 ? "PASS" : "BLOCKED",
            violations,
          }),
        ),
        tokenMapExtraRoles,
        catalogueBinding: {
          categoryOrder,
          categoryIds,
          validatorDirectories,
          categoryByValidatorDirectory: Object.fromEntries(
            CATEGORY_BY_VALIDATOR_DIRECTORY,
          ),
          uncategorisedValidatorDirectories:
            UNCATEGORISED_VALIDATOR_DIRECTORIES,
        },
        blueprintIdentity: measuredBlueprint,
        boundGates: [...measuredBoundGates.entries()].map(([id, gate]) => ({
          id,
          ...gate,
        })),
        ig2DerivedDimensions: DERIVED_IG2_DIMENSIONS,
        ig2Families: [...measuredFamilies.values()],
        driftFindings: measuredDriftFindings,
      },
      null,
      2,
    )}\n`,
  );
  process.exit(0);
}

/* ================================================================== */
/* C. Artifact verification.                                           */
/* ================================================================== */

const freezePath = inputPath(
  "freeze-under-test",
  "MIDGARD_ABI_FREEZE_PATH",
  FREEZE_PATH,
);
if (!existsSync(freezePath)) {
  process.stderr.write(`ABI freeze artifact does not exist: ${freezePath}\n`);
  process.exit(1);
}
const freeze = JSON.parse(readFileSync(freezePath, "utf8"));

if (
  freeze.schemaVersion !== "midgard-canonical-v1-abi-freeze-v1" ||
  !Array.isArray(freeze.goalIds) ||
  freeze.goalIds.length !== 2 ||
  !freeze.goalIds.includes("IG1") ||
  !freeze.goalIds.includes("IG2")
) {
  fail("the freeze artifact must be the IG1 + IG2 consolidated ABI review");
}
if (!Array.isArray(freeze.waivers) || freeze.waivers.length !== 0) {
  fail(
    "an ABI freeze admits no waivers; the waivers array must exist and be empty",
  );
}

/* --- 1. Cross-package identities ---------------------------------- */

if (
  !Array.isArray(freeze.crossPackageIdentities) ||
  freeze.crossPackageIdentities.length !== CROSS_PACKAGE_IDENTITIES.length
) {
  fail(
    `crossPackageIdentities must enumerate exactly ${String(CROSS_PACKAGE_IDENTITIES.length)} frozen identities`,
  );
} else {
  freeze.crossPackageIdentities.forEach((published, index) => {
    const expected = CROSS_PACKAGE_IDENTITIES[index];
    if (published?.id !== expected.id) {
      fail(`crossPackageIdentities[${String(index)}] must be ${expected.id}`);
      return;
    }
    const derived = derivedIdentities.get(expected.id);
    if (derived === undefined) return;
    if (published.cardinality !== derived.cardinality) {
      fail(
        `${expected.id} cardinality is ${String(published.cardinality)} but source declares ${String(derived.cardinality)}`,
      );
    }
    if (published.digest !== derived.digest) {
      fail(`${expected.id} digest is stale: source derives ${derived.digest}`);
    }
    if (!sameJson(published.declaredBy, derived.declaredBy)) {
      fail(
        `${expected.id} must name the exact declaring sources it was derived from`,
      );
    }
    if (derived.disagreeing.length > 0) {
      fail(
        `${expected.id} is not frozen: ${derived.disagreeing.join(", ")} disagrees with ${derived.declaredBy[0]}`,
      );
    }
    if (published.agreement !== "EXACT") {
      fail(`${expected.id} must publish agreement EXACT`);
    }
    if (published.declarationOrder !== derived.declarationOrder) {
      fail(
        `${expected.id} publishes declarationOrder ${String(published.declarationOrder)} but the sources measure ${derived.declarationOrder}`,
      );
    }
    // A positional identity whose declarations diverge in order is not frozen,
    // whatever its mapping says.
    if (
      expected.kind === "vector" &&
      derived.declarationOrder !== "IDENTICAL"
    ) {
      fail(
        `${expected.id} is positional and its declarations disagree in order`,
      );
    }
  });
}

/* --- 2. Derived relations ----------------------------------------- */

if (
  !Array.isArray(freeze.derivedRelations) ||
  freeze.derivedRelations.length !== measuredRelations.size
) {
  fail(
    `derivedRelations must enumerate exactly ${String(measuredRelations.size)} relations`,
  );
} else {
  const relationIds = [...measuredRelations.keys()];
  freeze.derivedRelations.forEach((published, index) => {
    const expectedId = relationIds[index];
    if (published?.id !== expectedId) {
      fail(`derivedRelations[${String(index)}] must be ${expectedId}`);
      return;
    }
    const violations = measuredRelations.get(expectedId);
    if (!sameJson(published.violations ?? [], violations)) {
      fail(
        `${expectedId} publishes ${JSON.stringify(published.violations ?? [])} but the tree measures ${JSON.stringify(violations)}`,
      );
    }
    if (published.status === "PASS" && violations.length > 0) {
      fail(
        `${expectedId} is PASS while it measures ${String(violations.length)} violations`,
      );
    }
    if (published.status !== "PASS" && violations.length === 0) {
      fail(`${expectedId} measures no violation but is not published PASS`);
    }
  });
}
if (!sameJson(freeze.tokenMapExtraRoles, tokenMapExtraRoles)) {
  fail(
    `tokenMapExtraRoles must equal the measured delta ${JSON.stringify(tokenMapExtraRoles)}`,
  );
}

/* --- 3. Catalogue binding ----------------------------------------- */

const catalogueBinding = freeze.catalogueBinding;
if (catalogueBinding === undefined || catalogueBinding === null) {
  fail("catalogueBinding is required");
} else {
  if (!sameJson(catalogueBinding.categoryOrder, categoryOrder)) {
    fail("catalogueBinding.categoryOrder must equal the source-derived order");
  }
  if (!sameJson(catalogueBinding.categoryIds, categoryIds)) {
    fail(
      `catalogueBinding.categoryIds must be the dense 4-byte range ${categoryIds[0]}..${categoryIds[categoryIds.length - 1]}`,
    );
  }
  if (!sameJson(catalogueBinding.validatorDirectories, validatorDirectories)) {
    fail(
      `catalogueBinding.validatorDirectories must equal the ${String(validatorDirectories.length)} directories the index carries`,
    );
  }
  if (
    !sameJson(
      catalogueBinding.categoryByValidatorDirectory,
      Object.fromEntries(CATEGORY_BY_VALIDATOR_DIRECTORY),
    )
  ) {
    fail(
      "catalogueBinding.categoryByValidatorDirectory must equal the gate-owned bijection",
    );
  }
  if (
    !sameJson(
      catalogueBinding.uncategorisedValidatorDirectories,
      UNCATEGORISED_VALIDATOR_DIRECTORIES,
    )
  ) {
    fail(
      "catalogueBinding.uncategorisedValidatorDirectories must equal the gate-owned uncategorised set",
    );
  }
}

/* --- 4. Blueprint identity ---------------------------------------- */

const publishedBlueprint = freeze.blueprintIdentity;
if (publishedBlueprint === undefined || publishedBlueprint === null) {
  fail("blueprintIdentity is required");
} else if (measuredBlueprint === null) {
  fail(
    `Aiken blueprint does not exist: ${blueprintPath}; run \`aiken build --env testnet\` first`,
  );
} else {
  if (publishedBlueprint.path !== BLUEPRINT_PATH) {
    fail(`blueprintIdentity.path must be ${BLUEPRINT_PATH}`);
  }
  // These five are the blueprint identity proper: they are identical whichever
  // `aiken.toml` environment the build selected, because an environment changes
  // constants inside validators and never the validator set itself. Measured
  // both ways before this list was fixed.
  for (const key of [
    "validatorCount",
    "uniqueValidatorTitles",
    "validatorTitleDigest",
    "compilerVersion",
    "plutusVersion",
    "declaredCompiler",
  ]) {
    if (publishedBlueprint[key] !== measuredBlueprint[key]) {
      fail(
        `blueprintIdentity.${key} is ${JSON.stringify(publishedBlueprint[key])} but the built blueprint measures ${JSON.stringify(measuredBlueprint[key])}`,
      );
    }
  }
  // `measuredSha256` is attribution, not identity. `plutus.json` is a gitignored
  // build output whose bytes DO depend on the selected environment, so pinning
  // them would make this gate fail on a legitimate `aiken build` that chose a
  // different env. It must still be a real digest: a blank or shortened value
  // would let a run claim provenance it never had.
  if (!/^[0-9a-f]{64}$/u.test(String(publishedBlueprint.measuredSha256))) {
    fail(
      "blueprintIdentity.measuredSha256 must record the sha256 of the blueprint that was measured",
    );
  }
  if (
    typeof publishedBlueprint.measuredEnv !== "string" ||
    publishedBlueprint.measuredEnv.length === 0
  ) {
    fail(
      "blueprintIdentity.measuredEnv must name the aiken environment the recorded digest came from",
    );
  }
  // IG1's "exact pinned compiler rebuild": the blueprint an ABI claim is
  // measured against must come from the compiler the repository declares. Since
  // #579's owner ruling A that compiler IS the patched fork — it is the sole
  // authority for build, check and fmt, and stock has no remaining role, so
  // there is no "released compiler" to prefer over it. The fork reports its own
  // build metadata (`v1.1.23+2a78108`) where `aiken.toml` declares only the
  // release (`v1.1.23`), so the release must match exactly and only a `+build`
  // suffix may follow it.
  const producedRelease = String(measuredBlueprint.compilerVersion).split(
    "+",
  )[0];
  if (producedRelease !== declaredCompiler) {
    fail(
      `the blueprint was produced by ${String(measuredBlueprint.compilerVersion)} but aiken.toml declares ${String(declaredCompiler)}`,
    );
  }
}

/* --- 5. Bound gates ----------------------------------------------- */

if (
  !Array.isArray(freeze.boundGates) ||
  freeze.boundGates.length !== measuredBoundGates.size
) {
  fail(
    `boundGates must enumerate exactly ${String(measuredBoundGates.size)} bound gates`,
  );
} else {
  const gateIds = [...measuredBoundGates.keys()];
  freeze.boundGates.forEach((published, index) => {
    const expectedId = gateIds[index];
    if (published?.id !== expectedId) {
      fail(`boundGates[${String(index)}] must be ${expectedId}`);
      return;
    }
    const measured = measuredBoundGates.get(expectedId);
    if (published.script !== measured.script) {
      fail(`${expectedId} must bind ${measured.script}`);
    }
    if (published.artifact !== measured.artifact) {
      fail(`${expectedId} must bind the artifact ${measured.artifact}`);
    }
    for (const path of [measured.script, measured.artifact]) {
      if (!indexHas(path)) {
        fail(`${expectedId} binds ${path}, which is not in the index`);
      }
    }
    if (!sameJson(published.counts, measured.counts)) {
      fail(
        `${expectedId} publishes ${JSON.stringify(published.counts)} but the bound surface measures ${JSON.stringify(measured.counts)}`,
      );
    }
  });
}

/* --- 6. IG2 proof-family gate ------------------------------------- */

const ig2 = freeze.ig2ProofFamilies;
let integrableFamilies = 0;
let ig2Violations = 0;
let familyCount = 0;

if (ig2 === undefined || ig2 === null) {
  fail("ig2ProofFamilies is required");
} else {
  if (!sameJson(ig2.dimensions, IG2_DIMENSION_IDS)) {
    fail(
      `ig2ProofFamilies.dimensions must be the exact GOAL_SPEC §11 IG2 clause order: ${IG2_DIMENSION_IDS.join(", ")}`,
    );
  }
  if (!sameJson(ig2.derivedDimensions, DERIVED_IG2_DIMENSIONS)) {
    fail(
      `ig2ProofFamilies.derivedDimensions must name the ${String(DERIVED_IG2_DIMENSIONS.length)} dimensions this gate decides from the tree`,
    );
  }
  if (
    !Array.isArray(ig2.families) ||
    ig2.families.length !== validatorDirectories.length
  ) {
    fail(
      `ig2ProofFamilies.families must enumerate exactly the ${String(validatorDirectories.length)} directories that carry verifier logic`,
    );
  } else {
    familyCount = ig2.families.length;
    ig2.families.forEach((family, index) => {
      const directory = validatorDirectories[index];
      if (family?.directory !== directory) {
        fail(
          `ig2ProofFamilies.families[${String(index)}] must be ${directory}`,
        );
        return;
      }
      const measured = measuredFamilies.get(directory);
      if ((family.category ?? null) !== measured.category) {
        fail(
          `${directory} must publish category ${JSON.stringify(measured.category)}`,
        );
      }
      if (family.integrated !== measured.integrated) {
        fail(
          `${directory} publishes integrated=${String(family.integrated)} but the frozen catalogue order says ${String(measured.integrated)}`,
        );
      }
      if (
        family.evidence === null ||
        typeof family.evidence !== "object" ||
        !sameJson(Object.keys(family.evidence), IG2_DIMENSION_IDS)
      ) {
        fail(
          `${directory} must publish all six IG2 dimensions in clause order`,
        );
        return;
      }
      const missing = [];
      for (const dimension of IG2_DIMENSION_IDS) {
        const cell = family.evidence[dimension];
        if (!STATUSES.has(cell?.status)) {
          fail(
            `${directory}.${dimension} status must be measured as PASS or BLOCKED`,
          );
          missing.push(dimension);
          continue;
        }
        if (typeof cell.evidence !== "string" || cell.evidence.length === 0) {
          fail(
            `${directory}.${dimension} must cite the evidence it was measured from`,
          );
        }
        if (!Array.isArray(cell.paths)) {
          fail(`${directory}.${dimension} must carry a paths array`);
        } else {
          for (const path of cell.paths) {
            if (!indexHas(path)) {
              fail(
                `${directory}.${dimension} cites ${path}, which is not in the index`,
              );
            }
          }
          // A PASS that cites nothing is an assertion, not a measurement.
          if (cell.status === "PASS" && cell.paths.length === 0) {
            fail(
              `${directory}.${dimension} is PASS and must cite at least one surface it was measured from`,
            );
          }
        }
        // The three decidable dimensions are decided here, not read.
        if (DERIVED_IG2_DIMENSIONS.includes(dimension)) {
          const expected = measured.derived[dimension] ? "PASS" : "BLOCKED";
          if (cell.status !== expected) {
            fail(
              `${directory}.${dimension} publishes ${String(cell.status)} but the tree measures ${expected}`,
            );
          }
        }
        if (cell.status !== "PASS") missing.push(dimension);
      }
      const integrable = missing.length === 0;
      if (family.integrable !== integrable) {
        fail(
          `${directory} publishes integrable=${String(family.integrable)} but ${String(missing.length)} dimensions are not PASS`,
        );
      }
      if (integrable) integrableFamilies += 1;
      // The IG2 clause itself: a family that is integrated while missing a
      // required dimension is a violation of the gate, not a pending task.
      if (measured.integrated && !integrable) {
        ig2Violations += 1;
        if (!Array.isArray(family.violates) || family.violates.length === 0) {
          fail(
            `${directory} is integrated while missing ${missing.join(", ")} and must publish the IG2 clauses it violates`,
          );
        } else if (
          !sameJson([...family.violates].sort(), [...missing].sort())
        ) {
          fail(
            `${directory} publishes violates ${JSON.stringify(family.violates)} but measures ${JSON.stringify(missing)}`,
          );
        }
      }
    });
  }
  if (ig2.integrableFamilies !== integrableFamilies) {
    fail(
      `ig2ProofFamilies.integrableFamilies is ${String(ig2.integrableFamilies)} but ${String(integrableFamilies)} families measure integrable`,
    );
  }
  if (ig2.violations !== ig2Violations) {
    fail(
      `ig2ProofFamilies.violations is ${String(ig2.violations)} but ${String(ig2Violations)} integrated families lack a required dimension`,
    );
  }
  if (!STATUSES.has(ig2.status)) {
    fail("ig2ProofFamilies.status must be measured as PASS or BLOCKED");
  }
  if (
    ig2.status === "PASS" &&
    (ig2Violations > 0 || integrableFamilies !== familyCount)
  ) {
    fail(
      "ig2ProofFamilies.status is PASS while a family with verifier logic lacks a required evidence dimension",
    );
  }
  if (
    ig2.status !== "PASS" &&
    ig2Violations === 0 &&
    familyCount > 0 &&
    integrableFamilies === familyCount
  ) {
    fail(
      "every proof family is integrable but ig2ProofFamilies.status is not PASS",
    );
  }
}

/* --- 7. Drift findings and freeze status -------------------------- */

if (!Array.isArray(freeze.driftFindings)) {
  fail(
    "driftFindings must exist; an empty array is the claim that nothing drifted",
  );
} else if (!sameJson(freeze.driftFindings, measuredDriftFindings)) {
  fail(
    `driftFindings must equal the ${String(measuredDriftFindings.length)} findings this gate measures: ${measuredDriftFindings.map(({ id }) => id).join(", ") || "(none)"}`,
  );
}

const relationViolations = [...measuredRelations.values()].reduce(
  (total, violations) => total + violations.length,
  0,
);
const everythingFrozen =
  ig2Violations === 0 &&
  familyCount > 0 &&
  integrableFamilies === familyCount &&
  relationViolations === 0 &&
  measuredDriftFindings.length === 0;

if (!STATUSES.has(freeze.freezeStatus)) {
  fail("freezeStatus must be measured as PASS or BLOCKED");
}
if (freeze.freezeStatus === "PASS" && !everythingFrozen) {
  fail(
    "freezeStatus is PASS while a relation, family dimension or drift finding is outstanding",
  );
}
if (freeze.freezeStatus !== "PASS" && everythingFrozen) {
  fail(
    "every identity, relation and family is measured clean but freezeStatus is not PASS: the artifact is stale",
  );
}

if (errors.length > 0) {
  process.stderr.write(
    `Canonical V1 ABI freeze verification failed (${String(errors.length)}):\n`,
  );
  for (const error of errors) process.stderr.write(`- ${error}\n`);
  process.exit(1);
}

process.stdout.write(
  [
    "Canonical V1 ABI freeze verified.",
    `freezeStatus: ${freeze.freezeStatus}`,
    `cross-package identities re-derived: ${String(derivedIdentities.size)}/${String(CROSS_PACKAGE_IDENTITIES.length)}`,
    `derived relations: ${String(measuredRelations.size)}, violations: ${String(relationViolations)}`,
    `catalogue categories: ${String(categoryOrder.length)}, validator directories: ${String(validatorDirectories.length)}`,
    `blueprint: ${String(measuredBlueprint?.validatorCount)} validators from ${String(measuredBlueprint?.compilerVersion)}`,
    `bound gates: ${String(measuredBoundGates.size)}`,
    `IG2 families: ${String(familyCount)}, integrable: ${String(integrableFamilies)}, violations: ${String(ig2Violations)}, status ${String(freeze.ig2ProofFamilies?.status)}`,
    `drift findings: ${String(measuredDriftFindings.length)}`,
    `waivers: ${String(freeze.waivers?.length ?? 0)}`,
    "",
  ].join("\n"),
);
