import { existsSync } from "node:fs";
import { dirname, join } from "node:path";

import { readSourceFacets } from "../../../../scripts/lib/source-facets.mjs";
import {
  arrayStrings,
  CannotLook,
  extractLiteral,
  kebab,
  listFiles,
  objectKeys,
  objectStringPairs,
  readSource,
  SOURCES,
  stripComments,
  topLevelElements,
  validatorFileCandidates,
} from "./family-checklist.top-level-elements.mjs";

export const checkFamily = (root, category) => {
  const results = [];
  const record = (id, status, detail) => results.push({ id, status, detail });

  const sdkCatalogue = readSource(root, "sdkCatalogue");
  const sdkOrder = arrayStrings(
    extractLiteral(
      sdkCatalogue,
      "FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER",
      SOURCES.sdkCatalogue,
    ),
  );
  const sdkIds = objectStringPairs(
    extractLiteral(
      sdkCatalogue,
      "FRAUD_PROOF_CATALOGUE_CATEGORY_IDS",
      SOURCES.sdkCatalogue,
    ),
  );
  if (sdkOrder.length === 0 || sdkIds.size === 0) {
    throw new CannotLook(`${SOURCES.sdkCatalogue}: catalogue tables are empty`);
  }

  if (sdkOrder.includes(category)) {
    record("catalogue-order", "ok", "in FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER");
  } else {
    record(
      "catalogue-order",
      "gap",
      `not in FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER (${SOURCES.sdkCatalogue}); check the spelling if the family already exists`,
    );
  }

  const id = sdkIds.get(category);
  if (id === undefined) {
    record(
      "catalogue-id",
      "gap",
      "no FRAUD_PROOF_CATALOGUE_CATEGORY_IDS entry",
    );
  } else if (!/^[0-9a-f]{8}$/u.test(id)) {
    record("catalogue-id", "gap", `ID "${id}" is not 8 lowercase hex digits`);
  } else {
    const sharers = [...sdkIds].filter(
      ([other, value]) => value === id && other !== category,
    );
    if (sharers.length > 0) {
      record(
        "catalogue-id",
        "gap",
        `ID ${id} is also used by ${sharers.map(([name]) => name).join(", ")}`,
      );
    } else {
      record("catalogue-id", "ok", `ID ${id}`);
    }
  }

  const core = readSource(root, "coreCatalogue");
  const coreOrder = arrayStrings(
    extractLiteral(
      core,
      "DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER",
      SOURCES.coreCatalogue,
    ),
  );
  const coreIds = objectStringPairs(
    extractLiteral(
      core,
      "DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_IDS",
      SOURCES.coreCatalogue,
    ),
  );
  const contractByCategory = objectStringPairs(
    extractLiteral(
      core,
      "DEPLOYMENT_MANIFEST_FRAUD_PROOF_CONTRACT_BY_CATEGORY",
      SOURCES.coreCatalogue,
    ),
  );
  const contractNames = arrayStrings(
    extractLiteral(
      core,
      "DEPLOYMENT_MANIFEST_CONTRACT_NAMES",
      SOURCES.coreCatalogue,
    ),
  );
  const contractByRole = objectStringPairs(
    extractLiteral(
      readSource(root, "coreReferenceContracts"),
      "DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE",
      SOURCES.coreReferenceContracts,
    ),
  );
  const tokenByRole = objectStringPairs(
    extractLiteral(
      readSource(root, "coreReferenceTokens"),
      "DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES",
      SOURCES.coreReferenceTokens,
    ),
  );
  if (contractNames.length === 0 || contractByRole.size === 0) {
    throw new CannotLook(
      `${SOURCES.coreCatalogue} / ${SOURCES.coreReferenceContracts}: identity tables are empty`,
    );
  }

  const firstContract = contractByCategory.get(category);
  const coreProblems = [];
  if (!coreOrder.includes(category)) {
    coreProblems.push(
      "not in DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER",
    );
  }
  if (coreIds.get(category) === undefined) {
    coreProblems.push(
      "no DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_IDS entry",
    );
  } else if (id !== undefined && coreIds.get(category) !== id) {
    coreProblems.push(
      `core ID ${coreIds.get(category)} differs from SDK ID ${id}`,
    );
  }
  if (firstContract === undefined) {
    coreProblems.push(
      "no DEPLOYMENT_MANIFEST_FRAUD_PROOF_CONTRACT_BY_CATEGORY entry",
    );
  }
  record(
    "core-identity",
    coreProblems.length === 0 ? "ok" : "gap",
    coreProblems.length === 0
      ? `first-step contract ${firstContract}`
      : `${coreProblems.join("; ")} (${SOURCES.coreCatalogue})`,
  );

  if (firstContract !== undefined) {
    // A family's contracts share its first-step contract's name as a prefix
    // (fraudProofMinAda, fraudProofMinAdaStep02TxWithdraw), except names that
    // belong to a longer first-step prefix of another category
    // (fraudProofNonExistentInputNoIndex is not fraudProofNonExistentInput's).
    const longerPrefixes = [...contractByCategory.values()].filter(
      (other) =>
        other !== firstContract &&
        other.length > firstContract.length &&
        other.startsWith(firstContract),
    );
    const familyContracts = contractNames.filter(
      (name) =>
        name.startsWith(firstContract) &&
        !longerPrefixes.some((other) => name.startsWith(other)),
    );
    const roleProblems = [];
    if (!familyContracts.includes(firstContract)) {
      roleProblems.push(
        `${firstContract} is not in DEPLOYMENT_MANIFEST_CONTRACT_NAMES`,
      );
    }
    let roleCount = 0;
    for (const contract of familyContracts) {
      const roles = [...contractByRole]
        .filter(([, value]) => value === contract)
        .map(([role]) => role);
      if (roles.length === 0) {
        roleProblems.push(`${contract} has no reference-script role`);
      }
      for (const role of roles) {
        roleCount += 1;
        if (!tokenByRole.has(role)) {
          roleProblems.push(
            `role "${role}" has no reference-script token name`,
          );
        }
      }
    }
    record(
      "reference-scripts",
      roleProblems.length === 0 ? "ok" : "gap",
      roleProblems.length === 0
        ? `${familyContracts.length} contract(s) named ${firstContract}*, ${roleCount} role(s), all with token names`
        : `${roleProblems.join("; ")} (${SOURCES.coreCatalogue}, ${SOURCES.coreReferenceContracts}, ${SOURCES.coreReferenceTokens})`,
    );
  } else {
    record("reference-scripts", "gap", "skipped: no first-step contract");
  }

  const build = readSource(root, "sdkBuild");
  const chainCall = new RegExp(
    `const ${category}\\s*=\\s*yield\\*\\s*(build\\w+Chain)\\b`,
    "u",
  ).exec(build);
  let familyFileStem;
  let validatorDirectories = [];
  if (chainCall === null) {
    record(
      "sdk-chain",
      "gap",
      `no \`const ${category} = yield* build...Chain\` in ${SOURCES.sdkBuild}`,
    );
  } else {
    const importLine = new RegExp(
      `import\\s*\\{[^}]*\\b${chainCall[1]}\\b[^}]*\\}\\s*from\\s*"\\./families/([\\w-]+)\\.js"`,
      "u",
    ).exec(build);
    if (importLine === null) {
      record(
        "sdk-chain",
        "gap",
        `${chainCall[1]} is not imported from ./families/`,
      );
    } else {
      familyFileStem = importLine[1];
      const familyPath = join(
        dirname(SOURCES.sdkBuild),
        "families",
        `${familyFileStem}.ts`,
      );
      if (!existsSync(join(root, familyPath))) {
        record("sdk-chain", "gap", `${familyPath} does not exist`);
      } else {
        const familyText = readSourceFacets(join(root, familyPath));
        const titles = [
          ...new Set(
            [...familyText.matchAll(/"(fraud_proofs\/[\w/]+\.[\w.]+)"/gu)].map(
              (match) => match[1],
            ),
          ),
        ];
        const missing = titles.filter(
          (title) =>
            !validatorFileCandidates(title).some((path) =>
              existsSync(join(root, path)),
            ),
        );
        validatorDirectories = [
          ...new Set(
            titles.map((title) => title.split("/")[1].replaceAll("_", "-")),
          ),
        ];
        if (titles.length === 0) {
          record(
            "sdk-chain",
            "gap",
            `${familyPath} names no fraud_proofs/ blueprint titles`,
          );
        } else if (missing.length > 0) {
          record(
            "sdk-chain",
            "gap",
            `no validator file for ${missing.join(", ")} under ${SOURCES.validators}`,
          );
        } else {
          record(
            "sdk-chain",
            "ok",
            `${chainCall[1]} in ${familyPath}; ${titles.length} blueprint title(s), each with a validator file`,
          );
        }
      }
    }
  }

  // The registry's type annotation is itself a `{...}` literal, so read the
  // value: the first `Object.freeze({` after the declaration.
  const registry = readSource(root, "registry");
  const declaration = registry.indexOf("const FAMILY_APPLICATION_REGISTRY");
  const freeze =
    declaration < 0 ? -1 : registry.indexOf("Object.freeze(", declaration);
  if (freeze < 0) {
    throw new CannotLook(
      `${SOURCES.registry}: no \`const FAMILY_APPLICATION_REGISTRY = Object.freeze(...)\` found`,
    );
  }
  const installedKeys = objectKeys(
    extractLiteral(
      `const REGISTRY_VALUE ${registry.slice(freeze + "Object.freeze(".length)}`,
      "REGISTRY_VALUE",
      SOURCES.registry,
    ),
  );
  const linearCategories = arrayStrings(
    extractLiteral(
      readSource(root, "linearSpec"),
      "LINEAR_FAMILY_CATEGORIES",
      SOURCES.linearSpec,
    ),
  );
  if (installedKeys.size === 0) {
    throw new CannotLook(`${SOURCES.registry}: registry has no keys`);
  }
  const spreadsLinear = installedKeys.has("LINEAR_FAMILY_APPLICATION_RECORDS");
  if (installedKeys.has(category)) {
    record(
      "application-record",
      "ok",
      "hand-written record in FAMILY_APPLICATION_REGISTRY",
    );
  } else if (spreadsLinear && linearCategories.includes(category)) {
    record(
      "application-record",
      "ok",
      "derived record via LINEAR_FAMILY_CATEGORIES",
    );
  } else {
    record(
      "application-record",
      "gap",
      `no FAMILY_APPLICATION_REGISTRY entry (${SOURCES.registry}); the watcher will not install it`,
    );
  }

  // Two tables must hold one row per category at its catalogue position; both
  // are checked at load by the fault-proofs package.
  const orderedRow = (id, sourceKey, constName, consequence) => {
    // A row is `{ category: "x", ... }` or a helper call such as
    // `manual("x", ...)`: take its `category:` value, else its first string.
    const rows = topLevelElements(
      extractLiteral(
        readSource(root, sourceKey),
        constName,
        SOURCES[sourceKey],
      ),
    )
      .map(stripComments)
      .map(
        (element) =>
          (/\bcategory:\s*"(\w+)"/u.exec(element) ??
            /"(\w+)"/u.exec(element))?.[1],
      );
    if (rows.length === 0) {
      throw new CannotLook(
        `${SOURCES[sourceKey]}: ${constName} names no category`,
      );
    }
    const index = rows.indexOf(category);
    const position = sdkOrder.indexOf(category);
    if (index < 0) {
      record(
        id,
        "gap",
        `no ${constName} row (${SOURCES[sourceKey]}); ${consequence}`,
      );
    } else if (index !== position) {
      record(
        id,
        "gap",
        `${constName} row ${index} but catalogue position ${position}; rows follow FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER`,
      );
    } else {
      record(id, "ok", `${constName} row ${index}, in catalogue order`);
    }
  };
  orderedRow(
    "classification-rule",
    "classification",
    "FRAUD_PROOF_CLASSIFICATION_RULES",
    "the module throws at load unless row i is catalogue category i",
  );
  orderedRow(
    "adapter-registration",
    "adapters",
    "workflowAdapterRegistrationRows",
    "validateWorkflowAdapterCoverage refuses a missing or misordered row",
  );

  const cursorKeys = objectKeys(
    extractLiteral(
      readSource(root, "definitions"),
      "CURSOR_FAMILY_DEFINITIONS",
      SOURCES.definitions,
    ),
  );
  record(
    "family-definition",
    "info",
    linearCategories.includes(category)
      ? "linear FamilyDefinition (LINEAR_FAMILY_CATEGORIES)"
      : cursorKeys.has(category)
        ? "cursor FamilyDefinition (CURSOR_FAMILY_DEFINITIONS)"
        : "none; the family uses a bespoke workflow (allowed, not assembled from a definition)",
  );

  const reasonsLiteral = extractLiteral(
    readSource(root, "reasons"),
    "TYPED_REASON_DISPOSITIONS",
    SOURCES.reasons,
  );
  const arms = [
    ...stripComments(reasonsLiteral).matchAll(
      /\b([A-Z]\w*)\s*:\s*(?:direct\(([^)]*)\)|\{[^}]*categories:\s*\[([^\]]*)\])/gu,
    ),
  ]
    .filter((match) =>
      arrayStrings(match[2] ?? match[3] ?? "").includes(category),
    )
    .map((match) => match[1]);
  record(
    "typed-reasons",
    "info",
    arms.length > 0
      ? `RejectionReason arm(s) routed here: ${arms.join(", ")}`
      : "no RejectionReason arm routes here (history or structural family)",
  );

  const status = readSource(root, "catalogueStatus");
  const rowShape =
    id === undefined
      ? null
      : new RegExp(`^\\|\\s*\`${id}\`\\s*\\|\\s*\`${category}\`\\s*\\|`, "mu");
  if (rowShape !== null && rowShape.test(status)) {
    record(
      "catalogue-status-row",
      "ok",
      `row for ${id} in ${SOURCES.catalogueStatus}`,
    );
  } else {
    record(
      "catalogue-status-row",
      "gap",
      `no \`| \`${id ?? "<id>"}\` | \`${category}\` |\` row in ${SOURCES.catalogueStatus}`,
    );
  }

  const journeys = readSource(root, "journeys");
  const owners = objectStringPairs(
    extractLiteral(journeys, "JOURNEY_FIXTURE_OWNERS", SOURCES.journeys),
  );
  const excluded = [...journeys.matchAll(/category\s*!==\s*"(\w+)"/gu)].map(
    (match) => match[1],
  );
  if (owners.size === 0) {
    throw new CannotLook(
      `${SOURCES.journeys}: JOURNEY_FIXTURE_OWNERS is empty`,
    );
  }
  if (owners.has(category)) {
    record(
      "journey-owner",
      "ok",
      `devnet journey owner "${owners.get(category)}"`,
    );
  } else if (excluded.includes(category)) {
    record(
      "journey-owner",
      "ok",
      "excluded from JOURNEY_CATEGORIES (interactive)",
    );
  } else {
    record(
      "journey-owner",
      "gap",
      `no JOURNEY_FIXTURE_OWNERS entry in ${SOURCES.journeys}`,
    );
  }

  // Test files are matched by name. A file named after a longer stem that
  // belongs to another family (reference-input-no-idx versus input-no-idx) is
  // that family's, not this one's.
  const stems = [
    ...new Set([kebab(category), familyFileStem, ...validatorDirectories]),
  ].filter((stem) => stem !== undefined && stem.length > 0);
  const otherStems = [
    ...sdkOrder.filter((other) => other !== category).map(kebab),
    ...[
      ...build.matchAll(
        /const (\w+)\s*=\s*yield\*\s*build\w+Chain[\s\S]*?(?=const \w+\s*=\s*yield\*|$)/gu,
      ),
    ]
      .filter((match) => match[1] !== category)
      .flatMap((match) => {
        const call = /yield\*\s*(build\w+Chain)/u.exec(match[0]);
        const source = new RegExp(
          `\\b${call[1]}\\b[^}]*\\}\\s*from\\s*"\\./families/([\\w-]+)\\.js"`,
          "u",
        ).exec(build);
        return source === null ? [] : [source[1]];
      }),
  ].filter((other) => !stems.includes(other));
  const namedAfter = (name, stem) =>
    new RegExp(`(^|-)${stem}(-|\\.test)`, "u").test(name);
  const testFiles = listFiles(root, SOURCES.faultProofTests)
    .filter((name) => name.endsWith(".test.ts"))
    .filter((name) =>
      stems.some(
        (stem) =>
          namedAfter(name, stem) &&
          !otherStems.some(
            (other) =>
              other.length > stem.length &&
              other.includes(stem) &&
              namedAfter(name, other),
          ),
      ),
    )
    .sort();
  if (testFiles.length === 0) {
    record(
      "tests",
      "gap",
      `no ${SOURCES.faultProofTests}/*.test.ts named after ${stems.join(" / ")} (heuristic: matched by file name)`,
    );
  } else {
    const read = (name) =>
      readSourceFacets(join(root, SOURCES.faultProofTests, name));
    const refusing = testFiles.filter((name) =>
      /expectOnchainRefusal\(|failed script execution/u.test(read(name)),
    );
    const lifecycle = testFiles.filter((name) => name.includes("lifecycle"));
    const coverage = testFiles.filter((name) =>
      /createLifecycleCoverageRecorder|assertCompleteLifecycleCoverage/u.test(
        read(name),
      ),
    );
    record(
      "tests",
      "ok",
      `${testFiles.length} test file(s) matched by name (heuristic)`,
    );
    record(
      "tests-lifecycle",
      "info",
      lifecycle.length > 0
        ? lifecycle.join(", ")
        : "no *lifecycle* test file matched",
    );
    record(
      "tests-onchain-refusal",
      "info",
      refusing.length > 0
        ? `validator-refusal assertion (expectOnchainRefusal or "failed script execution") in ${refusing.join(", ")}`
        : "no matched test asserts a validator refusal by name; negatives may live in shared suites, confirm they fail in the validator, not the builder",
    );
    record(
      "tests-lifecycle-coverage",
      "info",
      coverage.length > 0
        ? `complete-lifecycle recorder used in ${coverage.join(", ")}`
        : "no matched test uses the complete-lifecycle coverage recorder",
    );
  }

  return results;
};
