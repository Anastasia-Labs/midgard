import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

import {
  buildHighCardinalityNativeTxFixture,
  buildSizeBalancedNativeTxFixture,
  renderHighCardinalityAikenTest,
  renderSizeBalancedAikenTest,
  stableFixtureJson,
} from "../tests/fixtures/native-high-cardinality.js";

const scriptPackageRoot = path.resolve(
  path.dirname(fileURLToPath(import.meta.url)),
  "..",
);
const packageRoot = fs.existsSync(
  path.join(process.cwd(), "tests/fixtures/native-high-cardinality.ts"),
)
  ? process.cwd()
  : scriptPackageRoot;
const repoRoot = path.resolve(packageRoot, "../..");

const fixtureJsonPath = path.join(
  packageRoot,
  "tests/fixtures/native-high-cardinality.json",
);
const sizeBalancedFixtureJsonPath = path.join(
  packageRoot,
  "tests/fixtures/native-size-balanced-15_5k.json",
);
const aikenTestPath = path.join(
  repoRoot,
  "onchain/aiken/lib/midgard/fraud-proofs/native-tx.high-cardinality.test.ak",
);
const sizeBalancedAikenTestPath = path.join(
  repoRoot,
  "onchain/aiken/lib/midgard/fraud-proofs/native-tx.size-balanced.test.ak",
);

const main = async (): Promise<void> => {
  const fixture = await buildHighCardinalityNativeTxFixture();
  const sizeBalancedFixture = await buildSizeBalancedNativeTxFixture();

  fs.mkdirSync(path.dirname(fixtureJsonPath), { recursive: true });
  fs.writeFileSync(fixtureJsonPath, stableFixtureJson(fixture));
  fs.writeFileSync(
    sizeBalancedFixtureJsonPath,
    stableFixtureJson(sizeBalancedFixture),
  );
  fs.writeFileSync(aikenTestPath, renderHighCardinalityAikenTest(fixture));
  fs.writeFileSync(
    sizeBalancedAikenTestPath,
    renderSizeBalancedAikenTest(sizeBalancedFixture),
  );

  console.log(`wrote ${path.relative(repoRoot, fixtureJsonPath)}`);
  console.log(`wrote ${path.relative(repoRoot, sizeBalancedFixtureJsonPath)}`);
  console.log(`wrote ${path.relative(repoRoot, aikenTestPath)}`);
  console.log(`wrote ${path.relative(repoRoot, sizeBalancedAikenTestPath)}`);
};

main().catch((error: unknown) => {
  console.error(error);
  process.exitCode = 1;
});
