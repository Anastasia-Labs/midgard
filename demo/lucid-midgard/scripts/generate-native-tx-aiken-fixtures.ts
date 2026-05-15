import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import {
  buildHighCardinalityNativeTxFixture,
  renderHighCardinalityAikenTest,
  stableFixtureJson,
} from "../tests/fixtures/native-high-cardinality.js";

const packageRoot = path.resolve(
  path.dirname(fileURLToPath(import.meta.url)),
  "..",
);
const repoRoot = path.resolve(packageRoot, "../..");

const fixtureJsonPath = path.join(
  packageRoot,
  "tests/fixtures/native-high-cardinality.json",
);
const aikenTestPath = path.join(
  repoRoot,
  "onchain/aiken/lib/midgard/fraud-proofs/native-tx.high-cardinality.test.ak",
);

const main = async (): Promise<void> => {
  const fixture = await buildHighCardinalityNativeTxFixture();

  fs.mkdirSync(path.dirname(fixtureJsonPath), { recursive: true });
  fs.writeFileSync(fixtureJsonPath, stableFixtureJson(fixture));
  fs.writeFileSync(aikenTestPath, renderHighCardinalityAikenTest(fixture));

  console.log(`wrote ${path.relative(repoRoot, fixtureJsonPath)}`);
  console.log(`wrote ${path.relative(repoRoot, aikenTestPath)}`);
};

main().catch((error: unknown) => {
  console.error(error);
  process.exitCode = 1;
});
