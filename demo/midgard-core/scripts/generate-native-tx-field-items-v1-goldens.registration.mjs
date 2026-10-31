import { buildGolden } from "./generate-native-tx-field-items-v1-goldens.build-golden.mjs";
import {
  generatedAikenPath,
  generatedJsonPath,
  repositoryRoot,
  writeOrCheck,
} from "./generate-native-tx-field-items-v1-goldens.build-straddle.mjs";
import { renderAiken } from "./generate-native-tx-field-items-v1-goldens.render-aiken.mjs";
import { formatAikenSource } from "./golden-channel.mjs";

// ---------------------------------------------------------------------------
// Emission
// ---------------------------------------------------------------------------

const golden = buildGolden();

writeOrCheck(generatedJsonPath, `${JSON.stringify(golden, null, 2)}\n`);

writeOrCheck(
  generatedAikenPath,
  formatAikenSource({
    source: renderAiken(golden),
    fileName: "native-tx-field-items-v1-golden.test.ak",
    repositoryRoot,
    tmpPrefix: "midgardV1-569-aiken-format-",
  }),
);
