import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import * as codec from "../dist/codec/native-tx-field-access.js";
import { buildNativeTxFieldAccessGoldenV1 } from "../tests/fixtures/native-tx-field-access-v1.vectors.mjs";
import {
  goldenChannelEmitter,
  parseGoldenChannelArguments,
} from "./golden-channel.mjs";

const scriptDirectory = dirname(fileURLToPath(import.meta.url));

const packageRoot = resolve(scriptDirectory, "..");

export const repositoryRoot = resolve(packageRoot, "../..");

export const generatedAikenPath = join(
  repositoryRoot,
  "onchain/aiken/lib/midgard/native-tx-field-access-v1-golden.test.ak",
);

const { checkOnly } = parseGoldenChannelArguments(
  "usage: node scripts/generate-native-tx-field-access-v1-goldens.mjs [--check]",
);

export const writeOrCheck = goldenChannelEmitter({ repositoryRoot, checkOnly });

/** The vectors, run through the built twin. */
export const buildGolden = () => buildNativeTxFieldAccessGoldenV1(codec);

// ---------------------------------------------------------------------------
// Aiken rendering
// ---------------------------------------------------------------------------

export const section = (title) => [
  "// ---------------------------------------------------------------------------",
  `// ${title}`,
  "// ---------------------------------------------------------------------------",
  "",
];

export const headerLengthForCount = (count) =>
  count <= 23 ? 1 : count <= 255 ? 2 : 3;
