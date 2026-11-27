import { defineConfig } from "tsup";

// The layer entries are ESM only. tsup splits shared code into chunks for ESM
// but not for CommonJS, so a CommonJS layer would carry its own copy of the
// classes, schemas and Effect tags the root and the other layers export. The
// root and `blueprint-abi` are also built as CommonJS, in a second step of
// `midgardTasks.build` in package.json. Only the first step passes `--dts`;
// the third, scripts/write-cjs-declarations.mjs, writes the CommonJS
// declarations from the ESM ones instead of type-checking the SDK again.
export default defineConfig({
  entry: [
    "src/index.ts",
    "src/blueprint-abi/index.ts",
    "src/layers/core.ts",
    "src/layers/state-queue.ts",
    "src/layers/da.ts",
    "src/layers/operators.ts",
    "src/layers/users.ts",
    "src/layers/deployment.ts",
    "src/fraud-proof/index.ts",
  ],
  format: ["esm"],
  dts: false,
  clean: false,
  sourcemap: true,
});
