import eslint from "@eslint/js";
import simpleImportSort from "eslint-plugin-simple-import-sort";
import globals from "globals";
import tseslint from "typescript-eslint";

// Workspace packages are consumed by name so that their exports maps, the
// `midgard-source` exports condition (source-first resolution for tsc,
// typescript-eslint, and vitest), and the declared dependency graph stay
// authoritative. Reaching into a sibling's src/ or dist/ bypasses all three;
// the src/dist split is how stale-dist phantom suite failures were produced.
const workspacePackageBoundary = {
  group: [
    "**/lucid-midgard/src/**",
    "**/lucid-midgard/dist/**",
    "**/midgard-*/src/**",
    "**/midgard-*/dist/**",
    "**/da-committee-node/src/**",
    "**/da-committee-node/dist/**",
  ],
  message:
    "Import workspace packages by name (for example @al-ft/midgard-core/hex), never through ../<package>/src or ../<package>/dist. Add a workspace dependency and an exports entry if one is missing.",
};

export default tseslint.config(
  {
    ignores: [
      "**/coverage/**",
      "**/dist/**",
      "**/logs/**",
      "**/node_modules/**",
      "**/.architecture-f-wasm/**",
      "**/.probe-dist/**",
      "**/.tmp/**",
    ],
  },
  eslint.configs.recommended,
  {
    languageOptions: {
      globals: globals.node,
    },
    rules: {
      "no-unused-vars": [
        "error",
        {
          argsIgnorePattern: "^_",
          caughtErrorsIgnorePattern: "^_",
          destructuredArrayIgnorePattern: "^_",
          ignoreRestSiblings: true,
        },
      ],
      "no-restricted-imports": [
        "error",
        { patterns: [workspacePackageBoundary] },
      ],
    },
  },
  {
    files: ["**/*.{ts,tsx}"],
    extends: [...tseslint.configs.recommendedTypeChecked],
    languageOptions: {
      parserOptions: {
        projectService: {
          allowDefaultProject: [
            "lucid-midgard/tsup.config.ts",
            "lucid-midgard/vitest.config.ts",
            "midgard-core/vitest.config.ts",
            "midgard-fault-proofs/vitest.config.ts",
            "midgard-node/tsup.config.ts",
            "midgard-node/vitest.bench.config.ts",
            "midgard-node/vitest.config.ts",
            "midgard-node-tools/tsup.config.ts",
            "midgard-node-tools/vitest.config.ts",
            "midgard-sdk/tsup.config.ts",
            "midgard-sdk/vitest.config.ts",
            "midgard-validation/vitest.config.ts",
          ],
          maximumDefaultProjectFileMatchCount_THIS_WILL_SLOW_DOWN_LINTING: 32,
        },
        tsconfigRootDir: import.meta.dirname,
      },
    },
    plugins: {
      "simple-import-sort": simpleImportSort,
    },
    rules: {
      "no-unused-vars": "off",
      // This rule misclassifies noUncheckedIndexedAccess refinements and Lucid
      // schema bridges; its autofixes make the workspace fail typechecking.
      "@typescript-eslint/no-unnecessary-type-assertion": "off",
      // The protocol uses branded and structurally overlapping Lucid/libp2p
      // types intentionally; simplifying those unions changes public types.
      "@typescript-eslint/no-redundant-type-constituents": "off",
      "@typescript-eslint/no-duplicate-type-constituents": "off",
      // Legacy Lucid, Effect, Commander, and decoded-JSON boundaries infer
      // `any` despite runtime validation. Keep explicit `any` prohibited while
      // those library boundaries are migrated to `unknown` incrementally.
      "@typescript-eslint/no-unsafe-argument": "off",
      "@typescript-eslint/no-unsafe-assignment": "off",
      "@typescript-eslint/no-unsafe-call": "off",
      "@typescript-eslint/no-unsafe-member-access": "off",
      "@typescript-eslint/no-unsafe-return": "off",
      // Effect generators and static library methods trigger these rules even
      // when no dynamic `this` binding or yielded value is required.
      "@typescript-eslint/unbound-method": "off",
      "require-yield": "off",
      // Async interface implementations and test doubles intentionally return
      // promises without always awaiting within the implementation.
      "@typescript-eslint/require-await": "off",
      "@typescript-eslint/no-unused-vars": [
        "error",
        {
          argsIgnorePattern: "^_",
          caughtErrorsIgnorePattern: "^_",
          destructuredArrayIgnorePattern: "^_",
          ignoreRestSiblings: true,
        },
      ],
      "@typescript-eslint/restrict-template-expressions": [
        "error",
        { allowBoolean: true },
      ],
      "@typescript-eslint/switch-exhaustiveness-check": "error",
      "simple-import-sort/exports": "error",
      "simple-import-sort/imports": "error",
    },
  },
  {
    files: ["**/tests/**/*.ts", "**/*.test.ts"],
    rules: {
      // Test fixtures deliberately inspect malformed/untyped external data and
      // Vitest assertions routinely reference methods without invoking them.
      "@typescript-eslint/no-explicit-any": "off",
      "@typescript-eslint/no-base-to-string": "off",
      "@typescript-eslint/no-unsafe-enum-comparison": "off",
      "@typescript-eslint/no-unsafe-function-type": "off",
      "@typescript-eslint/only-throw-error": "off",
      "@typescript-eslint/prefer-promise-reject-errors": "off",
      "@typescript-eslint/unbound-method": "off",
    },
  },
  {
    files: [
      "midgard-node/src/index.ts",
      "midgard-node-tools/src/index.ts",
      "midgard-watcher/src/settlement-indexer.ts",
    ],
    rules: {
      // Commander and the legacy settlement decoder necessarily expose
      // explicit `any` in their callback/decoder adapter signatures.
      "@typescript-eslint/no-explicit-any": "off",
    },
  },
  {
    files: ["midgard-sdk/src/**/*.ts"],
    rules: {
      "no-restricted-imports": [
        "error",
        {
          patterns: [
            workspacePackageBoundary,
            {
              group: ["@/*"],
              message:
                "midgard-sdk src is resolved from source by every consumer through the midgard-source exports condition. The @/ alias exists only in this package's tsconfig/vitest config, so src must use relative specifiers.",
            },
          ],
        },
      ],
    },
  },
  {
    files: ["midgard-node/**/*.ts", "midgard-node-tools/**/*.ts"],
    rules: {
      "no-restricted-imports": [
        "error",
        {
          patterns: [
            workspacePackageBoundary,
            {
              group: ["@/*"],
              message:
                "midgard-node is compiled from source by midgard-node-tools through the midgard-source exports condition, so neither package has a @/ alias: use relative specifiers inside a package and `midgard-node/<subpath>` from the tooling package.",
            },
          ],
        },
      ],
    },
  },
);
