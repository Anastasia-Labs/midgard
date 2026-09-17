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
      // The ratchet block below re-enables all five for the packages that have
      // finished that migration.
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
    // The `no-unsafe-*` ratchet. A package joins this list once it has no `any`
    // reaching its own code — which in practice means routing the few
    // library-declaration leaks (`Array.isArray`, `Object.getPrototypeOf`,
    // `JSON.parse`, `new Array(n)`, `instanceof` on a class the library
    // declares with `any` type arguments) through a named guard that states the
    // honest type once. `@al-ft/midgard-core/narrowing` holds the general ones
    // and `midgard-validation/src/plutus-data-narrowing.ts` the Plutus-Data
    // ones; grep either for the pattern to copy when ratcheting the next
    // package. This list only ever grows.
    files: ["midgard-core/**/*.ts", "midgard-validation/**/*.ts"],
    rules: {
      "@typescript-eslint/no-unsafe-argument": "error",
      "@typescript-eslint/no-unsafe-assignment": "error",
      "@typescript-eslint/no-unsafe-call": "error",
      "@typescript-eslint/no-unsafe-member-access": "error",
      "@typescript-eslint/no-unsafe-return": "error",
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
      "midgard-watcher/src/indexers/settlement-indexer.ts",
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
    // Static architecture gate, reported as lint rather than as a test.
    // `lucid-midgard` surfaces every builder failure through typed Effect
    // errors (`BuilderInvariantError` and friends); an escape hatch that
    // converts a typed failure into a defect, swallows a cause, or runs an
    // Effect synchronously would silently change that contract. This used to
    // be a `readFileSync` grep inside tests/safe-program.test.ts, which
    // section 7 of docs/research/testing-best-practices.md prohibits as proof
    // of runtime behavior.
    files: ["lucid-midgard/src/**/*.ts"],
    rules: {
      "no-restricted-syntax": [
        "error",
        {
          selector:
            "MemberExpression[property.name=/^(orDie|orDieWith|catchAllDefect|catchAllCause|catchCause)$/]",
          message:
            "lucid-midgard surfaces builder failures through the typed Effect error channel. Do not convert a typed failure into a defect or swallow a Cause; add an error type instead.",
        },
        {
          selector: "Identifier[name=/^unsafeRun/]",
          message:
            "Do not run an Effect synchronously inside lucid-midgard src: return the Effect (or a Promise-shaped safe variant) and let the caller run it.",
        },
      ],
    },
  },
  {
    // Static gate over the shipped examples, reported as lint. The package is
    // a Midgard-L2 builder: an example that reaches for a Cardano L1 provider
    // (the examples do use its CML crypto primitives, which is not a provider)
    // or a local UPLC evaluator would document a submission shortcut that the
    // package does not support. tests/documentation-examples.test.ts executes
    // the examples; it must not also grep them.
    files: ["lucid-midgard/examples/**/*.ts"],
    rules: {
      "no-restricted-syntax": [
        "error",
        {
          selector:
            "Identifier[name=/^(Blockfrost|Maestro|Kupmios|Koios|Emulator)$/]",
          message:
            "The lucid-midgard examples run against the Midgard provider only. A Cardano L1 provider in an example documents a submission path the package does not implement.",
        },
      ],
    },
  },
  {
    // Static import-boundary gate for the independent watcher command
    // package, reported as lint rather than as a test. The watcher is an
    // adversarial verifier: it must never link the operator's own node, its
    // admin/database surface, or the DA committee service into its runtime.
    // This used to be a `readFileSync` grep over src inside
    // tests/runtime/scaffold.test.ts, which section 7 of
    // docs/research/testing-best-practices.md prohibits as proof of runtime
    // behavior.
    files: ["midgard-watcher/src/**/*.ts"],
    rules: {
      "no-restricted-imports": [
        "error",
        {
          patterns: [
            workspacePackageBoundary,
            {
              group: [
                "midgard-node",
                "midgard-node/*",
                "midgard-node-tools",
                "midgard-node-tools/*",
                "da-committee-node",
                "da-committee-node/*",
                "**/midgard-node/**",
                "**/midgard-node-tools/**",
                "**/da-committee-node/**",
              ],
              message:
                "The watcher verifies the operator independently: its runtime must not import the operator's node, its tooling, or the DA committee service. Model the boundary it needs (an HTTP/libp2p client, a config field) instead.",
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
