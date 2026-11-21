/**
 * The schema lint, kept out of the runtime barrel: only tests and benches
 * use it. Import from `@al-ft/midgard-l1-follower/lint`.
 */
export {
  type DeclaredTable,
  declaredTables,
  lintSchema,
  type SchemaLintProblem,
  TABLE_CLASSES,
  type TableClass,
} from "../schema/lint.js";
